// SPDX-License-Identifier: GPL-2.0+
/*
 * HWMON driver for ASUS motherboards that publish some sensor values
 * via the embedded controller registers
 *
 * Copyright (C) 2021 Eugene Shalygin <eugene.shalygin@gmail.com>
 * Heavily based on the asus-wmi-sensors code by Ed Brindley
 */
#define pr_fmt(fmt) KBUILD_MODNAME ": " fmt

#include <linux/dmi.h>
#include <linux/hwmon.h>
#include <linux/hwmon-sysfs.h>
#include <linux/init.h>
#include <linux/jiffies.h>
#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/mutex.h>
#include <linux/platform_device.h>
#include <linux/wmi.h>

MODULE_AUTHOR("Eugene Shalygin <eugene.shalygin@gmail.com>");
MODULE_DESCRIPTION("Asus WMI Sensors Driver for AMD 500s chipsets");
MODULE_LICENSE("GPL");
MODULE_VERSION("1");

#define ASUSWMI_MGMT2_GUID "466747A0-70EC-11DE-8A39-0800200C9A66"
#define METHODID_BLOCK_READ_EC 0x42524543 /* BREC */

#define ASUS_WMI_BLOCK_READ_REGISTERS_MAX 0x10 /* from the ASUS DSDT source */
/* from the ASUS_WMI_BLOCK_READ_REGISTERS_MAX value */
#define ASUS_WMI_MAX_BUF_LEN 0x80
#define MAX_SENSOR_LABEL_LENGTH 0x10

#define ASUS_EC_SENSORS_MAX 11
#define ASUS_EC_KNOWN_EC_REGISTERS 14

typedef union {
	u32 value;
	struct {
		u8 index;
		u8 bank;
		u8 size;
		u8 dummy;
	} addr;
} sensor_address;

static inline sensor_address make_sensor_address(u8 size, u8 bank, u8 index)
{
	sensor_address res;
	res.value = (size << 16) + (bank << 8) + index;
	return res;
}

typedef u8 board_t;
enum board {
	BOARD_PW_X570_A, // Pro WS X570-ACE
	BOARD_R_C8H, // ROG Crosshair VIII Hero
	BOARD_R_C8DH, // ROG Crosshair VIII Dark Hero
	BOARD_R_C8F, // ROG Crosshair VIII Formula
	BOARD_RS_X570_E_G, // ROG STRIX X570-E GAMING
	BOARD_RS_B550_E_G, // ROG STRIX B550-E GAMING
};

#define DMI_EXACT_MATCH_ASUS_BOARD_NAME(name) \
	{ .matches = { \
		DMI_EXACT_MATCH(DMI_BOARD_VENDOR, "ASUSTeK COMPUTER INC."), \
		DMI_EXACT_MATCH(DMI_BOARD_NAME, name), \
	}}
/*
 * keep in the same order as the board enum
 */
static struct dmi_system_id asus_wmi_ec_dmi_table[] __initdata = {
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("Pro WS X570-ACE"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG CROSSHAIR VIII HERO"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG CROSSHAIR VIII DARK HERO"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG CROSSHAIR VIII FORMULA"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG STRIX X570-E GAMING"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG STRIX B550-E GAMING"),
	{}
};

static u32 hwmon_attributes[] = {
	[hwmon_chip] = HWMON_C_REGISTER_TZ,
	[hwmon_temp] = HWMON_T_INPUT | HWMON_T_LABEL,
	[hwmon_in] = HWMON_I_INPUT | HWMON_I_LABEL,
	[hwmon_curr] = HWMON_C_INPUT | HWMON_C_LABEL,
	[hwmon_fan] = HWMON_F_INPUT | HWMON_F_LABEL,
};

struct sensor_info {
	char label[MAX_SENSOR_LABEL_LENGTH];
	enum hwmon_sensor_types type;
	sensor_address addr;
	u32 cached_value;
};

struct ec_info {
	struct sensor_info sensors[ASUS_EC_SENSORS_MAX];
	/* UTF-16 string to pass to BRxx() WMI function */
	char read_arg[((ASUS_WMI_BLOCK_READ_REGISTERS_MAX * 4) + 1) * 2];
	u8 read_buffer[ASUS_WMI_BLOCK_READ_REGISTERS_MAX];
	u8 nr_sensors; /* number of board EC sensors */
	/* number of EC registers to read (sensor might span more than 1 register) */
	u8 nr_registers;
	unsigned long last_updated; /* in jiffies */
};

struct asus_ec_sensors {
	struct platform_driver platform_driver;
	struct platform_device *platform_device;

	struct mutex lock;
	struct ec_info ec;

	board_t board;
	u8 buffer;
};

static inline void set_sensor_info(struct sensor_info *sensor_info,
				   const char *label,
				   enum hwmon_sensor_types type,
				   sensor_address addr, u8 *nr_regs)
{
	sensor_info->type = type;
	strcpy(sensor_info->label, label);
	sensor_info->cached_value = 0;
	sensor_info->addr.value = addr.value;
	*nr_regs += sensor_info->addr.addr.size;
}

static void fill_board_sensors(struct ec_info *ec, board_t board)
{
	struct sensor_info *si;

	si = ec->sensors;
	ec->nr_registers = 0;

	switch (board) {
	case BOARD_PW_X570_A:
	case BOARD_RS_B550_E_G:
	case BOARD_RS_X570_E_G:
	case BOARD_R_C8H:
	case BOARD_R_C8DH:
	case BOARD_R_C8F:
		set_sensor_info(si++, "Chipset", hwmon_temp,
				make_sensor_address(1, 0x00, 0x3A),
				&ec->nr_registers);
		set_sensor_info(si++, "CPU", hwmon_temp,
				make_sensor_address(1, 0x00, 0x3B),
				&ec->nr_registers);
		set_sensor_info(si++, "Motherboard", hwmon_temp,
				make_sensor_address(1, 0x00, 0x3C),
				&ec->nr_registers);
		set_sensor_info(si++, "VRM", hwmon_temp,
				make_sensor_address(1, 0x00, 0x3E),
				&ec->nr_registers);
	}

	switch (board) {
	case BOARD_RS_B550_E_G:
	case BOARD_RS_X570_E_G:
	case BOARD_R_C8H:
	case BOARD_R_C8DH:
	case BOARD_R_C8F:
		set_sensor_info(si++, "T_Sensor", hwmon_temp,
				make_sensor_address(1, 0x00, 0x3D),
				&ec->nr_registers);
	}

	switch (board) {
	case BOARD_RS_X570_E_G:
	case BOARD_R_C8H:
	case BOARD_R_C8DH:
	case BOARD_R_C8F:
		set_sensor_info(si++, "CPU_Opt", hwmon_fan,
				make_sensor_address(2, 0x00, 0xB0),
				&ec->nr_registers);
		set_sensor_info(si++, "CPU", hwmon_curr,
				make_sensor_address(1, 0x00, 0xF4),
				&ec->nr_registers);
	}

	switch (board) {
	case BOARD_PW_X570_A:
	case BOARD_RS_X570_E_G:
	case BOARD_R_C8H:
	case BOARD_R_C8F:
		set_sensor_info(si++, "Chipset", hwmon_fan,
				make_sensor_address(2, 0x00, 0xB4),
				&ec->nr_registers);
	}

	switch (board) {
	case BOARD_R_C8H:
	case BOARD_R_C8DH:
	case BOARD_R_C8F:
		set_sensor_info(si++, "Water", hwmon_fan,
				make_sensor_address(2, 0x00, 0xBC),
				&ec->nr_registers);
		set_sensor_info(si++, "Water_In", hwmon_temp,
				make_sensor_address(1, 0x01, 0x00),
				&ec->nr_registers);
		set_sensor_info(si++, "Water_Out", hwmon_temp,
				make_sensor_address(1, 0x01, 0x01),
				&ec->nr_registers);
	}

	ec->nr_sensors = si - ec->sensors;
}

static int get_version(u32 *version)
{
	/* we know only a single version so far */
	*version = 0;
	return 0;
}

/*
 * The next four functions converts to/from BRxx string argument format
 * The format of the string is as follows:
 * The string consists of two-byte UTF-16 characters
 * The value of the very first byte int the string is equal to the total length
 * of the next string in bytes, thus excluding the first two-byte character
 * The rest of the string encodes pairs of (bank, index) pairs, where both
 * values are byte-long (0x00 to 0xFF)
 * Numbers are encoded as UTF-16 hex values
*/

static inline char *hex_utf_16_le_pack(char *buf, u8 byte)
{
	*buf++ = hex_asc_hi(byte);
	*buf++ = 0;
	*buf++ = hex_asc_lo(byte);
	*buf++ = 0;
	return buf;
}

static void asus_wmi_decode_reply_buffer(const u8 *inp, u8 *out)
{
	u8 len = ACPI_MIN(ASUS_WMI_MAX_BUF_LEN, inp[0] / 4);
	const u8 *data = inp + 2;
	u8 i;

	for (i = 0; i < len; ++i, data += 4) {
		out[i] = (hex_to_bin(data[0]) << 4) + hex_to_bin(data[2]);
	}
}

static void asus_wmi_encode_registers(u16 *registers, u8 len, char *out)
{
	u8 i;

	// assert(len <= 30)
	*out++ = len * 8;
	*out++ = 0;
	for (i = 0; i < len; ++i) {
		out = hex_utf_16_le_pack(out, (registers[i] & 0xFF00) >> 8);
		out = hex_utf_16_le_pack(out, (registers[i] & 0x00FF));
	}
}

static void make_asus_wmi_block_read_query(struct ec_info *ec)
{
	u16 registers[ASUS_EC_KNOWN_EC_REGISTERS];
	u8 i, j, register_idx = 0;
	/* if we can get values for all the registers in a single query,
	 * the query will not change from call to call */
	if (ec->nr_registers <= ASUS_WMI_BLOCK_READ_REGISTERS_MAX &&
	    ec->read_arg[0] > 0) {
		/* no need to update */
		return;
	}

	for (i = 0; i < ec->nr_sensors; ++i) {
		for (j = 0; j < ec->sensors[i].addr.addr.size;
		     ++j, ++register_idx) {
			registers[register_idx] =
				(ec->sensors[i].addr.addr.bank << 8) +
				ec->sensors[i].addr.addr.index + j;
		}
	}

	asus_wmi_encode_registers(registers, ec->nr_registers, ec->read_arg);
}

static int asus_ec_block_read(u32 method_id, const char *query, u8 *out)
{
	struct acpi_buffer input;
	struct acpi_buffer output = { ACPI_ALLOCATE_BUFFER,
				      NULL }; // TODO use pre-allocated buffer
	acpi_status status;
	union acpi_object *obj;

	/* the first byte of the BRxx() argument string has to be the string size */
	input.length = (acpi_size)query[0] + 2;
	input.pointer = (void *)query;
	status = wmi_evaluate_method(ASUSWMI_MGMT2_GUID, 0, method_id, &input,
				     &output);
	if (ACPI_FAILURE(status)) {
		acpi_os_free(output.pointer);
		return -EIO;
	}

	obj = output.pointer;
	if (!obj || obj->type != ACPI_TYPE_BUFFER) {
		pr_err("unexpected reply type from ASUS ACPI code");
		acpi_os_free(output.pointer);
		return -EIO;
	}
	asus_wmi_decode_reply_buffer(obj->buffer.pointer, out);
	acpi_os_free(output.pointer);
	return 0;
}

static int update_ec_sensors(struct ec_info *ec)
{
	struct sensor_info *si;
	u32 value;
	int status;
	u8 i_sensor, read_reg_ct, i_sensor_register;

	make_asus_wmi_block_read_query(ec);
	status = asus_ec_block_read(METHODID_BLOCK_READ_EC, ec->read_arg,
				    ec->read_buffer);
	if (status)
		return status;

	read_reg_ct = 0;
	for (i_sensor = 0; i_sensor < ec->nr_sensors; ++i_sensor) {
		si = &ec->sensors[i_sensor];
		value = ec->read_buffer[read_reg_ct++];
		for (i_sensor_register = 1;
		     i_sensor_register < si->addr.addr.size;
		     ++i_sensor_register) {
			value <<= 8;
			value += ec->read_buffer[read_reg_ct++];
		}
		si->cached_value = value;
	}
	return 0;
}

static int scale_sensor_value(u32 value, int data_type)
{
	switch (data_type) {
	case hwmon_curr:
	case hwmon_temp:
	case hwmon_in:
		return value * 1000;
	default:
		return value;
	}
}

static u8 find_ec_sensor_index(const struct ec_info *ec,
			       enum hwmon_sensor_types type, int channel)
{
	u8 i;

	for (i = 0; i < ec->nr_sensors; ++i) {
		if (ec->sensors[i].type == type) {
			if (channel == 0) {
				return i;
			}
			--channel;
		}
	}
	return 0xFF;
}

static int get_cached_value_or_update(int sensor_index,
				      struct asus_ec_sensors *state, u32 *value)
{
	int ret;
	if (time_after(jiffies, state->ec.last_updated + HZ)) {
		ret = update_ec_sensors(&state->ec);

		if (ret) {
			pr_err("update_ec_sensors() failure\n");
			return -EIO;
		}

		state->ec.last_updated = jiffies;
	}

	*value = state->ec.sensors[sensor_index].cached_value;
	return 0;
}

/*
 * Now follow the functions that implement the hwmon interface
 */

static int asus_wmi_hwmon_read(struct device *dev, enum hwmon_sensor_types type,
			       u32 attr, int channel, long *val)
{
	int ret;
	u32 value = 0;

	struct asus_ec_sensors *state = dev_get_drvdata(dev);
	u8 sidx = find_ec_sensor_index(&state->ec, type, channel);

	mutex_lock(&state->lock);

	ret = get_cached_value_or_update(sidx, state, &value);
	mutex_unlock(&state->lock);

	if (!ret) {
		*val = scale_sensor_value(value, state->ec.sensors[sidx].type);
	}

	return ret;
}

static int asus_wmi_hwmon_read_string(struct device *dev,
				      enum hwmon_sensor_types type, u32 attr,
				      int channel, const char **str)
{
	struct asus_ec_sensors *state = dev_get_drvdata(dev);
	u8 sensor_index = find_ec_sensor_index(&state->ec, type, channel);
	*str = state->ec.sensors[sensor_index].label;

	return 0;
}

static umode_t asus_wmi_hwmon_is_visible(const void *drvdata,
					 enum hwmon_sensor_types type, u32 attr,
					 int channel)
{
	const struct asus_ec_sensors *state = drvdata;
	return find_ec_sensor_index(&state->ec, type, channel) != 0xFF ?
			     S_IRUGO :
			     0;
}

static int
asus_wmi_hwmon_add_chan_info(struct hwmon_channel_info *asus_wmi_hwmon_chan,
			     struct device *dev, int num,
			     enum hwmon_sensor_types type, u32 config)
{
	int i;
	u32 *cfg = devm_kcalloc(dev, num + 1, sizeof(*cfg), GFP_KERNEL);

	if (!cfg)
		return -ENOMEM;

	asus_wmi_hwmon_chan->type = type;
	asus_wmi_hwmon_chan->config = cfg;
	for (i = 0; i < num; i++, cfg++)
		*cfg = config;

	return 0;
}

static const struct hwmon_ops asus_wmi_hwmon_ops = {
	.is_visible = asus_wmi_hwmon_is_visible,
	.read = asus_wmi_hwmon_read,
	.read_string = asus_wmi_hwmon_read_string,
};

static struct hwmon_chip_info asus_wmi_chip_info = {
	.ops = &asus_wmi_hwmon_ops,
	.info = NULL,
};

static int __init supported_board_index(void)
{
	const struct dmi_system_id* dmi_entry;
	u32 version = 0;

	dmi_entry = dmi_first_match(asus_wmi_ec_dmi_table);
	if (!dmi_entry) {
		pr_info("Unsupported board");
		return -ENODEV;
	}

	if (get_version(&version)) {
		pr_err("Error getting version\n");
		return -ENODEV;
	}

	return dmi_entry - asus_wmi_ec_dmi_table;
}

static int __init configure_sensor_setup(struct asus_ec_sensors *asus_ec_sensors)
{
	int i;
	int nr_count[hwmon_max] = { 0 }, nr_types = 0;
	struct device *hwdev;
	struct device *dev = &asus_ec_sensors->platform_device->dev;
	struct hwmon_channel_info *asus_wmi_hwmon_chan;
	const struct hwmon_channel_info **ptr_asus_wmi_ci;
	const struct hwmon_chip_info *chip_info;
	const struct sensor_info *si;
	enum hwmon_sensor_types type;

	asus_ec_sensors->board = supported_board_index();
	if (asus_ec_sensors->board < 0) {
		return -ENODEV;
	}

	asus_ec_sensors->buffer = -1;
	mutex_init(&asus_ec_sensors->lock);

	fill_board_sensors(&asus_ec_sensors->ec, asus_ec_sensors->board);

	for (i = 0; i < asus_ec_sensors->ec.nr_sensors; ++i) {
		si = &asus_ec_sensors->ec.sensors[i];
		if (!nr_count[si->type])
			++nr_types;
		++nr_count[si->type];
	}

	if (nr_count[hwmon_temp])
		nr_count[hwmon_chip]++, nr_types++;

	asus_wmi_hwmon_chan = devm_kcalloc(
		dev, nr_types, sizeof(*asus_wmi_hwmon_chan), GFP_KERNEL);
	if (!asus_wmi_hwmon_chan)
		return -ENOMEM;

	ptr_asus_wmi_ci = devm_kcalloc(dev, nr_types + 1,
				       sizeof(*ptr_asus_wmi_ci), GFP_KERNEL);
	if (!ptr_asus_wmi_ci)
		return -ENOMEM;

	asus_wmi_chip_info.info = ptr_asus_wmi_ci;
	chip_info = &asus_wmi_chip_info;

	for (type = 0; type < hwmon_max; ++type) {
		if (!nr_count[type])
			continue;

		asus_wmi_hwmon_add_chan_info(asus_wmi_hwmon_chan, dev,
					     nr_count[type], type,
					     hwmon_attributes[type]);
		*ptr_asus_wmi_ci++ = asus_wmi_hwmon_chan++;
	}

	pr_info("board has %d EC sensors that span %d registers",
		asus_ec_sensors->ec.nr_sensors,
		asus_ec_sensors->ec.nr_registers);

	hwdev = devm_hwmon_device_register_with_info(
		dev, "asus-wmi-ec-sensors", asus_ec_sensors, chip_info, NULL);

	return PTR_ERR_OR_ZERO(hwdev);
}

static struct platform_device *asus_wmi_sensors_platform_device;

static int asus_wmi_probe(struct platform_device *pdev)
{
	struct asus_ec_sensors* state = platform_get_drvdata(pdev);

	if (state->board < 0) {
		return -ENODEV;
	}

	if (!wmi_has_guid(ASUSWMI_MGMT2_GUID)) {
		pr_info("ASUSHW GUID not found\n");
		return -ENODEV;
	}

	pr_info("driver loaded\n");
	return 0;
}

static struct platform_driver asus_wmi_sensors_platform_driver = {
	.driver = {
		.name	= "asus-wmi-ec-sensors",
	},
	.probe		= asus_wmi_probe
};

MODULE_DEVICE_TABLE(dmi, asus_wmi_ec_dmi_table);

static int __init asus_wmi_ec_init(void)
{
	struct asus_ec_sensors *state;

	asus_wmi_sensors_platform_device =
		platform_create_bundle(&asus_wmi_sensors_platform_driver,
				       asus_wmi_probe, NULL, 0, NULL, 0);

	if (IS_ERR(asus_wmi_sensors_platform_device))
		return PTR_ERR(asus_wmi_sensors_platform_device);

	state = devm_kzalloc(&asus_wmi_sensors_platform_device->dev,
			     sizeof(struct asus_ec_sensors), GFP_KERNEL);

	if (!state)
		return -ENOMEM;

	state->platform_device = asus_wmi_sensors_platform_device;
	state->platform_driver = asus_wmi_sensors_platform_driver;

	platform_set_drvdata(state->platform_device, state);

	return configure_sensor_setup(state);
}

static void __exit asus_wmi_ec_exit(void)
{
	platform_device_unregister(asus_wmi_sensors_platform_device);
	platform_driver_unregister(&asus_wmi_sensors_platform_driver);
}

module_init(asus_wmi_ec_init);
module_exit(asus_wmi_ec_exit);

// kate: tab-width 8; indent-width 8;
