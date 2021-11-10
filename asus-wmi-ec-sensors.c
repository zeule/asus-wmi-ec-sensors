// SPDX-License-Identifier: GPL-2.0+
/*
 * HWMON driver for ASUS motherboards that publish some sensor values
 * via the embedded controller registers
 *
 * Copyright (C) 2021 Eugene Shalygin <eugene.shalygin@gmail.com>
 * Heavily based on the asus-wmi-sensors code by Ed Brindley
 */

#include <asm/unaligned.h>
#include <linux/dev_printk.h>
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

#define ASUSWMI_MGMT2_GUID "466747A0-70EC-11DE-8A39-0800200C9A66"
#define METHODID_BLOCK_READ_EC 0x42524543 /* BREC */

#define ASUS_WMI_BLOCK_READ_REGISTERS_MAX 0x10 /* from the ASUS DSDT source */
/* from the ASUS_WMI_BLOCK_READ_REGISTERS_MAX value */
#define ASUS_WMI_MAX_BUF_LEN 0x80
#define MAX_SENSOR_LABEL_LENGTH 0x10

typedef union {
	u32 value;
	struct {
		u8 index;
		u8 bank;
		u8 size;
		u8 dummy;
	} components;
} sensor_address;

#define MAKE_SENSOR_ADDRESS(size, bank, index)                                 \
	{                                                                      \
		.value = (size << 16) + (bank << 8) + index                    \
	}

typedef u8 board_t;
enum board
{
	BOARD_PW_X570_A,				 // Pro WS X570-ACE
	BOARD_R_C8H,					 // ROG Crosshair VIII Hero
	BOARD_R_C8DH,					 // ROG Crosshair VIII Dark Hero
	BOARD_R_C8F,					 // ROG Crosshair VIII Formula
	BOARD_RS_B550_E_G,				 // ROG STRIX B550-E GAMING
	BOARD_RS_X570_E_G,				 // ROG STRIX X570-E GAMING
	BOARD_ROG_CROSSHAIR_VIII_IMPACT, // ROG CROSSHAIR VIII IMPACT
	BOARD_MAX
};

#define DMI_EXACT_MATCH_ASUS_BOARD_NAME(name)                                  \
	{                                                                      \
		.matches = {                                                   \
			DMI_EXACT_MATCH(DMI_BOARD_VENDOR,                      \
					"ASUSTeK COMPUTER INC."),              \
			DMI_EXACT_MATCH(DMI_BOARD_NAME, name),                 \
		}                                                              \
	}
/*
 * keep in the same order as the board enum
 */
static const struct dmi_system_id asus_wmi_ec_dmi_table[] __initdata = {
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("Pro WS X570-ACE"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG CROSSHAIR VIII HERO"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG CROSSHAIR VIII DARK HERO"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG CROSSHAIR VIII FORMULA"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG STRIX B550-E GAMING"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG STRIX X570-E GAMING"),
	DMI_EXACT_MATCH_ASUS_BOARD_NAME("ROG CROSSHAIR VIII IMPACT"),
	{}
};

static u32 hwmon_attributes[] = {
	[hwmon_chip] = HWMON_C_REGISTER_TZ,
	[hwmon_temp] = HWMON_T_INPUT | HWMON_T_LABEL,
	[hwmon_in] = HWMON_I_INPUT | HWMON_I_LABEL,
	[hwmon_curr] = HWMON_C_INPUT | HWMON_C_LABEL,
	[hwmon_fan] = HWMON_F_INPUT | HWMON_F_LABEL,
};

struct ec_sensor_info {
	char label[MAX_SENSOR_LABEL_LENGTH];
	enum hwmon_sensor_types type;
	sensor_address addr;
};

#define EC_SENSOR(sensor_label, sensor_type, size, bank, index)                \
	{                                                                      \
		.label = sensor_label, .type = sensor_type,                    \
		.addr = MAKE_SENSOR_ADDRESS(size, bank, index)                 \
	}

enum known_ec_sensor {
	SENSOR_TEMP_CHIPSET,
	SENSOR_TEMP_CPU,
	SENSOR_TEMP_MB,
	SENSOR_TEMP_T_SENSOR,
	SENSOR_TEMP_VRM,
	SENSOR_FAN_CPU_OPT,
	SENSOR_FAN_CHIPSET,
	SENSOR_FAN_WATER_FLOW,
	SENSOR_CURR_CPU,
	SENSOR_TEMP_WATER_IN,
	SENSOR_TEMP_WATER_OUT,
	SENSOR_MAX
};

/*
 * All the known sensors for ASUS EC controllers
 */
static const struct ec_sensor_info known_ec_sensors[] = {
	[SENSOR_TEMP_CHIPSET] = EC_SENSOR("Chipset", hwmon_temp, 1, 0x00, 0x3a),
	[SENSOR_TEMP_CPU] = EC_SENSOR("CPU", hwmon_temp, 1, 0x00, 0x3b),
	[SENSOR_TEMP_MB] = EC_SENSOR("Motherboard", hwmon_temp, 1, 0x00, 0x3c),
	[SENSOR_TEMP_T_SENSOR] =
		EC_SENSOR("T_Sensor", hwmon_temp, 1, 0x00, 0x3d),
	[SENSOR_TEMP_VRM] = EC_SENSOR("VRM", hwmon_temp, 1, 0x00, 0x3e),
	[SENSOR_FAN_CPU_OPT] = EC_SENSOR("CPU_Opt", hwmon_fan, 2, 0x00, 0xb0),
	[SENSOR_FAN_CHIPSET] = EC_SENSOR("Chipset", hwmon_fan, 2, 0x00, 0xb4),
	[SENSOR_FAN_WATER_FLOW] =
		EC_SENSOR("Water_Flow", hwmon_fan, 2, 0x00, 0xbc),
	[SENSOR_CURR_CPU] = EC_SENSOR("CPU", hwmon_curr, 1, 0x00, 0xf4),
	[SENSOR_TEMP_WATER_IN] =
		EC_SENSOR("Water_In", hwmon_temp, 1, 0x01, 0x00),
	[SENSOR_TEMP_WATER_OUT] =
		EC_SENSOR("Water_Out", hwmon_temp, 1, 0x01, 0x01),
};

static const enum known_ec_sensor known_board_sensors[BOARD_MAX][SENSOR_MAX + 1] = {
	[BOARD_PW_X570_A] = {
		SENSOR_TEMP_CHIPSET, SENSOR_TEMP_CPU, SENSOR_TEMP_MB, SENSOR_TEMP_VRM,
		SENSOR_FAN_CHIPSET,
		SENSOR_CURR_CPU,
		SENSOR_MAX
	},
	[BOARD_R_C8H] = {
		SENSOR_TEMP_CHIPSET, SENSOR_TEMP_CPU, SENSOR_TEMP_MB,
		SENSOR_TEMP_T_SENSOR, SENSOR_TEMP_VRM,
		SENSOR_TEMP_WATER_IN, SENSOR_TEMP_WATER_OUT,
		SENSOR_FAN_CPU_OPT, SENSOR_FAN_CHIPSET, SENSOR_FAN_WATER_FLOW,
		SENSOR_CURR_CPU,
		SENSOR_MAX
	},
	[BOARD_R_C8DH] = { /* Same as Hero but without chipset fan */
		SENSOR_TEMP_CHIPSET, SENSOR_TEMP_CPU, SENSOR_TEMP_MB,
		SENSOR_TEMP_T_SENSOR, SENSOR_TEMP_VRM,
		SENSOR_TEMP_WATER_IN, SENSOR_TEMP_WATER_OUT,
		SENSOR_FAN_CPU_OPT, SENSOR_FAN_WATER_FLOW,
		SENSOR_CURR_CPU,
		SENSOR_MAX
	},
	[BOARD_R_C8F] = { /* Same as Hero but without water */
		SENSOR_TEMP_CHIPSET, SENSOR_TEMP_CPU, SENSOR_TEMP_MB,
		SENSOR_TEMP_T_SENSOR, SENSOR_TEMP_VRM,
		SENSOR_FAN_CPU_OPT, SENSOR_FAN_CHIPSET,
		SENSOR_CURR_CPU,
		SENSOR_MAX
	},
	[BOARD_RS_B550_E_G] = {
		SENSOR_TEMP_CHIPSET, SENSOR_TEMP_CPU, SENSOR_TEMP_MB,
		SENSOR_TEMP_T_SENSOR, SENSOR_TEMP_VRM,
		SENSOR_FAN_CPU_OPT,
		SENSOR_CURR_CPU,
		SENSOR_MAX
	},
	[BOARD_RS_X570_E_G] = {
		SENSOR_TEMP_CHIPSET, SENSOR_TEMP_CPU, SENSOR_TEMP_MB,
		SENSOR_TEMP_T_SENSOR, SENSOR_TEMP_VRM,
		SENSOR_FAN_CHIPSET,
		SENSOR_CURR_CPU,
		SENSOR_MAX
	},
	[BOARD_ROG_CROSSHAIR_VIII_IMPACT] = {
		SENSOR_TEMP_CHIPSET, SENSOR_TEMP_CPU, SENSOR_TEMP_MB,
		SENSOR_TEMP_T_SENSOR, SENSOR_TEMP_VRM,
		SENSOR_FAN_CHIPSET,
		SENSOR_CURR_CPU,
		SENSOR_MAX
	},
};

struct ec_sensor {
	enum known_ec_sensor info_index;
	u32 cached_value;
};

struct ec_sensors_data {
	struct ec_sensor sensors[SENSOR_MAX];
	/* UTF-16 string to pass to BRxx() WMI function */
	char read_arg[((ASUS_WMI_BLOCK_READ_REGISTERS_MAX * 4) + 1) * 2];
	u8 nr_sensors; /* number of board EC sensors */
	/* number of EC registers to read (sensor might span more than 1 register) */
	u8 nr_registers;
	unsigned long last_updated; /* in jiffies */
	struct mutex lock;
};

struct asus_ec_sensors {
	struct ec_sensors_data sensors_data;
	board_t board;
};

static inline struct ec_sensors_data *get_sensor_data(struct device *pdev)
{
	return &((struct asus_ec_sensors *)dev_get_drvdata(pdev))->sensors_data;
}

static inline const struct ec_sensor_info *
get_sensor_info(const struct ec_sensors_data *state, int index)
{
	return &known_ec_sensors[state->sensors[index].info_index];
}

static int find_ec_sensor_index(const struct ec_sensors_data *ec,
				enum hwmon_sensor_types type, int channel)
{
	unsigned i;

	for (i = 0; i < ec->nr_sensors; ++i) {
		if (get_sensor_info(ec, i)->type == type) {
			if (channel == 0) {
				return i;
			}
			--channel;
		}
	}
	return -EDOM;
}

static void __init fill_board_sensors(struct ec_sensors_data *ec, board_t board)
{
	const enum known_ec_sensor *bsi = known_board_sensors[board];
	struct ec_sensor *s = ec->sensors;
	int i;

	ec->nr_sensors = ec->nr_registers = 0;
	for (i = 0; i < SENSOR_MAX && bsi[i] != SENSOR_MAX; i++) {
		s[i].info_index = bsi[i];
		s[i].cached_value = 0;
		ec->nr_sensors++;
		ec->nr_registers +=
			known_ec_sensors[bsi[i]].addr.components.size;
	}
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
	int len = ACPI_MIN(ASUS_WMI_MAX_BUF_LEN, inp[0] / 4);
	const u8 *data = inp + 2;
	unsigned i;

	for (i = 0; i < len; ++i, data += 4) {
		out[i] = (hex_to_bin(data[0]) << 4) + hex_to_bin(data[2]);
	}
}

static void asus_wmi_encode_registers(u16 *registers, u8 len, char *out)
{
	unsigned i;

	*out++ = len * 8;
	*out++ = 0;
	for (i = 0; i < len; ++i) {
		out = hex_utf_16_le_pack(out, (registers[i] & 0xFF00) >> 8);
		out = hex_utf_16_le_pack(out, (registers[i] & 0x00FF));
	}
}

static void make_asus_wmi_block_read_query(struct ec_sensors_data *ec)
{
	u16 registers[ASUS_WMI_BLOCK_READ_REGISTERS_MAX];
	const struct ec_sensor_info *si;
	unsigned i, j, register_idx = 0;
	/* if we can get values for all the registers in a single query,
	 * the query will not change from call to call */
	if (ec->nr_registers <= ASUS_WMI_BLOCK_READ_REGISTERS_MAX &&
	    ec->read_arg[0] > 0) {
		/* no need to update */
		return;
	}

	for (i = 0; i < ec->nr_sensors; ++i) {
		si = get_sensor_info(ec, i);
		for (j = 0; j < si->addr.components.size; ++j, ++register_idx) {
			registers[register_idx] =
				(si->addr.components.bank << 8) +
				si->addr.components.index + j;
		}
	}

	asus_wmi_encode_registers(registers, ec->nr_registers, ec->read_arg);
}

static int asus_ec_block_read(const struct device *dev, u32 method_id,
			      const char *query, u8 *out)
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
		dev_err(dev, "unexpected reply type from ASUS ACPI code");
		acpi_os_free(output.pointer);
		return -EIO;
	}
	asus_wmi_decode_reply_buffer(obj->buffer.pointer, out);
	acpi_os_free(output.pointer);
	return 0;
}

static void update_sensor_values(struct ec_sensors_data *ec, u8 *data)
{
	const struct ec_sensor_info *si;
	struct ec_sensor *s;
	int sidx;

	for (sidx = 0; sidx < ec->nr_sensors; ++sidx) {
		s = &ec->sensors[sidx];
		si = &known_ec_sensors[s->info_index];
		switch (si->addr.components.size) {
		case 1:
			s->cached_value = *data;
			break;
		case 2:
			s->cached_value = get_unaligned_be16(data);
			break;
		case 4:
			s->cached_value = get_unaligned_be32(data);
			break;
		}
		data += si->addr.components.size;
	}
}

static int update_ec_sensors(const struct device *dev,
			     struct ec_sensors_data *ec)
{
	u8 buffer[ASUS_WMI_BLOCK_READ_REGISTERS_MAX];
	int status;

	mutex_lock(&ec->lock);
	make_asus_wmi_block_read_query(ec);
	status = asus_ec_block_read(dev, METHODID_BLOCK_READ_EC, ec->read_arg,
				    buffer);

	if (!status) {
		update_sensor_values(ec, buffer);
	}
	mutex_unlock(&ec->lock);
	return status;
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

static int get_cached_value_or_update(const struct device *dev,
				      int sensor_index,
				      struct ec_sensors_data *state, u32 *value)
{
	int ret;
	if (time_after(jiffies, state->last_updated + HZ)) {
		ret = update_ec_sensors(dev, state);

		if (ret) {
			dev_err(dev, "update_ec_sensors() failure\n");
			return -EIO;
		}

		state->last_updated = jiffies;
	}

	*value = state->sensors[sensor_index].cached_value;
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

	struct ec_sensors_data *state = get_sensor_data(dev);
	int sidx = find_ec_sensor_index(state, type, channel);
	if (sidx < 0) {
		return sidx;
	}

	ret = get_cached_value_or_update(dev, sidx, state, &value);
	if (!ret) {
		*val = scale_sensor_value(value,
					  get_sensor_info(state, sidx)->type);
	}

	return ret;
}

static int asus_wmi_hwmon_read_string(struct device *dev,
				      enum hwmon_sensor_types type, u32 attr,
				      int channel, const char **str)
{
	struct ec_sensors_data *state = get_sensor_data(dev);
	int sensor_index = find_ec_sensor_index(state, type, channel);
	*str = get_sensor_info(state, sensor_index)->label;

	return 0;
}

static umode_t asus_wmi_hwmon_is_visible(const void *drvdata,
					 enum hwmon_sensor_types type, u32 attr,
					 int channel)
{
	const struct asus_ec_sensors *state = drvdata;
	return find_ec_sensor_index(&state->sensors_data, type, channel) !=
			       0xFF ?
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

static int __init supported_board_index(const struct device *dev)
{
	const struct dmi_system_id *dmi_entry;
	u32 version = 0;

	dmi_entry = dmi_first_match(asus_wmi_ec_dmi_table);
	if (!dmi_entry) {
		dev_info(dev, "Unsupported board");
		return -ENODEV;
	}

	if (get_version(&version)) {
		dev_err(dev, "Error getting version");
		return -ENODEV;
	}

	return dmi_entry - asus_wmi_ec_dmi_table;
}

static int __init configure_sensor_setup(struct platform_device *pdev)
{
	struct asus_ec_sensors *asus_ec_sensors = platform_get_drvdata(pdev);
	int nr_count[hwmon_max] = { 0 }, nr_types = 0;
	struct device *hwdev;
	struct device *dev = &pdev->dev;
	struct hwmon_channel_info *asus_wmi_hwmon_chan;
	const struct hwmon_channel_info **ptr_asus_wmi_ci;
	const struct hwmon_chip_info *chip_info;
	const struct ec_sensor_info *si;
	enum hwmon_sensor_types type;
	unsigned i;

	asus_ec_sensors->board = supported_board_index(dev);
	if (asus_ec_sensors->board < 0) {
		return -ENODEV;
	}

	mutex_init(&asus_ec_sensors->sensors_data.lock);

	fill_board_sensors(&asus_ec_sensors->sensors_data,
			   asus_ec_sensors->board);

	for (i = 0; i < asus_ec_sensors->sensors_data.nr_sensors; ++i) {
		si = get_sensor_info(&asus_ec_sensors->sensors_data, i);
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

	dev_info(dev, "board has %d EC sensors that span %d registers",
		 asus_ec_sensors->sensors_data.nr_sensors,
		 asus_ec_sensors->sensors_data.nr_registers);

	hwdev = devm_hwmon_device_register_with_info(
		dev, "asus-wmi-ec-sensors", asus_ec_sensors, chip_info, NULL);

	return PTR_ERR_OR_ZERO(hwdev);
}

static struct platform_device *asus_wmi_sensors_platform_device;

static int asus_wmi_probe(struct platform_device *pdev)
{
	struct asus_ec_sensors *state = platform_get_drvdata(pdev);

	if (state->board < 0) {
		return -ENODEV;
	}

	if (!wmi_has_guid(ASUSWMI_MGMT2_GUID)) {
		dev_info(&pdev->dev, "ASUSHW GUID not found\n");
		return -ENODEV;
	}

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

	platform_set_drvdata(asus_wmi_sensors_platform_device, state);

	return configure_sensor_setup(asus_wmi_sensors_platform_device);
}

static void __exit asus_wmi_ec_exit(void)
{
	platform_device_unregister(asus_wmi_sensors_platform_device);
	platform_driver_unregister(&asus_wmi_sensors_platform_driver);
}

module_init(asus_wmi_ec_init);
module_exit(asus_wmi_ec_exit);

MODULE_AUTHOR("Eugene Shalygin <eugene.shalygin@gmail.com>");
MODULE_DESCRIPTION(
	"HWMON driver for sensors accessible via EC in ASUS motherboards");
MODULE_LICENSE("GPL");
MODULE_VERSION("1");

// kate: tab-width 8; indent-width 8;
