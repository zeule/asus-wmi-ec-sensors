// SPDX-License-Identifier: GPL-2.0+
/*
 * HWMON driver for ASUS motherboards based on AMD 500 series chipsets (X570, B550)
 *
 * Copyright (C) 2021 Eugene Shalygin <eugene.shalygin@gmail.com>
 * Heavily based on the asus-wmi-sensors code by Ed Brindley
 * Uses sensor data from the Libre Hardware Monitor project
 */
#define PLATFORM_DRIVER
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

#define ASUSWMI_MGMT2_GUID		"466747A0-70EC-11DE-8A39-0800200C9A66"
#define METHODID_READ_SIO		0x5253494F // RSIO
#define METHODID_WRITE_SIO		0x5753494F // WSIO
#define METHODID_READ_HWMON		0x5248574D // RHWM
#define METHODID_WRITE_HWMON	0x5748574D // WHWM
#define METHODID_BLOCK_READ_HW	0x4252484D // BRHW
#define METHODID_BLOCK_READ_EC	0x42524543 // BREC
#define ASUSWMI_UNSUPPORTED_METHOD	0xFFFFFFFE

#define ASUS_WMI_BLOCK_READ_REGISTERS_MAX	0x10 // from the ASUS DSDT source
#define MAX_BUF_LEN				0x80


#define SENSORS_PER_BOARD_MAX	35
#define KNOWN_EC_REGISTERS	12
#define KNOWN_BOARDS_COUNT	2


typedef u8 board_t;
enum board {
	BOARD_C8H, // Crosshair VIII Hero
	BOARD_C8DH // Crosshair VIII Dark Hero
};

typedef u8 sensor_source_t;
enum sensor_source {
	SIO,
	EC
};

//enum

static u32 hwmon_attributes[] = {
	[hwmon_chip] = HWMON_C_REGISTER_TZ,
	[hwmon_temp] = HWMON_T_INPUT | HWMON_T_LABEL,
	[hwmon_in] = HWMON_I_INPUT | HWMON_I_LABEL,
	[hwmon_curr] = HWMON_C_INPUT | HWMON_C_LABEL,
	[hwmon_fan] = HWMON_F_INPUT | HWMON_F_LABEL,
};

struct known_sensor_info {
	enum hwmon_sensor_types type;
	u16 addr; // (bank << 8) + index
	u8 size; // 1 or 2
	sensor_source_t source;
};


static struct known_sensor_info known_sensors[] = {
	// please keep sorted by addr
	// Nuvoton (Super I/O) sensors

//	{hwmon_temp, }
/*	new TemperatureSourceData(SourceNct67Xxd.PECI_0, 0x073, 0x074, 7, 0x100),
                                new TemperatureSourceData(SourceNct67Xxd.CPUTIN, 0x075, 0x076, 7, 0x200, 0x491),
                                new TemperatureSourceData(SourceNct67Xxd.SYSTIN, 0x077, 0x078, 7, 0x300, 0x490),
                                new TemperatureSourceData(SourceNct67Xxd.AUXTIN0, 0x079, 0x07A, 7, 0x800, 0x492),
                                new TemperatureSourceData(SourceNct67Xxd.AUXTIN1, 0x07B, 0x07C, 7, 0x900, 0x493),
                                new TemperatureSourceData(SourceNct67Xxd.AUXTIN2, 0x07D, 0x07E, 7, 0xA00, 0x494),
                                new TemperatureSourceData(SourceNct67Xxd.AUXTIN3, 0x4A0, 0x49E, 6, 0xB00, 0x495),
                                new TemperatureSourceData(SourceNct67Xxd.AUXTIN4, 0x027, 0, -1, 0x621),
                                new TemperatureSourceData(SourceNct67Xxd.SMBUSMASTER0, 0x150, 0x151, 7, 0x622),
                                new TemperatureSourceData(SourceNct67Xxd.SMBUSMASTER1, 0x670, 0, -1, 0xC26),
                                new TemperatureSourceData(SourceNct67Xxd.PECI_1, 0x672, 0, -1, 0xC27),
                                new TemperatureSourceData(SourceNct67Xxd.PCH_CHIP_CPU_MAX_TEMP, 0x674, 0, -1, 0xC28, 0x400),
                                new TemperatureSourceData(SourceNct67Xxd.PCH_CHIP_TEMP, 0x676, 0, -1, 0xC29, 0x401),
                                new TemperatureSourceData(SourceNct67Xxd.PCH_CPU_TEMP,  0x678, 0, -1, 0xC2A, 0x402),
                                new TemperatureSourceData(SourceNct67Xxd.PCH_MCH_TEMP, 0x67A, 0, -1, 0xC2B, 0x404),
                                new TemperatureSourceData(SourceNct67Xxd.AGENT0_DIMM0, 0),
                                new TemperatureSourceData(SourceNct67Xxd.AGENT0_DIMM1, 0),
                                new TemperatureSourceData(SourceNct67Xxd.AGENT1_DIMM0, 0),
                                new TemperatureSourceData(SourceNct67Xxd.AGENT1_DIMM1, 0),
                                new TemperatureSourceData(SourceNct67Xxd.BYTE_TEMP0, 0),
                                new TemperatureSourceData(SourceNct67Xxd.BYTE_TEMP1, 0),
                                new TemperatureSourceData(SourceNct67Xxd.PECI_0_CAL, 0),
                                new TemperatureSourceData(SourceNct67Xxd.PECI_1_CAL, 0),
                                new TemperatureSourceData(SourceNct67Xxd.VIRTUAL_TEMP, 0)

                                0xC2F = 0x073: Chassis 1
0xC33: Chassis 2
0xC35 = 0x079: Chassis 3
0xC37 = 0x07b: High Amp
0xC39 :W+_Pump
*/
	// EC sensors
	{hwmon_temp, 0x003A, 1, EC}, // Chipset
	{hwmon_temp, 0x003B, 1, EC}, // CPU
	{hwmon_temp, 0x003C, 1, EC}, // Motherboard
	{hwmon_temp, 0x003D, 1, EC}, // T Sensor
	{hwmon_temp, 0x003E, 1, EC}, // VRM
	{hwmon_fan,  0x00B0, 2, EC}, // CPU Opt
	{hwmon_fan,  0x00BC, 2, EC}, // Water Flow
	{hwmon_curr, 0x00F4, 1, EC}, // CPU
	{hwmon_temp, 0x0100, 1, EC}, // Water In
	{hwmon_temp, 0x0101, 1, EC}, // Water Out
	// total EC: 10 sensors, 12 bytes of data
};

struct asus_wmi_sensor_info {
	u32 cached_value;
	//u8 inboard_index;
};

struct ec_cached_data {
	u16 registers[KNOWN_EC_REGISTERS];
	u8 nr_sensors; // number of board EC sensors
	u8 nr_registers; // number of EC registers to read (sensor might span more than 1 register)
};

struct asus_wmi_sensors {
	#ifdef PLATFORM_DRIVER
	struct platform_driver platform_driver;
 	struct platform_device *platform_device;
	#else
	struct wmi_driver wmi_driver;
	struct wmi_device *wmi_device;
	#endif

	unsigned long source_last_updated[3];	/* in jiffies */

	struct mutex lock;
	struct asus_wmi_sensor_info sensors[SENSORS_PER_BOARD_MAX]; //

	// UTF-16 string to pass to BRxx() WMI function
	char read_arg[((ASUS_WMI_BLOCK_READ_REGISTERS_MAX * 4) + 1) * 2];
	u8 read_buffer[ASUS_WMI_BLOCK_READ_REGISTERS_MAX];

	struct ec_cached_data ec;
	u8 board; // index in the known_boards array
	u8 buffer;
};

struct board_sensor {
    const char* name;
    u8 index; // in the known_sensors array
};

struct board_info {
	const char* name;
	u8 sensor_count;
	struct board_sensor sensors[SENSORS_PER_BOARD_MAX];
};

static struct board_info known_boards[KNOWN_BOARDS_COUNT] = {
	[BOARD_C8H] = {
		.name = "ROG CROSSHAIR VIII HERO",
		.sensor_count = 10, // for test
		.sensors = {
		    {"Chipset", 0}, {"CPU", 1}, {"Motherboard", 2}, {"T Sensor", 3}, {"VRM", 4},
		    {"CPU Opt", 5}, {"Water Flow", 6},
		    {"CPU", 7},
		    {"Water In", 8}, {"Water Out", 9}
		}
	},
	[BOARD_C8DH] = {
		.name = "ROG CROSSHAIR VIII DARK HERO",
		.sensor_count = 10, // for test
		.sensors = {
		    {"Chipset", 0}, {"CPU", 1}, {"Motherboard", 2}, {"T Sensor", 3}, {"VRM", 4},
		    {"CPU Opt", 5}, {"Water Flow", 6},
		    {"CPU", 7},
		    {"Water In", 8}, {"Water Out", 9}
		}
	},
};

static int get_version(u32 *version)
{
	// we know only a single version so far
	*version = 0;
	return 0;
}

static int get_sio_sensor_value(u8 index)
{
	return index; //dummy
}

// these functions converts to/from BREC string argument format

static u8 atoh(char val)
{
	if ((val >= 0x61) && (val <= 0x66)) {
		return val - 0x57;
	}
	if ((val >= 0x41) && (val <= 0x46)) {
		return val - 0x37;
	}
	if ((val >= 0x30) && (val <= 0x39)) {
		return val - 0x30;
	}
	return 0xFF;
}

static char htoa(u8 val)
{
	if (val <= 0x09) {
		return val + 0x30;
	}
	if ((val >= 0x0A) && (val <= 0x0F)) {
		return val - 0x0A + 0x41;
	}

	return 0xFF;
}

static void stoi(const u8* inp, u8* out)
{
	u8 len = ACPI_MIN(MAX_BUF_LEN, inp[0] / 4);
        const u8* data = inp + 2;
	u8 i;

	for (i = 0; i < len; ++i, data += 4)
	{
		out[i] = (atoh(data[0]) << 4) + atoh(data[2]);
        }
}

static void itos(const u16* in, u8 len, char* out)
{
        u8 i;
        u8 bank, index;

        // assert(len <= 30)
        *out++ = len * 8;
        *out++ = '0';
        for (i = 0; i < len; ++i) {
                bank = in[i] >> 8;
                index = in[i] & 0x00FF;
                *out++ = htoa((bank >> 4));
                *out++ = '0';
                *out++ = htoa(bank & 0x0F);
                *out++ = '0';
                *out++ = htoa((index >> 4));
                *out++ = '0';
                *out++ = htoa(index & 0x0F);
                *out++ = '0';
        }
}


static int asus_wmi_block_read(struct asus_wmi_sensors* aws, u32 method_id, const u16* registers, int len)
{
	struct acpi_buffer input;
	struct acpi_buffer output = { ACPI_ALLOCATE_BUFFER, NULL }; // TODO use pre-allocated buffer
	acpi_status status;
        union acpi_object *obj;

	itos(registers, len, aws->read_arg);
//         pr_debug("block read command to update %d registers: %s", len, aws->read_arg);
	// the first byte of the BRxx() argument string has to be the string size
	input.length = (acpi_size) aws->read_arg[0] + 1;
	input.pointer = aws->read_arg;
	status = wmi_evaluate_method(ASUSWMI_MGMT2_GUID, 0, method_id, &input, &output);
	if (ACPI_FAILURE(status)) {
		acpi_os_free(output.pointer);
		return -EIO;
	}

        obj = (union acpi_object*)output.pointer;
        if (obj->type != ACPI_TYPE_BUFFER) {
                pr_err("unexpected reply type from ASUS ACPI code");
                acpi_os_free(output.pointer);
		return -EIO;
        }
        stoi(obj->buffer.pointer, aws->read_buffer);
	acpi_os_free(output.pointer);
	return 0;
}

static int update_sio_sensors(struct asus_wmi_sensors* asus_wmi_sensors)
{
    /*
	const struct board_info* bi = &known_boards[asus_wmi_sensors->board];
	struct asus_wmi_sensor_info* si;
	u8 i;
	for (i = 0; i < bi->sensor_count; ++i) {
		si = &asus_wmi_sensors->sensors[i];
		if (known_sensors[si->known_sensor_index].source != SIO) {
			continue;
		}
		si->cached_value = get_sio_sensor_value(si->known_sensor_index);
	}
*/
	return 0;
}

static int update_ec_sensors(struct asus_wmi_sensors* aws)
{
	const struct board_info* bi;
	struct asus_wmi_sensor_info* si;
	const struct known_sensor_info* ki;
	u32 value;
	u8 sensor_ct, read_reg_ct, sensor_read_reg_ct;

	int status = asus_wmi_block_read(aws, METHODID_BLOCK_READ_EC, aws->ec.registers, aws->ec.nr_registers);
	if (status) return status;

	bi = &known_boards[aws->board];
	read_reg_ct = 0;
	for (sensor_ct = 0; sensor_ct < bi->sensor_count; ++sensor_ct) {
		si = &aws->sensors[sensor_ct];
		ki = &known_sensors[bi->sensors[sensor_ct].index];
		if (ki->source != EC) {
			continue;
		}
		value = aws->read_buffer[read_reg_ct++];
		for (sensor_read_reg_ct = 1; sensor_read_reg_ct < ki->size; ++sensor_read_reg_ct) {
		    value <<= 8;
		    value += aws->read_buffer[read_reg_ct++];
		}
		si->cached_value = value;
	}
	return 0;
}

static int update_source(enum sensor_source source, struct asus_wmi_sensors* asus_wmi_sensors)
{
	switch (source) {
		case SIO:
			return update_sio_sensors(asus_wmi_sensors);
		case EC:
			return update_ec_sensors(asus_wmi_sensors);
	}
	return 0;
}

static int scale_sensor_value(u32 value, int data_type) {
	switch (data_type) {
	case hwmon_curr:
		return value * 1000;
	case hwmon_temp:
		return value * 1000;
	case hwmon_in:
		return value * 1000;
	}
	return value; // FAN_RPM and WATER_FLOW don't need scaling
}

static u8 find_sensor_index (const struct asus_wmi_sensors* aws, enum hwmon_sensor_types type, int channel)
{
	const struct board_sensor* board_sensors = known_boards[aws->board].sensors;
	const u8 sensors_nr = known_boards[aws->board].sensor_count;
	u8 i;

	for (i = 0; i < sensors_nr; ++i) {
		if (known_sensors[board_sensors[i].index].type == type) {
			if (channel == 0) {
                                return i;
                        }
			--channel;
		}
	}
	return 0xFF;
}

static int get_cached_value_or_update(int sensor_index, struct asus_wmi_sensors *aws, u32 *value) {
	int ret;
	sensor_source_t src = known_sensors[known_boards[aws->board].sensors[sensor_index].index].source;
	if (time_after(jiffies, aws->source_last_updated[src] + HZ)) {
		ret = update_source(src, aws);

		if (ret) {
			pr_err("update_source failure\n");
			return -EIO;
		}

		aws->source_last_updated[src] = jiffies;
	}

	*value = aws->sensors[sensor_index].cached_value;
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

	struct asus_wmi_sensors* aws = dev_get_drvdata(dev);
        u8 sidx = find_sensor_index(aws, type, channel);

	mutex_lock(&aws->lock);

	ret = get_cached_value_or_update(sidx, aws, &value);
	mutex_unlock(&aws->lock);

	if (!ret) {
		*val = scale_sensor_value(value,
			known_sensors[known_boards[aws->board].sensors[sidx].index].type);
	}

	return ret;
}

static int
asus_wmi_hwmon_read_string(struct device *dev, enum hwmon_sensor_types type,
		       u32 attr, int channel, const char **str)
{
	struct asus_wmi_sensors *aws = dev_get_drvdata(dev);
        u8 sensor_index = find_sensor_index(aws, type, channel);
	*str = known_boards[aws->board].sensors[sensor_index].name;

	return 0;
}

static umode_t
asus_wmi_hwmon_is_visible(const void *drvdata, enum hwmon_sensor_types type,
		      u32 attr, int channel)
{
	const struct asus_wmi_sensors *aws = drvdata;
	return find_sensor_index(aws, type, channel) != 0xFF ? S_IRUGO : 0;
}

static int asus_wmi_hwmon_add_chan_info(struct hwmon_channel_info *asus_wmi_hwmon_chan,
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

static int known_board_index(const char* name)
{
	int i;
	for (i = 0; i < KNOWN_BOARDS_COUNT; ++i) {
		if (strcmp(known_boards[i].name, name) == 0) return i;
	}

	return -1;
}

static int configure_sensor_setup(struct asus_wmi_sensors *asus_wmi_sensors)
{
	int i, k;
	int nr_count[hwmon_max] = {0}, nr_types = 0;
	u32 nr_sensors;
	struct device *hwdev; 
	#ifdef PLATFORM_DRIVER
	struct device *dev = &asus_wmi_sensors->platform_device->dev;
	#else
	struct device *dev = &asus_wmi_sensors->wmi_device->dev;
	#endif
	struct hwmon_channel_info *asus_wmi_hwmon_chan;
	const struct hwmon_channel_info **ptr_asus_wmi_ci;
	const struct hwmon_chip_info *chip_info;
	const struct board_info* board_info;
	const struct known_sensor_info* si;
	enum hwmon_sensor_types type;

	asus_wmi_sensors->buffer = -1;
	mutex_init(&asus_wmi_sensors->lock);

	asus_wmi_sensors->board = known_board_index(dmi_get_system_info(DMI_BOARD_NAME));
	board_info = &known_boards[asus_wmi_sensors->board];
	nr_sensors = known_boards[asus_wmi_sensors->board].sensor_count;
	pr_debug("sensor count %u\n", nr_sensors);

	for (i = 0; i < nr_sensors; ++i) {
		si = &known_sensors[board_info->sensors[i].index];
		if (!nr_count[si->type])
			++nr_types;
		++nr_count[si->type];
	}

	if (nr_count[hwmon_temp])
		nr_count[hwmon_chip]++, nr_types++;

	asus_wmi_hwmon_chan = devm_kcalloc(dev, nr_types, sizeof(*asus_wmi_hwmon_chan),
				       GFP_KERNEL);
	if (!asus_wmi_hwmon_chan)
		return -ENOMEM;

	ptr_asus_wmi_ci = devm_kcalloc(dev, nr_types + 1, sizeof(*ptr_asus_wmi_ci),
				   GFP_KERNEL);
	if (!ptr_asus_wmi_ci)
		return -ENOMEM;

	asus_wmi_chip_info.info = ptr_asus_wmi_ci;
	chip_info = &asus_wmi_chip_info;
	
	for (type = 0; type < hwmon_max; ++type) {
		if (!nr_count[type])
			continue;

		asus_wmi_hwmon_add_chan_info(asus_wmi_hwmon_chan, dev, nr_count[type],
					 type, hwmon_attributes[type]);
		*ptr_asus_wmi_ci++ = asus_wmi_hwmon_chan++;

	}

	// need to compute size and command string for EC update
	asus_wmi_sensors->ec.nr_sensors = 0;
	asus_wmi_sensors->ec.nr_registers = 0;

	for (i = 0; i < nr_sensors; ++i) {
		si = &known_sensors[board_info->sensors[i].index];
		if (si->source != EC) continue;
		++asus_wmi_sensors->ec.nr_sensors;
		for (k = 0; k < si->size; ++k) {
			asus_wmi_sensors->ec.registers[asus_wmi_sensors->ec.nr_registers] = si->addr + k;
        		++asus_wmi_sensors->ec.nr_registers;
		}
	}

	pr_info("ASUS WMI board has %d EC sensors that span %d registers", asus_wmi_sensors->ec.nr_sensors, asus_wmi_sensors->ec.nr_registers);

	hwdev = devm_hwmon_device_register_with_info(dev, "asuswmisensorsamd500",
						     asus_wmi_sensors, chip_info,
						     NULL);

	return PTR_ERR_OR_ZERO(hwdev);
}

static int is_board_supported(void) {
	const char *board_vendor, *board_name, *bios_version;
	u32 version = 0;

	board_vendor = dmi_get_system_info(DMI_BOARD_VENDOR);
	board_name = dmi_get_system_info(DMI_BOARD_NAME);
	bios_version = dmi_get_system_info(DMI_BIOS_VERSION);

	if(get_version(&version)) {
		pr_err("Error getting version\n");
		return -ENODEV;
	}

	if (board_vendor && board_name && bios_version) {
		pr_info("Vendor: %s Board: %s BIOS version: %s WMI version: %u", board_vendor, board_name, bios_version, version);

		if (known_board_index(board_name) >= 0) {
			pr_info("Supported board");
			return 0;
		}
	}
	pr_info("Unsupported board");
	return -ENODEV;
}

#ifndef PLATFORM_DRIVER

static int asus_wmi_sensors_probe(struct wmi_device *wdev)
{
	struct device *dev = &wdev->dev;
	struct asus_wmi_sensors *asus_wmi_sensors;

	pr_info("asuswmisensorsamd500: WMI GUID matched - probing");

	if (is_board_supported()) {
		return -ENODEV;
	}

	asus_wmi_sensors = devm_kzalloc(dev, sizeof(struct asus_wmi_sensors), GFP_KERNEL);
	if (!asus_wmi_sensors)
		return -ENOMEM;

	asus_wmi_sensors->wmi_device = wdev;

	dev_set_drvdata(dev, asus_wmi_sensors);
	return configure_sensor_setup(asus_wmi_sensors);
}

static int asus_wmi_sensors_remove(struct wmi_device *wdev)
{
	struct asus_wmi_sensors *asus;

	asus = dev_get_drvdata(&wdev->dev);

	return 0;
}

static const struct wmi_device_id asus_wmi_sensors_id_table[] = {
	{ .guid_string = ASUS_HW_GUID },
	{ },
};

static struct wmi_driver asus_wmi_sensors = {
	.driver = {
		.name = "asus-wmi-sensors",
	},
	.probe = asus_wmi_sensors_probe,
	.remove = asus_wmi_sensors_remove,
	.id_table = asus_wmi_sensors_id_table,
};

module_wmi_driver(asus_wmi_sensors);
#endif

#ifdef PLATFORM_DRIVER
static struct platform_device *asus_wmi_sensors_platform_device;

static int asus_wmi_probe(struct platform_device *pdev)
{
	if (!wmi_has_guid(ASUSWMI_MGMT2_GUID)) {
		pr_info("ASUSHW GUID not found\n");
		return -ENODEV;
	}

	if (is_board_supported()) {
		return -ENODEV;
	}

	pr_info("driver loaded\n");
	return 0;
}

static struct platform_driver asus_wmi_sensors_platform_driver = {
	.driver = {
		.name	= "amd-500-asus-wmi-sensors",
	},
	.probe		= asus_wmi_probe
};

static int __init amd_500_asus_wmi_module_init(void)
{
	struct asus_wmi_sensors *asus_wmi_sensors;

	asus_wmi_sensors_platform_device = platform_create_bundle(&asus_wmi_sensors_platform_driver,
						 asus_wmi_probe,
						 NULL, 0, NULL, 0);

	if (IS_ERR(asus_wmi_sensors_platform_device))
		return PTR_ERR(asus_wmi_sensors_platform_device);

	asus_wmi_sensors = devm_kzalloc(&asus_wmi_sensors_platform_device->dev, sizeof(struct asus_wmi_sensors), GFP_KERNEL);

	if (!asus_wmi_sensors)
		return -ENOMEM;

	asus_wmi_sensors->platform_device = asus_wmi_sensors_platform_device;
	asus_wmi_sensors->platform_driver = asus_wmi_sensors_platform_driver;

	platform_set_drvdata(asus_wmi_sensors->platform_device, asus_wmi_sensors);
	
	return configure_sensor_setup(asus_wmi_sensors);
}

static void __exit amd_500_asus_wmi_module_exit(void)
{
	//hwmon_device_unregister(
 	platform_device_unregister(asus_wmi_sensors_platform_device);
 	platform_driver_unregister(&asus_wmi_sensors_platform_driver);

 }
 
 module_init(amd_500_asus_wmi_module_init);
 module_exit(amd_500_asus_wmi_module_exit);
 #endif


// kate: tab-width 8; indent-width 8;
