# asus-wmi-ec-sensors

Linux HWMON sensors driver for ASUS motherboards to read sensors from the embedded controller

Many ASUS motherboards do not publish all the available sensors via the Super I/O chip but the 
missing ones are available through the embedded controller (EC) registers.

This driver implements reading those sensor data via the WMI method `BREC`, which is known to be present
in all ASUS motherboards based on the AMD 500 series chipsets (and probably is available in other
models too). The driver needs to know exact register addresses for the sensors and thus support 
for each motherboard has to be added explicitly.

The EC registers do not provide critical values for the sensors and as such they are not published to 
the HWMON.

## Supported motherboards

1. ROG CROSSHAIR VIII HERO
2. ROG CROSSHAIR VIII DARK HERO
3. ROG CROSSHAIR VIII FORMULA
4. ROG STRIX X570-E GAMING

## Adding a new motherboard

First, you need to find out which EC registers to read (although) they are pretty typical. If you have 
no idea is there any, you can try the [hwinfo](https://www.hwinfo.com/) software under Windows that will
show the EC node and known sensors for it. When you have the data (or decided to probe the default set),
the following changes to the source code need to be made:

1. Add your board name to the `asus_wmi_ec_dmi_table` array and a new enum value to the `board` enum. You can find
the board name in `/sys/class/dmi/id/board_name` or using `dmidecode`.

2. Modify the `fill_board_sensors()` function to handle your board. If your board has more sensors than 
`ASUS_EC_SENSORS_MAX` modify that too. If the available sensors span more than `ASUS_EC_KNOWN_EC_REGISTERS`
byte registers, increase that value too. If the latter value turns out to be higher than `ASUS_WMI_BLOCK_READ_REGISTERS_MAX`,
a rewrite of the `update_ec_sensors()` will be required.

For each sensor you need to provide its size in bytes (for example, RPM counters span two single-byte registers), 
its bank index and register index within the bank. If the sensor spans two or more registers, provide the 
first one (the smaller number).

Compile and it should work.

