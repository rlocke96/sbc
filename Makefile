CC	= cc65
AS	= ca65
LD	= ld65

# global includes
ASFLAGS	+= -I inc

# for monitor
ASFLAGS += -g

# all files are allowed to use 65SC02 features
ASFLAGS += --cpu 65SC02
#
ASFLAGS += --feature labels_without_colons
ASFLAGS += --feature leading_dot_in_identifiers
ASFLAGS += -U
#

BUILD_DIR=build

CFG_DIR=$(BUILD_DIR)/cfg
BIOS_SOURCES = \
	bios/acia.s \
	bios/delay.s \
	bios/memory.s \
	bios/print.s \
	bios/sdcard.s \
	bios/spi.s \
	bios/rtc.s \
	bios/bios.s

MBR_SOURCES = \
	      mbr/mbr.s

PBR_SOURCES = \
	      pbr/pbr.s

KER_SOURCES = \
	      kernel/fat32.s \
	      kernel/match.s \
	      kernel/text_input.s \
	      kernel/zeropage.s \
	      kernel/kernel.s

BIOS_OBJS	= $(addprefix $(BUILD_DIR)/, $(BIOS_SOURCES:.s=.o))

MBR_OBJS	= $(addprefix $(BUILD_DIR)/, $(MBR_SOURCES:.s=.o))

PBR_OBJS	= $(addprefix $(BUILD_DIR)/, $(PBR_SOURCES:.s=.o))

KER_OBJS 	= $(addprefix $(BUILD_DIR)/, $(KER_SOURCES:.s=.o))

	BANK_BINS = \
		    $(BUILD_DIR)/bios.bin \
		    $(BUILD_DIR)/mbr.bin  \
		    $(BUILD_DIR)/pbr.bin  \
		    $(BUILD_DIR)/kernel.bin

	ROM_LABELS=$(BUILD_DIR)/rom_labels.h

all: $(BUILD_DIR)/rom.bin $(ROM_LABELS)

$(BUILD_DIR)/rom.bin: $(BANK_BINS)
	cat $(BANK_BINS) > $@

clean:
	rm -rf $(BUILD_DIR)

$(BUILD_DIR)/%.cfg: %.cfgtpl
	@mkdir -p $$(dirname $@)
	$(CC) -E $< -o $@

$(BUILD_DIR)/%.o: %.s
	@mkdir -p $$(dirname $@)
	$(AS) $(ASFLAGS) $< -o $@

$(BUILD_DIR)/bios.bin: $(BIOS_OBJS) $(BIOS_DEPS) $(CFG_DIR)/bios.cfg
	@mkdir -p $$(dirname $@)
	$(LD) -C $(CFG_DIR)/bios.cfg $(BIOS_OBJS) -o $@ -m $(BUILD_DIR)/bios.map -Ln $(BUILD_DIR)/bios.sym

$(BUILD_DIR)/mbr.bin: $(MBR_OBJS) $(MBR_DEPS) $(CFG_DIR)/mbr.cfg
	@mkdir -p $$(dirname $@)
	$(LD) -C $(CFG_DIR)/mbr.cfg $(MBR_OBJS) -o $@ -m $(BUILD_DIR)/mbr.map -Ln $(BUILD_DIR)/mbr.sym

$(BUILD_DIR)/pbr.bin: $(PBR_OBJS) $(PBR_DEPS) $(CFG_DIR)/pbr.cfg
	@mkdir -p $$(dirname $@)
	$(LD) -C $(CFG_DIR)/pbr.cfg $(PBR_OBJS) -o $@ -m $(BUILD_DIR)/pbr.map -Ln $(BUILD_DIR)/pbr.sym

$(BUILD_DIR)/kernel.bin: $(KER_OBJS) $(KER_DEPS) $(CFG_DIR)/kernel.cfg
	@mkdir -p $$(dirname $@)
	$(LD) -C $(CFG_DIR)/kernel.cfg $(KER_OBJS) -o $@ -m $(BUILD_DIR)/ker.map -Ln $(BUILD_DIR)/ker.sym

$(BUILD_DIR)/rom_labels.h: $(BANK_BINS)
	./scripts/symbolize.sh 0 build/bios.sym   > $@
