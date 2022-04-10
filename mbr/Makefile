CC  = cc65
AS  = ca65
LD  = ld65

# global includes
ASFLAGS += -I ../inc

# for monitor
ASFLAGS += -g

# all files are allowed to use 65SC02 features
ASFLAGS += --cpu 65SC02

ASFLAGS += --feature labels_without_colons
ASFLAGS += --feature leading_dot_in_identifiers
ASFLAGS += -U

BUILD_DIR=../build

CFG_DIR=$(BUILD_DIR)/cfg

SOURCES = \
  mbr.s

OBJS = $(addprefix $(BUILD_DIR)/, $(SOURCES:.s=.o))

all: $(BUILD_DIR)/mbr.bin

install: $(BUILD_DIR)/mbr.bin
	scp $(BUILD_DIR)/mbr.bin garage:~/6502/image/

clean:
	rm -Rf $(BUILD_DIR)/mbr.*
	rm -Rf $(BUILD_DIR)/mbr

$(BUILD_DIR)/%.cfg: %.cfgtpl
	@mkdir -p $$(dirname $@)
	$(CC) -E $< -o $@

$(BUILD_DIR)/%.o: %.s
	@mkdir -p $$(dirname $@)
	$(AS) $(ASFLAGS) $< -o $@

$(BUILD_DIR)/mbr.bin: $(OBJS) $(DEPS) $(CFG_DIR)/mbr.cfg
	@mkdir -p $$(dirname $@)
	$(LD) -C $(CFG_DIR)/mbr.cfg $(OBJS) -o $@ -m $(BUILD_DIR)/mbr.map -Ln $(BUILD_DIR)/mbr.sym
