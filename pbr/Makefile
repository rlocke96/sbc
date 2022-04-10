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
  pbr.s

OBJS = $(addprefix $(BUILD_DIR)/, $(SOURCES:.s=.o))

all: $(BUILD_DIR)/pbr.bin

install: $(BUILD_DIR)/pbr.bin
	scp $(BUILD_DIR)/pbr.bin garage:~/6502/image/

clean:
	rm -Rf $(BUILD_DIR)/pbr.*
	rm -Rf $(BUILD_DIR)/pbr

$(BUILD_DIR)/%.cfg: %.cfgtpl
	@mkdir -p $$(dirname $@)
	$(CC) -E $< -o $@

$(BUILD_DIR)/%.o: %.s
	@mkdir -p $$(dirname $@)
	$(AS) $(ASFLAGS) $< -o $@

$(BUILD_DIR)/pbr.bin: $(OBJS) $(DEPS) $(CFG_DIR)/pbr.cfg
	@mkdir -p $$(dirname $@)
	$(LD) -C $(CFG_DIR)/pbr.cfg $(OBJS) -o $@ -m $(BUILD_DIR)/pbr.map -Ln $(BUILD_DIR)/pbr.sym
