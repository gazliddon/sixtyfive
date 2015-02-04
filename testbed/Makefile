OS    := linux

BIN   := bin/$(OS)
BUILD := build

AS    := $(BIN)/dasm
DEST  := $(BUILD)/hmctest.bin

all: $(BUILD) $(DEST)
	@echo Compiled

clean:
	@rm -rf $(BUILD)

$(BUILD)/%.bin : %.65
	@echo Assembling $< to $@
	@$(AS) $< -o$@

$(BUILD):
	mkdir -p $(BUILD)
