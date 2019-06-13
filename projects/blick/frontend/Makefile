BUILD_DIR ?= ../assets
ELM_FLAGS ?=

BUNDLE_FILE ?= "$(BUILD_DIR)/main.js"

all: build

debug: ELM_FLAGS += --debug
debug: clean build

release: ELM_FLAGS += --optimize
release: clean build

build:
	elm make src/Main.elm --output "$(BUNDLE_FILE)" $(ELM_FLAGS)

clean:
	rm -rf elm-stuff/0.19.0/*
	rm -f "$(BUNDLE_FILE)"

watch:
	ls src/**.elm | entr make debug all

.PHONY: all debug release build clean watch
