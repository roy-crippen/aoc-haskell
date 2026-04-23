# Simple Makefile for aoc2024-haskell
# Builds with Cabal and places the executable in the project root

.PHONY: all build

# Default target
all: build

# Build and copy the binary to ./aoc2024-haskell
build:
	cabal build
	cp $$(cabal list-bin fetch-aoc-data) ./fetch-aoc-data
	chmod +x ./fetch-aoc-data
	@echo "✅ ./fetch-aoc-data is ready in the project root"


