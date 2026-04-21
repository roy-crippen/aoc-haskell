# Simple Makefile for aoc2024-haskell
# Builds with Cabal and places the executable in the project root

.PHONY: all build

# Default target
all: build

# Build and copy the binary to ./aoc2024-haskell
build:
	cabal build
	cp $$(cabal list-bin aoc2024-haskell) ./aoc2024-haskell
	chmod +x ./aoc2024-haskell
	@echo "✅ ./aoc2024-haskell is ready in the project root"


