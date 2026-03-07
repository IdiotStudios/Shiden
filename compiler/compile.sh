#!/bin/bash

set -e

ROOT_DIR=$(dirname "$0")/compiler
ASM_DIR="$ROOT_DIR/libraries"
OUTPUT_DIR="$ROOT_DIR/target/asm_output"

mkdir -p "$OUTPUT_DIR"

for ASM_FILE in "$ASM_DIR"/*.asm; do
    BASENAME=$(basename "$ASM_FILE" .asm)

    # Compile to ELF binary
    ELF_OUTPUT_FILE="$OUTPUT_DIR/$BASENAME"
    echo "Compiling $ASM_FILE -> $ELF_OUTPUT_FILE (ELF Binary)"
    nasm -f elf64 "$ASM_FILE" -o "$ELF_OUTPUT_FILE"

    # Compile to Windows executable (PE)
    PE_OUTPUT_FILE="$OUTPUT_DIR/$BASENAME.exe"
    echo "Compiling $ASM_FILE -> $PE_OUTPUT_FILE (Windows EXE)"
    nasm -f win64 "$ASM_FILE" -o "$PE_OUTPUT_FILE"

done

LIBRARY_FILE="$OUTPUT_DIR/libruntime_helpers.a"
echo "Creating static library: $LIBRARY_FILE"
ar crus "$LIBRARY_FILE" "$OUTPUT_DIR"/*.o

echo "Compilation complete. Static library created at $LIBRARY_FILE"