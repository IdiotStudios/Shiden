#!/bin/bash

set -e

ROOT_DIR=$(dirname "$0")
ASM_DIR="$ROOT_DIR/src"
OUTPUT_DIR="$ROOT_DIR/asm_output"

rm -rf "$OUTPUT_DIR"

mkdir -p "$ASM_DIR"
mkdir -p "$OUTPUT_DIR"

if [ -z "$(find "$ASM_DIR" -type f -name "*.asm")" ]; then
    echo "No assembly files found in $ASM_DIR. Exiting."
    exit 1
fi

while IFS= read -r ASM_FILE; do
    RELATIVE_PATH=${ASM_FILE#"$ASM_DIR"/}
    MODULE_STEM=${RELATIVE_PATH%.asm}
    OUTPUT_BASENAME=$(echo "$MODULE_STEM" | tr '/' '_')

    OUTPUT_FILE_ELF="$OUTPUT_DIR/$OUTPUT_BASENAME.o"
    OUTPUT_FILE_PE="$OUTPUT_DIR/$OUTPUT_BASENAME.obj"

    echo "Assembling $ASM_FILE -> $OUTPUT_FILE_ELF (ELF object)"
    nasm -f elf64 "$ASM_FILE" -o "$OUTPUT_FILE_ELF"

    echo "Assembling $ASM_FILE -> $OUTPUT_FILE_PE (PE object)"
    nasm -f win64 "$ASM_FILE" -o "$OUTPUT_FILE_PE"
done < <(find "$ASM_DIR" -type f -name "*.asm")

LIBRARY_FILE="$OUTPUT_DIR/libruntime_helpers.a"
echo "Creating static library: $LIBRARY_FILE"
ar crs "$LIBRARY_FILE" "$OUTPUT_DIR"/*.o

# Link all object files into a single ELF binary and Windows EXE
FINAL_ELF_BINARY="$OUTPUT_DIR/shiden"
FINAL_PE_BINARY="$OUTPUT_DIR/shiden.exe"

# Ensure only one entry point (_start) is defined per target format
ELF_ENTRY_FILE="$OUTPUT_DIR/main.o"
PE_ENTRY_FILE="$OUTPUT_DIR/main.obj"

ELF_OBJ_FILES=$(find "$OUTPUT_DIR" -type f -name "*.o" ! -name "main.o")
PE_OBJ_FILES=$(find "$OUTPUT_DIR" -type f -name "*.obj" ! -name "main.obj")

# Link ELF binary
ld -o "$FINAL_ELF_BINARY" "$ELF_ENTRY_FILE" $ELF_OBJ_FILES

# Link Windows EXE
x86_64-w64-mingw32-ld -o "$FINAL_PE_BINARY" "$PE_ENTRY_FILE" $PE_OBJ_FILES

echo "Final ELF binary created at $FINAL_ELF_BINARY"
echo "Final Windows EXE created at $FINAL_PE_BINARY"

echo "Compilation complete. Static library created at $LIBRARY_FILE"