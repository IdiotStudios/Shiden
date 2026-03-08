#!/bin/bash

set -e

ROOT_DIR=$(dirname "$0")
ASM_DIR="$ROOT_DIR/src"
OUTPUT_DIR="$ROOT_DIR/asm_output"
BUILD_PE=${BUILD_PE:-1}

rm -rf "$OUTPUT_DIR"

mkdir -p "$ASM_DIR"
mkdir -p "$OUTPUT_DIR"

if [ -z "$(find "$ASM_DIR" -type f -name "*.asm")" ]; then
    echo "No assembly files found in $ASM_DIR. Exiting."
    exit 1
fi

find "$ASM_DIR" -type f -name "*.asm" | while IFS= read -r ASM_FILE; do
    RELATIVE_PATH=${ASM_FILE#"$ASM_DIR"/}
    MODULE_STEM=${RELATIVE_PATH%.asm}
    OUTPUT_BASENAME=$(echo "$MODULE_STEM" | tr '/' '_')

    case "$ASM_FILE" in
        *"_windows.asm") ;;
        *)
            OUTPUT_FILE_ELF="$OUTPUT_DIR/$OUTPUT_BASENAME.o"
            echo "Assembling $ASM_FILE -> $OUTPUT_FILE_ELF (ELF object)"
            nasm -w-label-redef-late -f elf64 "$ASM_FILE" -o "$OUTPUT_FILE_ELF"
            ;;
    esac
done

if [ "$BUILD_PE" = "1" ]; then
    find "$ASM_DIR" -type f -name "*.asm" | while IFS= read -r ASM_FILE; do
        case "$ASM_FILE" in
            *"_windows.asm")
                LINUX_FILE="${ASM_FILE%_windows.asm}.asm"
                RELATIVE_PATH=${LINUX_FILE#"$ASM_DIR"/}
                MODULE_STEM=${RELATIVE_PATH%.asm}
                OUTPUT_BASENAME=$(echo "$MODULE_STEM" | tr '/' '_')
                OUTPUT_FILE_PE="$OUTPUT_DIR/$OUTPUT_BASENAME.obj"
                echo "Assembling $ASM_FILE -> $OUTPUT_FILE_PE (PE object)"
                nasm -w-label-redef-late -f win64 "$ASM_FILE" -o "$OUTPUT_FILE_PE"
                ;;
            *)
                WINDOWS_VARIANT="${ASM_FILE%.asm}_windows.asm"
                if [ ! -f "$WINDOWS_VARIANT" ]; then
                    RELATIVE_PATH=${ASM_FILE#"$ASM_DIR"/}
                    MODULE_STEM=${RELATIVE_PATH%.asm}
                    OUTPUT_BASENAME=$(echo "$MODULE_STEM" | tr '/' '_')
                    OUTPUT_FILE_PE="$OUTPUT_DIR/$OUTPUT_BASENAME.obj"
                    echo "Assembling $ASM_FILE -> $OUTPUT_FILE_PE (PE object)"
                    nasm -w-label-redef-late -f win64 "$ASM_FILE" -o "$OUTPUT_FILE_PE"
                fi
                ;;
        esac
    done
fi

LIBRARY_FILE="$OUTPUT_DIR/libruntime_helpers.a"
echo "Creating static library: $LIBRARY_FILE"
ar crs "$LIBRARY_FILE" "$OUTPUT_DIR"/*.o

FINAL_ELF_BINARY="$OUTPUT_DIR/shiden"
FINAL_PE_BINARY="$OUTPUT_DIR/shiden.exe"

ELF_ENTRY_FILE="$OUTPUT_DIR/main.o"
ELF_OBJ_FILES=$(find "$OUTPUT_DIR" -type f -name "*.o" ! -name "main.o")

ld -o "$FINAL_ELF_BINARY" "$ELF_ENTRY_FILE" $ELF_OBJ_FILES

echo "Final ELF binary created at $FINAL_ELF_BINARY"

if [ "$BUILD_PE" = "1" ]; then
    PE_ENTRY_FILE="$OUTPUT_DIR/main.obj"
    PE_OBJ_FILES=$(find "$OUTPUT_DIR" -type f -name "*.obj" ! -name "main.obj")
    x86_64-w64-mingw32-gcc -o "$FINAL_PE_BINARY" "$PE_ENTRY_FILE" $PE_OBJ_FILES -lkernel32 -lshell32 -nostdlib -Wl,--subsystem,console -Wl,--image-base,0x400000
    echo "Final Windows EXE created at $FINAL_PE_BINARY"
fi

echo "Compilation complete. Static library created at $LIBRARY_FILE"