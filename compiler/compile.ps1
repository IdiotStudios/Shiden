$RootDir = (Get-Item -Path "." -Verbose).FullName + "\compiler"
$AsmDir = "$RootDir\libraries\runtime\linux"
$OutputDir = "$RootDir\target\asm_output"

if (-Not (Test-Path -Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir
}

Get-ChildItem -Path $AsmDir -Recurse -Filter "*.asm" | ForEach-Object {
    $BaseName = $_.BaseName

    # Compile to ELF binary
    $ElfOutputFile = "$OutputDir\$BaseName"
    Write-Host "Compiling $($_.FullName) -> $ElfOutputFile (ELF Binary)"
    nasm -f elf64 $_.FullName -o $ElfOutputFile

    # Compile to Windows executable (PE)
    $PeOutputFile = "$OutputDir\$BaseName.exe"
    Write-Host "Compiling $($_.FullName) -> $PeOutputFile (Windows EXE)"
    nasm -f win64 $_.FullName -o $PeOutputFile
}

$LibraryFile = "$OutputDir\libruntime_helpers.a"
Write-Host "Creating static library: $LibraryFile"
& ar crus $LibraryFile (Get-ChildItem -Path $OutputDir -Filter "*.o").FullName

Write-Host "Compilation complete. Static library created at $LibraryFile"