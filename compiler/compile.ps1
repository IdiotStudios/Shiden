$ErrorActionPreference = "Stop"

$RootDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$AsmDir = Join-Path $RootDir "src"
$OutputDir = Join-Path $RootDir "asm_output"
$BuildPE = if ($env:BUILD_PE) { [int]$env:BUILD_PE } else { 1 }

# Clean and create directories
if (Test-Path $OutputDir) {
    Remove-Item -Path $OutputDir -Recurse -Force
}
New-Item -ItemType Directory -Path $OutputDir -ErrorAction SilentlyContinue | Out-Null
New-Item -ItemType Directory -Path $AsmDir -ErrorAction SilentlyContinue | Out-Null

# Check for assembly files
$AsmFiles = @(Get-ChildItem -Path $AsmDir -Recurse -Filter "*.asm" -ErrorAction SilentlyContinue)
if ($AsmFiles.Count -eq 0) {
    Write-Host "No assembly files found in $AsmDir. Exiting."
    exit 1
}

# Compile ELF objects (skip _windows.asm files)
$AsmFiles | Where-Object { $_.Name -notlike "*_windows.asm" } | ForEach-Object {
    $RelativePath = $_.FullName.Substring($AsmDir.Length).TrimStart('\', '/')
    $ModuleStem = $RelativePath -replace '\.asm$', ''
    $OutputBasename = $ModuleStem -replace '\\', '_'
    $OutputFile = Join-Path $OutputDir "$OutputBasename.o"

    Write-Host "Assembling $($_.FullName) -> $OutputFile (ELF object)"
    & nasm -w-label-redef-late -f elf64 $_.FullName -o $OutputFile
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

# Compile PE objects if enabled
if ($BuildPE -eq 1) {
    $AsmFiles | ForEach-Object {
        $FileName = $_.Name
        $FilePath = $_.FullName
        
        if ($FileName -like "*_windows.asm") {
            # This is a Windows-specific file, compile it for PE
            $LinuxFile = $FilePath -replace '_windows\.asm$', '.asm'
            $RelativePath = $LinuxFile.Substring($AsmDir.Length).TrimStart('\', '/')
            $ModuleStem = $RelativePath -replace '\.asm$', ''
            $OutputBasename = $ModuleStem -replace '\\', '_'
            $OutputFile = Join-Path $OutputDir "$OutputBasename.obj"
            
            Write-Host "Assembling $FilePath -> $OutputFile (PE object)"
            & nasm -w-label-redef-late -f win64 $FilePath -o $OutputFile
            if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
        } else {
            # Check if Windows variant exists
            $WindowsVariant = $FilePath -replace '\.asm$', '_windows.asm'
            if (-Not (Test-Path $WindowsVariant)) {
                # No Windows variant, compile this as PE too
                $RelativePath = $FilePath.Substring($AsmDir.Length).TrimStart('\', '/')
                $ModuleStem = $RelativePath -replace '\.asm$', ''
                $OutputBasename = $ModuleStem -replace '\\', '_'
                $OutputFile = Join-Path $OutputDir "$OutputBasename.obj"
                
                Write-Host "Assembling $FilePath -> $OutputFile (PE object)"
                & nasm -w-label-redef-late -f win64 $FilePath -o $OutputFile
                if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
            }
        }
    }
}

# Create static library from ELF objects
$LibraryFile = Join-Path $OutputDir "libruntime_helpers.a"
Write-Host "Creating static library: $LibraryFile"
$OFiles = @(Get-ChildItem -Path $OutputDir -Filter "*.o" -ErrorAction SilentlyContinue)
if ($OFiles.Count -gt 0) {
    & ar crs $LibraryFile @OFiles
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

$FinalElfBinary = Join-Path $OutputDir "shiden"
$FinalPeBinary = Join-Path $OutputDir "shiden.exe"

# Link ELF binary
$ElfEntryFile = Join-Path $OutputDir "main.o"
$ElfObjFiles = @(Get-ChildItem -Path $OutputDir -Filter "*.o" | Where-Object { $_.Name -ne "main.o" })

& ld -o $FinalElfBinary $ElfEntryFile @ElfObjFiles
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host "Final ELF binary created at $FinalElfBinary"

# Link PE binary if enabled
if ($BuildPE -eq 1) {
    $PeEntryFile = Join-Path $OutputDir "main.obj"
    $PeObjFiles = @(Get-ChildItem -Path $OutputDir -Filter "*.obj" | Where-Object { $_.Name -ne "main.obj" })
    
    & x86_64-w64-mingw32-gcc -o $FinalPeBinary $PeEntryFile @PeObjFiles -lkernel32 -lshell32 -nostdlib -Wl,--subsystem,console -Wl,--image-base,0x400000
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    
    Write-Host "Final Windows EXE created at $FinalPeBinary"
}

Write-Host "Compilation complete. Static library created at $LibraryFile"