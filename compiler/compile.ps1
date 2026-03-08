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

# Compile PE objects
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
    if ($LASTEXITCODE -ne 0) { 
        Write-Host "Warning: ar command failed. Static library creation skipped."
    }
}

$FinalElfBinary = Join-Path $OutputDir "shiden"
$FinalPeBinary = Join-Path $OutputDir "shiden.exe"

# Link ELF binary using mingw-w64 linker
$ElfEntryFile = Join-Path $OutputDir "main.o"
$ElfObjFiles = @(Get-ChildItem -Path $OutputDir -Filter "*.o" | Where-Object { $_.Name -ne "main.o" })

if ((Test-Path $ElfEntryFile) -and ($ElfObjFiles.Count -gt 0)) {
    $LdCommand = $null
    if (Get-Command x86_64-w64-mingw32-ld -ErrorAction SilentlyContinue) {
        $LdCommand = "x86_64-w64-mingw32-ld"
    } elseif (Get-Command ld -ErrorAction SilentlyContinue) {
        $LdCommand = "ld"
    }
    
    if ($LdCommand) {
        & $LdCommand -o $FinalElfBinary $ElfEntryFile @ElfObjFiles
        if ($LASTEXITCODE -eq 0) {
            Write-Host "Final ELF binary created at $FinalElfBinary"
        } else {
            Write-Host "Warning: ELF linking failed."
            exit 1
        }
    } else {
        Write-Host "Error: No linker found (ld or x86_64-w64-mingw32-ld). Install mingw-w64."
        exit 1
    }
}

# Link PE binary if enabled
if ($BuildPE -eq 1) {
    $PeEntryFile = Join-Path $OutputDir "main.obj"
    $PeObjFiles = @(Get-ChildItem -Path $OutputDir -Filter "*.obj" | Where-Object { $_.Name -ne "main.obj" })
    
    $GccPath = Get-Command x86_64-w64-mingw32-gcc -ErrorAction SilentlyContinue
    if ($GccPath) {
        if ((Test-Path $PeEntryFile) -and ($PeObjFiles.Count -gt 0)) {
            $GccArgs = @("-o", $FinalPeBinary, $PeEntryFile) + @($PeObjFiles | ForEach-Object { $_.FullName }) + @("-lkernel32", "-lshell32", "-nostdlib", "-Wl,--subsystem,console", "-Wl,--image-base,0x400000")
            & x86_64-w64-mingw32-gcc @GccArgs
            if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
            Write-Host "Final Windows EXE created at $FinalPeBinary"
        } else {
            Write-Host "Error: main.obj not found or no PE objects available."
            exit 1
        }
    } else {
        Write-Host "Warning: x86_64-w64-mingw32-gcc not found. PE linking skipped. Install mingw-w64 to build Windows executables."
    }
}

Write-Host "Compilation complete."