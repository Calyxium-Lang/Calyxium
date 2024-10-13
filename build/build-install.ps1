param (
    [string]$platform,
    [string]$env
)

$requiredPackages = @("dune", "menhir", "llvm", "ppx_deriving", "odoc", "zarith")
$installPathLinux = "/usr/local/bin"
$installPathWindows = "$env:ProgramFiles\Calyxium\bin"

function Show-Progress {
    param (
        [int]$currentIndex,
        [int]$totalItems
    )
    
    $progressPercent = [math]::Floor(($currentIndex / $totalItems) * 100)
    $progressBarLength = 50
    $filledLength = [math]::Floor(($progressPercent / 100) * $progressBarLength)

    $progressBar = ('#' * $filledLength) + ('-' * ($progressBarLength - $filledLength))

    Write-Host -NoNewline "`r[$progressBar] $progressPercent% ($currentIndex of $totalItems)"
}

function Check-OpamEnvLinux {
    $opamEnv = $(opam var root)
    if (-not $opamEnv) {
        Write-Host "Running 'eval $(opam env)' on Linux..."
        eval $(opam env)
    }
}

function Check-OpamEnvWindows {
    $opamEnv = & opam var root
    if (-not $opamEnv) {
        Write-Host "Running 'opam env' on Windows..."
        (& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }
    }
}

function Ensure-Packages {
    $totalPackages = $requiredPackages.Count
    $index = 1

    $missingPackages = @()
    foreach ($package in $requiredPackages) {
        $installed = & opam list --installed $package | Select-String $package
        if (-not $installed) {
            $missingPackages += $package
        }
    }

    if ($missingPackages.Count -eq 0) {
        Write-Host "All required packages are already installed. Skipping installation."
        return
    }

    foreach ($package in $missingPackages) {
        Show-Progress -currentIndex $index -totalItems $totalPackages
        & opam install $package
        $index++
    }
}

function Build-Project {
    switch ($env) {
        "dev" {
            $profile = "dev"
        }
        "release" {
            $profile = "release"
        }
        default {
            Write-Host "Unknown environment. Use 'dev' or 'release' as the environment."
            exit 1
        }
    }

    Write-Host "Building the project using 'dune build --profile $profile'..."
    & dune build --profile $profile

    if ($LASTEXITCODE -eq 0) {
        Write-Host "Project build successful."
    } else {
        Write-Host "Project build failed."
        exit 1
    }

    Write-Progress -Activity "Building Project" -Completed
}


function Install-OnLinux {
    $binaryPath = "_build/default/bin/calyxium.exe"
    $targetPath = Join-Path $installPathLinux "calyxium"

    if (Test-Path $binaryPath) {
        Write-Host "Installing to $targetPath..."
        sudo cp $binaryPath $targetPath
        sudo chmod +x $targetPath
        Write-Host "Installation successful!"
    } else {
        Write-Host "Build binary not found. Make sure the build step completed successfully."
        exit 1
    }
}

function Install-OnWindows {
    $binaryPath = "_build/default/bin/clayxium.exe"
    $targetPath = Join-Path $installPathWindows "calyxium.exe"

    if (Test-Path $binaryPath) {
        Write-Host "Creating install directory at $installPathWindows..."
        New-Item -ItemType Directory -Force -Path $installPathWindows | Out-Null

        Write-Host "Copying binary to $targetPath..."
        Copy-Item $binaryPath -Destination $targetPath

        if (-not ($env:Path -like "*$installPathWindows*")) {
            Write-Host "Adding $installPathWindows to system PATH..."
            [System.Environment]::SetEnvironmentVariable('Path', $env:Path + ";$installPathWindows", [System.EnvironmentVariableTarget]::Machine)
        }

        Write-Host "Installation successful!"
    } else {
        Write-Host "Build binary not found. Make sure the build step completed successfully."
        exit 1
    }
}

if (-not (Get-Command opam -ErrorAction SilentlyContinue)) {
    Write-Host "Opam is not installed. Please install it before running this script."
    exit 1
}

switch ($platform) {
    "linux" {
        Write-Host "Running on Linux..."
        Check-OpamEnvLinux
        Ensure-Packages
        Build-Project
        Install-OnLinux
    }
    "windows" {
        Write-Host "Running on Windows..."
        Check-OpamEnvWindows
        Ensure-Packages
        Build-Project
        Install-OnWindows
    }
    default {
        Write-Host "Unknown platform. Use 'linux' or 'windows' as an argument."
        exit 1
    }
}