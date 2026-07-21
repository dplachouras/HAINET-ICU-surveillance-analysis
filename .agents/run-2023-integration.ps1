$ErrorActionPreference = "Stop"

$rscript = "C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
& $rscript .agents\run-2023-integration.R
Write-Output ("INTEGRATION_EXIT=" + $LASTEXITCODE)
exit $LASTEXITCODE