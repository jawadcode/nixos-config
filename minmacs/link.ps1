New-Item $env:USERPROFILE\.emacs.d\init.el       -ItemType SymbolicLink -Target "$(Get-Location)/init.el"
New-Item $env:USERPROFILE\.emacs.d\early-init.el -ItemType SymbolicLink -Target "$(Get-Location)/early-init.el"
