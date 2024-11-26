Remove-Item -Recurse -Force "$env:USERPROFILE\.emacs.d"

New-Item -ItemType Directory "$env:USERPROFILE\.emacs.d"
New-Item -ItemType Directory "$env:USERPROFILE\.emacs.d\codemacs"
New-Item -ItemType Directory "$env:USERPROFILE\.emacs.d\mathmacs"

New-Item "$env:USERPROFILE\.emacs.d\common" -ItemType SymbolicLink -Target "$(Get-Location)\common"

New-Item "$env:USERPROFILE\.emacs.d\codemacs\early-init.el" -ItemType SymbolicLink -Target "$(Get-Location)\codemacs\early-init.el"
New-Item "$env:USERPROFILE\.emacs.d\codemacs\init.el" -ItemType SymbolicLink -Target "$(Get-Location)\codemacs\init.el"

New-Item "$env:USERPROFILE\.emacs.d\mathmacs\early-init.el" -ItemType SymbolicLink -Target "$(Get-Location)\mathmacs\early-init.el"
New-Item "$env:USERPROFILE\.emacs.d\mathmacs\init.el" -ItemType SymbolicLink -Target "$(Get-Location)\mathmacs\init.el"

emacs.exe --no-init-file --load .\gen-env-file.el