Set-Alias -Name np -Value C:\Windows\notepad.exe
Set-Alias -Name vim -Value nvim

Function PSconda {(& "C:\software (other)\Miniconda\Scripts\conda.exe" "shell.powershell" "hook") | Out-String | Invoke-Expression}

Set-Alias -Name startconda -Value PSconda
