param([string]$Source,[string]$Target)
# Read-only conversion in a fresh Word automation document; never close a user's document.
$sourcePath = (Resolve-Path -LiteralPath $Source).Path
$targetPath = [System.IO.Path]::GetFullPath($Target)
$word = New-Object -ComObject Word.Application
$document = $null
if ($word.Documents.Count -ne 0) {
  [System.Runtime.InteropServices.Marshal]::ReleaseComObject($word) | Out-Null
  throw 'Word automation returned a session with open documents; conversion stopped to preserve them.'
}
try {
  $word.Visible = $false
  $word.DisplayAlerts = 0
  $document = $word.Documents.Open($sourcePath,$false,$true)
  $document.ExportAsFixedFormat($targetPath,17)
  Write-Output $targetPath
} finally {
  if ($null -ne $document) { $document.Close(0);[System.Runtime.InteropServices.Marshal]::ReleaseComObject($document)|Out-Null }
  $saveChanges = 0
  $word.Quit([ref]$saveChanges)
  [System.Runtime.InteropServices.Marshal]::ReleaseComObject($word)|Out-Null
}
