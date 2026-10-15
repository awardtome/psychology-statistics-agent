from pathlib import Path
import subprocess, shutil, zipfile, hashlib, json

desktop = Path(__file__).resolve().parents[1]
repo = desktop.parents[1]
version = json.loads((desktop / 'package.json').read_text(encoding='utf-8'))['version']
delivery = repo.parent / f'PsychologyStatisticsAgent-{version}-standalone'
delivery.mkdir(exist_ok=True)
installer = desktop / 'dist' / f'Psychology-Statistics-Agent-{version}-Setup.exe'
if not installer.exists() or installer.stat().st_size < 100_000_000:
    raise RuntimeError('Build the complete installer before packaging delivery.')
shutil.copy2(installer, delivery / installer.name)
shutil.copy2(desktop / 'README.zh-CN.md', delivery / '使用说明.md')
paths = subprocess.check_output(['git', 'ls-files', '--cached', '--others', '--exclude-standard', '-z'], cwd=repo, text=True, encoding='utf-8').split('\0')
excluded = ['node_modules/', 'runtime/', 'dist/', 'test-artifacts/']
with zipfile.ZipFile(delivery / f'Psychology-Statistics-Agent-{version}-Source.zip', 'w', zipfile.ZIP_DEFLATED) as archive:
    for relative in sorted(set(paths)):
        file = repo / relative
        if not file.is_file() or relative == 'agent/runtime-dependencies.csv' or any(part in relative for part in excluded):
            continue
        archive.write(file, 'psychology-statistics-agent-source/' + relative)
hashes = {}
for name in [installer.name, f'Psychology-Statistics-Agent-{version}-Source.zip']:
    hashes[name] = hashlib.sha256((delivery / name).read_bytes()).hexdigest()
(delivery / 'SHA256.json').write_text(json.dumps(hashes, indent=2), encoding='utf-8')
print(json.dumps({'delivery': str(delivery), 'installer_bytes': installer.stat().st_size, 'sha256': hashes}, ensure_ascii=False, indent=2))
