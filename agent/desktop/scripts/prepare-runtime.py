"""Copy the selected installed Windows R runtime; never modify the source runtime."""
from pathlib import Path
import argparse, csv, shutil, json, os

desktop = Path(__file__).resolve().parents[1]
repo = desktop.parents[1]
parser = argparse.ArgumentParser(description='Prepare the bundled Windows R engine from an existing installation.')
parser.add_argument('--r-home', default=os.environ.get('PSYCH_AGENT_R_HOME'), required=not bool(os.environ.get('PSYCH_AGENT_R_HOME')))
args = parser.parse_args()
source = Path(args.r_home).resolve()
if not (source / 'library').is_dir() or not (source / 'bin').is_dir():
    raise RuntimeError('Select an existing Windows R installation with bin and library folders.')
package_root = repo / 'vendor' / 'meta3level'
target = desktop / 'runtime' / 'R'
target.mkdir(parents=True, exist_ok=True)
for folder in ['bin', 'etc', 'modules', 'share']:
    shutil.copytree(source / folder, target / folder, dirs_exist_ok=True)
for name in ['COPYING', 'README', 'README.R-4.5.1']:
    shutil.copy2(source / name, target / name)
packages = {}
for folder in (source / 'library').iterdir():
    description = folder / 'DESCRIPTION'
    if description.exists() and 'Priority: base' in description.read_text(encoding='utf-8', errors='replace'):
        packages[folder.name] = folder
with (repo / 'agent' / 'runtime-dependencies.csv').open(encoding='utf-8-sig', newline='') as handle:
    for row in csv.DictReader(handle):
        packages[row['package']] = Path(row['path'])
manifest = []
for name, folder in packages.items():
    shutil.copytree(folder, target / 'library' / name, dirs_exist_ok=True)
    description = (folder / 'DESCRIPTION').read_text(encoding='utf-8', errors='replace')
    fields = dict(line.split(': ', 1) for line in description.splitlines() if ': ' in line and not line.startswith(' '))
    manifest.append({'package': name, 'version': fields.get('Version'), 'license': fields.get('License')})
(desktop / 'runtime' / 'THIRD-PARTY-PACKAGES.json').write_text(json.dumps(manifest, ensure_ascii=False, indent=2), encoding='utf-8')
backend = desktop / 'backend'
package_source = backend / 'meta3level'
package_source.mkdir(parents=True, exist_ok=True)
for folder in ['R', 'man', 'inst']:
    shutil.copytree(package_root / folder, package_source / folder, dirs_exist_ok=True)
for name in ['DESCRIPTION', 'NAMESPACE', 'LICENSE', 'LICENSE.md']:
    shutil.copy2(package_root / name, package_source / name)
shutil.copy2(repo / 'skills' / 'meta-analysis-agent' / 'scripts' / 'workflow.R', backend / 'workflow.R')
shutil.copy2(repo / 'skills' / 'meta-analysis-agent' / 'assets' / 'example-correlations.csv', backend / 'example-correlations.csv')
print(f'Prepared portable R with {len(manifest)} packages and the complete meta3level computation source.')
