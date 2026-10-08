#!/usr/bin/env bash
# Build the website of one publication version into docs/<VERSION>/ for GitHub Pages.
#
# Run from the repository root of a checkout without uncommitted changes:
#   SITE_PROFILE=smoke scripts/build_publication_site.sh VERSION
# VERSION names the folder, for example submission-1. PUBLICATION_REPOSITORY_URL
# (https://github.com/<owner>/<name>) gives the pages their source links.
# SITE_NOTE is the status line of the start page (Markdown); it defaults to the
# preview note. PREREGISTRATION_DATE (YYYY-MM-DD) is the date the preregistration
# shows; without it, the date of the built commit, so that the same commit
# always gives the same date. The DOI build passes the submission date.
# SITE_REPRODUCE=report (default) renders committed results without fitting.
# SITE_REPRODUCE=models reruns models using saved resampling results; full
# recomputes the analyses and simulations. SITE_PROFILE=smoke matches the
# committed synthetic demonstration's reduced computational settings.
#
# Every step runs in the image of the Dockerfile: the analysis on the synthetic
# data, which renders the results HTML; its PDF from that same HTML;
# the preregistration as HTML and PDF;
# the Code Browser from the committed source; the section links between the
# preregistration and the Code Browser; the start page from scripts/site/; the
# shared header on every page. docs/<VERSION>/ then holds index.html,
# preregistration.html, preregistration.pdf, code-browser.html and
# results-report.html and results-report.pdf; docs/index.html lists every version.
set -euo pipefail

version="${1:?usage: scripts/build_publication_site.sh VERSION}"
profile="${SITE_PROFILE:-full}"
reproduction="${SITE_REPRODUCE:-report}"
case "$reproduction" in report|models|full) ;; *) echo "SITE_REPRODUCE must be report, models or full." >&2; exit 1 ;; esac
repository_url="${PUBLICATION_REPOSITORY_URL:?set PUBLICATION_REPOSITORY_URL to https://github.com/<owner>/<name>}"
repository_url="${repository_url%/}"
note="${SITE_NOTE:-**Preview.** The preregistration and the results report are under review; this is not the registered version.}"
image=social-motives-ideology
site="docs/$version"

[[ "$version" =~ ^[A-Za-z0-9._-]+$ ]] || { echo "VERSION may hold letters, digits, '.', '_' and '-' only." >&2; exit 1; }
[ -z "$(git status --porcelain -- panel scripts Dockerfile .dockerignore)" ] ||
  { echo "Commit first: the site is built from committed source only." >&2; exit 1; }
[ ! -e "$site" ] || { echo "$site exists; a published version is never rebuilt in place." >&2; exit 1; }
commit="$(git rev-parse HEAD)"
source_dirty=false
[ -z "$(git status --porcelain --untracked-files=normal)" ] || source_dirty=true
prereg_date="${PREREGISTRATION_DATE:-$(git log -1 --format=%cs HEAD)}"
[[ "$prereg_date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || { echo "PREREGISTRATION_DATE must be YYYY-MM-DD." >&2; exit 1; }
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

docker build -t "$image" .

# 1. Render saved reporting inputs, rerun models, or recompute the full analysis.
container="$image-site-$version"
docker rm "$container" >/dev/null 2>&1 || true
run_started="$(date +%s)"
docker run --name "$container" -e ZM_PROFILE="$profile" \
  -e ZM_SOURCE_COMMIT="$commit" -e ZM_SOURCE_DIRTY="$source_dirty" "$image" \
  Rscript scripts/reproduce.R "$reproduction"
run_finished="$(date +%s)"
docker cp "$container:/opt/zm/panel/analysis/report/results_draft.html" "$work/results-report.html"
if [ "$reproduction" != report ]; then
  mkdir "$work/synthetic_review"
  docker cp "$container:/opt/zm/panel/analysis/data/derived/synthetic_review/." "$work/synthetic_review/"
  for bundle in report_inputs.rds resampling_results.rds; do
    docker cp "$container:/opt/zm/panel/analysis/data/derived/$bundle" "$work/$bundle"
  done
fi
docker rm "$container" >/dev/null

# Reject stale, incomplete or mismatched outputs of a computation. Report-only
# rendering retains the committed calculation provenance and files unchanged.
if [ "$reproduction" != report ]; then
docker run --rm -i -v "$PWD:/work:ro" -v "$work:/out" \
  -e SOURCE_COMMIT="$commit" -e SOURCE_DIRTY="$source_dirty" -e RUN_PROFILE="$profile" \
  -e RUN_STARTED="$run_started" -e RUN_FINISHED="$run_finished" "$image" python3 - <<'PYTHON'
import datetime, hashlib, json, os
from pathlib import Path

exports = Path('/out/synthetic_review')
root = Path('/work/panel/analysis')
expected = {'coefficients.csv', 'prediction_decisions.csv', 'generating_comparison.csv',
            'variable_dictionary.csv', 'prior_width_sensitivity.json', 'planted_checks.json',
            'calculation_receipt.json', 'generating_values.json'}
def require(condition, message):
    if not condition:
        raise SystemExit('Synthetic exports rejected: ' + message)
def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

require({p.name for p in exports.iterdir()} == expected | {'manifest.json'}, 'expected nine result files')
manifest = json.loads((exports / 'manifest.json').read_text())
receipt = json.loads((exports / 'calculation_receipt.json').read_text())
require(manifest['schema_version'] == 1 and manifest['data_source'] == 'synthetic', 'wrong export source/schema')
entries = manifest['files']
require(len(entries) == len(expected) and {entry['file'] for entry in entries} == expected, 'incomplete manifest')
for entry in entries:
    require(digest(exports / entry['file']) == entry['sha256'], 'file checksum: ' + entry['file'])
require(receipt['hash_algorithm'] == 'sha256', 'wrong fingerprint algorithm')
require(receipt['git_head'] == os.environ['SOURCE_COMMIT'], 'receipt source commit differs from this build')
require(receipt['git_dirty'] is (os.environ['SOURCE_DIRTY'] == 'true'), 'receipt dirty state differs from this build')
require(receipt['cfg']['profile_name'] == os.environ['RUN_PROFILE'], 'receipt profile differs from this build')
require(manifest['saved_run'] == dict(profile=receipt['cfg']['profile_name'], built_at=receipt['built_at'],
                                    git_head=receipt['git_head'], git_dirty=receipt['git_dirty']), 'manifest run differs from receipt')
built = datetime.datetime.strptime(receipt['built_at'], '%Y-%m-%dT%H:%M:%SZ').replace(tzinfo=datetime.timezone.utc).timestamp()
require(int(os.environ['RUN_STARTED']) <= built <= int(os.environ['RUN_FINISHED']), 'receipt was not produced during this run')
source_paths = {'_targets.R', 'config/analysis_plan.yaml', 'renv.lock'} | {
    path.relative_to(root).as_posix() for path in (root / 'R').glob('*.R')}
input_paths = {'data/intake/synthetic.csv', 'data/intake/synthetic_demographics.csv',
               'data/intake/synthetic_preparation.rds', 'data/intake/synthetic.yaml'} | {
    '../preregistration/codebook_' + name + '.csv' for name in ('items', 'scales', 'factors')}
for field, paths in (('source_files', source_paths), ('input_files', input_paths)):
    entries = receipt[field]
    require(len(entries) == len(paths) and {entry['path'] for entry in entries} == paths, 'incomplete ' + field)
    for entry in entries:
        require(digest(root / entry['path']) == entry['sha256'], 'input/source checksum: ' + entry['path'])
print('Verified fresh synthetic exports against this commit, profile and all recorded inputs/sources.')
PYTHON
fi

# Export the same report HTML to PDF without rerunning any analysis. The
# renderer checks text, figures, histogram dimensions and internal links.
docker run --rm -v "$work:/out" "$image" /opt/report-pdf/bin/python \
  scripts/render_results_pdf.py --input-html /out/results-report.html \
  --output-pdf /out/results-report.pdf --qa-dir /out/results-pdf-qa \
  --source-commit "$commit"

# 2. The preregistration as HTML and PDF. Its date is fixed in the container's
#    copy (a date written into the source stays as it is).
docker run --rm -v "$work:/out" -e PREREG_DATE="$prereg_date" "$image" bash -c '
  cd ../preregistration
  sed -i "s/^date: last-modified$/date: $PREREG_DATE/" preregistration.qmd
  quarto render preregistration.qmd --to html && quarto render preregistration.qmd --to pdf
  for f in preregistration.html preregistration.pdf; do
    if [ -f "renders/$f" ]; then cp "renders/$f" /out/; else cp "$f" /out/; fi
  done'

# The published survey sits beside the PDF, just as beside the HTML. Rebase
# only its link annotation; leave the rendered page content unchanged.
docker run --rm -i -v "$work:/out" "$image" /opt/report-pdf/bin/python - <<'PYTHON'
import fitz
with fitz.open('/out/preregistration.pdf') as document:
    changed = False
    for page in document:
        for link in page.get_links():
            if link.get('file') == '../qualtrics/ZMASCpanel.qsf':
                link['file'] = 'ZMASCpanel.qsf'
                page.update_link(link)
                changed = True
    if changed:
        document.saveIncr()
PYTHON

# 3. The Code Browser from the committed source of this checkout (mounted read-only).
docker run --rm -v "$PWD:/work:ro" -v "$work:/out" -e GIT_OPTIONAL_LOCKS=0 \
  -e PUBLICATION_REPOSITORY_URL="$repository_url" "$image" bash -c '
  git config --global --add safe.directory "*"
  cd /work/panel/analysis
  Rscript -e "source(\"scripts/generate_pipeline_view.R\"); generate_pipeline_view(output_path = \"/out/code-browser.html\")"'

# 4. The section links between the preregistration and the Code Browser.
docker run --rm -v "$PWD:/work" -v "$work:/out" -w /work "$image" \
  python3 scripts/link_publication_sections.py --preregistration /out/preregistration.html \
  --browser /out/code-browser.html --output "/work/$site"
cp "$work/preregistration.pdf" "$work/results-report.html" "$work/results-report.pdf" "$site/"
# These are the selected publication inputs: the survey is already stripped of
# account metadata by refresh_publication_copy.py before this checkout is built.
cp panel/preregistration/codebook_items.csv panel/preregistration/codebook_scales.csv \
  panel/preregistration/codebook_factors.csv panel/qualtrics/ZMASCpanel.qsf "$site/"

# 5. The start page of this version from scripts/site/, then the shared header
#    on all four pages, and the list of all versions.
if [ "$profile" != "full" ]; then
  note="**Synthetic demonstration with reduced computation settings (profile: $profile).** $note"
fi
mkdir -p "$work/start"
cp scripts/site/start.css "$work/start/"
docker run --rm -i -v "$PWD:/work:ro" -v "$work/start:/start" -e SITE_NOTE_TEXT="$note" -e VERSION="$version" \
  -e REPOSITORY="$repository_url" -e COMMIT="$commit" "$image" python3 - /work/scripts/site/index.qmd /start/index.qmd <<'PYTHON'
import os, sys
text = open(sys.argv[1], encoding="utf-8").read()
source = f"commit [`{os.environ['COMMIT'][:7]}`]({os.environ['REPOSITORY']}/tree/{os.environ['COMMIT']})"
for key, value in (("@SITE_NOTE@", os.environ["SITE_NOTE_TEXT"]), ("@VERSION@", os.environ["VERSION"]),
                   ("@REPOSITORY@", os.environ["REPOSITORY"]), ("@SOURCE@", source)):
    text = text.replace(key, value)
open(sys.argv[2], "w", encoding="utf-8").write(text)
PYTHON
docker run --rm -v "$work/start:/start" -w /start "$image" quarto render index.qmd
cp "$work/start/index.html" "$site/index.html"
docker run --rm -v "$PWD:/work" -w /work "$image" bash -c "
  python3 scripts/add_publication_header.py '$site/index.html' overview --repository '$repository_url' &&
  python3 scripts/add_publication_header.py '$site/preregistration.html' preregistration --repository '$repository_url' &&
  python3 scripts/add_publication_header.py '$site/code-browser.html' code-browser --repository '$repository_url' &&
  python3 scripts/add_publication_header.py '$site/results-report.html' results-report --repository '$repository_url'"
touch docs/.nojekyll
{
  echo '<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">'
  echo '<title>Motives and Ideology</title>'
  echo '<body style="max-width:46rem;margin:4rem auto;padding:0 1.2rem;font:17px/1.6 system-ui,-apple-system,sans-serif;color:#212529">'
  echo '<h1 style="font-weight:650">Motives and Ideology</h1><p>Each version stays as it was published.</p><ul>'
  for folder in docs/*/; do
    name="$(basename "$folder")"
    echo "<li><a href=\"$name/\" style=\"color:#2780e3\">$name</a></li>"
  done
  echo '</ul></body></html>'
} > docs/index.html

# Install only after the site succeeds, so the Code Browser still sees clean
# source. Reduced-profile builds keep recalculated outputs beside the versioned
# site; the canonical saved results are refreshed separately.
if [ "$reproduction" != report ]; then
exports="panel/analysis/data/derived/synthetic_review"
[ "$profile" = full ] || exports="$site/synthetic-review"
mkdir -p "$(dirname "$exports")"
replacement="$(mktemp -d "$(dirname "$exports")/.synthetic-review.XXXXXX")"
cp -R "$work/synthetic_review/." "$replacement/"
[ ! -e "$exports" ] || mv "$exports" "$replacement.previous"
if mv "$replacement" "$exports"; then
  rm -rf "$replacement.previous"
else
  [ ! -e "$replacement.previous" ] || mv "$replacement.previous" "$exports"
  exit 1
fi
bundles="panel/analysis/data/derived"
[ "$profile" = full ] || bundles="$site/reproduction"
mkdir -p "$bundles"
cp "$work/report_inputs.rds" "$work/resampling_results.rds" "$bundles/"
echo "Saved verified $profile results to $exports."
fi
echo "Built $site from ${commit:0:7} (profile $profile; reproduction $reproduction)."
