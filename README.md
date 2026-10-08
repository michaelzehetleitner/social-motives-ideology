# Social Motives and Ideology

Publication materials for *Motives and Ideology: Bischof's Zürich Model of Social
Motivation, Right-wing Authoritarianism and Social Dominance Orientation*.

This repository contains the preregistration, study materials, analysis code
and synthetic demonstration results. The preregistration was submitted to
PsychArchives, the service of the Leibniz Institute for Psychology (ZPID), on
8 October 2026. Its DOI will be added here when assigned. The website shows each version at
<https://michaelzehetleitner.github.io/social-motives-ideology/>.

## Contents

- [Preregistration source](panel/preregistration/preregistration.qmd) and
  [bibliography](<panel/preregistration/literature/ZM panel preregistration.bib>).
- [Analysis pipeline](panel/analysis/_targets.R), [functions](panel/analysis/R),
  [analysis plan](panel/analysis/config/analysis_plan.yaml) and
  [results report](panel/analysis/report/results_draft.qmd).
- [Synthetic input](panel/analysis/data/synthetic), [cleaned synthetic intake](panel/analysis/data/intake)
  and the [prior-recovery pipeline](panel/analysis/_targets_prior_recovery.R) with its
  [summary](panel/analysis/data/derived/prior_recovery_summary.csv).
- [Compact synthetic results](panel/analysis/data/derived/synthetic_review):
  coefficient summaries, prior-width comparisons, classifications and generating-value checks,
  with a variable dictionary, generating values and the saved calculation receipt.
- [Saved reporting inputs](panel/analysis/data/derived/report_inputs.rds) and
  [resampling results](panel/analysis/data/derived/resampling_results.rds) for the
  reproduction levels below. Full fitted models and posterior draws stay in local, ignored stores.
- [Runnable tests and fixtures](panel/analysis/tests).
- [Survey definition](panel/qualtrics/ZMASCpanel.qsf) and three fixed codebooks:
  [items](panel/preregistration/codebook_items.csv),
  [value labels](panel/preregistration/codebook_factors.csv),
  [scales](panel/preregistration/codebook_scales.csv).
  Covariate labels and column mappings are in the
  [analysis-plan YAML](panel/analysis/config/analysis_plan.yaml).

The codebooks are authoritative inputs. They are not regenerated during
analysis or publication builds. The survey definition documents the instrument; it is not a build
instruction. All supplied response-shaped data are synthetic.

The compact result files can be inspected without fitting models. Their manifest
names the contents, while the calculation receipt records the source revision,
input/source hashes, settings and run profile. The committed synthetic
demonstration uses the reduced `smoke` settings; its outputs retain that profile
and the provenance of their saved run.

## Three reproduction levels

Choose how much computation to repeat. Every level rebuilds the results report.

| Level | Recomputed | Reused |
|---|---|---|
| `report` | Tables, figures and report HTML | Committed reporting inputs from the saved analysis |
| `models` | Regression and joint models, factor analyses, correlations, model checks and full-sample network comparisons | Network bootstrap summaries, reliability bootstrap intervals and prior-recovery simulation results |
| `full` | The analysis, bootstrap resampling and prior-recovery simulation | The supplied prepared synthetic intake |

The saved reporting inputs include plot coordinates, intervals, diagnostics and
failure records needed by the complete report. The model level verifies that
saved resampling results match its data and settings; changed resampling inputs
require the full level. Reused results retain the provenance of their original
calculation. Report wording and layout can be edited while reusing the saved
numerical results.

From `panel/analysis`, after installing the reproduction environment, run one:

```sh
ZM_PROFILE=smoke Rscript scripts/reproduce.R report
ZM_DATA=synthetic ZM_PROFILE=smoke Rscript scripts/reproduce.R models
ZM_DATA=synthetic ZM_PROFILE=smoke Rscript scripts/reproduce.R full
```

The `full` reproduction level repeats every analysis stage. The computational
profile is separate: these commands reproduce the committed synthetic
demonstration's reduced settings. The full computational profile is required
for empirical analysis and is optional for a new synthetic calculation:

```sh
ZM_DATA=synthetic ZM_PROFILE=full Rscript scripts/reproduce.R full
```

The `models` level cannot reuse committed smoke resampling results under the
full computational profile; its compatibility checks reject that mismatch.

Report-only reproduction does not need a targets store or model fitting. The
models level still performs sampling; its elapsed time depends on the machine
and any convergence retries. Reduced-run timings do not estimate the runtime
of the full computational profile.

## Code Browser and section links

The Code Browser is generated exclusively from the implemented pipeline,
analysis functions and report code. Its renderer and template are implementation dependencies.

From `panel/analysis`, with the R environment available:

```sh
Rscript scripts/generate_pipeline_view.R
```

This requires committed, unchanged source. For a clearly labelled preparation
preview, use:

```sh
Rscript -e 'source("scripts/generate_pipeline_view.R"); generate_pipeline_view(allow_uncommitted = TRUE)'
```

Set `PUBLICATION_REPOSITORY_URL=https://github.com/michaelzehetleitner/social-motives-ideology` for source links to this repository;
the generator holds no repository address of its own.

After rendering the HTML preregistration and Code Browser, assemble their
reciprocal section links from the repository root:

```sh
python3 scripts/link_publication_sections.py --preregistration panel/preregistration/renders/preregistration.html --browser panel/analysis/report/code-browser.html --output site
```

The [section map](scripts/publication_sections.json) is explicit. The build
fails if a mapped document section or code section has disappeared. Updating
the map follows the documents' scientific content. Linking adds navigation to generated HTML
and leaves both QMD sources unchanged.

## Reproduce the analysis in Docker

The [Dockerfile](Dockerfile) builds the complete analysis environment on
Ubuntu 24.04: R 4.5.2, the package versions of [renv.lock](panel/analysis/renv.lock),
CmdStan 2.36.0 and Quarto 1.9.37. The image contains the cloned repository's
`panel/` folder and nothing from the machine that builds it. Report PDF export
uses PyMuPDF 1.27.2.2 and Pillow 12.0.0 in a separate Python environment, plus
XeLaTeX with the standalone and amsmath packages from TinyTeX. The PDF retains
the report HTML's tables and histogram images.

You need Docker (Docker Desktop on macOS or Windows) and internet access during
the build. For the full profile, give Docker at least 24 GB of memory
(Docker Desktop: Settings → Resources → Memory limit).

1. Clone the repository and enter it. For a published version, add
   `--branch` with its tag (named in the preregistration), for example
   `--branch submission-1 --depth 1`:

   ```sh
   git clone https://github.com/michaelzehetleitner/social-motives-ideology.git social-motives-ideology
   cd social-motives-ideology
   ```

2. Build the image (20–30 minutes the first time, about 4 GB):

   ```sh
   docker build -t social-motives-ideology .
   ```

3. Choose one reproduction level. The report command renders the committed
   results; the other commands repeat the synthetic demonstration's smoke settings:

   ```sh
   docker run --name social-motives-ideology-report -e ZM_PROFILE=smoke social-motives-ideology Rscript scripts/reproduce.R report
   docker run --name social-motives-ideology-models -e ZM_PROFILE=smoke social-motives-ideology Rscript scripts/reproduce.R models
   docker run --name social-motives-ideology-full -e ZM_PROFILE=smoke social-motives-ideology Rscript scripts/reproduce.R full
   ```

4. Copy the rendered HTML from the chosen container. For report-only reproduction:

   ```sh
   docker cp social-motives-ideology-report:/opt/zm/panel/analysis/report/results_draft.html .
   ```

   Use the corresponding container name for the models or full level.

5. Render the preregistration as HTML and PDF into `./renders`:

   ```sh
   docker run --rm -v "$PWD/renders:/opt/zm/panel/preregistration/renders" social-motives-ideology bash -c 'cd ../preregistration && quarto render preregistration.qmd --to html && quarto render preregistration.qmd --to pdf'
   ```

Every core runs one R process; `-e ZM_CORES=n` on `docker run` limits the
cores and memory used for new calculations. MCMC draws can differ between
platforms even with the same seeds. Newly fitted results are compared within
Monte Carlo uncertainty; report-only reproduction uses the saved numbers.

Run the test suite separately:

```sh
docker run --rm social-motives-ideology Rscript -e 'source("R/config.R"); zm_setup(); testthat::test_dir("tests/testthat")'
```

## Website (GitHub Pages)

Each published version has its own folder under [docs](docs): a start page,
the preregistration and synthetic results report (both HTML and PDF), and the
Code Browser, with the section links between preregistration and Code
Browser and one header on every page that leads to the others. The three
codebooks and the cleaned survey definition are downloadable from each version. The start page
comes from [scripts/site](scripts/site); `SITE_NOTE` sets its status line. A version folder is never rebuilt, so the registered version stays as
it was submitted; [docs/index.html](docs/index.html) lists all versions.

Build a version from committed source and saved reporting inputs. The default
rebuilds the website, HTML and PDFs without fitting models:

```sh
SITE_PROFILE=smoke PUBLICATION_REPOSITORY_URL=https://github.com/michaelzehetleitner/social-motives-ideology scripts/build_publication_site.sh VERSION
```

Set `SITE_REPRODUCE=models` or `SITE_REPRODUCE=full` to recalculate before
building the site. `SITE_PROFILE=smoke` matches the committed synthetic
demonstration and labels its reduced computation settings on the generated site.

The results PDF is exported from the run's self-contained HTML, including its
histogram images, Appendix and Electronic Supplement. This export does not run
R or fit models again. To repeat only the PDF export from an existing HTML:

```sh
docker run --rm -v "$PWD/renders:/out" social-motives-ideology /opt/report-pdf/bin/python scripts/render_results_pdf.py --input-html /out/results-report.html --output-pdf /out/results-report.pdf --qa-dir /out/results-pdf-qa
```

The export records its input hash and checks text, embedded images, histogram
sizes, links and printable bounds. Inspect the generated contact sheets in
`results-pdf-qa` before publication; these checks do not replace visual review.

Commit the new folder. GitHub serves it after a one-time setting in the
repository: Settings → Pages → Build and deployment → Deploy from a branch,
branch `main`, folder `/docs`.

## Reproduction environment without Docker

[renv.lock](panel/analysis/renv.lock) records R 4.5.2 and package versions.
Analysis also needs CmdStan, a C++ toolchain and Quarto. Point
`ZM_CMDSTAN_PATH` and `CMDSTAN` at the installed CmdStan directory; the
parallel workers read `CMDSTAN`. Restore the package
environment with `renv::restore()` from `panel/analysis` before running it.

```sh
cd panel/analysis
ZM_PROFILE=smoke Rscript scripts/reproduce.R report
ZM_DATA=synthetic ZM_PROFILE=smoke Rscript scripts/reproduce.R models
ZM_DATA=synthetic ZM_PROFILE=smoke Rscript scripts/reproduce.R full
Rscript -e 'source("R/config.R"); zm_setup(); testthat::test_dir("tests/testthat")'
```

The published synthetic demonstration uses the smoke profile's reduced
computation. Empirical analysis requires the full computational profile.
The prior-recovery project reruns the
prior-choice simulation in its own store
(`TAR_PROJECT=prior_recovery_smoke` for a two-replicate check). The results QMD
reads the saved reporting inputs; the preregistration reads the included
prior-recovery summary and figure.
Survey metadata cleanup and release validation are still being prepared. No
publication tag or DOI is claimed.

## License

The code (the analysis pipeline, its functions, tests, scripts and the Code
Browser generator) is released under the [MIT License](LICENSE). The
preregistration text, the results report, the codebooks and the synthetic data
are released under the Creative Commons Attribution 4.0 International License
([CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)). Questionnaire items
taken or translated from published instruments remain under the terms of their
original authors; the sources are cited in the preregistration.
