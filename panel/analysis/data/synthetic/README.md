# Synthetic raw export

Generated test data for the ZM panel analysis pipeline. Not real responses.
Produced by `scripts/make_synthetic_data.R` from `config/simulation_truth.yaml`
(ground truth) with `R/simulate_testdata.R`; the export layout follows
`R/io_qualtrics.R::zm_raw_schema()` (97 columns), and the file carries the
column attributes of the Qualtrics random-data export in `template/`.

## Files

- `zm_panel_synthetic.sav` — the raw export as a Qualtrics SAV file
  (776 rows, 97 columns). Read it with `read_qualtrics_export()`.
- `zm_panel_synthetic_latent.csv` — one row per respondent (same order as
  the export, `respondent_row` = row number, `ResponseId` for joining):
  the nine latent scores the items were generated from, `age`, `male`,
  `gender`, `income_true`, and `exclusion_reason`.

## How it was generated

- Seed: `20260905` (from `simulation_truth.yaml`); regenerate with
  `/opt/homebrew/bin/Rscript scripts/make_synthetic_data.R` from `panel/analysis`.
- Latent motives follow the declared motive correlations; outcome latents
  are `X beta + residual` with the declared partial coefficients and residual
  correlations (variance 1 per outcome). Sample moments are exact: residuals
  are orthogonal to the predictors, so an OLS of each outcome latent on the
  standardised predictors returns `true_beta` exactly.
- Items: loading 0.65 on the scale's latent, location and spread per scale,
  rounded and clamped to 1..6; reverse-keyed items stored as displayed
  (`(min + max) - x`), like the live survey.
- Party scalometers, `possibly_left` / `possibly_conservative` and
  `quota_group` follow the M4 rule (liked = scalometer > 0).
- Exclusion rows are generated in exact counts and shaped like the survey
  flow produces them; they are interleaved with valid rows by a seeded shuffle.

## Scenario

This export realises the **`trouble`** scenario of `simulation_truth.yaml`
(regenerate it with `--scenario=trouble`). The truth file declares it as:

> The base truth with a prestige contrast and three planted faults. Everything the base truth states and this scenario does not name — the exclusion counts including the 26 quota-full rows, the motive correlations, the measurement model, the quota derivation — is unchanged, so n_kept stays 706 and the AP1 table is the same as under `clean`.

Planted faults:

- `true_beta` override: `asc_conv ~ zm_prestige` = -0.15 (preregistration Table 2 predicts `±`).
- Contaminated residual: 5% of the `asc_agg` residual draws multiplied by 4.
- Cross-loading: `ASC_con_3` also loads 0.65 on the `asc_sub` latent.
- Blanked cells, applied to the finished export among the rows AP1 keeps
  (so AP1 excludes none of them and AP3's missing-data rule has to act):

| column | blanked | model variable it makes missing |
|---|---|---|
| `ASC_aag_1` | 3 | asc_agg |
| `UMS_int_1` | 2 | zm_security |
| `demo_age` | 2 | age |
| `demo_income_hh_net` | 2 | income |
| `demo_gender` | 1 | gender |

## Row counts

Total rows: 776. Valid complete rows (complete, consented, age 18–69, both
attention checks correct, including divers): 706.

Exclusion reason (from the latent file):

| reason | n |
|---|---|
| age_under_18 | 4 |
| attention_1_failed | 12 |
| attention_2_failed | 8 |
| consent_refused | 6 |
| incomplete | 14 |
| none | 706 |
| quota_full | 26 |

`survey_status` in the export:

| survey_status | n |
|---|---|
| (empty: incomplete) | 14 |
| attention_failed | 20 |
| complete | 710 |
| consent_refused | 6 |
| quota_full | 26 |

Quota group among rows with `survey_status = complete`:

| quota_group | n |
|---|---|
| conservative_leaning | 216 |
| left_leaning | 236 |
| mixed | 258 |

