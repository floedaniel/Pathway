# Pathwise

Pathwise answers FinnPRIO question **ENT3**, "How large a volume of the considered host plant commodity is traded into the PRA area annually?", for Norway. The only input is an **EPPO code** or a **scientific name** of a pest.

It is an R Shiny app. All numbers, classes and regulation snippets come straight from EPPO, Statistics Norway (SSB), the Norwegian customs tariff and Lovdata, and are computed the same way every time. An LLM writes only one sentence per regulation unit in the write-up. Its output is checked before it is shown.

* **Online:** https://01a158ac-9fd3-b5d5-8776-1d40f1995348.share.connect.posit.cloud/ (Posit Connect Cloud)
* **Worked example:** [`docs/vignette.md`](docs/vignette.md) (*Agrilus anxius*, step by step)
* **Design decisions:** [`docs/decisions.md`](docs/decisions.md). **What the sources contain:** [`docs/findings.md`](docs/findings.md)

## Quick start

```r
# In RStudio: open pathway_app.Rproj (renv activates on start), then
renv::restore()      # first time only: installs the package versions in renv.lock
shiny::runApp()      # or shiny::runApp(port = 3838)
```

If R says `there is no package called 'config'`, the R session started before renv was active: Session > Restart R.

Keys are read from environment variables, or from the git-ignored folder `API_kays/`. An environment variable wins over a file.

| Variable | File in `API_kays/` | Used for |
|---|---|---|
| `EPPO_KEY` | `EPPO_beta.txt` | EPPO Global Database API v2 |
| `OPENAI_API_KEY` | `tore_vkm_openai.txt` | write-up (default provider) |
| `ANTHROPIC_API_KEY`, `HF_TOKEN` | `anthropic_key.txt`, `hugging_face.txt` | alternative LLM providers |

The app stops at start with a clear message if a needed key is missing. SSB, the customs tariff and Lovdata need no key. Keys are never committed or uploaded with the app.

## What you see

Type an EPPO code or a scientific name (e.g. `AGRLAX` or `Agrilus anxius`) and press **Run**. The pathways are chosen from the pest's EPPO host-commodity list, or from its host plants when EPPO has no list. There is nothing else to choose.

* **Header:** pest and code, where the pathways come from, number of conifer and non-conifer hosts, number of countries where the pest is present, years.
* **Panel 1, SSB table:** one block per pathway.
  * *All host commodities (ENT3)*: median annual import in kg over 10 years, all origins, and its FinnPRIO class (`small`, `medium`, `large`). Beside it, the kg from countries where the pest is present and its share. This share is context only, not ENT3. Conifer and non-conifer rows appear only when the pest has both kinds of host.
  * Notes on the basis (EPPO list or host plants) and on where the number is an upper bound.
  * *CN8 lines in this pathway*: code, customs-tariff wording, median kg/year, share, kg from pest-present countries, flags.
  * *Pest-present countries* and *Top origins*: one value per column (country, ISO code, kg/year, share, NOK/year, years with imports, main CN8 code, its description).
  * *Host search in SSB table 08801*: every 08801 line whose label names a host genus (Latin or Norwegian name, e.g. *bjørk* for *Betula*), with its status: counted, label excludes the host, or not counted and why.
* **Panel 2, SSB graph:** imports per year as stacked bars by country of origin. It shows the top 6 countries plus "Other countries", with the largest at the bottom, and one facet per host group. Each legend entry names the country and its largest CN8 lines. A switch shows all origins or pest-present countries only. You can download it as PNG.
* **Panel 3, Lovdata:** units of FOR-2000-12-01-1333 that name the pest, then origin rules (host genus plus a pest-present country), then units naming only the host. Each has an id such as *Vedlegg 3, nr. 11*, a link and a copy button.
* **Panel 4, FinnPRIO write-up:** one editable text per computed pathway, with Copy and Regenerate. It is labelled with the model that wrote the regulation lines.
* **Derived inputs** (collapsed): present countries, the EPPO host-commodity table with its FinnPRIO mapping, and the host plants.
* **Footer:** fetch dates of EPPO, SSB (and when table 08801 was last updated) and Lovdata.

## How it works

```
EPPO code or scientific name
  -> EPPO: resolve name to code; inactive code -> its replacement   (unknown: stop)
  -> EPPO hosts, classes "Major host" and "Host"                     (none: stop)
  -> host lineage: conifer | non-conifer | monocot                   (no plant: stop)
  -> EPPO distribution: countries with status "Present ..."          (none: stop)
  -> EPPO host commodities (gd.eppo.int/taxon/{code}/pathwayshosts):
       category + host genus -> FinnPRIO pathway and CN8 lines;
       no list, or only soil/machinery -> both pathways from host plants, labelled
  -> SSB 08801: kg and NOK of the pathway's CN8 lines, all origins, 10 years
  -> sum over origins per year, median over 10 years -> FinnPRIO class
  -> SSB host search: all 08801 labels searched for the host genera
  -> Lovdata FOR-2000-12-01-1333: units naming the pest, origin rules, host units
  -> write-up: fixed layout filled by R; LLM sentence per regulation unit, checked
```

Stops are shown as one red message. Nothing else is shown and no LLM call is made.

### Host classification
Hosts are classified from the EPPO taxonomy lineage, without a hand-made list:
* **conifer:** lineage contains Pinopsida, Pinales or Coniferales;
* **non-conifer:** any other plant (kingdom Plantae), herbaceous plants included;
* **monocot:** EPPO category "Basal monocotyledons" or "Commelinids".

EPPO commodity groups used as hosts work as follows. "Plants" and "woody plants" mean all plants, with no genus filter. "Vegetable", "fruit", "herbaceous" and "herbaceous ornamental plants" mean non-conifer.

### Pathways from EPPO host commodities
EPPO lists host commodities, at genus level, for EPPO/EU-listed pests. There is no API endpoint for them. The app reads the table on `gd.eppo.int/taxon/{code}/pathwayshosts` once per pest and caches it for 30 days. `data/eppo_commodity_map.csv` maps each EPPO category to a FinnPRIO pathway:

| EPPO category | FinnPRIO pathway | CN8 scope |
|---|---|---|
| plants for planting | `plants_for_planting` | 0602 |
| bulbs or tubers | `plants_for_planting` | 0601 |
| non-squared wood, squared wood | `wood` | 4401 firewood and chips, 4403, 4404, 4406, 4407 |
| bark, plant waste | `wood` | 4401 40-49 sawdust and wood waste |
| packaging material | `wood` | 4415 (only packaging traded as goods) |
| manufactured articles | `wood` | 4416, 9406.10 (partial) |
| seeds; cut flowers or branches, pollen; fruits or vegetables, stored products | `seeds`; `other_plant_parts`; `food_fodder` | not computed: assess manually |
| soil/growing medium, agricultural machinery, all commodities | none | information only |

A pathway counts only the CN8 lines of the categories listed for it. Those lines are then filtered by the host genera listed for that pathway.

Some pests have no list: about 20 % of A1/A2 pests and all Alert-list pests. Others list only soil or machinery. For these the app uses the host plants for both computed pathways and says so in the header. In this fallback, 0601 bulbs and tubers count only when a host is a monocot.

### CN8 lines
Lines are in `data/commodity_cn8.csv` (134 lines), built by `dev/build_commodity_cn8.R` from the current SSB labels and the customs tariff (Tolletaten). Every line records its source.

* **Scope:**
  * Plants: all 0602 lines (FOR-2000-12-01-1333 vedlegg 5A nr. 1) and 0601.
  * Wood: chapter 44 lines in the scope of vedlegg 5A nr. 8, plus prefabricated wooden buildings (9406.10).
  * Excluded: tropical wood, eucalyptus round wood, pellets and briquettes, impregnated wood.
* **Wording:** SSB labels are shortened. The tariff is the authority for what a line covers. 06029021, for example, is all trees and shrubs with soil not specified elsewhere, not only the genera SSB's label lists. The spruce lines cover fir *and* spruce.
* **Genus filter:**
  * A line that names a genus (birch, oak, pine, spruce and fir, beech, poplar, ash, maple, cherry, *Rhododendron*, *Rosa*, *Pelargonium*, ...) counts only when a host is in that genus.
  * A residual line ("other broadleaved") counts only when a host genus is outside the genera listed in its wording.
  * Mixed lines (firewood, chips, waste, packaging, catch-all plants) count for any host and are marked as an upper bound.

### ENT3 classes
Classes come from `data/finnprio_thresholds.csv`. They are applied to the median annual kg, summed over all origins, for 2016-2025: under 1 000 000 is small, up to and including 10 000 000 is medium, and anything above is large.

If nothing was imported, the result is *none recorded*, not `non_existent`; the assessor decides. Imports from pest-present countries are shown as kg and share, never as a class.

### Write-up
`R/prompts.R` builds a fixed plain-text layout that pastes cleanly into FinnPRIO:

```
ENT3: Large

Annual import into Norway of wood (CN 44, 9406.10): 291.9 million kg (median 2016-2025). ... upper bound.
Basis: EPPO host commodities: ...; host genera: Betula.

Largest CN8 lines (kg per year, share of total):
lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 83.8 million kg (29%, mixed line)
...
Imports from countries where the pest is present (information only, not ENT3):
United States of America: 44 932 kg per year (median 2016-2025)
  lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 306 069 kg in total 2016-2025
...
Regulation:
Vedlegg 3, nr. 11: <one sentence>
...
Note: ...
```

R fills in the class, the numbers, the CN8 codes and the notes. The LLM (OpenAI `gpt-5.5-2026-04-23` by default) writes one English sentence per regulation unit: first the units naming the pest, then the origin rules. Each line must start with an id shown in Panel 3; otherwise the unit's own text is shown instead. The result is cached per set of units and model, and Regenerate bypasses the cache.

## Read the results with care
* Import statistics show **recorded trade**, not pest pressure or entry.
* CN8 lines cover whole commodity groups, so the class is an **upper bound** for the host-specific volume, and most pests come out `large`. The pest-present share and the CN8 line table help judge how much of it matters.
* Wood packaging mostly travels with other goods and has no SSB line. Manufactured articles are counted only as casks and prefabricated wooden buildings. Soil has no CN line.
* Seeds, food and fodder, and other plant parts are listed for manual assessment.

## Data sources
| Source | Access | Cache |
|---|---|---|
| EPPO Global Database API v2, `https://api.eppo.int/gd/v2` | key, header `X-Api-Key` | 30 days, taxonomy permanent |
| EPPO GD host commodities, `https://gd.eppo.int/taxon/{code}/pathwayshosts` | open web page | 30 days |
| SSB table 08801, PxWebApi v2, `https://data.ssb.no/api/pxwebapi/v2` | open, 40 calls/min | 30 days |
| Tolletaten customs tariff structure, https://data.toll.no/no/dataset/tolltariffstruktur | open; copy in `docs/reference/` | used when building the CN8 table |
| Lovdata public data, current central regulations (bulk archive) | open | 7 days |
| OpenAI (default) / Anthropic / Hugging Face | key | per regulation units and model |

The cache lives in `cache/`, which is git-ignored. Fetch dates are shown in the app footer.

## Configuration
`config.yml` sets:
* the EPPO host classes (`[Major host, Host]`);
* the years (`ssb_years: 10`) and the statistic (`volume_stat: median`, `volume_n: 10`);
* the LLM provider and model, the language, and cache lifetimes.

Lookup tables are in `data/`:
* `commodity_cn8.csv`: CN8 lines;
* `eppo_commodity_map.csv`: EPPO category to FinnPRIO pathway;
* `finnprio_thresholds.csv`;
* `country_map.csv`: SSB country codes that are not ISO2;
* `law_taxon_aliases.csv`: taxon names as the regulation writes them;
* `genus_names_no.csv`: Norwegian host names from the tariff.

## Project layout
```
app.R                     UI and server
R/settings.R              keys, config, start-up check
R/api_eppo.R              EPPO client and host-commodity page reader (cached), name/code resolution
R/classify_hosts.R        conifer / non-conifer / monocot from lineage
R/api_ssb.R               SSB client (chunked, cached), CN8 line selection with genus filter
R/volume_class.R          median kg and FinnPRIO class (pure functions)
R/host_search.R           search of SSB labels for the host genera
R/law_index.R             Lovdata download, parsing, search
R/pipeline.R              run_assessment(): the whole flow for one pest
R/prompts.R, llm_provider.R   write-up layout, regulation prompt, checks, LLM call
R/mod_*.R                 Shiny modules for input and panels 1-4
data/                     lookup tables (see Configuration)
dev/build_commodity_cn8.R builds data/commodity_cn8.csv and data/genus_names_no.csv
dev/record_eppo_fixtures.R  records EPPO responses for the tests
dev/deploy.R, write_manifest.R  deployment to Posit Connect Cloud
docs/                     vignette, decisions, findings, reference copies (EPPO user guide, tariff)
tests/testthat/           offline tests (recorded EPPO responses, fake LLM)
Pathwise_notes.Rmd        project notes and open questions
```

## Tests
```r
testthat::test_dir("tests/testthat")
```
233 tests, all offline. EPPO responses and host-commodity pages are recorded in `tests/testthat/fixtures/`; refresh them with `Rscript dev/record_eppo_fixtures.R`. Lovdata is a stored copy of the regulation, and the LLM is faked.

## Deployment
The app runs on Posit Connect Cloud (account `danielflow`):

```sh
Rscript dev/deploy.R          # redeploy the current code to the same URL
Rscript dev/write_manifest.R  # after renv::snapshot(), when packages change
```

`dev/deploy.R` reads the keys into the R session and sends them as encrypted environment variables. It uploads only the app files (no keys, tests, docs or cache) and refuses to run if a key file would be included.

The app is public: anyone with the link can run it, which uses the EPPO and OpenAI keys. Access can be restricted in the Connect Cloud dashboard.

The first run on a fresh server is slow, because it downloads the Lovdata archive and fills the caches.
