# Pathwise

Pathwise answers FinnPRIO question **ENT3**, "How large a volume of the considered host plant commodity is traded into the PRA area annually?", for Norway, from a single input: the **EPPO code** of a pest.

It is an R Shiny app. All numbers, classes and regulation snippets are deterministic and come straight from EPPO, Statistics Norway (SSB) and Lovdata. An LLM only writes the short justification text, from those results, and its output is checked before it is shown.

## Quick start

```r
# In RStudio: open pathway_app.Rproj (renv activates on start), then
renv::restore()      # first time only: installs the package versions in renv.lock
shiny::runApp()      # or shiny::runApp(port = 3838)
```

Keys are read from environment variables, or from the git-ignored folder `API_kays/`:

| Variable | File in `API_kays/` | Used for |
|---|---|---|
| `EPPO_KEY` | `EPPO_beta.txt` | EPPO Global Database API v2 |
| `OPENAI_API_KEY` | `tore_vkm_openai.txt` | ENT3 write-up (default provider) |
| `ANTHROPIC_API_KEY`, `HF_TOKEN` | `anthropic_key.txt`, `hugging_face.txt` | alternative LLM providers |

The app stops at start with a clear message if a needed key is missing. SSB and Lovdata need no key.

## What you see

Type an EPPO code (or look one up by name), pick pathways, press **Run**.

* **Header:** pest, host groups, number of hosts, number of countries where the pest is present, years.
* **Panel 1, SSB table:** one block per pathway. The first row, *All host commodities (ENT3)*, gives the median annual import in kg over 10 years and its FinnPRIO class (`small`, `medium`, `large`). Beside it: the kg imported from countries where the pest is present, and its share (context only, not ENT3). Warnings explain where the class is an upper bound. Top origins are listed below.
* **Panel 2, SSB graph:** imports per year by origin (top 6, rest as Other), one facet per host group, pest-present origins marked with `*`. Downloadable as PNG.
* **Panel 3, Lovdata:** units of FOR-2000-12-01-1333 that name the pest (or its higher taxon) first, then units naming host genera. Each has an id such as *Vedlegg 2, nr. 10*, a link and a copy button.
* **Panel 4, FinnPRIO write-up:** one editable text per pathway (max 200 characters), labelled with the model that wrote it, with a copy button and Regenerate.
* **Derived inputs:** the EPPO host table and present countries.

## How it works

```
EPPO code
  -> EPPO: valid, active code                         (else stop)
  -> EPPO hosts, classes "Major host" and "Host"      (else stop)
  -> host lineage: conifer | non-conifer | monocot    (no plant host: stop)
  -> EPPO distribution: countries with status "Present ..."   (none: stop)
  -> SSB 08801: imports of the pathway's CN8 lines, all origins, 10 years
  -> median annual kg, summed over origins -> FinnPRIO class
  -> Lovdata FOR-2000-12-01-1333: units matching pest names, taxa, host genera
  -> LLM write-up from the labelled results, validated
```

### Host classification
From the EPPO taxonomy lineage, no hand-made list:
* **conifer:** lineage contains Pinopsida, Pinales or Coniferales;
* **non-conifer:** any other plant (kingdom Plantae), herbaceous plants included;
* **monocot:** EPPO category "Basal monocotyledons" or "Commelinids" (used for bulbs and tubers).

### Pathways and CN8 lines
Pathway ids follow FinnPRIO. Lines are in `data/commodity_cn8.csv`, built by `dev/build_commodity_cn8.R` from the current SSB labels; every line records its source.

| Pathway | CN8 lines | Notes |
|---|---|---|
| `plants_for_planting` | all 0602 lines; 0601 bulbs and tubers only when a host is a monocot | conifer plants only appear in catch-all 0602 lines (upper bound); soil has no CN line |
| `wood` | ch. 44 lines in the scope of FOR-2000-12-01-1333 vedlegg 5A nr. 8, plus prefabricated wooden buildings | tropical, eucalyptus round wood, pellets/briquettes and impregnated wood excluded; most wood packaging travels with other goods and has no SSB line |
| `seeds`, `food_fodder`, `other_plant_parts` | none | listed for manual assessment |

### ENT3 classes
From `data/finnprio_thresholds.csv`, applied to the median annual kg over all origins:
`< 1 000 000` small, `<= 10 000 000` medium, otherwise large. No recorded imports give *none recorded*, not `non_existent`; the assessor decides.

### Write-up
`R/prompts.R` gives the model labelled EPPO, SSB and LAW blocks and strict rules. The reply is rejected if it contains markdown or line breaks, exceeds 200 characters, states a class other than Panel 1's, or cites a Lovdata id that is not in Panel 3. After one retry the app falls back to a draft built only from Panel 1 numbers.

## Read the results with care
* Import statistics show **recorded trade**, not pest pressure or entry.
* CN8 lines cover whole commodity groups, so classes are an **upper bound** for host-specific volume (most come out `large`). The pest-present share helps judge how much of it matters.
* Classes assume the selected pathway.

## Data sources
| Source | Access | Cache |
|---|---|---|
| EPPO Global Database API v2, `https://api.eppo.int/gd/v2` | key, header `X-Api-Key` | 30 days, taxonomy permanent |
| SSB table 08801, PxWebApi v2, `https://data.ssb.no/api/pxwebapi/v2` | open, 40 calls/min | 30 days |
| Lovdata public data, current central regulations (bulk archive) | open | 7 days |
| OpenAI (default) / Anthropic / Hugging Face | key | per prompt and model |

Cache lives in `cache/` (git-ignored). Fetch dates are shown in the app footer.

## Configuration
`config.yml`: host classes, years (`ssb_years: 10`), statistic (`volume_stat: median`, `volume_n: 10`), LLM provider and model, language, write-up length, cache lifetimes. Lookup tables are in `data/`.

## Project layout
```
app.R                     UI and server
R/api_eppo.R              EPPO client (cached), code check, hosts, present countries
R/api_ssb.R               SSB client (chunked, cached), CN8 line selection
R/classify_hosts.R        conifer / non-conifer / monocot from lineage
R/volume_class.R          median kg and FinnPRIO class (pure functions)
R/law_index.R             Lovdata download, parsing, search
R/pipeline.R              run_assessment(): the whole flow for one code
R/prompts.R, llm_provider.R   write-up prompt, validation, LLM call
R/mod_*.R                 Shiny modules for input and panels 1-4
data/                     CN8 lines, thresholds, country exceptions, law aliases
dev/                      scripts that build data/commodity_cn8.csv and record test fixtures
docs/decisions.md         every design decision, dated
docs/findings.md          what the live APIs and the regulation turned out to contain
tests/testthat/           offline tests (recorded EPPO responses, fake LLM)
```

## Tests
```r
testthat::test_dir("tests/testthat")
```
The tests run offline: EPPO responses are recorded in `tests/testthat/fixtures/` (refresh with `Rscript dev/record_eppo_fixtures.R`), Lovdata is a stored copy of the regulation, and the LLM is faked.
