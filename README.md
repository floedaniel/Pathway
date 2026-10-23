# Pathwise

Pathwise answers FinnPRIO question **ENT3**, "How large a volume of the considered host plant commodity is traded into the PRA area annually?", for Norway. The only input is an **EPPO code** or a **scientific name** of a pest.

Only imports from the **countries where the pest is present** (EPPO distribution) are used. For *Agrilus anxius* that means imports from Canada and the USA, nothing else. Norway itself is never an origin.

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

* **Header:** pest and code, where the pathways come from, number of conifer and non-conifer hosts, the number of countries where the pest is present (whose imports are used), years.
* **Panel 1, SSB table:** all numbers are imports from the countries where the pest is present.
  * **Summary** first: one row per pathway. Columns: what it is based on, kg per year (median over 10 years), ENT3 class (`small`, `medium`, `large`), largest source country, largest customs code. An EPPO commodity without a customs code (stored products, pollen) is listed as "assess manually".
  * **One numbered card per pathway** ("1. Plants for planting", "2. Wood"), with the ENT3 answer in the card header. Every card has the same two tables, with the same number columns: kg per year (median), kg in total over the 10 years, share of total, NOK in total, years with imports.
    * *By country*: one row per country with imports, plus its largest customs code. Countries without recorded imports are listed below.
    * *By customs code*: the customs (CN8) codes that make up the ENT3 amount, with a short tariff name (full text on hover). **Only the host?** says "Yes (Betula)" for a code that names the host genus and "No, also other species" for e.g. pallets of any wood.
    * Both tables end in a **Total** row. Shares are of the 10-year total, so they add up to 100 %. The total per year is the ENT3 amount: the median of the yearly totals, which is not the sum of the row medians.
    * Notes at the bottom of the card.
    * **Your selection:** untick a customs code to leave it out, or use *All codes*, *No codes* or *Host-specific codes only*. Switch a pathway off with *Include*. The ENT3 amount and class are then recomputed from the remaining codes, and the graph, the write-up ("Excluded by the assessor: ...") and the CSV follow. *Reset selection* brings everything back; a new run always starts with everything.
  * **Where the host is named in the import statistics** (own card): every customs code in SSB table 08801 whose name mentions a host genus (Latin or Norwegian name, e.g. *bjørk* for *Betula*). Each is marked counted, or not counted with the reason.
* **Panel 2, SSB graph:** imports per year from the pest-present countries as stacked bars by country. It shows the top 6 countries plus "Other countries", with the largest at the bottom, and one facet per host group. Each legend entry names the country and its largest CN8 lines. It can also be stacked by customs code. Only the codes and pathways kept in Panel 1 are shown. You can download it as PNG or CSV.
* **Panel 3, Lovdata:** units of FOR-2000-12-01-1333 that name the pest, then origin rules (host genus plus a pest-present country), then units naming only the host. Each has an id such as *Vedlegg 3, nr. 11*, a link and a copy button. Long texts are collapsed; click to show all.
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
  -> SSB 08801: kg and NOK of the pathway's CN8 lines, 10 years,
       only from the countries where the pest is present (Norway excluded)
  -> sum over those countries per year, median over 10 years -> FinnPRIO class
  -> SSB host search: all 08801 labels searched for the host genera (Latin and Norwegian names)
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
EPPO lists host commodities for EPPO/EU-listed pests, by host genus or species (e.g. *Betula*, *Solanum lycopersicum*). There is no API endpoint for them. The app reads the table on `gd.eppo.int/taxon/{code}/pathwayshosts` once per pest and caches it for 30 days. `data/eppo_commodity_map.csv` maps each EPPO category to a FinnPRIO pathway:

| EPPO category | FinnPRIO pathway | CN8 scope |
|---|---|---|
| plants for planting | `plants_for_planting` | 0602; 0701 10 seed potatoes (potato hosts only) |
| bulbs or tubers | `plants_for_planting` | 0601 (not for potato); 0701 10 seed potatoes |
| non-squared wood, squared wood | `wood` | 4401 firewood and chips, 4403, 4404, 4406, 4407 |
| bark, plant waste | `wood` | 4401 40-49 sawdust and wood waste |
| packaging material | `wood` | 4415 (only packaging traded as goods) |
| manufactured articles | `wood` | 4416, 9406.10 (partial) |
| fruits or vegetables | `food_fodder` | fresh or chilled products of chapters 07 and 08 (0701-0709, 0714, 0801-0810) |
| cut flowers or branches | `other_plant_parts` | fresh 0603 cut flowers; 0604 20 foliage, branches, Christmas trees |
| seeds | `seeds` | seed grain (10 .. "såkorn"), seeds for sowing (12 09 "såvare"), in kg |
| stored products, pollen | `food_fodder`, `other_plant_parts` | no customs code: assess manually |
| soil/growing medium, agricultural machinery, all commodities | none | information only |

A pathway counts only the CN8 lines of the categories listed for it. Those lines are then filtered by the hosts listed for that pathway (see *Host matching* below).

Some pests have no list: about 20 % of A1/A2 pests and all Alert-list pests. Others list only soil or machinery. For these the app builds **plants for planting and wood only**, from the host plants, and says so in the header. Food and fodder, cut flowers and seeds need an EPPO list. In this fallback, 0601 bulbs and tubers count only when a host is a monocot.

### CN8 lines
Lines are in `data/commodity_cn8.csv` (423 lines: 33 plants for planting, 102 wood, 218 food and fodder, 43 other plant parts, 27 seeds). They are built by `dev/build_commodity_cn8.R` from the current SSB labels, the customs tariff (Tolletaten) and the crop-name table `data/cn8_host_taxa.csv`. Every line records its source.

* **Scope:**
  * Plants: all 0602 lines (FOR-2000-12-01-1333 vedlegg 5A nr. 1), 0601, and seed potatoes 0701 10.
  * Wood: chapter 44 lines in the scope of vedlegg 5A nr. 8, plus prefabricated wooden buildings (9406.10). Excluded: tropical wood, eucalyptus round wood, pellets and briquettes, impregnated wood.
  * Food and fodder: only **fresh or chilled** products (the tariff wording says "frisk", "fersk" or "kjølt"). Frozen, dried and preserved products and mushrooms are out. Lines worded "fresh or dried" (nuts, citrus, bananas) are in and flagged.
  * Other plant parts: fresh cut flowers (0603) and fresh foliage, branches and Christmas trees (0604 20); dried or treated ones are out.
  * Seeds: seed grain and seeds for sowing, counted in kg (FinnPRIO assesses seeds in pieces; kg by decision).
* **Wording:** SSB labels are shortened. The tariff is the authority for what a line covers. 06029021, for example, is all trees and shrubs with soil not specified elsewhere, not only the genera SSB's label lists. The spruce lines cover fir *and* spruce.

### Host matching
Hosts are matched on the **EPPO taxonomy**. Each host's EPPO name and lineage from family down (e.g. Solanaceae > *Solanum* > *Solanum lycopersicum*) comes from the EPPO API and is cached permanently.

Each code names the taxa it covers: a genus (birch round wood = *Betula*), a species (tomatoes = *Solanum lycopersicum*, oranges = *Citrus x aurantium var. sinensis*) or a family (orchids = Orchidaceae). For the new pathways these names are in `data/cn8_host_taxa.csv`, the crop-name table. That table maps each code pattern to its pathway, the Norwegian words used in the labels, the taxa, exclusions, host group and flags. All Latin names are EPPO preferred names; `Rscript dev/check_host_taxa.R` checks them against the EPPO database.

* **Code that names taxa:** counts when a host belongs to one of them. A genus-level host (e.g. "*Solanum*") also counts for the species codes of its genus (tomatoes, potatoes, aubergines).
* **Residual code with exclusions** ("other broadleaved", "other Allium"): counts when a host is not covered by the exclusions.
* **"Not elsewhere specified" code** ("vegetables n.e.s.", "fruits n.e.s.", "vegetable seeds n.e.s."): counts for hosts that no other code of the pathway names. Some are scoped: forage seeds only for grasses and legumes, vegetable seeds only for vegetable crops. They are marked as an upper bound.
* **Mixed code** (pallets, firewood, catch-all plants, foliage and branches): counts for any host and is marked as an upper bound.
* The hybrid sign is ignored when comparing names ("Citrus x limon" = "Citrus limon").

### ENT3 classes
Classes come from `data/finnprio_thresholds.csv`. They are applied to the median annual kg, summed over the countries where the pest is present, for 2016-2025: under 1 000 000 is small, up to and including 10 000 000 is medium, and anything above is large.

If nothing was imported from those countries, the result is *none recorded*, not `non_existent`; the assessor decides.

Imports from countries where the pest is absent are not used anywhere in the app. `ssb_imports()` drops them as soon as the data arrive, so the tables, the graph and the write-up never see them. SSB is still queried for all origins: the cached data are then shared between pests, and the filter is applied afterwards. Norway, where some pests are already present, is never treated as an origin. It is also left out of the countries used to find Lovdata origin rules.

### Write-up
`R/prompts.R` builds a fixed plain-text layout that pastes cleanly into FinnPRIO:

```
ENT3: Small

Annual import into Norway of wood (CN 44, 9406.10) from the countries where the pest is present
(Canada, United States of America): 49 144 kg (median 2016-2025). Imports from other countries are
not counted. ... upper bound.
Basis: EPPO host commodities: ...; hosts: Betula.

Largest CN8 lines (median kg per year, share of total, kg in total):
lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 32 526 kg per year (66%, mixed line), 327 671 kg in total 2016-2025
...
Imports by country:
United States of America: 44 932 kg per year (median 2016-2025)
  lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 306 069 kg in total 2016-2025
...
No recorded imports from: <countries>  (when some pest-present countries have none)
Regulation:
Vedlegg 3, nr. 11: <one sentence>
...
Note: ...
```

R fills in the class, the numbers, the CN8 codes and the notes. The LLM (OpenAI `gpt-5.4-mini-2026-03-17` by default, a small model: the task is short) writes one English sentence per regulation unit: first the units naming the pest, then the origin rules. Each line must start with an id shown in Panel 3; otherwise the unit's own text is shown instead. The result is cached per set of units and model, and Regenerate bypasses the cache.

## Read the results with care
* Import statistics show **recorded trade**, not pest pressure or entry.
* Only trade from countries where EPPO records the pest as present is counted. Countries missing from the EPPO distribution, and goods re-exported through third countries, are not covered.
* CN8 lines cover whole commodity groups, so the class is an **upper bound** for the host-specific volume. The CN8 line table helps judge how much of it matters.
* Wood packaging mostly travels with other goods and has no SSB line. Manufactured articles are counted only as casks and prefabricated wooden buildings. Soil has no CN line.
* Food and fodder counts only fresh or chilled products. Seeds are in kg, not pieces. Stored products and pollen have no customs code.
* "Not elsewhere specified" codes (other vegetables, other fruit, other seeds) count for hosts without a code of their own, so exotic hosts can get several of them.

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
* `commodity_cn8.csv`: CN8 lines (built by `dev/build_commodity_cn8.R`);
* `cn8_host_taxa.csv`: the crop-name table, code pattern -> pathway, Norwegian words, host taxa (EPPO names);
* `eppo_commodity_map.csv`: EPPO category to FinnPRIO pathway;
* `finnprio_thresholds.csv`;
* `country_map.csv`: SSB country codes that are not ISO2;
* `law_taxon_aliases.csv`: taxon names as the regulation writes them;
* `genus_names_no.csv`: Norwegian host names from the tariff and the crop-name table (for the host search).

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
dev/check_host_taxa.R    checks the Latin names in data/cn8_host_taxa.csv against EPPO
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
All tests run offline (253 checks). EPPO responses and host-commodity pages are recorded in `tests/testthat/fixtures/`; refresh them with `Rscript dev/record_eppo_fixtures.R`. Lovdata is a stored copy of the regulation, and the LLM is faked.

## Deployment
The app runs on Posit Connect Cloud (account `danielflow`):

```sh
Rscript dev/deploy.R          # redeploy the current code to the same URL
Rscript dev/write_manifest.R  # after renv::snapshot(), when packages change
```

`dev/deploy.R` reads the keys into the R session and sends them as encrypted environment variables. It uploads only the app files (no keys, tests, docs or cache) and refuses to run if a key file would be included.

The app is public: anyone with the link can run it, which uses the EPPO and OpenAI keys. Access can be restricted in the Connect Cloud dashboard.

The first run on a fresh server is slow, because it downloads the Lovdata archive and fills the caches.
