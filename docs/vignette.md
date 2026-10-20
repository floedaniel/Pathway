# Pathwise vignette: *Agrilus anxius* step by step

This walk-through follows one pest, the bronze birch borer *Agrilus anxius* (EPPO code `AGRLAX`), through every step of Pathwise. All numbers are from a run on 2026-10-20 (SSB table 08801, years 2016-2025). Later runs can differ slightly when SSB revises its data or EPPO updates its records.

Pathwise only uses imports from the countries where the pest is present. For *A. anxius* these are Canada and the USA; trade from every other country is left out.

To reproduce it, start the app (`shiny::runApp()`) and type `Agrilus anxius` or `AGRLAX`. The same run can be done from the R console:

```r
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
load_local_keys()
settings <- load_settings()
r <- run_assessment("Agrilus anxius", settings)
r$classes          # ENT3 per pathway
```

A first run takes about 30 seconds: it downloads EPPO records, the SSB data and the Lovdata archive. After that, cached runs take about a second.

## 1. From name to EPPO code

`resolve_input()` sends the name to EPPO's `tools/name2codes` and gets `AGRLAX`. A code typed directly is checked against EPPO instead. A code that EPPO has deactivated is replaced by the code it points to (`replacedby`). For example, `LITYMC` becomes `LITYCR`.

## 2. Hosts and their classification

EPPO lists 18 hosts in the classes the app uses: 15 Major hosts (e.g. *Betula pendula*, *B. pubescens*, *B. papyrifera*) and 3 Hosts (*Betula*, *B. nigra*, *B. pumila*). Other EPPO host classes (Alternate, Wild/Weed, Experimental, ...) are not used.

Each host's EPPO taxonomy lineage decides its group. All 18 are in Betulaceae, with no Pinopsida in the lineage, so they are **non-conifer**. The header shows "Hosts: 0 conifer, 18 non-conifer".

## 3. Where the pest is present, and so which imports count

The pest's EPPO distribution has two countries with a status starting with "Present": **Canada** and the **United States**. These two countries are the only origins whose imports Pathwise uses. The header says "SSB imports only from the 2 countries where the pest is present".

SSB is queried for all origins, and the data are cached that way so that other pests can reuse them. `ssb_imports()` then drops every country except Canada and the USA. Nothing after that step sees the other countries.

Norway is never an origin. For pests already present in Norway, such as *Ips typographus*, the text adds "The pest is also present in Norway".

The two countries are also used in the Lovdata search, to find origin rules.

## 4. Pathways from the EPPO host-commodity list

EPPO's page `gd.eppo.int/taxon/AGRLAX/pathwayshosts` lists six host commodities, all for the genus *Betula*:

| EPPO category | FinnPRIO pathway |
|---|---|
| plants for planting | plants_for_planting |
| squared wood | wood |
| non-squared wood | wood |
| bark | wood |
| manufactured articles | wood |
| packaging material | wood |

So the app computes two pathways, **plants for planting** and **wood**. The header says "Pathways from: EPPO host commodities (6 entries)". The full table, with each category's FinnPRIO pathway, is under *Derived inputs*.

If EPPO had no list for the pest, the app would build both pathways from the host plants and say so in the header.

## 5. CN8 lines and the genus filter

Each pathway takes the CN8 lines of its EPPO categories from `data/commodity_cn8.csv` and filters them by the host genus *Betula*:

* Birch lines, such as *Tømmer av bjørk* (4403 95) and *Trelast av bjørk* (4407 96), count.
* Lines for other single genera, such as oak, pine, beech and poplar, do not count.
* Residual lines such as "sawn wood, not of conifers, oak, beech ..." (4407 99) count, because birch is not among the genera they exclude.
* Mixed lines, such as pallets, firewood of broadleaved trees, prefabricated wooden buildings and catch-all plant lines, count for any host. They are marked as an upper bound.

The genus filter matters. Before it existed, about 90 % of the non-conifer wood imported from Canada and the USA was oak. With the filter, those imports fell from 5.3 million kg to under 50 t per year, mostly pallets.

## 6. ENT3 classes (Panel 1)

For each pathway, the app sums the kg over Canada and the USA per year, then takes the median over 2016-2025:

| Pathway | Median kg/year from CA + US | ENT3 |
|---|---|---|
| Plants for planting | 949 | **small** |
| Wood | 49 145 | **small** |

Both are `small` (under 1 million kg).

The country table shows where the volume comes from:

| Pathway | Country | kg/year | Main CN8 line |
|---|---|---|---|
| Plants for planting | United States | 946 | 06029021 trees and shrubs with soil n.e.s. |
| Plants for planting | Canada | 0 (imports in 1 of 10 years) | 06029021 |
| Wood | United States | 44 932 | 44152000 pallets |
| Wood | Canada | 2 302 | 44079900 sawn broadleaved wood, not oak, beech, conifers or tropical |

The *CN8 lines in this pathway* table gives each line's median, its share of the total, and its total over the ten years. The totals matter because many small flows have a median of 0:

* **Plants for planting:** 06029021 accounts for 766 kg/year (81 %), or 7 998 kg in total. The other lines have a median of 0 but some imports: cuttings 06021029 (924 kg in total), green pot plants 06029023 (749 kg) and 06029039 (581 kg).
* **Wood:**
  * Pallets 44152000: 32 526 kg/year (66 %), 327 671 kg in total.
  * Crates 44151000: 14 060 kg/year (29 %), 130 249 kg in total.
  * Broadleaved chips 44012200: 400 kg/year.
  * Sawn broadleaved wood 44079900 (156 887 kg in total) and prefabricated wooden buildings 94061090 (53 569 kg in total) came in a few years only, so their median is 0.

All of these are mixed lines: they cover the host but much else besides, so even the small numbers are upper bounds for birch.

## 7. Host search in SSB (Panel 1)

Every 08801 label valid in 2016-2025 is searched for *Betula* and its Norwegian name *bjørk*. Whole words only, and exclusion clauses such as "unntatt bjørk" are recognised:

| Status | Lines |
|---|---|
| counted: Wood | 6 birch lines: round wood and pulpwood (44039500, 44039601, 44039609, and the older codes 44039901, 44039902), sawn birch (44079600) |
| label excludes the host | 6 lines, e.g. "Tømmer ... unnt av bartrær, ..., bjørk, poppel" (4403 99), veneer and plywood "ikke av ... bjørk" / "unntatt bjørk" |
| not counted: outside the CN8 scope | birch veneer sheets (4408 90 03) and birch-faced plywood (4412 32 01, 4412 33 01) |

The search does not change ENT3. It shows where the host appears in the statistics and which of those lines the app leaves out. Of the six birch lines, only sawn birch (44079600) had imports from Canada or the USA in 2016-2025: 31 kg in total from the USA. Whether veneer and plywood should count, for example as EPPO's "manufactured articles", is an open question in `Pathwise_notes.Rmd`.

## 8. Graph (Panel 2)

The bars show imports per year from Canada and the USA, stacked by country, with the largest at the bottom. Each legend entry names the country and its largest CN8 lines:

* Wood: "United States of America (44152000, 44151000, 44079900)" and "Canada (44079900, 94061090, 44152000)".
* Plants for planting: "United States of America (06029021, 06021029, 06029023)" and "Canada (06029021)".

## 9. Regulation (Panel 3)

17 units of FOR-2000-12-01-1333 match:

1. **Units naming the pest:** Vedlegg 1, nr. 2.1 (*Agrilus anxius* is a regulated pest) and Vedlegg 4A, nr. 45 (requirements for *Betula* plants that name the pest).
2. **Origin rules** (host genus and a pest-present country):
   * Vedlegg 3, nr. 11: ban on *Betula* plants, plant parts, chips and wood waste from Canada and the USA.
   * Vedlegg 4A, nr. 43, 44, 46, 47: requirements for *Betula* wood and isolated bark.
3. **Units naming only the host:** e.g. Vedlegg 2, nr. 8, Vedlegg 4A, nr. 10 and nr. 48-51, and Vedlegg 5A lines for round and sawn wood.

## 10. Write-up (Panel 4)

One text per pathway, ready to paste into FinnPRIO. This is the wood text from the app, with the regulation sentences written by `gpt-5.5-2026-04-23`:

```
ENT3: Small

Annual import into Norway of wood (CN 44, 9406.10) from the countries where the pest is present (Canada, United States of America): 49 144 kg (median 2016-2025). Imports from other countries are not counted. The figure covers all products in these CN lines, not only host commodities, so it is an upper bound.
Basis: EPPO host commodities: squared wood, non-squared wood, bark, manufactured articles, packaging material; host genera: Betula.

Largest CN8 lines (median kg per year, share of total, kg in total):
lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 32 526 kg per year (66%, mixed line), 327 671 kg in total 2016-2025
kasser, esker, sprinkelkasser, dunker og liknende emballasje; kabeltromler (44151000): 14 060 kg per year (29%, mixed line), 130 249 kg in total 2016-2025
tre i form av fliser eller spon, av lauvtrær (44012200): 400 kg per year (<1%, mixed line), 14 152 kg in total 2016-2025
Trelast, saget/kuttet i lengden; også høvlet, pusset el skjøtet, tykkelse o 6 mm, ikke av bartrær, eik, bøk el div tropiske trær (44079900): 0 kg per year (0%, mixed line), 156 887 kg in total 2016-2025
av tre, ellers (94061090): 0 kg per year (0%, mixed line), 53 569 kg in total 2016-2025

Imports by country:
United States of America: 44 932 kg per year (median 2016-2025)
  lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 306 069 kg in total 2016-2025
  kasser, esker, sprinkelkasser, dunker og liknende emballasje; kabeltromler (44151000): 109 622 kg in total 2016-2025
  Trelast, saget/kuttet i lengden; også høvlet, pusset el skjøtet, tykkelse o 6 mm, ikke av bartrær, eik, bøk el div tropiske trær (44079900): 36 501 kg in total 2016-2025
Canada: 2 302 kg per year (median 2016-2025)
  Trelast, saget/kuttet i lengden; også høvlet, pusset el skjøtet, tykkelse o 6 mm, ikke av bartrær, eik, bøk el div tropiske trær (44079900): 120 386 kg in total 2016-2025
  av tre, ellers (94061090): 45 043 kg in total 2016-2025
  lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 21 602 kg in total 2016-2025

Regulation:
Vedlegg 1, nr. 2.1: lists Agrilus anxius (Gory), American birch borer.
Vedlegg 4A, nr. 45: sets import requirements for Betula plants and propagating material, excluding seeds, requiring origin in a country known free from Agrilus anxius.
Vedlegg 3, nr. 11: bans import of Betula plants, plant parts except seeds, chips, shavings and wood waste from Canada and the USA.
Vedlegg 4A, nr. 43: sets import requirements for Betula wood, excluding Betula chips, shavings, wood waste and ISPM15 wood packaging in use or previously used.
Vedlegg 4A, nr. 44: sets import requirements for isolated Betula bark from Canada and the USA, requiring official confirmation that the bark is free from wood.
Vedlegg 4A, nr. 46: sets import requirements for wood of listed broadleaf genera, including Betula, whether or not retaining its natural round surface.

Note: Genus-specific wood lines (e.g. birch, oak, pine) count only for host genera; mixed lines (firewood, chips, waste, packaging) are an upper bound.
Note: Wood packaging: CN 4415 only captures packaging traded as goods. Most packaging travels with other products and has no SSB line.
Note: Bark is part of this pathway; isolated bark is counted in the wood waste lines (4401 40-49).
Note: Manufactured articles: only casks (4416) and prefabricated wooden buildings (9406.10) are counted; other manufactured wood is not.
```

Everything except the regulation sentences comes from R: the class, the numbers, the CN8 codes and tariff labels, and the notes. If the LLM fails, or returns a line that does not start with a Panel 3 id, the app shows the unit's own text instead.

When a pest is present in many countries, the text names the number of countries instead of listing them. It lists up to 10 countries with imports, then says how many further countries had smaller imports, and ends with one line naming the countries without recorded imports. For *Ips typographus* (43 origin countries) the wood text starts "from the 43 countries where the pest is present: 435.2 million kg ... The pest is also present in Norway."

## 11. Reading the result

* **ENT3 = small for both pathways.** Norway imports under a ton of plants for planting and about 49 t of wood per year in the relevant CN lines from the countries where *A. anxius* occurs.
* **The volumes are still upper bounds.** The counted lines are mixed: pallets, crates, all trees and shrubs, sawn broadleaved wood. The birch-specific SSB lines carried only 31 kg of sawn birch from the USA over ten years.
* **The regulation already blocks the main host material.** Norway bans *Betula* plants, plant parts, chips and wood waste from Canada and the USA (Vedlegg 3, nr. 11), and sets requirements for *Betula* wood and bark (Vedlegg 4A, nr. 43, 44, 46).
