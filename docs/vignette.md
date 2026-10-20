# Pathwise vignette: *Agrilus anxius* step by step

This walk-through follows one pest, the bronze birch borer *Agrilus anxius* (EPPO code `AGRLAX`), through every step of Pathwise. All numbers are from a run on 2026-10-20 (SSB table 08801, years 2016-2025). Later runs can differ slightly when SSB revises its data or EPPO updates its records.

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

## 3. Where the pest is present

The pest's EPPO distribution has two countries with a status starting with "Present": **Canada** and the **United States**. These countries do not restrict the import data. ENT3 counts all origins. They are used for the context columns, the graph switch and the Lovdata search.

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

The genus filter matters. Before it existed, about 90 % of "non-conifer wood from pest-present countries" was oak. With the filter, imports from Canada and the US fell from 5.3 million kg to about 47 t per year, mostly pallets.

## 6. ENT3 classes (Panel 1)

For each pathway, the app sums the kg over all origin countries per year, then takes the median over 2016-2025:

| Pathway | Median kg/year, all origins | ENT3 | kg/year from CA + US | Share |
|---|---|---|---|---|
| Plants for planting | 35 300 028 | **large** | 949 | < 0.01 % |
| Wood | 291 906 144 | **large** | 49 145 | 0.02 % |

Both classes are `large` (above 10 million kg). The *CN8 lines in this pathway* table shows why the number is an upper bound:

* **Plants for planting:** the largest line is 06029021. In the customs tariff this is all trees and shrubs with soil not specified elsewhere: 15.1 million kg/year, or 43 %. It is a mixed line that includes birch but much else besides. Perennials (06029022, 16 %) and flowering pot plants (06029049, 8 %) follow.
* **Wood:** pallets (44152000, 83.8 million kg, 29 %), firewood of broadleaved trees (44011200, 21 %) and prefabricated wooden houses (94061019, 19 %; 94061090, 12 %) dominate. None of these is specific to birch.

The pest-present tables put the relevant trade in perspective:

* **Plants for planting:** the United States sends 946 kg/year, mainly 06029021 (7 993 kg in total over 2016-2025). Canada's median is 0 kg, with one year of imports (5 kg in total).
* **Wood:** the United States sends 44 932 kg/year, mostly pallets (44152000) and crates (44151000). Canada sends 2 302 kg/year, mostly sawn broadleaved wood (44079900) in one year.

The assessor answers ENT3 with the class (`large`) and can use these context numbers in the justification.

## 7. Host search in SSB (Panel 1)

Every 08801 label valid in 2016-2025 is searched for *Betula* and its Norwegian name *bjørk*. Whole words only, and exclusion clauses such as "unntatt bjørk" are recognised:

| Status | Lines |
|---|---|
| counted: Wood | 6 birch lines: round wood and pulpwood (44039500, 44039601, 44039609, and the older codes 44039901, 44039902), sawn birch (44079600) |
| label excludes the host | 6 lines, e.g. "Tømmer ... unnt av bartrær, ..., bjørk, poppel" (4403 99), veneer and plywood "ikke av ... bjørk" / "unntatt bjørk" |
| not counted: outside the CN8 scope | birch veneer sheets (4408 90 03) and birch-faced plywood (4412 32 01, 4412 33 01) |

The search does not change ENT3. It shows where the host appears in the statistics and which of those lines the app leaves out. Whether veneer and plywood should count, for example as EPPO's "manufactured articles", is an open question in `Pathwise_notes.Rmd`.

## 8. Graph (Panel 2)

The bars show imports per year, stacked by country of origin: the top 6 countries plus "Other countries", with the largest at the bottom. Each legend entry names the country and its largest CN8 lines. For wood, the largest origins are "Sweden (44152000, 44013901, 94061019)", then Estonia, Latvia, Lithuania, Finland and Russia. For plants for planting they are "Netherlands (06029021, 06029022, 06029039)", then Germany, Denmark, Sweden, Italy and Belgium. The switch *Pest-present origins only* redraws the graph for Canada and the United States, which makes their small volumes visible.

## 9. Regulation (Panel 3)

17 units of FOR-2000-12-01-1333 match:

1. **Units naming the pest:** Vedlegg 1, nr. 2.1 (*Agrilus anxius* is a regulated pest) and Vedlegg 4A, nr. 45 (requirements for *Betula* plants that name the pest).
2. **Origin rules** (host genus and a pest-present country):
   * Vedlegg 3, nr. 11: ban on *Betula* plants, plant parts, chips and wood waste from Canada and the USA.
   * Vedlegg 4A, nr. 43, 44, 46, 47: requirements for *Betula* wood and isolated bark.
3. **Units naming only the host:** e.g. Vedlegg 2, nr. 8, Vedlegg 4A, nr. 10 and nr. 48-51, and Vedlegg 5A lines for round and sawn wood.

## 10. Write-up (Panel 4)

One text per pathway, ready to paste into FinnPRIO. This is the wood text. The regulation lines are shortened here; in the app the LLM gives one sentence per unit:

```
ENT3: Large

Annual import into Norway of wood (CN 44, 9406.10): 291.9 million kg (median 2016-2025). The figure covers all products in these CN lines, not only host commodities, so it is an upper bound.
Basis: EPPO host commodities: squared wood, non-squared wood, bark, manufactured articles, packaging material; host genera: Betula.

Largest CN8 lines (kg per year, share of total):
lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 83.8 million kg (29%, mixed line)
Ved til brensel i form av stokker, kubber, vedtrær, kvister, kvistbunter eller liknende, av lauvtrær (44011200): 61.8 million kg (21%, mixed line)
bolighus, unntatt hytter, brakker og koier, andre (94061019): 54.3 million kg (19%, mixed line)
av tre, ellers (94061090): 34.5 million kg (12%, mixed line)
kasser, esker, sprinkelkasser, dunker og liknende emballasje; kabeltromler (44151000): 4.2 million kg (1%, mixed line)

Imports from countries where the pest is present (information only, not ENT3):
United States of America: 44 932 kg per year (median 2016-2025)
  lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 306 069 kg in total 2016-2025
  kasser, esker, sprinkelkasser, dunker og liknende emballasje; kabeltromler (44151000): 109 622 kg in total 2016-2025
  Trelast, saget/kuttet i lengden; ... ikke av bartrær, eik, bøk el div tropiske trær (44079900): 36 501 kg in total 2016-2025
Canada: 2 302 kg per year (median 2016-2025)
  Trelast, saget/kuttet i lengden; ... ikke av bartrær, eik, bøk el div tropiske trær (44079900): 120 386 kg in total 2016-2025
  av tre, ellers (94061090): 45 043 kg in total 2016-2025
  lastepaller, kassepaller og andre paller av tre; pallekarmer (44152000): 21 602 kg in total 2016-2025

Regulation:
Vedlegg 1, nr. 2.1: <sentence: lists Agrilus anxius as a regulated pest>
Vedlegg 4A, nr. 45: <sentence>
Vedlegg 3, nr. 11: <sentence: bans Betula plants, plant parts, chips and wood waste from Canada and the USA>
...

Note: Genus-specific wood lines (e.g. birch, oak, pine) count only for host genera; mixed lines (firewood, chips, waste, packaging) are an upper bound.
Note: Wood packaging: CN 4415 only captures packaging traded as goods. Most packaging travels with other products and has no SSB line.
Note: Bark is part of this pathway; isolated bark is counted in the wood waste lines (4401 40-49).
Note: Manufactured articles: only casks (4416) and prefabricated wooden buildings (9406.10) are counted; other manufactured wood is not.
```

Everything except the regulation sentences comes from R: the class, the numbers, the CN8 codes and tariff labels, and the notes. If the LLM fails, or returns a line that does not start with a Panel 3 id, the app shows the unit's own text instead.

## 11. Reading the result

* **ENT3 = large for both pathways.** Norway imports large volumes of plants for planting and wood in the CN lines that can carry birch material.
* **The volumes are upper bounds.** Most of the volume is mixed lines (pallets, firewood, prefabricated houses, all trees and shrubs) with no host specified. Only six SSB lines are birch-specific.
* **Trade from where the pest occurs is tiny.** It is about 1 t/year of plants and about 47 t/year of wood, mostly pallets and crates, and Norway bans *Betula* material from Canada and the USA (Vedlegg 3, nr. 11).
* These context numbers belong in the justification, not in the class.
