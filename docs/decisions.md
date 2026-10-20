# Decisions

Amendments to the plan in `Pathwise_notes.Rmd`. Newer entries win.

## 2026-10-20

1. **Woody hosts: reviewed list.** `data/woody_taxa.csv` with columns `rank` (family|genus), `taxon`, `woody` (TRUE|FALSE), `notes`, `reviewed`. Genus rows override family rows (mixed families: Rosaceae has *Fragaria*, Fabaceae and Asteraceae have both woody and herbaceous members).
   - conifer = Pinopsida lineage.
   - broadleaf = Magnoliopsida AND (genus or family listed woody=TRUE).
   - Magnoliopsida hosts not on the list are **unreviewed**. They are not counted and not silently dropped. They show in the host table as "unreviewed" and the header strip counts them.
   - New stop `unreviewed_hosts`: no host maps to a group but unreviewed hosts exist -> "No reviewed woody hosts. Unreviewed: {list}. Add to woody_taxa.csv." Without unreviewed hosts, the `no_groups` stop applies.
   - The first draft is built from the host families/genera of 5-10 pests named by the user, plus the families in the 0602 CN8 lines. woody=TRUE is never filled in by guessing; the user reviews and commits.
   - v2 herbaceous groups plug in through the same file.
2. **Thresholds.** `data/finnprio_thresholds.csv` columns `small_lt = 1000000`, `medium_le = 10000000`. `x < small_lt` -> small; `x <= medium_le` -> medium; else large. Boundary tests: 999,999.6 -> small, 1,000,000 -> medium, 10,000,000 -> medium, 10,000,000.1 -> large.
3. **Aggregate before classifying.** Sum all origins per group, then classify. The per-country classification in `old_scripts/ssb_data_ENT3.R` is not reused.
4. **SSB 08801.** `commodity_cn8.csv` stores the full versioned code (e.g. `06021010_1995`); validity from the version suffix and metadata. Unit check reads the M1/M2 labels: M1 = kg, M2 = stk where present. kg and pc are never summed; a pc-only line is classified in pc separately.
5. **EPPO API.** Use whichever API is current; document the choice in code comments.
6. **SSB API.** If v0 is being phased out, build on PxWebApi v2. https only, respect query limits.
7. **Countries.** If SSB Land codes are ISO2, `country_map.csv` holds only the exceptions.
8. **Secrets.** `EPPO_KEY`, `ANTHROPIC_API_KEY`, `HF_TOKEN` in `~/.Renviron` (set by the user). Never read from key files, never committed. Checked at startup (`check_secrets()`), with a clear failure message.
9. **Later (before Step 9).** HF models.
10. **API choices (verified 2026-10-20).** EPPO Global Database API v2 (`https://api.eppo.int/gd/v2`, header `X-Api-Key`, max 2000 requests per 10 s); v1 is marked legacy. SSB PxWebApi v2 (`https://data.ssb.no/api/pxwebapi/v2`, max 800,000 cells per call, 40 calls/min); SSB Land codes are ISO2 except CS_, XI (historical) and 97 (unspecified).
11. **Statistic.** Median over 10 years (`volume_stat: median`, `volume_n: 10`); trade data are skewed.
12. **Write-up.** ENT3 only, no other FinnPRIO fields. English. Max 200 characters (to be tested).
13. **Two pathways.** Plants for planting (0602) and wood (ch 44: 4401, 4403, 4404, 4406, 4407, 4415), each with conifer/broadleaf groups, in `commodity_cn8.csv` (`pathway` column). Agglomerated wood (pellets, briquettes) excluded. 0602 has no conifer-specific line in 2016-2025, so conifer plants for planting are only in catch-all lines.
14. **CN8 review applied (no manual review for now).** `commodity_cn8.csv` has `role` (class|info) and `flags` (upper_bound, waste, treated, tropical, with_bark, debarked). Ambiguous lines stay in with `upper_bound`; wood waste and treated wood are info rows; 06029092 is out (v2). With-bark subtotal is info only. Wood uses Q1 kg only.
15. **Pathway selector.** One ENT3 class and one write-up per pathway; default plants for planting. Write-up: class, number with unit, statistic with years, one Lovdata id; no sources line.
16. **EPPO host classes.** API v2 has no "Minor host"; `host_classes: [Major host, Host]` (pending confirmation).
17. **Angiosperm class name.** EPPO v2 lineage uses "Angiospermae"; broadleaf = Angiospermae AND woody in `woody_taxa.csv` (draft list used unreviewed for now, `woody_use_unreviewed: true`).
18. **Lovdata.** Regulation from the public bulk archive (`gjeldende-sentrale-forskrifter.tar.bz2`, file `sf/sf-20001201-1333.xml`), cached 7 days. Units: § articles, annex rows, annex paragraphs. Search: pest names + EPPO synonyms + pest lineage (+ aliases in `data/law_taxon_aliases.csv`), host genera (+ conifer aliases), countries. A unit is returned when a pest or host term matches; country terms only raise the score.

## 2026-10-20 (later): taxonomic rule replaces the woody list
Supersedes items 1, 14 and 17 above.

19. **Host classification, programmatic.** conifer = lineage contains Pinopsida, Pinales or Coniferales. non_conifer = any other plant (kingdom Plantae), incl. cycads, Ginkgo, ferns, herbaceous plants. No woody test. `woody_taxa.csv` and the `unreviewed_hosts` / `no_groups` stops are removed; new stop `no_plant_hosts` when no selected host is a plant.
20. **Host classes.** Major host + Host only. Alternate, Wild/Weed and the other classes are not used.
21. **CN8 scope from the regulation.** `commodity_cn8.csv` is built by `dev/build_commodity_cn8.R` from current SSB labels. Plants: all 0602 lines (vedlegg 5A nr. 1) count for non_conifer; 5 catch-all lines also count for conifer (`upper_bound;conifer_catch_all`). Wood: vedlegg 5A nr. 8 scope matched on wording (its tariff numbers are an older format); tropical, eucalyptus round wood, agglomerated and impregnated/treated wood out; non-conifer and unsplit wood flagged `upper_bound`. Each line keeps its `source`. Regulation split is conifer / oak / other; "deciduous trees and shrubs" (vedlegg 4A pt 32) is a habit category and not used.
22. **Wood unit.** Wood is classified in kg only (Q2 pieces exist only for pallets/crates). pc classes only for plants for planting.
23. **FinnPRIO pathways.** Ids exactly FinnPRIO's: seeds, plants_for_planting, wood, food_fodder, other_plant_parts, hitchhiking, natural_spread, intentional_introduction. Up to 5 selected. Computed: plants_for_planting, wood. Fixed (ENT3 non_existent, no data): hitchhiking, natural_spread, intentional_introduction. Manual (v2): seeds, food_fodder, other_plant_parts. Pre-select plants_for_planting, plus wood when a conifer host is present. One ENT3 and one write-up per pathway.
24. **Class A per pathway** is the total over the union of the pathway's host commodity lines (`all_hosts`, role `ent3`); conifer / non_conifer rows are information.
25. **0601 bulbs and tubers** belong to plants_for_planting and count only when a host is a monocot (EPPO category Basal monocotyledons or Commelinids). UI notes that dicot bulbs/tubers are missed and that soil has no CN line. Wood: bark is part of the pathway (wood waste lines); UI warns that CN 4415 captures only packaging traded as goods.
26. **Keys.** Env vars win; otherwise the app reads the git-ignored `API_kays/` folder (supersedes item 8's "never read from key files").

## 2026-10-20 (later): simplifications
Supersedes items 22-25 where they conflict.

27. **No fixed pathways.** Hitchhiking, natural spread and intentional introduction are removed from the app. Pathways: plants_for_planting, wood (computed); seeds, food_fodder, other_plant_parts (assess manually). No 5-pathway limit needed.
28. **kg only.** Pieces (pc) removed everywhere: no Q2 in SSB queries, no pc thresholds, no pc classes.
29. **Class B replaced.** A class for pest-present origins looked like an ENT3 answer although it is not one. Panel 1 now shows imports from pest-present origins as kg and share of the total (context only, no class).
30. **Tidy Panel 1, median only.** One value per column: host commodities | median kg/year (all origins) | ENT3 class | kg from pest-present origins | share. Latest-year and max columns removed. Group rows shown only when the pest has both conifer and non-conifer hosts. "Top origins" open by default.
31. **Write-up LLM: OpenAI**, key in `API_kays/tore_vkm_openai.txt` (Step 9).
32. **LLM write-up (Step 9).** ellmer, provider `openai`, model `gpt-5.5-2026-04-23` (pinned dated release from the key's model list). The LLM gets labelled EPPO / SSB / LAW blocks; output is validated (no markdown or line breaks, <= 200 chars, states the Panel 1 ENT3 class and no other, cites only ids present in Panel 3). One retry with the problems listed, then the deterministic draft. Cached per prompt + model; Regenerate bypasses the cache. Each box is labelled with its source.
33. **Genus-specific CN8 lines.** Lines that name one genus (wood: furu/Pinus, gran/Picea, eik/Quercus, bøk/Fagus, bjørk/Betula, poppel og osp/Populus, ask/Fraxinus, lønn/Acer, kirsebær/Prunus; plants: Rhododendron, Rosa) count only when a host of that group is in the genus. Residual lines ("other conifers/broadleaved") count only when a host genus is outside the listed genera. Mixed lines (firewood, chips, waste, packaging, catch-all plants) count for any host. Groups with a host known only above genus are not filtered. Columns `genus` / `excludes_genera` in `commodity_cn8.csv`, set in `dev/build_commodity_cn8.R`.
34. **Lovdata tiers.** A pest's higher taxon counts only as "<taxon> spp." (genus alone is no hit). Units naming a host genus and a pest-present country are `origin_rule` (origin-specific bans/requirements) and rank right after units naming the pest.
35. **Pest-present origins in Panel 1.** Own table per pathway: every pest-present country with median kg/year, share of all imports, NOK, years with imports and main commodity line; countries without imports listed. Graph: biggest origin at the bottom of the stack; switch for pest-present origins only.
36. **Write-up 400 characters** (200 was too short). Must state ENT3 class, kg and statistic, imports from pest-present countries (main countries or none recorded) and the regulation, including origin rules. English country names in UI and write-up; Norwegian names only for the Lovdata search.
37. **CN8 codes everywhere.** Every table names the CN8 code (8 digits, SSB version suffix dropped): new Panel 1 table "CN8 lines in this pathway" (code, description, median kg/year, share, kg from pest-present origins, flags); "Main CN8 line" columns in the pest-present and top-origins tables. The write-up cites CN8 codes; the validator rejects codes that are not lines of the pathway.
38. **Detailed write-up (max 1200 characters).** Starts with the pathway and ENT3 class; total kg and median years; main CN8 lines with kg and share; imports from pest-present countries per country (median kg/year) with their CN8 lines as totals over the period (medians of sparse lines are often 0, totals add up); mixed lines named as such; regulation with what each id does; one caveat.
39. **Lines that list genera** (06029021 Buxus, Dracaena, Laurus, Camellia, Sciadopitys, Ilex, Magnolia, palms, Hamamelis, Chimonanthus; 06021024 Pelargonium; 06021025 Chrysanthemum, Saintpaulia, Scaevola, Streptocarpus) count only for hosts in those genera (or family, for palms). Residual lines are judged on host genera only, not families. Plant lines that name species only by Norwegian common names are left general.
40. **Write-up layout (supersedes 38).** Fixed layout from the user's example: "ENT3: <Class>"; annual import with CN scope, median years and upper-bound sentence; "Largest CN8 lines" (code: mass, share, mixed line); "Imports from countries where the pest is present (information only, not ENT3)" per country with median and CN8 line totals, or "no recorded imports"; "Regulation:" lines; "Note:" lines. R fills in all numbers, the class, codes and notes. The LLM writes only one English sentence per Lovdata unit (units naming the pest, then origin rules, in regulation order); each line must start with a Panel 3 id, else the unit text is shown. No character limit.
