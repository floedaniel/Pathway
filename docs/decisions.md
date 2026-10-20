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
9. **Later (before Step 9).** Write-up template, language, HF models.
