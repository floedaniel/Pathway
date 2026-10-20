# Findings

Moved from the Q & A in Pathwise_notes.Rmd.

- **EPPO API v2:** host classes Major host, Host, Alternate, Wild/Weed, Experimental, Doubtful host, Non-host (no "Minor host"). Lineage: Kingdom Plantae > Class (Pinopsida; Angiospermae) > Category (e.g. Basal monocotyledons, Fabids) > Order > Family > Genus. Unknown codes return HTTP 400. https://eppo.cloud/api/docs is an unrelated company.
- **Lovdata:** 305 citable units. Bark beetles regulated as "Scolytidae spp. (ikke-europeiske arter)" (Vedlegg 2, nr. 10); alias file maps Scolytinae -> Scolytidae, conifers -> Coniferales/bartrær. Not named in the regulation: Xylella, Aromia, Lymantria, Dendroctonus, Polygraphus, Fusarium circinatum. Vedlegg 5A tariff numbers are an older format. Mattilsynet holds genus-level import data (§ 20, § 23a): a data request, not v1.
- **First runs:** ~30 s cold, seconds when cached. Most classes are `large`, as expected for upper-bound CN8 lines; Class B is often much lower (IPSXCA wood: A 1.26 billion kg, B 0.43 million kg).

