# Build data/commodity_cn8.csv from SSB table 08801 metadata (current CN8 labels).
# Run from the project root: Rscript dev/build_commodity_cn8.R
#
# Rules (decided 2026-10-20, see docs/decisions.md):
# - Groups: conifer, non_conifer ("both" = counted in each). Pathways:
#   plants_for_planting (0602) and wood (scope of FOR-2000-12-01-1333 vedlegg 5A nr. 8).
# - Plants: every 0602 line counts for non_conifer (herbaceous plants are plants
#   for planting too); the catch-all lines also count for conifer (no conifer-specific
#   0602 line exists), flagged upper_bound. 0601 (bulbs, tubers) counts only when a
#   host is a monocot (group "monocot").
# - Wood: lines whose wording matches a vedlegg 5A nr. 8 entry. Tropical wood,
#   eucalyptus round wood, agglomerated wood and impregnated/treated wood are out.
#   The regulation's tariff numbers are an older HS format, so the match is made on
#   the SSB wording; the regulation entry is kept in `source`.
# - Only lines valid in the table window (from 2016) are kept.

meta <- jsonlite::fromJSON("https://data.ssb.no/api/pxwebapi/v2/tables/08801/metadata?lang=no",
                           simplifyVector = FALSE)
lab <- unlist(meta$dimension$Varekoder$category$label)
yr <- regmatches(lab, regexpr("\\(\\d{4}-(\\d{4})?\\)", lab))
d <- data.frame(cn8 = names(lab), label = lab, row.names = NULL)
d$valid_from <- as.integer(substr(yr, 2, 5))
d$valid_to <- suppressWarnings(as.integer(substr(yr, 7, 10)))
d$q2_unit <- sub(".*M2=([^)]*)\\).*", "\\1", d$label)
d$description <- trimws(sub("\\s*\\(\\d{4}-(\\d{4})?\\).*$", "", d$label))
d <- d[is.na(d$valid_to) | d$valid_to >= 2016, ]
low <- tolower(d$description)
code <- d$cn8
has <- function(p) grepl(p, low, perl = TRUE)

# ---- plants for planting ---------------------------------------------------
catch_all <- c("06021029_2001", "06029010_2011", "06029021_2011", "06029059_2011", "06029098_2016")
plants <- d[startsWith(code, "0602"), ]
plants$pathway <- "plants_for_planting"
plants$commodity_group <- ifelse(plants$cn8 %in% catch_all, "both", "non_conifer")
plants$flags <- ifelse(plants$cn8 %in% catch_all, "upper_bound;conifer_catch_all", "")
plants$source <- "vedlegg 5A nr. 1 (planter og formeringsmateriale, unntatt frø)"
# 0601 bulbs, tubers, corms, rhizomes: part of plants for planting in FinnPRIO,
# counted only when at least one host is a monocot (group "monocot")
bulbs <- d[startsWith(code, "0601"), ]
bulbs$pathway <- "plants_for_planting"
bulbs$commodity_group <- "monocot"
bulbs$flags <- "monocot_only"
bulbs$source <- "vedlegg 5A nr. 1 (planter og formeringsmateriale); FinnPRIO: bulbs/tubers are plants for planting"
plants <- rbind(bulbs, plants)

# ---- wood (vedlegg 5A nr. 8) -----------------------------------------------
con <- has("bartr|furu|gran|s-p-f|hem-fir")
excl_con <- has("(unnt|ikke)( av)? bartrær")
tropical <- has("tropisk|meranti|teak|mahogni|virola|lauan|sapelli|iroko") & !has("unnt(att)? av (bartrær, )?tropiske|ikke av bartrær, tropiske|(unnt|ikke)( av)? bartrær, (eik|tropiske)|unnt av gran, furu og tropiske|ikke teak")
treated <- has("impregnert|kreosot|maling") & !has("ikke behandlet|uimpregnert|ikke impregnert|unnt skurtømmer og massevirke og impregnerte")
agglom <- has("agglomerert|pellet|brikett") & !has("ikke agglomerert") & !code %in% c("44013901_2012", "44013902_2012", "44013909_2012")

rule <- function(prefix, src) startsWith(code, prefix)
entry <- rep(NA_character_, nrow(d))
set <- function(sel, src) entry[sel & is.na(entry)] <<- src
set(startsWith(code, "440111") | startsWith(code, "440112") | code == "44011000_1988", "44.01.11/12 ved til brensel")
set(startsWith(code, "440121") | startsWith(code, "440122"), "44.01.21/22 fliser eller spon")
set(grepl("^4401(40|41|49)", code) | code %in% c("44013901_2012", "44013902_2012", "44013909_2012"), "44.01.40 sagflis og treavfall, ikke agglomerert")
set(grepl("^4403(2[0-6])", code), "44.03.21-26 tømmer av bartrær")
set(startsWith(code, "440391"), "44.03.91 tømmer av eik")
set(grepl("^4403(92|93|94)", code), "44.03.93/94 tømmer av bøk")
set(grepl("^4403(95|96)", code) | code %in% c("44039901_1992", "44039902_1992"), "44.03.95/96 tømmer av bjørk")
set(startsWith(code, "440397"), "44.03.97 tømmer av poppel og osp")
set(startsWith(code, "440399") & !has("^(tømmer|massevirke) av eukalyptus"), "44.03.99 tømmer av annet")
set(startsWith(code, "440410") | startsWith(code, "440420"), "44.04.10/20 kløyvde stokker, stolper og staurer")
set(code %in% c("44061000_2001") | grepl("^4406(11|12)", code), "44.06.11/12 sviller, uimpregnerte")
set(grepl("^4407(1)", code), "44.07.11/12/19 trelast av bartrær")
set(grepl("^4407(9)", code), "44.07.91-99 trelast av eik, bøk, lønn, kirsebær, ask, bjørk, poppel, annet")
set(startsWith(code, "441510"), "44.15.10 kasser og emballasje av tre")
set(startsWith(code, "441520"), "44.15.20 paller av tre")
set(code == "44160010_1988", "44.16.0010 fat og tønner av eik")
set(startsWith(code, "940610"), "94.06.10 prefabrikkerte bygninger av tre")

wood_sel <- !is.na(entry) & !tropical & !treated & !agglom
wood <- d[wood_sel, ]
wl <- low[wood_sel]
wcon <- con[wood_sel] & !excl_con[wood_sel]
wnon <- has("lauvtr|eik|bøk|bjørk|poppel|osp|lønn|kirsebær|ask")[wood_sel] | excl_con[wood_sel]
wood$pathway <- "wood"
wood$commodity_group <- ifelse(wcon & !wnon, "conifer", ifelse(wnon & !wcon, "non_conifer", "both"))
wood$flags <- ""
bark_no <- grepl("ikke befridd for bark|ikke bedfridd for bark", wl)
bark_yes <- grepl("befridd for bark|bedfridd for bark", wl) & !bark_no & !grepl("også (befridd for bark|avbarket)", wl)
add_flag <- function(f, sel) wood$flags[sel] <<- ifelse(nzchar(wood$flags[sel]), paste(wood$flags[sel], f, sep = ";"), f)
add_flag("upper_bound", wood$commodity_group != "conifer" | wood$cn8 %in% c("44013901_2012", "44013902_2012", "44013909_2012"))
add_flag("with_bark", bark_no)
add_flag("debarked", bark_yes)
wood$source <- paste("vedlegg 5A nr. 8:", entry[wood_sel])

out <- rbind(plants, wood)
out$notes <- ""
out$reviewed <- ""

# ---- genus-specific and residual lines (from the SSB wording) ----------------
# `genus`: the line covers only these genera; it counts only when a host belongs
# to one of them. `excludes_genera`: residual line ("other ..." / "unnt ...");
# it counts only when a host genus is outside the list. Others count for any host.
genus_rules <- list(
  "^4403(21|22)|^44032005" = "Pinus", "^4403(23|24)" = "Picea",
  "^44071003|^44071008|^440711" = "Pinus", "^440712" = "Picea",
  "^440713" = "Picea;Pinus;Abies", "^440714" = "Tsuga;Abies",
  "^440391" = "Quercus", "^4403(92|93|94)" = "Fagus", "^4403(95|96)|^4403990[12]_1992" = "Betula",
  "^440397" = "Populus",
  "^440791" = "Quercus", "^440792" = "Fagus", "^440793" = "Acer", "^440794" = "Prunus",
  "^440795" = "Fraxinus", "^440796" = "Betula", "^440797" = "Populus",
  "^44160010" = "Quercus",
  "^060230" = "Rhododendron", "^060240" = "Rosa",
  # plant lines that list their genera in the label (palmer = family Arecaceae;
  # kransgran = Sciadopitys, a conifer, hence group "both")
  "^06029021" = "Buxus;Dracaena;Laurus;Camellia;Sciadopitys;Ilex;Magnolia;Arecaceae;Hamamelis;Chimonanthus",
  "^06021024" = "Pelargonium",
  "^06021025" = "Chrysanthemum;Saintpaulia;Scaevola;Streptocarpus"
)
residual_rules <- list(
  "^4403(25|26)" = "Pinus;Picea", "^44032006" = "Pinus",
  "^4407100[49]" = "Pinus", "^44071900" = "Pinus;Picea",
  "^44039900_2017" = "Quercus;Fagus;Betula;Populus;Eucalyptus", "^44039908_2002" = "Quercus;Fagus",
  "^44079900_2007" = "Quercus;Fagus", "^44079900_2017" = "Quercus;Fagus;Acer;Prunus;Fraxinus;Betula;Populus"
)
apply_rules <- function(rules) {
  v <- rep("", nrow(out))
  for (p in names(rules)) v[grepl(p, out$cn8) & v == ""] <- rules[[p]]
  v
}
out$genus <- apply_rules(genus_rules)
out$excludes_genera <- apply_rules(residual_rules)

# ---- EPPO host-commodity categories (data/eppo_commodity_map.csv) -----------
# A line counts for a pest only when EPPO lists one of its categories for the pest.
category_rules <- list(
  "^0602" = "plants for planting",
  "^0601" = "bulbs or tubers",
  "^4403|^44011[12]|^440110|^44012|^4404" = "non-squared wood",
  "^4406|^4407" = "squared wood",
  "^4401(40|41|49)|^4401390[129]_2012" = "bark;plant waste",
  "^4415" = "packaging material",
  "^4416|^940610" = "manufactured articles"
)
out$eppo_category <- apply_rules(category_rules)
stopifnot(all(nzchar(out$eppo_category)))

out <- out[, c("pathway", "cn8", "description", "commodity_group", "eppo_category", "genus", "excludes_genera",
               "flags", "source", "valid_from", "valid_to", "notes", "reviewed")]
write.csv(out, "data/commodity_cn8.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
print(table(out$pathway, out$commodity_group))
excluded <- d[!is.na(entry) & !wood_sel, c("cn8", "description")]
cat("\nIn a vedlegg 5A entry but excluded (tropical / treated / agglomerated):\n")
print(substr(paste(excluded$cn8, excluded$description), 1, 110))
