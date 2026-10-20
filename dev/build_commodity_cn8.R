# Build data/commodity_cn8.csv from SSB table 08801 metadata (current CN8 labels).
# Run from the project root: Rscript dev/build_commodity_cn8.R
#
# Rules (decided 2026-10-20, see docs/decisions.md):
# - Groups: conifer, non_conifer ("both" = counted in each). Pathways:
#   plants_for_planting (0602) and wood (scope of FOR-2000-12-01-1333 vedlegg 5A nr. 8).
# - Plants: every 0602 line counts for non_conifer (herbaceous plants are plants
#   for planting too); the catch-all lines also count for conifer (no conifer-specific
#   0602 line exists), flagged upper_bound.
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
out <- out[, c("pathway", "cn8", "description", "commodity_group", "flags", "source",
               "valid_from", "valid_to", "q2_unit", "notes", "reviewed")]
write.csv(out, "data/commodity_cn8.csv", row.names = FALSE, na = "", fileEncoding = "UTF-8")
print(table(out$pathway, out$commodity_group))
excluded <- d[!is.na(entry) & !wood_sel, c("cn8", "description")]
cat("\nIn a vedlegg 5A entry but excluded (tropical / treated / agglomerated):\n")
print(substr(paste(excluded$cn8, excluded$description), 1, 110))
