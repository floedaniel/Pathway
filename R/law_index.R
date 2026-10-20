# Lovdata: FOR-2000-12-01-1333 (forskrift om planter og tiltak mot planteskadegjørere).
# Source: Lovdata public data, bulk archive of current central regulations
# (no key needed, free licence): https://api.lovdata.no/v1/publicData/get/gjeldende-sentrale-forskrifter.tar.bz2
# The regulation is the file sf/sf-20001201-1333.xml (HTML) inside the archive.
# Units: one per § article, one per annex (vedlegg) table row, one per paragraph
# of annexes without tables. Each unit has an id, text and a deep link.

law_archive_url <- "https://api.lovdata.no/v1/publicData/get/gjeldende-sentrale-forskrifter.tar.bz2"
law_member <- "sf/sf-20001201-1333.xml"
law_doc_id <- "FOR-2000-12-01-1333"

# Download the archive and extract the regulation. Returns the local file path.
law_download <- function(dir = "data/law/raw") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(dir, "forskrifter.tar.bz2")
  utils::download.file(law_archive_url, archive, mode = "wb", quiet = TRUE)
  utils::untar(archive, files = law_member, exdir = dir)
  file.path(dir, law_member)
}

squish <- function(x) trimws(gsub("\\s+", " ", x))

law_url <- function(lovdata_url) paste0("https://lovdata.no/dokument/", lovdata_url)

# Parse the regulation HTML into a tibble of citable units.
law_parse <- function(file) {
  doc <- xml2::read_html(file)
  units <- list()

  for (a in xml2::xml_find_all(doc, "//article[contains(concat(' ', @class, ' '), ' legalArticle ')]")) {
    paras <- xml2::xml_find_all(a, "./article[contains(@class, 'legalP')]")
    units[[length(units) + 1]] <- tibble::tibble(
      id = squish(xml2::xml_text(xml2::xml_find_first(a, ".//span[@class='legalArticleValue']"))),
      heading = squish(xml2::xml_text(xml2::xml_find_first(a, ".//span[@class='legalArticleTitle']"))),
      text = squish(paste(xml2::xml_text(paras), collapse = " ")),
      url = law_url(xml2::xml_attr(a, "data-lovdata-url"))
    )
  }

  for (s in xml2::xml_find_all(doc, "//section[starts-with(@data-name, 'vedlegg')]")) {
    title <- squish(xml2::xml_text(xml2::xml_find_first(s, "./h2")))
    annex <- sub("^(Vedlegg [0-9]+[A-Z]?)\\..*$", "\\1", title)
    url <- law_url(xml2::xml_attr(s, "data-lovdata-url"))
    rows <- xml2::xml_find_all(s, ".//tr")
    if (length(rows) > 0) {
      for (i in seq_along(rows)) {
        # join text nodes with spaces, so codes in separate <p> do not run together
        cells <- vapply(xml2::xml_find_all(rows[[i]], "./td|./th"), function(td) {
          squish(paste(xml2::xml_text(xml2::xml_find_all(td, ".//text()")), collapse = " "))
        }, "")
        cells <- cells[nzchar(cells)]
        if (length(cells) < 2) next  # header bands such as "Insekter, midder, nematoder"
        if (cells[1] == "Nr.") next  # column headers
        nr <- if (grepl("^[0-9][0-9. ]*[a-z]?$", cells[1])) {
          paste0("nr. ", sub("\\.$", "", cells[1]))
        } else if (grepl(":$", cells[1])) {
          sub(":$", "", cells[1])  # definitions (vedlegg 13)
        } else {
          paste0("rad ", i)
        }
        units[[length(units) + 1]] <- tibble::tibble(
          id = paste0(annex, ", ", nr), heading = title,
          text = paste(cells, collapse = " | "), url = url
        )
      }
    } else {
      paras <- squish(xml2::xml_text(xml2::xml_find_all(s, ".//article[contains(@class, 'legalP')]|.//p")))
      paras <- paras[nzchar(paras)]
      for (i in seq_along(paras)) {
        units[[length(units) + 1]] <- tibble::tibble(
          id = paste0(annex, ", avsnitt ", i), heading = title, text = paras[i], url = url
        )
      }
    }
  }
  out <- dplyr::bind_rows(units)
  out[!duplicated(out$id) & nzchar(out$text), ]
}

# Cached index: downloaded and parsed at most once per lovdata TTL.
law_index <- function(settings, download = law_download) {
  entry <- cached("lovdata", law_doc_id, function() law_parse(download()), settings)
  structure(entry$value, fetched_at = entry$fetched_at)
}

# Search terms (Flow step 8). pest_names: EPPO scientific names (preferred and
# synonyms); pest_lineage / groups add higher taxa and their aliases from
# data/law_taxon_aliases.csv; host_genera and countries (Norwegian names) as given.
law_search_terms <- function(pest_names, pest_lineage = character(), host_genera = character(),
                             groups = character(), countries = character(),
                             aliases = utils::read.csv("data/law_taxon_aliases.csv", colClasses = "character",
                                                       encoding = "UTF-8")) {
  pest_taxa <- c(pest_lineage, if ("conifer" %in% groups) "Pinopsida")
  alias_of <- function(taxa) aliases$alias[aliases$taxon %in% taxa]
  list(
    pest = unique(c(pest_names, pest_lineage, alias_of(pest_lineage))),
    host = unique(c(host_genera, alias_of(if ("conifer" %in% groups) "Pinopsida"))),
    country = unique(countries)
  )
}

term_hits <- function(text, terms) {
  if (length(terms) == 0) return(rep(list(character()), length(text)))
  pats <- paste0("(?<!\\p{L})", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", terms), "(?!\\p{L})")
  hit <- vapply(pats, function(p) grepl(p, text, ignore.case = TRUE, perl = TRUE), logical(length(text)))
  hit <- matrix(hit, nrow = length(text))
  lapply(seq_along(text), function(i) terms[hit[i, ]])
}

# Units matching a pest term or a host term. Country terms only raise the score.
# Score: 3 per pest term, 2 per host term, 1 per country term. Units naming the
# pest always come first, so many host genera cannot push them down.
law_search <- function(index, terms) {
  p <- term_hits(index$text, terms$pest)
  h <- term_hits(index$text, terms$host)
  c <- term_hits(index$text, terms$country)
  n <- function(x) lengths(x)
  out <- index
  out$matched <- mapply(function(a, b, d) paste(c(a, b, d), collapse = ", "), p, h, c)
  out$pest_match <- n(p) > 0
  out$score <- 3 * n(p) + 2 * n(h) + n(c)
  out <- out[n(p) > 0 | n(h) > 0, ]
  out <- out[order(!out$pest_match, -out$score, out$id), ]
  attr(out, "fetched_at") <- attr(index, "fetched_at")
  out
}
