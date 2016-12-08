if (!file.exists("data-raw/segway")) {
  dir.create("data-raw/segway")
  url <- "http://noble.gs.washington.edu/proj/encyclopedia/segway_encyclopedia.bed.gz"
  download.file(url,
                paste0("data-raw/segway/", basename(url)),
                quiet = T)
}

segway_data <- readr::read_tsv("data-raw/segway/segway_encyclopedia.bed.gz")

use_data(segway_data, compress = "xz")
