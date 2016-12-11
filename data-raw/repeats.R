library(valr)
library(readr)
library(R.utils)
library(dplyr)

if (!file.exists("data-raw/repeats")) {
  dir.create("data-raw/repeats")
  ucsc <- db_ucsc("hg19")
  repeats <- tbl(ucsc, "rmsk")
  repeats <- select(repeats,
                    genoName,
                    genoStart,
                    genoEnd,
                    repName,
                    swScore,
                    strand,
                    repClass,
                    repFamily) %>% collect(n = Inf)
  write_tsv(repeats, "data-raw/repeats/hg19_rmsk.bed")
  gzip("data-raw/repeats/hg19_rmsk.bed")
}

repeat_data <- readr::read_tsv("data-raw/repeats/hg19_rmsk.bed.gz")

repeat_data <- dplyr::rename(repeat_data,
                      chrom = genoName,
                      start = genoStart,
                      end   = genoEnd,
                      name  = repName,
                      score = swScore
                      )
use_data(repeat_data, compress = "xz", overwrite = TRUE)
