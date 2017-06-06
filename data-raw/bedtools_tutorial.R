library(purrr)
library(valr)
library(devtools)
library(R.utils)

#https://github.com/hadley/babynames/blob/master/data-raw/names.R
if (!file.exists("data-raw/bedtools")) {
  dir.create("data-raw/bedtools")
  urls <- c(
    "https://s3.amazonaws.com/bedtools-tutorials/web/cpg.bed",
    "https://s3.amazonaws.com/bedtools-tutorials/web/exons.bed",
    "https://s3.amazonaws.com/bedtools-tutorials/web/gwas.bed",
    "https://s3.amazonaws.com/bedtools-tutorials/web/genome.txt",
    "https://s3.amazonaws.com/bedtools-tutorials/web/hesc.chromHmm.bed")

  map(urls, ~download.file(.x,
                          paste0("data-raw/bedtools/", basename(.x)),
                          quiet = T))

  bed_files <- dir("data-raw/bedtools", full.names = TRUE)
  map(bed_files, ~gzip(.x))
}

bed_files <- dir('data-raw/bedtools', pattern = 'bed.gz', full.names = TRUE)
genome_file <- dir('data-raw/bedtools', pattern = 'genome.txt.gz', full.names = TRUE)
n_bedfields <- c(4, 6, 4, 4)

bed_dat <- map2(bed_files, n_bedfields, ~read_bed(.x, n_fields = .y))
genome_dat <- map(genome_file, ~read_genome(.x))

tutorial_files <- c(bed_dat, genome_dat)
names(tutorial_files) <- c("cpg", "exons", "gwas", "hesc_chromHmm", "genome")

use_data(tutorial_files, compress = "xz", overwrite = TRUE)
