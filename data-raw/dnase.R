library(purrr)
library(valr)
library(devtools)
library(R.utils)

#https://github.com/hadley/babynames/blob/master/data-raw/names.R
if (!file.exists("data-raw/dnase")) {
  tmp <- tempfile(fileext = ".gz")
  download.file("https://s3.amazonaws.com/bedtools-tutorials/web/maurano.dnaseI.tgz", tmp, quiet = TRUE)
  untar(tmp, exdir = "data-raw/dnase", compressed = "gzip")
  unlink(tmp)
  bed_files <- dir("data-raw/dnase", "\\.bed$", full.names = TRUE)
  map(bed_files, ~gzip(.x))
}

dnase_files <- dir('data-raw/dnase', pattern = 'merge.bed.gz', full.names = TRUE)
dnase_data <- map(dnase_files, ~read_bed(.x, n_fields = 4))

use_data(dnase_data, compress = "xz")
