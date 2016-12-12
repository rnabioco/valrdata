if (!file.exists("data-raw/hg19_genes")) {
  dir.create("data-raw/hg19_genes")
  ucsc <- valr::db_ucsc("hg19")
  genes <- dplyr::tbl(ucsc, "refFlat")
  genes <- dplyr::collect(genes, n = Inf)
  readr::write_tsv(genes, "data-raw/hg19_genes/hg19_genes.bed")
  R.utils::gzip("data-raw/hg19_genes/hg19_genes.bed")
}

genes <- readr::read_tsv("data-raw/hg19_genes/hg19_genes.bed.gz")
genomefile <- valr::valr_example('hg19.chrom.sizes.gz')
genome <- valr::read_genome(genomefile)

genes <- dplyr::select(genes,
                       chrom, txStart, txEnd, geneName, exonCount, strand)

genes <- dplyr::rename(genes,
                       start = txStart,
                       end   = txEnd,
                       name = geneName,
                       score = exonCount
)

hg19_genes <- valr::bed_merge(genes,
                         name = values_unique(name),
                         score = 0,
                         strand = first(strand))

use_data(hg19_genes, compress = "xz", overwrite = TRUE)
