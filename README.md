valrdata 
====

[![Build Status](https://travis-ci.org/rnabioco/valrdata.svg?branch=master)](https://travis-ci.org/rnabioco/valrdata)

A `Shiny` app and R datapackage for demonstrating genome analysis with [`valr`](https://github.com/rnabioco/valr) 

Installation
-------

```r
# install.packages(devtools)
devtools::install_github("rnabioco/valrdata")
```
Shinyapp
------
```r
valrdata::launch_valr_app()
```

Datasets
--------

### DNaseI Hypersensitivity Data
 DNaseI Hypersensitivity sites in the human genome (`hg19`) identified by DNaseI-Seq in a set of 20 fetal tissues. Organized into a list of data_frames.

```r
valrdata::dnase_data
```
>
Maurano et al. Systematic Localization of Common Disease-Associated Variation in Regulatory DNA. Science. 2012. Vol. 337 no. 6099 pp. 1190-1195.
>

http://www.sciencemag.org/content/337/6099/1190.short

https://s3.amazonaws.com/bedtools-tutorials/web/maurano.dnaseI.tgz

### Segway Encyclopedia

Annotations of human regulatory elements for 164 human cell types identified using `Seqway` organized in an extended `bed+` wide format. 

```r
valrdata::segway_data
```

>
Libbrecht MW, Rodriguez O, Hoffman MM, Bilmes JA, Noble WS. 2016. A unified encyclopedia of human functional elements through fully automated annotation of 164 human cell types. Biorxiv preprint: http://dx.doi.org/10.1101/086025.
>

http://noble.gs.washington.edu/proj/encyclopedia/

http://noble.gs.washington.edu/proj/encyclopedia/segway_encyclopedia.bed.gz


### Repetitive Elements

A dataset containing annotations for repeat classes in the `hg19` human genome, as annotated by `RepeatMasker` in `bed6+` format. 

```r
valrdata::repeat_data
```

http://genome.ucsc.edu/cgi-bin/hgTables


### Gene Annotations

A dataset containing RefSeq `refFlat` gene annotations for `hg19` in `bed6` format.

```r
valrdata::hg19_genes
```

http://genome.ucsc.edu/cgi-bin/hgTables





