# example code to display under plots
#-----------------------------------------------------------------

coverage_code <-
"
library(dplyr)
library(ggplot2)
library(valr)
library(valrdata)

genomefile <- valr_example('hg19.chrom.sizes.gz')
genome <- read_genome(genomefile)

# generate 1 bp intervals, + strand only for now
tss <- valrdata::hg19_genes %>%
  filter(strand == '+') %>%
  mutate(end = start + 1)


#use first dnaseI dataset
y <- valrdata::dnase_data[[1]] %>%
  mutate(dataset = names(dnase_data)[1])

#set default plotting parameters
region_size <- 10000
win_size <- 200

x <- tss %>%
  bed_slop(genome, both = region_size) %>%
  bed_makewindows(genome, win_size)

res <- bed_map(x, y, sums = sum(value), dataset = first(dataset)) %>%
  group_by(dataset, .win_id) %>%
  filter(sums != 'NA') %>%
  summarize(means = mean(sums, na.rm = T), sds = sd(sums, na.rm = T))

x_labels <- pretty(-region_size:region_size, n = 11)
x_breaks <- seq(1, length(res$.win_id), length.out = length(x_labels))
sd_limits <- aes(ymax = means + sds, ymin = means - sds)

ggplot(res, aes(x = .win_id, y = means, color = dataset)) +
  geom_point() + geom_pointrange(sd_limits) +
  scale_x_continuous(labels = x_labels, breaks = x_breaks) +
  ggtitle('DNaseI hypersensitivity signal near TSSs') +
  xlab('Position\n(bp from TSS)') + ylab('Signal') +
  theme_bw()
"

dnase_stats_code <-


"
library(valr)
library(valrdata)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ComplexHeatmap)

#First load all 20 BED files containing DNase I hypersensitivity sites from 20 fetal tissues.
data <- dnase_data

#Then generate a 20x20 table containing a Jaccard statistic for each of the 400 pairwise comparisons.
res <- data %>%
  cross2(.,.) %>%
  map(lift(bed_jaccard)) %>%
  map('jaccard') %>%
  flatten_dbl() %>%
  matrix(nrow = 20, ncol = 20)

#get column and rownames as tissue + sample_num
colnames(res) <- names(data)
rownames(res) <- names(data)

#Visualize Jaccard coefficiencts as heatmp
Heatmap(res, color_space = 'Blues', heatmap_legend_param = list(title = 'Jaccard\nCoefficients'))

pca <- broom::tidy(prcomp(res)) %>% as_data_frame()

pca_comps <- filter(pca, PC <= 2) %>%
tidyr::spread(PC, value) %>%
setNames(c('label','PC1','PC2'))

ggplot(pca_comps, aes(PC1, PC2)) +
geom_point(size = 3, color = 'red') +
geom_text_repel(aes(label = label))
"

segway_data <-
"
library(valr)
library(valrdata)
library(dplyr)
library(tidyr)


tidy_data <- valrdata::segway_data %>%
  tidyr::gather(cell_line, label, -chrom, -start, -end, -sum_score, -mean_score)

# a table with 27,024,740 rows

repeat_overlap <- bed_intersect(segway_data, valrdata::repeat_data) %>%
  distinct(chrom, start.x, end.x, sum_score.x, mean_score.x, .keep_all = T)

repeat_overlap <-   tidyr::gather(repeat_overlap, cell_line, label,
  -chrom, -start.x, -end.x, -sum_score.x, -mean_score.x)


"




