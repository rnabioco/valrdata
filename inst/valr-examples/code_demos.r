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
  mutate(end = start + 1,
         dataset = 'Brain-1')


#use first dnaseI dataset
y <- valrdata::dnase_data[[1]]

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
  ggtitle('H3K4me3 ChIP signal near TSSs') +
  xlab('Position\n(bp from TSS)') + ylab('Signal') +
  theme_bw()
"
