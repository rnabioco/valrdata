# utility functions for shiny app
#-----------------------------------------------------------------

select_dnase_data <- function(dnase_data, input_file){
  # take a list of bedgraphs and only get those selected by user
  # add dataset column to keep track of the selected dataset

  if ("Average All Datasets" %in% input_file) {
    res <- bind_rows(dnase_data) %>%
      mutate(dataset = "Average of All Datasets")
  } else {
    res <- dnase_data[input_file] %>%
      bind_rows(., .id = "dataset") %>%
      select(-dataset, everything(), dataset)
  }
  res
}

# function for plotting coverage data
plot_metagene <- function(x, y, genome, win_size, region_size){

  x <- x %>%
    bed_slop(genome, both = region_size) %>%
    bed_makewindows(genome, win_size)

  res <- bed_map(x, y, sums = sum(value), dataset = first(dataset)) %>%
    group_by(dataset, .win_id) %>%
    filter(sums != "NA") %>%
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
}

plot_usr_metagene <- function(x, y, genome, win_size, region_size){

  x <- x %>%
    bed_slop(genome, both = region_size) %>%
    bed_makewindows(genome, win_size)

  res <- bed_map(x, y, sums = sum(value), dataset = first(dataset)) %>%
    group_by(dataset, .win_id) %>%
    filter(sums != "NA") %>%
    summarize(means = mean(sums, na.rm = T), sds = sd(sums, na.rm = T))

  x_labels <- pretty(-region_size:region_size, n = 11)
  x_breaks <- seq(1, length(res$.win_id), length.out = length(x_labels))
  sd_limits <- aes(ymax = means + sds, ymin = means - sds)

  ggplot(res, aes(x = .win_id, y = means, color = dataset)) +
    geom_point() + geom_pointrange(sd_limits) +
    scale_x_continuous(labels = x_labels, breaks = x_breaks) +
    xlab('Position\n(bp from TSS)') + ylab('Signal') +
    theme_bw()
}
# -----------------------------------------------------------------------

# options for saving from datatable
savingOptions <- list(
  "dom" = 'lBfrtip',
  buttons = list('copy', 'print', list(
    extend = 'collection',
    buttons = list(list(extend = 'csv',
                        fieldBoundary = ""
    ),
    list(extend = 'csv',
         fieldSeparator = "\t",
         fieldBoundary = "",
         text = "TSV",
         extension = ".tsv"),
    list(extend = 'pdf')
    ),
    text = 'Download'
  )),
  list(orderClasses = TRUE),
  lengthMenu = list(c(5, 10, 25, -1), c('5', '10', '25', 'All')),
  pageLength = 5
)
