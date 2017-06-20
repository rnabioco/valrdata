library(shinydashboard)
library(valrdata)
library(valr)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
source("utilities.r")
source("code_demos.r")

options(shiny.maxRequestSize = 100*1024^2)

#----------------------------------------------------------------
# Input Files
#use dnase_data and hg19_genes from valrdata

genomefile <- valr_example('hg19.chrom.sizes.gz')
genome <- read_genome(genomefile)

# generate 1 bp intervals, + strand only for now
tss <- valrdata::hg19_genes %>%
  filter(strand == '+') %>%
  mutate(end = start + 1)

# generate vector of primary chromosomes
chroms <- unique(tss$chrom)
chroms <- chroms[!str_detect(chroms, "_")]
chroms <- c(chroms, "All Chromosomes")

# generate vector of datasets
datasets <- names(valrdata::dnase_data)
dataset_options <- c(datasets, "Average All Datasets")
#---------------------------------------------------------------
ui <- dashboardPage(

  dashboardHeader(title = "valr-examples", titleWidth = 150),
  dashboardSidebar(
    width = 200,
    id="",
    ## the following javascript fixes an error whereby tabs can only be clicked once
    tags$head(
      tags$script(
        HTML(
          "
          $(document).ready(function(){
          // Bind classes to menu items, easiet to fill in manually
          var ids = ['table','usr_data','plot'];
          for(i=0; i<ids.length; i++){
          $('a[data-value='+ids[i]+']').addClass('my_subitem_class');
          }

          // Register click handeler
          $('.my_subitem_class').on('click',function(){
          // Unactive menuSubItems
          $('.my_subitem_class').parent().removeClass('active');
          })
          })

          "
          )
        )
      ),
  #  menuItem("Data Table", tabName = "table", icon = icon("table")),
    menuItem("Metagene Plot", tabName =  "plot", icon = icon("signal", lib = "glyphicon"), selected = T),
    menuItem("User Custom Metagene Plot", tabName = "usr_data", icon =  icon("signal", lib = "glyphicon"))
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", href="github.css", type ="text/css"),
      tags$script(src = "rainbow-custom.min.js")
        ),
    tabItems(
      tabItem(tabName = "table",
        fluidRow(
          box(
            selectInput("select_dataset_for_dt",
                        label = "Datasets to Display:",
                        choices = datasets,
                        selected = datasets[1],
                        multiple = FALSE
                        ),
            DT::dataTableOutput("bed_data"),
              width = 12)
          )
        ),

      tabItem(tabName = "usr_data",
        tags$h3("Generate a simple metagene from your own BED data"),
        fluidRow(
          box(column(width = 6,
                   tags$h4("Input bedgraph"),
                   tags$p("Bedgraph must be tab deliminated and contain 4 columns, \n
                           a header line is allowed if it begins with #, \n
                           gzipped files are allowed if they end in .gz, e.g.",
                          pre(code(
                              paste("chr1  11873  14409 1.0",
                                    "chr1  14361  19759 2.0",
                                    "chr1  14406  29370 1.5",
                                    "chr1  34610  36081 2.5", sep = "\n")
                   ))),
                   fileInput("usr_bedgraph", "Select bedgraph File")
                 ),
              column(width = 6,
                   tags$h4("Input bed"),
                   tags$p("Input gene annotations should be in BED format. \n
                          The TSS and TES will be determined based on the start and end values, e.g.",
                          pre(code(
                            paste("chr1  59486147  59553919  LINC01358 0 +",
                                  "chr8  67474409  67525484  MYBL1 0 -",
                                  "chr9  42493516  42498676  GXYLT1P3  0 +",
                                  "chr19  6210391 6279959 MLLT1 0 -",
                                  sep = "\n")
                          ))),
                   fileInput("usr_bed", "Select BED File containing genes"),

                   numericInput("n_fields", "Number of fields in BED file:", 3, min = 3, max = 100),
                   actionButton(
                       "file_load_Btn",
                       "Load & Analyze",
                       class = "btn-primary"
                   )
                 )
              , width = 12
              )
        ),

        fluidRow(
          column(width= 6,
                 box(DT::dataTableOutput("usr_bgdata"),
                     width = 12)),
          column(width= 6,
                 box(DT::dataTableOutput("usr_beddata"),
                     width = 12))
          ),

        fluidRow(
          column(width = 3,
                 box(selectInput("tx_position",
                                 "Region to plot",
                                 c("TSS",
                                   "TES"), selected = "TSS"),
                     sliderInput("usr_window", "Window Size:", 100, 1000, 200),
                     sliderInput("usr_region", "Region Size:", 2500, 20000, 10000),
                     width = 12
                 )
          ),
          column(width = 9,
                 box(
                   title = "Coverage Plot",
                   plotOutput("usr_coverage"),
                   width = 12
                 )
          )
        )
      ),

      tabItem(tabName = "plot",
        fluidRow(
          tags$h3("Plot DnaseI hypersensitivity data from multiple tissues "),
          tags$p("DNaseI hypersensitivity data was taken from ",
                 tags$a(href="www.sciencemag.org/content/337/6099/1190.short",
                        "Maurano et al. Science. 2012"),
                 " and includes data from multiple human tissues"),
          tags$p("Select chromosomes and datasets to plot on the left. Muliple datasets can be selected.",
                 "The BED data used to generate the plots is loaded as an R object ",
                        span(code("valrdata::dnase_data"))),
          column(width = 4,
              box(
                title = "Controls",
                sliderInput("window", "Window Size:", 100, 1000, 200),
                sliderInput("region", "Region Size:", 2500, 20000, 10000),
                selectInput("select_chrom", label = "Chromosomes to Include:",
                            choices = chroms,
                            selected = "chr22",
                            multiple = TRUE),
                selectInput("select_dataset", label = "Datasets to Display:",
                            choices = dataset_options,
                            selected = dataset_options[1],
                            multiple = TRUE),
                width = 12
              )

          ),
          column(width = 8,
            box(
              title = "Coverage Plot",
              plotOutput("coverage"),
              width = 12
            ),
            actionButton("show_code", "Display example plotting code"),
            conditionalPanel(
              condition = "input.show_code % 2",
              h4("Example Code"),
              pre(code(HTML(coverage_code), class="language-r"))
            )

          )
          )
        )
      )
    )
  )



#----------------------------------------------------------------
#Server
server <- function(input, output) {

  reactData <- reactiveValues(usr_dataset = NULL)
  output$usr_data_uploaded <- reactive({ FALSE })

  # a large table, reative to input$show_vars
  output$bed_data = DT::renderDataTable({
    in_dataset <- input$select_dataset_for_dt
    if(is.null(in_dataset)){
      valrdata::dnase_data[[1]]
    } else {
      valrdata::dnase_data[[in_dataset]]
    }
  }, extensions = c('Buttons'),
  options = savingOptions,
  filter = 'top',
  style = 'bootstrap',
  selection = list(mode = 'single',
                   target = 'row'),
  rownames = FALSE)

  observeEvent(input$file_load_Btn, {
    #import data and  assign to reactData

    # need to rename file for compressed input
    # shiny strips off the extension from input files
    # readr::read_tsv checks for .gz for decompression
    # https://github.com/tidyverse/readr/issues/491

    .usr_bgfile <- input$usr_bedgraph
    input_file_format <- tools::file_ext(.usr_bgfile$name)
    new_file_name <- paste0(.usr_bgfile$datapath, ".", input_file_format)
    file.rename(.usr_bgfile$datapath, new_file_name)
    bg <- read_bedgraph(new_file_name)

    .usr_bedfile <- input$usr_bed
    input_file_format <- tools::file_ext(.usr_bedfile$name)
    new_file_name <- paste0(.usr_bedfile$datapath, ".", input_file_format)
    file.rename(.usr_bedfile$datapath, new_file_name)
    bed <- read_bed(new_file_name, n_fields = input$n_fields)

    reactData$usr_bedgraph <- bg
    reactData$usr_bed <- bed

    output$datasetChosen <- reactive({ TRUE })

    # generate datatable for visualization
    output$usr_bgdata <- DT::renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
      .usr_dat <- reactData$usr_bedgraph
      if (is.null(.usr_dat)){
        return(NULL)
      } else {
        .usr_dat
      }
    },
    options = savingOptions,
    filter = 'top',
    style = 'bootstrap',
    selection = list(mode = 'single',
                   target = 'row'),
    rownames = FALSE)

    # generate datatable for visualization
    output$usr_beddata <- DT::renderDataTable({
      # input$file1 will be NULL initially. After the user selects
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
        .usr_dat <- reactData$usr_bed
        if (is.null(.usr_dat)){
          return(NULL)
        } else {
          .usr_dat
        }
      },
      options = savingOptions,
      filter = 'top',
      style = 'bootstrap',
      selection = list(mode = 'single',
                     target = 'row'),
      rownames = FALSE)

    output$usr_coverage = renderPlot({

      selected_bedgraph <- reactData$usr_bedgraph

      if(input$tx_position == "TSS"){
        tx_pos <- reactData$usr_bed %>%
          filter(strand == '+') %>%
          mutate(end = start + 1)
      } else {
        tx_pos <- reactData$usr_bed %>%
          filter(strand == '+') %>%
          mutate(start = end - 1)
      }

      if(!"dataset" %in% colnames(selected_bedgraph)){
        selected_bedgraph$dataset <- "user_data"
      }
      plot_usr_metagene(tx_pos, selected_bedgraph, genome,
                    win_size = input$usr_window,
                    region_size = input$usr_region)
    })

})




  output$coverage = renderPlot({
    in_dataset <- input$select_dataset
    in_chrom <- input$select_chrom

    selected_bedgraph <- select_dnase_data(valrdata::dnase_data, in_dataset)
    if ( "All Chromosomes" %in% in_chrom) {
      selected_bed <- tss
    } else {
      selected_bed <- filter(tss, chrom %in% in_chrom)
    }

    plot_metagene(selected_bed, selected_bedgraph, genome,
                  win_size = input$window,
                  region_size = input$region)
  })

  render_code <- HTML(coverage_code)

}


#---------------------------------------------------------
#Run App
shiny::shinyApp(ui, server)



