library(shinydashboard)
library(valrdata)
library(valr)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
source("utilities.r")
source("code_demos.r")

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
datasets <- c(datasets, "Average All Datasets")
#---------------------------------------------------------------
ui <- dashboardPage(

  dashboardHeader(title = "valr-examples", titleWidth = 150),
  dashboardSidebar(
    width = 150,
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
    menuItem("Data Table", tabName = "table", icon = icon("table")),
    menuItem("Upload data", tabName = "usr_data", icon = icon("table")),
    menuItem("Metagene Plot", tabName =  "plot", icon = icon("signal", lib = "glyphicon"), selected = T)
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", href="github.css", type ="text/css"),
      tags$script(src = "rainbow-custom.min.js")
        ),
    tabItems(
      tabItem(tabName = "table",
        fluidRow(
          box(DT::dataTableOutput("bed_data"),
              width = 12)
        )),

      tabItem(tabName = "usr_data",
        fluidRow(
          column(width = 6,
                 box(
                   fileInput("usr_file", "Choose Bed File"
                            # accept = c(
                            #   "text/csv",
                            #   "text/comma-separated-values,text/plain",
                            #   ".csv")
                   ),
                   tags$hr(),
                   numericInput("n_fields", "n_fields:", 3, min = 3, max = 100),
                   width = 12
                 )
              )),
        fluidRow(
          box(DT::dataTableOutput("usr_data"),
              width = 12)
        )),

      tabItem(tabName = "plot",
        fluidRow(
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
                            choices = datasets,
                            selected = datasets[1],
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



#-----------------------------------------------------
#Server
server <- function(input, output) {

  # a large table, reative to input$show_vars
  output$bed_data = DT::renderDataTable({
    genes
  }, extensions = c('Buttons'),
  options = savingOptions,
  filter = 'top',
  style = 'bootstrap',
  selection = list(mode = 'single',
                   target = 'row'),
  rownames = FALSE)


  output$usr_data <- DT::renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    .usr_dat <- input$usr_file
    if (is.null(.usr_dat))
      return(NULL)

    bed <- read.table(.usr_dat$datapath)
    bed
  })

 #plot_data_input <- reactiveValues(
#      current_dataset = input$select_dataset,
#      current_chrom = input$select_chrom
#  )


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



