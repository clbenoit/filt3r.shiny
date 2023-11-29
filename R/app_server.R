#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny shinybusy
#' @importFrom rjson fromJSON
#' @importFrom DT renderDataTable datatable
#' @importFrom dplyr select %>% filter
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom golem get_golem_options
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  options(shiny.maxRequestSize=250*1024^2)

  reactives <- reactiveValues(sample = NULL, command_line = NULL,
                              version = NULL, nb_total = NULL, nb_filtered = NULL,
                              percentage = NULL, nb_reads_in_events = NULL,
                              results_table = NULL,  results_table_filtered = NULL)

  observeEvent(input$fastqs,{
    req(input$fastqs)
    print("uploaded fastq files")
    print(unique(gsub("_S.*_L.*_R.*_001.fastq.gz","",input$fastqs$name)))
    reactives$sample <- unique(gsub("_S.*_L.*_R.*_001.fastq.gz","",input$fastqs$name))
  })

  output$sample <- renderText(paste0(reactives$sample, " is ready to be processed. \n"))
  observeEvent(reactives$sample,{
  output$sampleUI <- renderUI({
    req(reactives$sample)
    if(!is.null(reactives$sample)){
      fluidPage(fluidRow(column(width = 12, textOutput("sample"), br())))
    }
  })
  })

  observeEvent(input$computeITD,{
    if(is.null(reactives$sample)){
      print("seendsweet")
      sendSweetAlert(session = session,title = "No input data available",
                     text = "Please upload fastq files first",
                     type = "error")
    }
  })

  fasta <- system.file("extdata","references/flt3_exon14-15.fa", package = "filt3r.shiny")
  tempdir <- tempdir()
  observeEvent(input$computeITD,{
    req(input$fastqs)
    req(reactives$sample)
    show_modal_spinner(
      spin = "fading-circle",
      text = "Computing FLT3 ITDs..."
    )

    print(paste0(file.path(get_golem_options("filt3r_path"), "filt3r"),
                 " --nb-threads 2 --ref ",fasta, " -k 12 --sequences ",
                 paste0(input$fastqs$datapath, collapse = ","),
                 " --out ", file.path(tempdir, paste0(reactives$sample,".json")), " --vcf"))

    system(paste0(file.path(get_golem_options("filt3r_path"), "filt3r"),
                  " --nb-threads 2 --ref ",fasta, " -k 12 --sequences ",
                  paste0(input$fastqs$datapath, collapse = ","),
                  " --out ", file.path(tempdir, paste0(reactives$sample,".json")), " --vcf"))

    json_data <- jsonlite::fromJSON(txt = file.path(tempdir, paste0(reactives$sample,".json")), flatten = TRUE)
    reactives$command_line <- json_data$`command line`
    reactives$version <- json_data$version
    reactives$nb_total <- json_data$nb_total
    reactives$nb_filtered <- json_data$nb_filtered
    reactives$percentage <- json_data$percentage
    reactives$nb_reads_in_events <- json_data$nb_reads_in_events
    reactives$results_table <- as.data.frame(json_data$details) %>%
      select(reference_pos, everything())
    remove_modal_spinner()
  })

  observeEvent(c(input$is_wt_duplication,
                 input$is_duplication,
                 reactives$results_table),{
    req(reactives$results_table)
    if (input$is_duplication == TRUE) {
      results_table_filtered <- reactives$results_table %>% dplyr::filter(is_duplication ==  "TRUE")
    } else {
      results_table_filtered <- reactives$results_table
    }
    if (input$is_wt_duplication == TRUE) {
      results_table_filtered <- results_table_filtered %>% dplyr::filter(is_wt_duplication == "TRUE")
    }
    reactives$results_table_filtered <- results_table_filtered
  })

  output$nb_reads_in_eventsBox <- renderUI({
    req(reactives$nb_reads_in_events)
    fluidRow(infoBox(width = "100%",
      title = "Number of reads in events",
      icon = icon("check"),
      reactives$nb_reads_in_events, fill = TRUE ,color = "purple"))
  })
  output$percentageBox <- renderUI({
    req(reactives$percentage)
    fluidRow(infoBox(width = "100%",
                     icon = icon("percentage"),
                     title = "% of reads overlaping a breakpoint",
                     reactives$percentage, fill = TRUE ,color = "purple"))
  })
  output$nb_totalBox <- renderUI({
    req(reactives$nb_total)
    fluidRow(infoBox(width = "100%",
                     title = "Number of reads analysed",
                     reactives$nb_total, fill = TRUE ,color = "purple"))
  })
  output$nb_filteredBox <- renderUI({
    req(reactives$nb_filtered)
    fluidRow(infoBox(width = "100%",
                     icon = icon("stack-overflow"),
                     title = "Nb of reads matching the reference",
                     reactives$nb_filtered, fill = TRUE ,color = "purple"))
  })

  output$results_table <- DT::renderDataTable(DT::datatable(data = reactives$results_table_filtered,
                                                            rownames = FALSE,
                                                            #extensions = c("Buttons"),
                                                            extensions = c("FixedColumns","FixedHeader","Buttons"),
                                                            options = list(
                                                                           autoWidth = TRUE,
                                                                           scrollX = TRUE,
                                                                           columnDefs = list(
                                                                             list(targets = "_all", width = '200px')  # Set a minimum width for all columns
                                                                           ),
                                                                           #scrollY = "500px", lengthMenu = c(20, 40, 60, 80),
                                                                           fixedColumns = list(leftColumns = 2),fixedHeader = TRUE,
                                                                           dom = 'l<"sep">Bfrtip', # place where buttons are placed
                                                                           buttons = c('colvis','copy','excel')))

                            )


  observeEvent(input$about, {
    # Your logic when the link is clicked
    showModal(modalDialog(size = "l",
      title = "About this app",
      HTML("<h3> FiLT3r </h3>
      FiLT3r is a software that detects internal duplications in raw sequencing data. </br>
      FiLT3r uses an alignment-free approach, which is thus resource-frugal.Compared to the state-of-the-art, FiLT3r is faster and uses less memory, while having better results both for detecting the duplications and for quantifying them. </br>
      FiLT3r was tested on capture data and RNA-Seq data for FLT3-ITD quantification. Detailed results can be found in <a href=\"https://doi.org/10.1186/s12859-022-04983-6\">Baudry et al, 2022</a>.</br></br>
      For any questions or problems relating to the tool : <b>mikael.salson@univ-lille.fr</b></br>

      <h3> FiLT3r-Shiny </h3>
      This tool has been wrapped into a graphical interface developed within the Molecular Biology platform of the Grenoble-Alpes University Hospital.</br></br>
      For any questions or problems relating to the application : <b>benoitclement.sand@gmail.com</b></br>"),
      easyClose = TRUE
    ))
  })


}

