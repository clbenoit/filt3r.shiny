#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard shinydashboardPlus shinyjs
#' @importFrom DT dataTableOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(skin = "purple",
                  #skin = "midnight",
                  #skin = "blue-light",
                  options = list(sidebarExpandOnHover = TRUE),
                  dashboardHeader(
                    titleWidth = '25%',
                    title = span(img(src = 'www/CHUlogo.png', width = 40, height = 39), " FiLT3r Shiny app"),
                    tags$li(class = "dropdown",
                            actionLink(label = "About the app", inputId = "about"))#,,
                                         #style = "position: relative; margin: 10px 10px 10px 10px; display:center-align;"))
                    #         actionButton(label = NULL, inputId = "godbinfo",icon = icon("database"),
                    #                      style = "position: relative; margin: 10px 10px 10px 10px; display:center-align;"))
                  ),
                  dashboardSidebar(width = '25vw', #id = "sidebars",
                                   minified = FALSE, collapsed = FALSE,
                                   fluidPage(br(),
                                    fluidRow(
                                    column(width = 12,
                                            fileInput(inputId = "fastqs",
                                                      label = "Upload your fastq files here : ",
                                                      multiple  = TRUE,
                                                      accept = c("fastq.gz","fq.gz"),
                                                      width = "100%")),
                                    column(width = 12,
                                           uiOutput("sampleUI")),
                                    column(width = 12, offset = 0,
                                               actionButton("computeITD",
                                                            label = "compute filt3r analysis",
                                                            width = "92%")
                                                            #style='margin:auto'),#)
                                           ),
                                    br(),br(),br(),br(),
                                    column(width = 12, column(width = 6, fluidRow(checkboxInput("is_duplication", label = "Filter out no duplication",
                                                                    value = TRUE, width = "100%"))),
                                    column(width = 6, fluidRow(checkboxInput("is_wt_duplication", label = "Filter out no wt duplication",
                                                                    value = TRUE, width = "100%"))))
                                   ))),
                  dashboardBody(
                    fluidPage(
                      br(),
                      fluidRow(
                        column(width = 6, uiOutput("nb_totalBox")),
                        column(width = 6, uiOutput("nb_filteredBox")),
                        column(width = 6, uiOutput("percentageBox")),
                        column(width = 6, uiOutput("nb_reads_in_eventsBox")),
                      ),br(),
                      fluidRow(DT::dataTableOutput("results_table"))
                    )
                  )
      )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  useShinyjs()
  tags$head(tags$style(HTML(".sep {width: 20px;height: 1px;float: left;}")))
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "filt3r.shiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

