#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(purrr)
library(rlang)
library(stringr)
# devtools::install_github("jienagu/noteMD")
library(noteMD)
library(DT)
library(r2d3)
library(webshot)
library(htmlwidgets)
library(memor)

 webshot::install_phantomjs()

temp_flight2=readRDS("temp_flight2.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("D3 Interactive Bar Chart"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "airlines",
        label = "Airlines:",
        choices = c("ALL", unique(temp_flight2$name)),
        size = 10,selectize = FALSE,
        selected = "ALL"
      ),
      textInput("author", "Please write your name here:", "Your name"),
     # actionButton("remove", "Remove data table(s)!"),br(),br(),br(),
      downloadButton('describe_download',"Download Report",class="butt" ),br(),
      tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
      radioButtons('format', 'Document format', c('PDF', 'Word'),
                   inline = TRUE),br(),
     tags$head(tags$style(".button{background-color:#032863;} .button{color: #e6ebef;}")),
   #  downloadButton('presentation_download',"Download Presentation",class="button" ),br(),br(),
     downloadButton('presentation_download2',"Download Interactive Presentation",class="button" ),br()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Analytics Dashboard",
          value = "page1",
          fluidRow(
            column(
              width = 8,
              d3Output("airbar")
            )
          ),
          fluidRow(
            column(12,
                   helpText("Note: Any comments made in the box will be printed if you download the summary report.") ),
            column(12,
                   tags$textarea(
                     "Please using any **markdown** syntax!",
                     id    = 'markdowninput',
                     rows  = 3,
                     style = 'width:100%;')) ),
          helpText("Preview:"),
          htmlOutput('htmlmarkdown'),br(),br()
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$htmlmarkdown = reactive({
    note_in_html(input$markdowninput)
  })
  
  tab_list <- NULL
  
  sel_flights <- reactive({
    if (input$airlines != "ALL") temp_flight2 <- filter(temp_flight2, name == input$airlines)
    temp_flight2
  })
  
  bar_graphD3=reactive({
    grouped <- ifelse(input$airlines != "ALL", expr(monthly), expr(name))
    
    flightdata <- sel_flights() %>%
      group_by(!!grouped) %>%
      tally() %>%
      collect() %>%
      mutate(
        y = n,
        x = !!grouped
      ) %>%
      select(x, y)
    
    flightdata <- flightdata %>%
      mutate(label = x)
    
    r2d3(flightdata, "bar_plot.js")
  })
  
  
  output$airbar = renderD3({
    bar_graphD3()
  })
 
  # airline/month bar click (server) ---------------------------------
  observeEvent(input$bar_clicked != "", {
    if (input$airlines == "ALL") {
      updateSelectInput(session, "airlines", selected = input$bar_clicked)
    } 
   }, ignoreInit = TRUE )
 
  
  output$describe_download = downloadHandler(
    filename<- function(){
      paste("Summary",Sys.Date(),switch(
        input$format, PDF = '.pdf', Word = '.docx'
      ),sep = "")
    },
    
    content = function(file) {
      if (input$format=="PDF"){
        #### Progressing indicator
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('summary_report.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'summary_report.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('summary_report.Rmd', pdf_document())
                       file.rename(out, file)
                       
                     })
        ### below is the end of pdf content
      }else{
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('summary_report_word.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'summary_report_word.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('summary_report_word.Rmd', word_document())
                       file.rename(out, file)
                     })
      }
      
    })
  
  # output$presentation_download = downloadHandler(
  #   filename<- function(){
  #     paste("Presentation",Sys.Date(),".pdf",sep = "")
  #   },
  #   
  #   content = function(file) {
  #       #### Progressing indicator
  #       withProgress(message = 'Download in progress',
  #                    detail = 'This may take a while...', value = 0, {
  #                      for (i in 1:15) {
  #                        incProgress(1/15)
  #                        Sys.sleep(0.01)
  #                      }
  #                      
  #                      ## End of progression
  #                      src <- normalizePath('presentation_summary.Rmd')
  #                      
  #                      # temporarily switch to the temp dir, in case you do not have write
  #                      # permission to the current working directory
  #                      owd <- setwd(tempdir())
  #                      on.exit(setwd(owd))
  #                      file.copy(src, 'presentation_summary.Rmd', overwrite = TRUE)
  #                      
  #                      library(rmarkdown)
  #                      out <- render('presentation_summary.Rmd',  beamer_presentation())
  #                      file.rename(out, file)
  #                      
  #                    })
  # 
  #     
  #   },
  #   contentType = 'application/pdf')
  
  output$presentation_download2 = downloadHandler(
    filename<- function(){
      paste("Presentation",Sys.Date(),".html",sep = "")
    },
    
    content = function(file) {
      #### Progressing indicator
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     ## End of progression
                     src <- normalizePath('test2.Rmd')
                     
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'test2.Rmd', overwrite = TRUE)
                     
                     library(rmarkdown)
                     out <- render('test2.Rmd',  ioslides_presentation())
                     file.rename(out, file)
                     
                   })
      
      
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

