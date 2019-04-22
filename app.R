library(shiny)
# devtools::install_github("jienagu/noteMD")
library(noteMD)
library(DT)
library(r2d3)
library(webshot)
library(htmlwidgets)
library(leaflet)
library(rlang)
library(dplyr)
# webshot::install_phantomjs()
## Updated
temp_flight2=readRDS("temp_flight2.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Create Interactive Presentation from Shiny"),
  
  # Sidebar with a slider input for number of bins 
  textInput("author", "Please write down your name here:", "古杰娜"),
  textInput("presentation_title", "Title of your presentation:", "这里写标题"),
  tags$head(tags$style(".button{background-color:#032863;} .button{color: #e6ebef;}")),
  downloadButton('presentation_download2',"Download Interactive Presentation",class="button" ),br(),br(),
  ### CSS color to change the color of interface
  tags$style(HTML("
    .tabbable > .nav > li > a[data-value='page1'] {background-color: #F3B7C3;  color:black}
    .tabbable > .nav > li > a[data-value='page2'] {background-color: #acd2ee;   color:black}
    .tabbable > .nav > li > a[data-value='page3'] {background-color: #eedeac;  color:black}
    .shiny-input-container { color:#a27839}
    .container-fluid {color:navy; background-color: #f4dee7}
  ")),
    # Show a plot of the generated distribution
  fluidPage(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "Slide 1",
          value = "page1",br(),
          textInput("slide1_title", "Slide 1 Title:", "标题1"),br(),
          fluidRow(
            column(12,
                   helpText("Note: Any comments made in the box will be printed if you download the summary report.") ),
            column(12,
                   tags$textarea(
"重要的事情**说三遍**：*大碗宽面*

* 第一遍: 用 `UTF-8`
* 第二遍: 用 `UTF-8`
* 第三遍: 吼吼吼~用 `UTF-8` ",
                     id    = 'markdowninput',
                     rows  = 3,
                     style = 'width:100%;')) ),
          helpText("Preview:"),
          htmlOutput('htmlmarkdown'),br(),br()
        ),
        tabPanel(
          title = "Slide 2",
          value = "page2",br(),
          textInput("slide2_title", "Slide 2 Title:", "标题2"),br(),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "airlines",
                label = "Airlines:",
                choices = c("ALL", unique(temp_flight2$name)),
                size = 16,selectize = FALSE,
                selected = "ALL"
              ) ),
            column(
              width = 8,
              d3Output("airbar")
            )
          )
        ),
        tabPanel(
          title = "Slide 3",
          value = "page3",br(),
          textInput("slide3_title", "Slide 3 Title:", "标题3"),br(),
          fluidRow(
            column(
              width = 12,
              leafletOutput("mymap")
              )
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
  
  
  sel_flights <- reactive({
    if (input$airlines != "ALL") temp_flight2 <- filter(temp_flight2, name == input$airlines)
    temp_flight2
  })
  
  mymap_reactive=reactive({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=-96.571671, lat=39.183609, popup="Where I am!")
  })
  output$mymap <- renderLeaflet({
    mymap_reactive()
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

