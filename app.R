library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(likert)
library(sortable)
library(shinyWidgets)

ui <- navbarPage(
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    )
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = 'minty',
    bg = '#f7f7f7',
    fg = '#000',
    primary = '#0a1420',
    secondary = "#0a1420"
  ),
  title = span(
    'Shiny.Likert',
    span(
      a(
        icon('github'), 
        href = 'https://github.com/jhk0530', 
        target = "_blank",
        style = 'color: white;'
      ),
      # ' ',
      # a(
      #   icon('linkedin'),
      #   href = 'https://www.linkedin.com/in/jinhwan-kim/',
      #   target = "_blank",
      #   style = 'color: white;'
      # ),
      style = "right: 1em; position: absolute;"
    )
  ),
  footer = span(HTML(
  '<p class="footer-heart">
   Made with 
   <g-emoji class="g-emoji" alias="heart" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/2764.png">
    <img class="emoji" alt="heart" height="20" width="20" src="https://github.githubassets.com/images/icons/emoji/unicode/2764.png">
   </g-emoji> by 
   <a href="https://www.linkedin.com/in/jinhwan-kim/" target = "_blank">Jinhwan Kim</a> and 
   <a href="https://www.zarathu.com/" target ="_blank">Zarathu</a>
    </p>'), class = 'footer'),
  lang = "ko",
  tabPanel(
    title = NULL,
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        fileInput(
          inputId = 'file',
          label = 'Upload CSV File',
          accept = '.csv'
        ),
        uiOutput(outputId = 'columnUI'),
        uiOutput(outputId = 'orderUI')
      ),
      mainPanel(
        width = 9,
        uiOutput(outputId = 'plotUI'),
        uiOutput(outputId = 'optionUI')
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  v <- ''
  vv <- ''
  u <- character(0)
  observeEvent(input$file, {
    req(input$file)
    v <<- read.csv(input$file$datapath)
    output$columnUI <- renderUI({
      tagList(
        hr(),
        selectInput(
          inputId = 'columns', 
          label = 'Select Likert Column', 
          choices = colnames(v),
          multiple = TRUE
        )
      )
    })
    
  })
  
  observeEvent(input$columns, {
    u <- character(0)
    
    for(i in input$columns){
      u <- union(u, unique(v[,i]))
    }
    
    u <<- u
    
    output$orderUI <- renderUI({
      tagList(
        hr(),
        rank_list(
          text = 'Bad goes Top',
          labels = u,
          input_id = 'factorOrder'
        ),
        br(),
        sliderInput(
          inputId = 'height', 
          label = 'Chart Height', 
          value = 400, 
          min = 200, 
          max = 1000, 
          step = 100
        ),
        actionButton(inputId = 'draw', label = 'draw', style = 'width: 100%')
      )
    })
    
    output$optionUI <- renderUI({
      tagList(
        h6("Chart options"),
        hr(),
        fluidRow(
          column(
            width = 4,
            checkboxGroupButtons(
              inputId = "pps",
              label = "Show %",
              choices = c('Bad', "Mid", "Good"),
              selected = c('Bad', "Mid", "Good"),
              justified = TRUE
            )
          ),
          column(
            width = 4,
            colourInput(inputId = 'text.color', "Text Color", value = '#000000')
          ),
          column(
            width = 4,
            sliderInput(
              inputId = 'text.size', 
              label = 'Text Size', 
              min = 1, 
              max = 10, 
              value = 3, 
              step = 1
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            sliderInput(
              inputId = 'center', 
              label = 'Center', 
              value = (1 + length(u))/2, 
              min = 1.5, 
              max = length(u) - 0.5, 
              step = 0.5
            )
          ),
          column(
            width = 4,
            radioGroupButtons(
              inputId = 'inc', 
              label = 'Include center',
              choices = c('O', 'X'),
              justified = TRUE
            )
          ),
          column(
            width = 4,
            radioGroupButtons(
              inputId = "centered",
              label = 'Centered chart',
              choices = c('O', 'X'),
              justified = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            textInput(inputId = 'legend', label = 'legend', value = 'Response')
          ),
          column(
            width = 4,
            radioGroupButtons(
              inputId = 'legend.position', 
              label = 'Legend position', 
              choices = c('right', 'none'), 
              selected = 'right',
              justified = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            colourInput(inputId = 'low.color', "Bad Color", value = '#D8B365')
          ),
          column(
            width = 4,
            colourInput(inputId = 'neutral.color', 'Mid Color', value = '#e5e5e5'),
          ),
          column(
            width = 4,
            colourInput(inputId = 'high.color', "Good Color", value = '#5AB4AC')
          )
        )
        # checkboxInput(inputId = 'hist', 'Include Histogram', value = TRUE),
      )
    })
  })
  
  observeEvent(input$draw, {
    
    output$plotUI <- renderUI({
      plotlyOutput(outputId = 'plot', height = paste0(input$height, 'px'))
    })
    
    
    idx <- sapply(input$columns, function(i){
      which(colnames(v) == i)
    })
    
    vv <<- v[,idx]
    if(length(idx) > 1){
      for(i in 1:ncol(vv)){
        vv[,i] <- factor(vv[,i], levels = input$factorOrder)
      }
    }
    else{
      vv <- data.frame(as.factor(vv))
      colnames(vv) <- input$columns
    }
    
    ll <- likert(vv)
    
    
    output$plot <- renderPlotly({
      ggplotly(plot(
        ll, 
        plot.percents = ("Mid" %in% input$pps),
        plot.percent.low = ("Bad" %in% input$pps),
        plot.percent.high = ("Good" %in% input$pps),
        center = input$center, 
        include.center = ("O" == input$inc),
        centered = ("O" == input$centered),
        #include.histogram = input$hist # NOT WORK WITH PLOTLY
        low.color = input$low.color,
        high.color = input$high.color,
        neutral.color = input$neutral.color,
        text.size = input$text.size,
        text.color = input$text.color,
        legend = input$legend,
        legend.position = input$legend.position
      ))
    })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
