### IMPORTANT ### this function assumes that the group size is not the first column. If it is the rowname, it will be fine
### IMPORTANT ### requires tidyr and ggplot2 and shiny

library(tidyverse)

#argument definitons:
  #gname: name of graph, subgname: subtitle, xlab: label for x axis, ylab = label for y axis, 
  #scalelab: scale label, graphlowcol: lower color of graph, graphhighcol: higher color of graph, decimals: decimals to round to on the graph,
  #slcutoff: small/large cutoff for number modifiers, smallsize: small number size, largesize: large number size, smallcolor: small number color, largecolor: large number color
ehm <- function(data, gname = "", subgname = '', xlab = 'Effect Size', ylab = '# of Mice per Group',
         scalelab = 'Percentage', graphlowcol = 'white', graphhighcol = 'green', decimals = 0, slcutoff = 90, smallsize = 4, largesize = 6, smallcolor= 1, largecolor = 'blue'){
  dataehm <- data.frame(data)
  dataehm <- gather(dataehm, 'dose')
  dataehm$ngroups <- rep(sort(as.numeric(as.character(rownames(data)))), ncol(data))
  dataehm$rvalue <- round(dataehm$value, decimals)
  #possibly code if-else to make argument calling easier - have a size that is used if smallsize and largesize are the same, same for color
  dataehm$ngroups <- as.factor(dataehm$ngroups)
  levels(dataehm$ngroups) <- rownames(data)
  dataehm$pgood <- smallsize
  dataehm$pgood[dataehm$rvalue >= slcutoff] <- largesize
  dataehm$pgood2 <- smallcolor
  dataehm$pgood2[dataehm$rvalue >= slcutoff] <- largecolor
  ggplot(dataehm, aes(main = gname, x = dose, y = ngroups, label = rvalue)) + geom_tile(aes(fill = dataehm$rvalue)) + 
  scale_fill_gradient(low = graphlowcol, high = graphhighcol) + geom_text(size = dataehm$pgood, color = dataehm$pgood2) + labs(title = gname, subtitle = subgname, x = xlab, y = ylab, fill = scalelab)
}



library(shiny)

ui <- fluidPage(

  # App title ----
  titlePanel("Percentage tables"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input
      selectInput(inputId = "prior",
                  label = "Number of Actual Drugs out of 100:", choices = c('5', '10', '20', '50')),
      sliderInput(inputId = "cutoff", label = "Emphasis Cutoff", min = 1, max = 100, value = 80),
      sliderInput(inputId = "size", label = "Emphasis Size", min = 3, max = 8, value = 4),
      textInput(inputId = "color", label = "Emphasis Color", value = "black", placeholder = 'ex. "black"'),
      textInput(inputId = "name", label = "Graph Name", value = '', placeholder = "ex. percentage accuracy graph for 50 real drugs"),
      textInput(inputId = "downloadname", label = "File Name", value = "", placeholder = "do NOT include file extension"),
      downloadButton("downloadPlot", label = "Download"),
      radioButtons(inputId = 'downloadyn', label = "Download?", choices = c("Yes", "No"))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic
server <- function(input, output) {

  output$distPlot <- renderPlot({

    serverplot <- ehm(trueFalsePositivePercentage[[input$prior]], slcutoff = input$cutoff, largesize = input$size, largecolor = input$color, gname = input$name)
    serverplot

    })

    output$downloadPlot <- downloadHandler(
            filename = function() { paste0(input$downloadname, ".png") },
            content = function(file) {
                  device <- function(..., width, height){
                                    grDevices::png(..., width = width, height = height, res = 300, units = "in")
                            }

                  serverplot <- ehm(trueFalsePositivePercentage[[input$prior]], slcutoff = input$cutoff, largesize = input$size, largecolor = input$color, gname = input$name)
                  ggsave(file, plot = serverplot, device = device)
            }
    )

   }


# Create Shiny app ----
shinyApp(ui = ui, server = server)

