library(shiny)
# library(ShinyDash)
library(shinydashboard)

fluidPage(
  headerPanel(div(img(src="https://lh3.googleusercontent.com/RKnIFxevargA8knOxlyNkH_ifzXHdTf3SNQBiSMJIW02vk2DuwRssHZR18L415-kxJHWYhthtkcO7DfL8Q")
  ), 
  ),
  
  sidebarLayout(position = "right",

                sidebarPanel(
                  (h3(strong("Prospect Weights"))
                   
                  ),
                  fileInput('file1', 'Choose CSV File',
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                    tags$hr(),
                    textInput("Speed", label = h4("Speed"), placeholder = "Enter Score"),
                    textInput("Throwing", label = h4("Throwing"), placeholder = "Enter Score"),
                    textInput("Hitting", label = h4("Hitting"), placeholder = "Enter Score"),
                    textInput("Fielding", label = h4("Fielding"), placeholder = "Enter Score"),


                  tags$hr(),
                  
                  actionButton("EnterButton",label = "Enter",icon = icon("refresh"))
                  
                ),
                mainPanel(
                  
                  # br(),
                  # br(),
                  # br(),
                  # h3(strong("Index:"), align = "center"),
                  #h3(textOutput("text1"), align = "center"),
                  
                  tableOutput("outputId")
                  
                )
  )
  
)
