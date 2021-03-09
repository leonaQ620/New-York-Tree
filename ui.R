#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(leaflet)
library(shinythemes)

# Define UI for application that draws a histogram
navbarPage("Street Tree in NewYork",
           theme = shinytheme('cosmo'),
           tabPanel("Tree MAP",
                    sidebarLayout(
                        sidebarPanel(
                           
                            selectInput(
                                inputId = "Zip_City",
                                label = "Choose the City:",
                                choices = unique(tree$zip_city),
                                selected="Forest Hills"
                            ),
                            
                            p(h3(strong("For the first time, you have access to information 
                                 about every street tree in New York City."))),
                            br(),
                            p(strong("Selecting the city, 
                              find the distribution and specific number of trees in the city. 
                              See a specific position of trees by clicking on the number. 
                              At the same time, a status of the tree can be distinguished by color.")),
                            br(),
                            p(strong("Based on a tree diagram, You can better know 
                            the specific cities with the most dead and stump trees. 
                            Through the city selection, you can view the specific location of stump or dead tree.")),
                           br(),br(),
                            p(h4("%trees of each borough")),
                            p(strong("Queens"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              span(strong("37%"),style ="font-size: 120%;", style = "color:orange")),
                            p(strong("Brooklyn"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              span(strong("26%"),style ="font-size: 120%;", style = "color:red")),
                            p(strong("Staten Island"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              span(strong("15%"),style ="font-size: 120%;", style = "color:green")),
                            p(strong("Bronx"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              span(strong("12%"),style ="font-size: 120%;", style = "color:blue")),
                            p(strong("Manhattan"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              span(strong("10%"),style ="font-size: 120%;", style = "color:grap"))
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                            p(span(strong("Mapped on Tree"), style = "font-size: 120%;"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              span(strong("Trees Alive"), style = "font-size: 120%;"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              span(strong("Trees Dead"), style = "font-size: 120%;"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              span(strong("Trees Stump"), style = "font-size: 120%;")),
                            p(span(strong("683,788"), style = "font-size: 150%;",style="color:green"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              span(strong("652,173"), style = "font-size: 150%;",style="color:green"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              span(strong("13,961"), style = "font-size: 150%;",style="color:green"),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                              span(strong("17,654"), style = "font-size: 150%;",style="color:green")),
                            br(),
                            leafletOutput(outputId ="mymap"),
                            br(),br(),
                            plotOutput(outputId ="PlotT"),
                            
                            
                        )    
                    )
           ),
        tabPanel("The Condition of Trees",

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(inputId = "Date",
                           label = "Choose the date",
                           start = min(tree$created_at),
                           end = max(tree$created_at),
                           min = min(tree$created_at),
                           max = max(tree$created_at),
                           startview = "month",
                           weekstart = 0
                           
            ),
            
            checkboxGroupInput(inputId = "User_Type",
                               label="Choose the User_Type",
                               choices = unique(tree$user_type),
                               selected = unique(tree$user_type)
                               ),
            checkboxGroupInput(inputId = "Borough",
                        label="Choose the Borough",
                        choices = unique(tree$borough),
                        selected = unique(tree$borough)
                ),
            
            checkboxGroupInput(inputId = "Status",
                        label="Choose the status of trees",
                        choices = unique(tree$status),
                        selected = unique(tree$status)
            ),
            checkboxGroupInput(inputId = "Guard",
                               label="Choose the Guard type",
                               choices = c("None","Helpful","Harmful","Unsure"),
                               selected = c("None","Helpful","Harmful","Unsure")
                               
            ),
            p(h4("%trees of each Health condition")),
            p(strong("Good"),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              span(strong("76.16%"),style ="font-size: 120%;", style = "color:green")),
            p(strong("Fair"),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              span(strong("18.84%"),style ="font-size: 120%;", style = "color:blue")),
            p(strong("Poor"),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              span(strong("5.01%"),style ="font-size: 120%;", style = "color:red"))
            
        ),
            
        mainPanel(
            p(h4("Records created by User_Type")),
            p(span(strong("NYC Parks Staff: 169,986"),style = "color:red"),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              span(strong("TreeCount Staff: 296,284"),style = "color:green"),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
              span(strong("Volunteer: 217,518"),style = "color:blue")),
            br(),
            plotOutput("PlotTS", width = "100%",height = 500),
            br(),br(),
            plotOutput(outputId = "Plot")
            
            
       )
    )
            ),
           
    tabPanel("Species of Trees",
             
             fluidRow(
                 column(4, offset = 4,
                        selectInput( inputId = "Species",
                                     label="Choose the species of trees",
                                     choices = c("London planetree", "honeylocust",
                                                 "Callery pear", "pin oak",         
                                                 "Norway maple"),
                                     multiple = TRUE,
                                     selected = "London planetree")
                 ),
                 column(4,
                        selectInput(inputId = "Boroughd",
                                    label="Choose the Borough",
                                    choices = unique(tree$borough),
                                    multiple = TRUE,
                                    selected = "Queens")
             ),
             br(),
             fluidRow(
                 plotOutput("STmap",width = "100%", height = 700),
                 br(),br(),
                 br(),br(),
                 plotOutput("PlotH")
             )
                 )
    ),
    tabPanel("Tree Problems",
            sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput(inputId = "Problems",
                                        label = "Choose the Problems",
                                        choices = c("Stones","BranchLights",      
                                               "Stones,BranchLights", "RootOther",          
                                               "TrunkOther"),
                                        selected = "Stones"),
                     p(strong("Stones: Light Blue")),
                     p(strong("BranchLights: Grape")),
                     p(strong("Stones&BranchLights: Green")),
                     p(strong("RootOther: Blue")),
                     p(strong("TrunkOther: Yellow")),
    
                ),
                 mainPanel(
                     leafletOutput("Plotp1"),
                     br(),br(),
                     plotOutput("Plotp2",height = 700)
                     
                 )
             )
    )
)

