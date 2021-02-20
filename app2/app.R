library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
#load("labor.Rdata")
#labor <- read.csv("labor.csv", header=T)
labor <- read.csv(file = "labor.csv")

labor <- as.data.frame(labor)
# Define UI for application that plots features of employer -----------
ui <- fluidPage(
    
    # Theme selector --------------------------------------------------
    shinythemes::themeSelector(),
    ## for set theme, comment above out
    theme = shinythemes::shinytheme("united"),
    
    # Application title -----------------------------------------------
    titlePanel("Employment Landscape"),
    
    # Sidebar layout with a input and output definitions --------------
    sidebarLayout(
        
        # Inputs: Select variables to plot ------------------------------
        sidebarPanel(
            
            # Select variable for y-axis ----------------------------------
            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        choices = c("Jan 2020 (Emp)" = "Emp.Jan.20",
                                    "Jan 2021 (Emp)" = "Emp.Jan.21",
                                    "Volume Change (Emp)" = "Emp.Volume.Change"), 
                        selected = "Emp.Volume.Change"),
            
            # Select variable for x-axis ----------------------------------
            selectInput(inputId = "x", 
                        label = "X-axis:",
                        choices = c("Workforce Development Area" = "WDA", 
                                    "Employer" = "Employer"), 
                        selected = "WDA"),
            
            # Select variable for color -----------------------------------
            selectInput(inputId = "z", 
                        label = "Color by:",
                        choices = c("Workforce Development Area" = "WDA", 
                                    "Employer" = "Employer"), 
                        selected = "Employer"),
            
            # Set alpha level ---------------------------------------------
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5),
            
            # Set point size ----------------------------------------------
            sliderInput(inputId = "size", 
                        label = "Size:", 
                        min = 0, max = 5, 
                        value = 2),
            
            # Show data table ---------------------------------------------
            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            # Enter text for plot title ---------------------------------------------
            textInput(inputId = "plot_title", 
                      label = "Plot title", 
                      placeholder = "Top Employer Data"),
            
            # Horizontal line for visual separation -----------------------
            hr(),
            
            # Select which types of employer to plot ------------------------
            checkboxGroupInput(inputId = "selected_type",
                               label = "Select County/Counties:",
                               choices = c("Berks", "Bucks", "Central", "Chester",  "Delaware",
                                           "Lackawanna", "Lancaster", "Lehigh Valley", "Luzerne-Schuylkill", "Montgomery",
                                           "North Central", "Northern Tier", "Northwest", "Philadelphia", "Pocono",
                                           "South Central", "Southern Alleghenies", "Southwest Corner",
                                           "Three Rivers", "Tri-County", "West Central", "Westmoreland-Fayette"),
                               selected = "Southern Alleghenies"),
            
            # Select sample size ----------------------------------------------------
            numericInput(inputId = "n_samp", 
                         label = "Sample size:", 
                         min = 1, max = nrow(labor), 
                         value = 50)
        ),
        
        # Output: -------------------------------------------------------
        mainPanel(
            
            # Show scatterplot --------------------------------------------
            plotOutput(outputId = "scatterplot"),
            br(),        # a little bit of visual separation
            
            # Print number of obs plotted ---------------------------------
            uiOutput(outputId = "n"),
            br(), br(),    # a little bit of visual separation
            
            # Show data table ---------------------------------------------
            DT::dataTableOutput(outputId = "employertable")
        )
    )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output, session) {
    
    # Create a subset of data filtering for selected title types ------
    employer_subset <- reactive({
        req(input$selected_type) # ensure availablity of value before proceeding
        filter(labor, Employer %in% input$selected_type)
    })
    
    # Update the maximum allowed n_samp for selected type employer ------
    observe({
        updateNumericInput(session, 
                           inputId = "n_samp",
                           value = min(50, nrow(employer_subset())),
                           max = nrow(employer_subset())
        )
    })
    
    # Create new df that is n_samp obs from selected type employer ------
    top_sample <- reactive({ 
        req(input$n_samp) # ensure availablity of value before proceeding
        sample_n(employer_subset(), input$n_samp)
    })
    
    # Convert plot_title toTitleCase ----------------------------------
    pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
    
    # Create scatterplot object the plotOutput function is expecting --
    output$scatterplot <- renderPlot({
        ggplot(data = top_sample(), aes_string(x = input$x, y = input$y,
                                               color = input$z)) +
            geom_point(alpha = input$alpha, size = input$size) +
            labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
                 y = toTitleCase(str_replace_all(input$y, "_", " ")),
                 color = toTitleCase(str_replace_all(input$z, "_", " ")),
                 title = pretty_plot_title()
            )
    })
    
    # Print number of employment plotted ----------------------------------
    output$n <- renderUI({
        types <- top_sample()$title_type %>% 
            factor(levels = input$selected_type) 
        counts <- table(types)
        
        HTML(paste("There are", counts, input$selected_type, "employer in this dataset. <br>"))
    })
    
    # Print data table if checked -------------------------------------
    output$employertable <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = top_sample()[1:10,], 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        }
    )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
