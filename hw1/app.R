library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(packcircles)
library(plotly)
library(viridis)
library(hrbrthemes)

## Loading Data - see cleaning_data.Rmd for how data was created

load("topemployers.Rdata")
#load("labor.Rdata")
#labor <- read.csv(file = "labor.csv")

#labor <- as.data.frame(labor)

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
               
            downloadButton("download1", "Download Employer Data"),
            br(), br(),
            # Select variable for y-axis ----------------------------------
            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        choices = c("Jan 2020" = "Emp.Jan.20",
                                    "Jan 2021" = "Emp.Jan.21",
                                    "Volume Change" = "Emp.Volume.Change"), 
                        selected = "Emp.Volume.Change"),
            
            # Select variable for x-axis ----------------------------------
            selectInput(inputId = "x", 
                        label = "X-axis:",
                        choices = c("Measure" = "Emp.Measure", 
                                    "Industry" = "Emp.Industry.Title"), 
                        selected = "Emp.Industry.Title"),
            
            # Select variable for color -----------------------------------
            selectInput(inputId = "z", 
                        label = "Color by:",
                        choices = c("Workforce Development Area" = "WDA", 
                                    "Employer Option" = "Employer"), 
                        selected = "WDA"),
            
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
                         min = 1, max = nrow(top.employers), 
                         value = 50)),
        
        # Output: -------------------------------------------------------
        mainPanel(
            tabsetPanel(
                tabPanel("Employer", plotOutput(outputId = "scatterplot"),
                         br(),
                         uiOutput(outputId = "n"),
                         br(), br(),
                         DT::dataTableOutput(outputId = "employertable")),
                tabPanel("Industry", plotOutput(outputId = "barplot")),
                tabPanel("Occupation", plotOutput(outputId = "lollipop"))
            )
            # Show scatterplot --------------------------------------------
            #plotOutput(outputId = "scatterplot"),
            #br(),        # a little bit of visual separation
            
            # Print number of obs plotted ---------------------------------
            #uiOutput(outputId = "n"),
            #br(), br(),    # a little bit of visual separation
            
            # Show data table ---------------------------------------------
            #DT::dataTableOutput(outputId = "employertable")
        )
    )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output, session) {
    
    # Create a subset of data filtering for selected title types ------
    top_subset <- reactive({
        req(input$selected_type) # ensure availablity of value before proceeding
        filter(top.employers, WDA %in% input$selected_type)
    })
    
    
    # Update the maximum allowed n_samp for selected type employer ------
    observe({
        updateNumericInput(session, 
                           inputId = "n_samp",
                           value = min(50, nrow(top_subset())),
                           max = nrow(top_subset())
        )
    })
    
    # Create new df that is n_samp obs from selected type employer ------
    top_sample <- reactive({ 
        req(input$n_samp) # ensure availablity of value before proceeding
        sample_n(top_subset(), input$n_samp)
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
                 title = pretty_plot_title()) + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    output$barplot <- renderPlot({
        ggplot(data = top_sample(), aes_string(x = input$x, y = input$y,
                                               color = input$z)) +
            geom_bar(stat='identity', position='stack') +
            labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
                 y = toTitleCase(str_replace_all(input$y, "_", " ")),
                 color = toTitleCase(str_replace_all(input$z, "_", " ")),
                 title = pretty_plot_title()) + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    output$lollipop <- renderPlot({ ggplot(top.employers) +
        geom_segment( aes(x=Emp.Jan.20, xend=Emp.Jan.20, y=Emp.Jan.21, yend=Emp.Jan.21), color="grey") +
        geom_point( aes(x=Emp.Jan.20, y=Emp.Jan.21), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
        geom_point( aes(x=Emp.Jan.20, y=Emp.Jan.21), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
        coord_flip()+
        theme_ipsum() +
        xlab("") +
        ylab("Value of Employer")
    })
    
    
    ## This plot ended up being illegible, so I opted for the lollipop
    
    output$circleplot <- renderPlot({
        
        packing <- circleProgressiveLayout(top.employers$Emp.Volume.Change, sizetype='area')
        
        data <- cbind(top.employers, packing)
        
        dat.gg <- circleLayoutVertices(packing, npoints=10)
        
        ggplot() + 
        
        # Make the bubbles
        geom_polygon(data = dat.gg, aes(input$x, input$y, fill=as.factor(input$y)), colour = "black", alpha = 0.6) +
        
        # Add text in the center of each bubble + control its size
        geom_text(data = data, aes(x, y, size=top.employers$Emp.Volume.Change, label=input$y)) +
        scale_size_continuous(range = c(1,4)) +
        
        # General theme:
        theme_void() + 
        theme(legend.position="none") +
        coord_equal()
    })
    
    #output$circleplot <- renderPlot({
    #    ggplot(data = top_sample(), aes_string(x = input$x, y = input$y,
    #                                           color = input$z)) +
    #        geom_bar(stat='identity', position='stack') +
    #        labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
    #             y = toTitleCase(str_replace_all(input$y, "_", " ")),
    #             color = toTitleCase(str_replace_all(input$z, "_", " ")),
    #             title = pretty_plot_title()
    #        ) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    #})
    
    # Print number of employment plotted ----------------------------------
    output$n <- renderUI({
        types <- top_sample()$Employer %>% 
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
    
    output$downloadTest <- downloadHandler(
        filename <- function(){
            paste("topemployers.RData")
        },
        
        content = function(file) {
            save(top.employers, file = file)
        }
    )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)
