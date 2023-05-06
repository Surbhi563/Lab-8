library(ggplot2)
library(shiny)
library(dplyr)
library(plotly)
data(iris)


# Define UI for histogram plot
ui_hist <- fluidPage(
  # Title
  titlePanel("Iris Histograms"),
  
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      # Bin size slider for each property
      sliderInput("sepal_length_bin", "Sepal Length bin size:",
                  min = 0.1, max = 2, value = 0.5),
      sliderInput("sepal_width_bin", "Sepal Width bin size:",
                  min = 0.1, max = 2, value = 0.5),
      sliderInput("petal_length_bin", "Petal Length bin size:",
                  min = 0.1, max = 2, value = 0.5),
      sliderInput("petal_width_bin", "Petal Width bin size:",
                  min = 0.1, max = 2, value = 0.5),
      # Color picker for each property
      selectInput("sepal_length_color", "Sepal Length color:",
                  choices = c("Red", "Blue", "Green", "Yellow")),
      selectInput("sepal_width_color", "Sepal Width color:",
                  choices = c("Red", "Blue", "Green", "Yellow")),
      selectInput("petal_length_color", "Petal Length color:",
                  choices = c("Red", "Blue", "Green", "Yellow")),
      selectInput("petal_width_color", "Petal Width color:",
                  choices = c("Red", "Blue", "Green", "Yellow"))
    ),
    
    # Main panel
    mainPanel(
      # Histogram plots for each property
      plotlyOutput("sepal_length_hist"),
      plotlyOutput("sepal_width_hist"),
      plotlyOutput("petal_length_hist"),
      plotlyOutput("petal_width_hist")
    )
  )
)



# Define UI for scatter plot
ui_scatter <- fluidPage(
  # Title
  titlePanel("Iris Scatter Plot"),
  
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      # X-axis dropdown
      selectInput("x_axis", "X-axis:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
      # Y-axis dropdown
      selectInput("y_axis", "Y-axis:", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
      # Color picker for each species
      selectInput("setosa_color", "Setosa color:",
                  choices = c("#1F77B4", "#FF7F0E", "#2CA02C")),
      selectInput("versicolor_color", "Versicolor color:",
                  choices = c("#1F77B4", "#FF7F0E", "#2CA02C")),
      selectInput("virginica_color", "Virginica color:",
                  choices = c("#1F77B4", "#FF7F0E", "#2CA02C")),
      # Point size and shape for each species
      sliderInput("setosa_size", "Setosa point size:", min = 1, max = 10, value = 5),
      sliderInput("versicolor_size", "Versicolor point size:", min = 1, max = 10, value = 5),
      sliderInput("virginica_size", "Virginica point size:", min = 1, max = 10, value = 5),
      selectInput("setosa_shape", "Setosa point shape:",
                  choices = c("circle", "square", "triangle", "diamond")),
      selectInput("versicolor_shape", "Versicolor point shape:",
                  choices = c("circle", "square", "triangle", "diamond")),
      selectInput("virginica_shape", "Virginica point shape:",
                  choices = c("circle", "square", "triangle", "diamond")),
      # Checkbox group for species to display
      checkboxGroupInput("species_display", "Select species to display:", choices = c("setosa", "versicolor", "virginica"),
                         selected = c("setosa", "versicolor", "virginica"))
    ),
    
    # Main panel
    mainPanel(
      # Scatter plot
      plotlyOutput("scatter_plot")
    )
  )
)


# Define UI for violin/box plot
ui_violin_box <- fluidPage(
  # Title
  titlePanel("Iris Violin/Box Plot"),
  
  # Sidebar panel
  sidebarLayout(
    sidebarPanel(
      # Species dropdown
      # Color picker for each species
      selectInput("setosa_color_violin_box", "Setosa color:",
                  choices = c("#1F77B4", "#FF7F0E", "#2CA02C")),
      selectInput("versicolor_color_violin_box", "Versicolor color:",
                  choices = c("#1F77B4", "#FF7F0E", "#2CA02C")),
      selectInput("virginica_color_violin_box", "Virginica color:",
                  choices = c("#1F77B4", "#FF7F0E", "#2CA02C")),
      # Checkbox for plot type
     radioButtons("plot_type_violin_box", "Select plot type:",
                         choices = c("violin", "box"), selected = "box"),
      # Checkbox group for variables to display
     radioButtons("variables_display", "Select variables to display:",
                         choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                         selected = "Sepal.Length")
    ),
    
    # Main panel
    mainPanel(
      # Violin/Box plot
      plotlyOutput("violin_box_plot")
    )
  )
)



ui <- navbarPage(
  "Iris Data Visualization",
  
  tabPanel("Histogram",
           ui_hist),
  
  tabPanel("Scatter Plot",
           ui_scatter),
  
  tabPanel("Violin/Box Plot",
           ui_violin_box)
)




server <- function(input, output,session) {
  # Histograms
    # Histogram plot for Sepal Length
    output$sepal_length_hist <- renderPlotly({
      ggplot(iris, aes(x = Sepal.Length)) + 
        geom_histogram(binwidth = input$sepal_length_bin, 
                       fill = input$sepal_length_color) +
        labs(title = "Sepal Length Histogram")
    }, 
    # include childScope argument to avoid unused argument error
   )
    
    # Histogram plot for Sepal Width
    output$sepal_width_hist <- renderPlotly({
      ggplot(iris, aes(x = Sepal.Width)) + 
        geom_histogram(binwidth = input$sepal_width_bin, 
                       fill = input$sepal_width_color) +
        labs(title = "Sepal Width Histogram")
    })
    
    # Histogram plot for Petal Length
    # Petal Length histogram
    output$petal_length_hist <- renderPlotly({
      ggplot(iris, aes(x = Petal.Length)) +
        geom_histogram(binwidth = input$petal_length_bin, fill = ifelse(input$petal_length_color == "Blue", "blue", 
                                                                        ifelse(input$petal_length_color == "Red", "red", 
                                                                               ifelse(input$petal_length_color == "Yellow", "yellow", 
                                                                                      "green")))) +
        labs(title = "Petal Length Histogram")
    })
    
    # Petal Width histogram
    output$petal_width_hist <- renderPlotly({
      ggplot(iris, aes(x = Petal.Width)) +
        geom_histogram(binwidth = input$petal_width_bin, fill = ifelse(input$petal_width_color == "Blue", "blue", 
                                                                       ifelse(input$petal_width_color == "Red", "red", 
                                                                              ifelse(input$petal_width_color == "Yellow", "yellow", 
                                                                                     "green"))))  +
        labs(title = "Petal Width Histogram")
    })
    output$violin_box_plot <- renderPlotly({
      # Filter
      data <- iris
      # Plot
      if("violin" %in% input$plot_type_violin_box) {
        ggplot(data, aes(x = Species, y = !!sym(input$variables_display[1]), fill = Species)) +
          geom_violin() +
          labs(title = "Iris Violin/Box Plot") +
          scale_fill_manual(values = c(input$setosa_color_violin_box, input$versicolor_color_violin_box, 
                                       input$virginica_color_violin_box)) +
          coord_flip()
      } else {
        ggplot(data, aes(x = Species, y= !!sym(input$variables_display[1]), fill = Species)) +
          geom_boxplot() +
          labs(title = "Iris Box Plot") +
          scale_fill_manual(values = c(input$setosa_color_violin_box, input$versicolor_color_violin_box,
                                       input$virginica_color_violin_box))
      }
    })
    data <- reactive({
      iris %>% filter(Species %in% input$species_display)
    })
    
    # Scatter plot
    output$scatter_plot <- renderPlotly({
      # Plot
      ggplot(data(), aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis), color = Species)) +
        geom_point(aes(size = ifelse(Species == "setosa", input$setosa_size, 
                                     ifelse(Species == "versicolor", input$versicolor_size, 
                                            input$virginica_size)),
                       shape = ifelse(Species == "setosa", input$setosa_shape, 
                                      ifelse(Species == "versicolor", input$versicolor_shape, 
                                             input$virginica_shape)))) +
        labs(title = "Iris Scatter Plot",
             x = input$x_axis, y = input$y_axis,
             color = "Species", size = "Size", shape = "Shape") +
        scale_color_manual(values = c(input$setosa_color, input$versicolor_color, input$virginica_color)) 
      
    })
  }
  
  
  


# Run the app
shinyApp(ui = ui, server = server)
