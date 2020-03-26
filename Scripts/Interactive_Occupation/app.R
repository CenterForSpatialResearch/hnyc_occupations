### 1. Set Up

library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(extrafont)
library(extrafontdb)
loadfonts()


df = readr::read_csv("../../data/combined.csv")
df = df %>% mutate(sex = factor(sex),
                   race = factor(race),
                   city = factor(city),
                   occ = factor(occ))
options(scipen = 999)
options(shiny.plot.res = 192)
ref_pop_df = data.frame(year = c(1850, 1880, 1910),
                        perc_pop = c(654589, 1805898, 3967191),
                        perc_lab = c(185032, 705728, 1822234))

### 2. UI
ui <- fluidPage(
  
  # Theme
  theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel("HNYC Occupation Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year", label = "Select year:",
                  choices = c(1850, 1880, 1910)),
      
      br(),
      
      sliderInput(inputId = "n_occupations", 
                  label = "Number of Occupations shown:",
                  min = 0,
                  max = 25,
                  value = 10,
                  step = 1),
      
      br(),
      
      selectInput(inputId = "facet", label = "Facet options:",
                  choices = c("None", 
                              "Sex" = "sex", 
                              "Race" = "race", 
                              "City" = "city")),
      
      br(),
      
      selectInput(inputId = "label", label = "Text options:",
                  choices = c("None",
                              "Count",
                              "Percentage of population" = "perc_pop",
                              "Percentage of labor force" = "perc_lab")),
      
      br(),
      
      actionButton(inputId = "save", label = "Save plot to computer")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      plotOutput(outputId = "plot")
      
    )
  )
)

### Server
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    ## If else for if the graph needs to be faceted
    if (input$facet == "None") {
      
      plot = df_filtered() %>%
        ggplot(aes(y = count, x = reorder(occ, count))) + 
        geom_col(fill = graph_colour()) +
        labs(title = paste0("Graph of Occupation Counts of Top ", 
                            input$n_occupations," Occupations in ", 
                            input$year))
      
    } else {

      plot = df_filtered() %>%
        ggplot(aes(y = count, x = reorder(occ, count), fill = facet_var)) + 
        geom_col() +
        labs(title = paste0("Graph of Occupation Counts of Top ", 
                            input$n_occupations," Occupations in ", 
                            input$year, " by " , input$facet)) +
        facet_wrap(~facet_var) +
        scale_fill_brewer(palette = "Set3")
    }
    
    ## If else for if text labels need to be added
    if (input$label != "None") {
      
      plot = plot + scale_y_continuous(expand = expand_scale(mult = c(0,0.2)))
      
      if (input$label == "Count") {
        
        plot = plot + geom_text(aes(label = count), 
                                hjust = -0.1)
        
      } else {
        
        divisor = ref_pop_df %>% 
          filter(year == input$year) %>%
          .[input$label] %>%
          as.numeric()
        
        plot = plot + geom_text(aes(label = paste0(round(count / divisor * 100,
                                                         digits = 2), 
                                                   "%")),
                                hjust = -0.1)
      }
    }
    
    plot = plot + 
      coord_flip() + 
      labs(y = "Count", x = "Occupation") + 
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "none",
            text = element_text(family = "Quicksand"))
    
    plot
    
  })
  
  df_filtered = reactive({
    
    if (input$facet == "None") {
      
      df %>% filter(year == input$year) %>%
        group_by(occ) %>%
        summarise(count = n()) %>%
        top_n(n = input$n_occupations)
      
    } else {
      
      df %>% 
        filter(year == input$year) %>%
        group_by(occ, get(input$facet)) %>%
        summarise(count = n()) %>%
        select(occ, facet_var = paste("get(input$facet)"), count) %>%
        group_by(facet_var) %>%
        top_n(n = input$n_occupations)
      
    }
  })
  
  graph_colour = reactive({
    
    # Default colour palette for when no facetting is used
    case_when(
      input$year == 1850 ~ "#ffa600",
      input$year == 1880 ~ "#bc5090",
      input$year == 1910 ~ "#003f5c"
    )
  })
  
  count = 1
  
  observe({
    
    if (input$save) {
      ggsave(filename = paste0("image", count,".png"))
      count = count + 1aaaaaaaaa
    }
    
    
    
  })
}

### App
shinyApp(ui = ui, server = server)

