library(shiny)
library(dplyr)
library(easyalluvial)
library(ggplot2)
library(tibble)

# -------------------------------------------------------------------------

# GENERATING DATA IN WIDE FORMAT

set.seed(123)
dat <-
    tibble(
        SubID = rep(1:250, each = 4),
        Baseline = sample(as.factor(0:4), 1000, replace = T),
        `24Hr` = sample(as.factor(0:4), 1000, replace = T),
        `Day 7-10` = sample(as.factor(0:4), 1000, replace = T),
        `Day 90` = sample(as.factor(0:4), 1000, replace = T)
    )

# -------------------------------------------------------------------------

# FUNCTION TO BE USED ON SERVER SIDE

placement <- function(Time) {
    
    v <- vector()
    
    for (i in 1:length(Time)) {
        
        if ( i == 1) {
            
            v[i] <-  (Time[i] / 2)
        }
        
        else {
            
            v[i] <-  ((Time[i] / 2) + sum(Time[1:(i - 1)]))
        }
        
    }
    
    return(v)
    
}


# -------------------------------------------------------------------------

ui <- fluidPage(

    pageWithSidebar(
        
        headerPanel('Alluvial Plot'),
        
        sidebarPanel(
            sliderInput('t1', 'Baseline', min = 0, max = 4, c(0,4), step = 1),
            sliderInput('t2', '24Hr',     min = 0, max = 4, c(0,4), step = 1),
            sliderInput('t3', 'Day 7-10', min = 0, max = 4, c(0,4), step = 1),
            sliderInput('t4', 'Day 90',   min = 0, max = 4, c(0,4), step = 1),
            submitButton("Submit")
        ),

        mainPanel(
           plotOutput("plot")
        )
    )
)


# -------------------------------------------------------------------------

server <- function(input, output, session) {

    output$plot <- renderPlot({
        
        dat <- 
        dat[,2:5] %>% filter(Baseline   %in% c(input$t1[1]:input$t1[2]),
                             `24Hr`     %in% c(input$t2[1]:input$t2[2]),
                             `Day 7-10` %in% c(input$t3[1]:input$t3[2]),
                             `Day 90`   %in% c(input$t4[1]:input$t4[2]))
        
        
         Baseline <-  xtabs(formula = ~  Baseline,  data = dat)
         Baseline <-  Baseline[Baseline != 0]
        
        `24Hr` <-     xtabs(formula = ~ `24Hr`,     data = dat)
        `24Hr` <-     `24Hr`[`24Hr` != 0]
        
        `Day 7-10` <- xtabs(formula = ~ `Day 7-10`, data = dat)
        `Day 7-10` <- `Day 7-10`[`Day 7-10` != 0]
        
        `Day 90` <-   xtabs(formula = ~ `Day 90`,   data = dat)
        `Day 90` <-   `Day 90`[`Day 90` != 0]
        
        y_Baseline = placement(Baseline)
        y_24Hr =     placement(`24Hr`)
        y_Day710 =   placement(`Day 7-10`)
        y_Day90 =    placement(`Day 90`)
        
        
        alluvial_wide(data = dat, 
                      colorful_fill_variable_stratum = FALSE,
                      order_levels = c('Baseline',
                                           '24Hr',
                                           'Day 7-10',
                                           'Day 90'),

                      col_vector_flow = c('dodgerblue3',
                                              'forestgreen',
                                              'darkorchid',
                                              'darkorange',
                                              'red3'),

                      col_vector_value = c('dodgerblue3',
                                               'forestgreen',
                                               'darkorchid',
                                               'darkorange',
                                               'red3'),
                      stratum_labels = FALSE,
                      auto_rotate_xlabs = FALSE) + 
            
            annotate(geom = "text",
                     y = y_Baseline,
                     x = "Baseline",
                     label = Baseline) +
            annotate(geom = "text",
                     y = y_24Hr,
                     x = "24Hr",
                     label = `24Hr`) +
            annotate(geom = "text",
                     y = y_Day710,
                     x = "Day 7-10",
                     label = `Day 7-10`) +
            annotate(geom = "text",
                     y = y_Day90,
                     x = "Day 90",
                     label = `Day 90`) +
            
            theme_minimal() +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  text = element_text(face = "bold"))
        
            
    })
    
}

shinyApp(ui = ui, server = server)
