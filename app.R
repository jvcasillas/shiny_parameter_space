#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
#

# Load packages
library("shiny")
library("ggplot2")
library("tidyr")
library("dplyr")
library("broom")
library("patchwork")

# Set theme for ggplot
param_theme <- function(...) {
  list(
    theme_bw(...), 
    theme(
      panel.grid.major = element_line(size = 0.2),
      panel.grid.minor = element_line(size = 0.2)
    )
  )
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Parameters"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2, 
            sliderInput(
              inputId = "b_0",
              label = "Intercept",
              min = -2, max = 2, value = 0, step = 0.1, ticks = F), 
          sliderInput(
              inputId = "b_1",
              label = "Slope",
              min = -2, max = 2, value = 1, step = 0.1, ticks = F),
            sliderInput(
              inputId = "sigma",
              label = "Sigma",
              min = 0, max = 4, value = 2, step = 0.1, ticks = F),
            sliderInput(
              inputId = "n",
              label = "N",
              min = 50, max = 500, value = 100, step = 1, ticks = F)
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 10, 
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate data
        dat <- tibble(
          x = rnorm(input$n, 0, 2), 
          y = input$b_0 + x * input$b_1 + rnorm(input$n, 0, input$sigma)
        )

        # Fit model
        mod <- lm(y ~ x, data = dat)

        # Data space plot
        p1 <- dat %>% 
          ggplot(., aes(x = x, y = y)) + 
            geom_vline(xintercept = 0, lty = 3) + 
            geom_hline(yintercept = 0, lty = 3) + 
            geom_point() + 
            geom_abline(intercept = coef(mod)[1], slope = coef(mod)[2], 
              color = "#cc0033", size = 1.2) + 
            coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) + 
            labs(title = "Data space") + 
            param_theme(base_size = 16)

        # Parameter space plot
        p2 <- tidy(mod) %>% 
          select(term, estimate, error = std.error) %>% 
          mutate(
            term = stringr::str_replace(term, "\\(Intercept\\)", "y")) %>% 
          pivot_wider(names_from = "term", values_from = c("estimate", "error")) %>% 
          transmute(
            x = estimate_x, 
            y = estimate_y, 
            xmin = estimate_x - (error_x * 1.96), 
            xmax = estimate_x + (error_x * 1.96), 
            ymin = estimate_y - (error_y * 1.96), 
            ymax = estimate_y + (error_y * 1.96)
            ) %>% 
          ggplot(., aes(x = x, y = y)) + 
            geom_vline(xintercept = 0, lty = 3) + 
            geom_hline(yintercept = 0, lty = 3) + 
            geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1) + 
            geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.1) + 
            geom_point(size = 5, pch = 21, fill = "#cc0033", stroke = 1.2) + 
            coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) + 
            scale_y_continuous(position = "right") + 
            labs(title = "Parameter space", y = "Intercept", x = "Slope") + 
            param_theme(base_size = 16)

        # Print plots together
        p1 + p2
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
