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
library("bslib")

# shinylive::export("app", "docs")

theme <- bs_theme(
  # Controls the default grayscale palette
  bg = "#fff", fg = "#202123",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#cc0033", secondary = "#48DAC6",
  base_font = c("Grandstander", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "#cc0033"
)

# Set theme for ggplot
param_theme <- function(...) {
  list(
    theme_bw(...), 
    theme(
      panel.grid.major = element_line(linewidth = 0.2),
      panel.grid.minor = element_line(linewidth = 0.2)
    )
  )
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = theme, 
    # Application title
    titlePanel("Exploring parameter space", windowTitle = "Parameter space"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3, 
          withMathJax(),
            p("$$y_i \\sim Normal(\\mu_i, \\sigma)$$"), 
            p("$$\\mu_i = \\alpha + \\beta x_i$$"), 
            br(), 
            sliderInput(
              inputId = "b_0",
              label = "Intercept",
              min = -1.5, max = 1.5, value = 0, step = 0.1, ticks = F), 
            sliderInput(
              inputId = "b_1",
              label = "Slope",
              min = -1.5, max = 1.5, value = 0.5, step = 0.1, ticks = F),
            br(), 
            p(strong("Created by:"), 
              tags$a("Joseph V. Casillas", href="https://www.jvcasillas.com"),
            br(), 
            strong("Source code:"), 
              tags$a("Github", href="https://github.com/jvcasillas/shiny_parameters/"))
        ),

        # Show a plot of the generated distribution
        mainPanel(width = 9, br(), 
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    n <- 25
    x <- rnorm(n, 0, 1)
    
    output$distPlot <- renderPlot({
        # generate data
        dat <- tibble(
          x = x, 
          y = input$b_0 + (x * input$b_1) + rnorm(n, 0, 1)
        )

        # Fit model
        mod <- lm(y ~ x, data = dat)

        # Data space plot
        p1 <- augment(mod) |> 
          ggplot() + 
          aes(x = x, y = y) + 
          geom_vline(xintercept = 0, lty = 3) + 
          geom_hline(yintercept = 0, lty = 3) + 
          geom_point(aes(fill = .resid), pch = 21, size = 4, show.legend = F) + 
          scale_fill_gradient2() + 
          geom_abline(intercept = coef(mod)[1], slope = coef(mod)[2], 
            color = "#cc0033", linewidth = 1.2) + 
          coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) + 
          labs(title = "Data space") + 
          param_theme(base_size = 16)

        # Parameter space plot
        p2 <- ggplot(data = tibble(x = input$b_1, y = input$b_0)) + 
          aes(x = x, y = y) + 
          geom_vline(xintercept = input$b_1, lty = 3) + 
          geom_hline(yintercept = input$b_0, lty = 3) + 
          geom_point(size = 8, color = "#cc0033") + 
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
