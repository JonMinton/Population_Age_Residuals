

country_codes <- read.csv("data/country_codes__new.csv", stringsAsFactors=F)
hmd_years <- read.csv("data/hmd_years.csv")

country_info <- country_codes %>% left_join(hmd_years, by=c("short"="country"))
country_info$labels <- with(country_info,
  paste0(
    long, " (", min_year, "-", max_year, ")"
  )
)


shinyUI(fluidPage( 
  titlePanel("Population Residuals Explorer"),br(),
  hr(),
  h2("Information and instructions"),
  helpText("Please selection a group of countries to combine. The estimated age and year specific mortality rates,
and the population residuals, for this group of countries will then be calculated and presented both individually, 
as shaded contour plots, and also together, as a composite plot, in which the shade indicates population residual,
and the contour lines indicate mortality rate contours"),
  hr(),
  fluidRow(
    column(
      7,
      h3("Select group of countries to combine"),
      selectInput(
        "country_group_selection", "select a country",
        choices=c("", country_info$labels),
        selected="",
        multiple=TRUE
      ),  
      actionButton(
        "country_group_selection_compile",
        label="Click to run with country selection"
        )
    ),
    column(
      7,
      h3("Select range"),
      sliderInput(
        inputId = "year_range", 
        label = "select range of years",
        min=1900, max=2011,
        value=c(1970, 2011), step=1,
        format="####"
      ),
      sliderInput(
        inputId = "age_range",
        label = "select range of ages",
        min=0, max=100,
        value=c(20, 60), step=1
      )      
    )
  ),
  hr(),
  tableOutput("table01")
#   h2(textOutput("title_scp_mort")),
#   plotOutput("plot_scp_mort", height="100%"),
#   br(),
#   hr(),
#   h2(textOutput("title_scp_ppr")),
#   plotOutput("plot_scp_ppr", height="100%"),
#   br(),
#   hr(),
#   hr(textOutput("title_composite")),
#   plotOutput("plot_composite", height="100%")
  
))
