

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
and the contour lines indicate mortality rate contours"),br(),
  helpText("If you want to use the countries in the main manuscript, use the following selection:
Belgium (1841-2012);  Switzerland (1876-2011); Czech Republic (1950-2011); 
East Germany (1956-2011); West Germany (1956-2011); Denmark (1835-2011); 
Spain (1908-2012); Estonia (1959-2011); France (Total) (1816-2012);
Northern Ireland (1922-2011); Scotland (1855-2011); England and Wales (Total) (1841-2011);
Lithuania (1959-2011); Latvia (1959-2011)"),
  hr(),
  fluidRow(
    column(
      4,
      h3("Select group of countries to combine"),
      selectInput(
        "country_group_selection", "select a country",
        choices=c(country_info$labels),
        selected="",
        multiple=TRUE
      )  

    ),
    column(
      4,
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
        value=c(20, 50), step=1
      )      
    ),
    column(
      3,
      actionButton(
        "country_group_selection_compile",
        label="Click to run with country selection"
      )
      )
  ),
  hr(),
  tableOutput("table01"),
  plotOutput("plot_scp_mort", height="100%"),
  plotOutput("plot_scp_ppr", height="100%")
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
