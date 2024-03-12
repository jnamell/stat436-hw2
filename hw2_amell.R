#jnamell

library(tidyverse)
library(shiny)
library(ggplot2)
library(RColorBrewer)

#download and tidy data
energy <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv') |> 
  select(country, year, contains("_electricity"), -c(per_capita_electricity, fossil_electricity, low_carbon_electricity, 
                                                     renewables_electricity, other_renewable_electricity)) |> #drops aggregate values
  rename('other renewable_electricity'=other_renewable_exc_biofuel_electricity) |> 
  filter(year>=1985) |> #spotty data before then
  pivot_longer(cols=-c(country, year), names_to=c("type", NA), names_sep = "_elec", values_to="power_in_TWh") |> 
  drop_na() |> 
  filter(power_in_TWh>0)

ui <- fluidPage(
  titlePanel("Sources of Power"),
  sidebarLayout(
    sidebarPanel(selectInput("country", "Country/Region", choices=as.list(unique(energy$country))),
                 selectInput("year", "Year", choices=1985:2020), 
                 width=2),
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("60%", "40%"),
                    plotOutput("over_time_plot"), 
                    plotOutput("year_breakdown_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  colors = brewer.pal(9, "Set1") #standard color palette for all plots
  names(colors) = levels(factor(energy$type))
  
  observe({ #only show years with data for selected country
    updateSelectInput(inputId = "year", choices = min((energy |> filter(country==input$country))$year):2020)
  })
  
  output$over_time_plot <- renderPlot({
    energy |> filter(country==input$country) |> 
      ggplot(aes(x=year, y=power_in_TWh, color=type))+
      geom_line(key_glyph='rect')+
      geom_vline(xintercept=as.numeric(input$year), linetype="dotted")+
      annotate(x=as.numeric(input$year), y=+Inf, vjust=2, label="Selected Year", geom="label")+
      scale_color_manual("Power Source", values=colors)+
      theme(#removes background and margins
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        legend.margin = unit(0, "null"),
        legend.key.height=unit(1, "cm"))+
      labs(title = "Power Sources over Time", x="Year", y="Power in TWh")
  })
  
  output$year_breakdown_plot <- renderPlot({
    energy |> filter(country==input$country & year==input$year) |> 
      ggplot(aes(x="",y=power_in_TWh, fill=type))+
      geom_bar(stat="identity")+
      coord_polar("y")+
      scale_fill_manual("Power Source", values=colors)+
      theme(#removes axis, background, and margins
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),          
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        legend.margin = unit(0, "null"),
        legend.position = "none")+
      labs(title = "Breakdown of Power Sources for Selected Year", x=NULL, y=NULL)
  })
}
shinyApp(ui, server)