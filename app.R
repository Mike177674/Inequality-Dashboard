library(tidyverse)
library(ggridges)
library(patchwork)
library(shiny)
library(viridis)
library(sf)
library(plotly)
library(DT)
library(ggradar)
library(maps)
library(ggrepel)

income_inequality_processed <- readr::read_csv(
  "https://ourworldindata.org/grapher/gini-coefficient-before-and-after-tax-lis.csv?v=1&csvType=full&useColumnShortNames=true"
) |>
  dplyr::mutate(
    Year = as.integer(Year)
  )

income_inequality_raw <- readr::read_csv(
  "https://ourworldindata.org/grapher/inequality-of-incomes-before-and-after-taxes-and-transfers-scatter.csv?v=1&csvType=full&useColumnShortNames=true"
) |>
  dplyr::mutate(
    Year = as.integer(Year)
  )

regions = income_inequality_raw%>%
  filter(!is.na(owid_region))%>%
  select(Entity, owid_region)
income_inequality_processed = income_inequality_processed%>%
  left_join(regions, by = 'Entity')

income_inequality_processed2 = income_inequality_processed%>%
  mutate(pre_tax_Gini = gini_mi_eq)%>%
  mutate(post_tax_Gini = gini_dhi_eq)%>%
  mutate(Unequality_Decrease = pre_tax_Gini - post_tax_Gini)

gini_data = income_inequality_processed2%>%
  drop_na(Unequality_Decrease)%>%
  group_by(Entity)%>%
  filter(Year == min(Year) | Year == max(Year))%>%
  mutate(time_point = if_else(Year == min(Year), "first", "last"))%>%
  ungroup()%>%
  select(-gini_mi_eq,-gini_dhi_eq,-Unequality_Decrease)%>%
  pivot_wider(names_from = time_point, values_from = c(pre_tax_Gini, post_tax_Gini,Year))


gini_data[9, "pre_tax_Gini_last"] = gini_data[9, "pre_tax_Gini_first"]
gini_data[9, "post_tax_Gini_last"] = gini_data[9, "post_tax_Gini_first"]
gini_data[9, "Year_last"] = gini_data[9, "Year_first"]



#App

line = function(data, averages) {
  ggplot(data%>%filter(Entity!='South Africa' & Entity!='Dominican Republic' & Entity!='Brazil'))+
    geom_segment(aes(x=pre_tax_Gini_first, y = post_tax_Gini_first, xend = pre_tax_Gini_last, yend=post_tax_Gini_last, color = owid_region),
                 arrow = arrow(length = unit(0.3, "cm")),
                 linewidth = 0.5)+
    geom_text_repel(aes(x = pre_tax_Gini_first, y = post_tax_Gini_first, 
                        label = paste0(Entity, " ('", substr(Year_first,3,4), ")")),
                    size = 3,
                    force = 10,
                    max.overlaps = 5)+
    geom_point(aes(x=pre_tax_Gini_first, y = post_tax_Gini_first, color = owid_region), size = .7)+
    geom_point(aes(x=pre_tax_Gini_last, y = post_tax_Gini_last, color = owid_region), size = .7)+
    geom_segment(data = averages, aes(x=pre_tax_Gini_first, y = post_tax_Gini_first, xend = pre_tax_Gini_last, yend=post_tax_Gini_last),
                 arrow = arrow(length = unit(0.4, "cm")),
                 linewidth = 0.6,
                 linetype = "solid")+
    labs(x = "Gini Pre Tax", 
         y = "Gini Post Tax",
         title = "A: Relationship between Gini Coefficient Pre and Post Tax",
         subtitle = "Arrows show trend from first to last available year - (around 2020)",
         shape = "Time Point",
         color = "Region") +
    theme_classic()+
    coord_fixed(ratio = 1)
}

map = function(data, category){
  if(category == 'Pre Tax'){
    cat = data$pre_tax_Gini_last
  }
  if(category == 'Post Tax'){
    cat = data$post_tax_Gini_last
  }
  if(category == 'Difference Caused by Taxation'){
    cat = data$pre_tax_Gini_last - data$post_tax_Gini_last
  }
  plot_geo(data,
           locationmode = 'country names')%>%
    add_trace(locations = ~Entity,
              z = ~cat,
              color = ~cat,
              text = ~paste0(Entity, ", ", Year_last, ", Gini: ", cat),
              hoverinfo = 'text',
              reversescale = T,
              zmin= min(data$cat, na.rm = TRUE),
              zmax= max(data$cat, na.rm = TRUE))%>%
    layout(geo = list(scope = 'world'),
           title = paste0("B: Inequality Map -- ", category," (Last Avaible Year)"))%>%
    config(displayModeBar = FALSE) %>%
    colorbar(title = "Gini Coefficient")
}


ui <- fluidPage(
  titlePanel(
    h1("Inequality and Taxes", align = "center"),
  ),
  textOutput("credit"),
  textOutput("explain"),
  textOutput("total_index"),
  textOutput("space"),
  fluidRow(
    column(width = 3, 
           selectInput("selector", "Select Countires",
                       choices = sort(gini_data$Entity),
                       multiple = TRUE),
           selectInput("mapCat", "Pre or Post Tax (Map)",
                       choices= c('Pre Tax', 'Post Tax', 'Difference Caused by Taxation'),
                       selected = 'Pre Tax', 
                       multiple = FALSE)
    )),
  fluidRow(
    column(width = 12, plotOutput("line")), 
  ),
  fluidRow(
    column(width = 12, plotlyOutput("map")), 
  )
)

server <- function(input, output) {
  gini_data2 <- reactive({
    gini_data%>%
      mutate(selected = (length(input$selector) == 0 | Entity %in% input$selector))%>%
      filter(selected == TRUE)
  })
  
  averages = reactive({
    gini_data2()%>%
      ungroup()%>%
      filter(Entity!='South Africa' & Entity!='Dominican Republic' & Entity!='Brazil')%>%
      summarize(
        pre_tax_Gini_first = mean(pre_tax_Gini_first),
        pre_tax_Gini_last = mean(pre_tax_Gini_last, na.rm = TRUE),
        post_tax_Gini_first = mean(post_tax_Gini_first), 
        post_tax_Gini_last = mean(post_tax_Gini_last, na.rm = TRUE)
      )
  })
  
  output$credit <-renderText(paste0("Source: Our World in Data"))
  
  output$explain <-renderText(paste0("Gini Coefficient is a measure of inequality. Higher values represent higher inequality."))
  
  output$total_index <-renderText(paste0("'Post Tax' refers to inequality after Taxation and Redistribution."))
  
  output$space <-renderText(paste0("______________________________________"))
  
  output$line <-renderPlot({
    line(gini_data2(), averages())
  })
  
  output$map <- renderPlotly({
    map(gini_data2(), input$mapCat)
  })
  
}

shinyApp(ui, server)