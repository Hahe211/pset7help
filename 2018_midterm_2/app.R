library(tidyverse)
library(dplyr)
library(shiny)
library(fs)
library(reshape2)

forecast <- read_rds("forecast.rds")
results <- read_csv("mt_2_results.csv")

forecast <- forecast %>%
  # ignore senate and gubernatorial polls
  filter(!(str_detect(source, "sen")), !(str_detect(source, "gov"))) %>%
  # pick out the latter part of the source name
  mutate(source = sapply(strsplit(source, "elections-poll-"), `[`, 2)) %>%
  # pick out house districts
  mutate(source = substr(source, 1, 4)) %>%
  # change the format of the House district
  mutate(district = paste(toupper(substr(source, 1, 2)), substr(source, 3,5), sep = "-"))

results <- results %>%
  mutate(district = paste(state, district, sep = "-")) %>%
  # calculate Republican advantage
  mutate(total = dem_votes + rep_votes + other_votes, rep_adv = (rep_votes-dem_votes)/total) %>%
  select(district, rep_adv) %>%
  # only choose rows that have matching districts with poll data
  filter(district %in% forecast$district)

young <- forecast %>%
  filter(ager != "[DO NOT READ] Don't know/Refused", ager != "[DO NOT READ] Refused") %>%
  group_by(district, ager) %>%
  count() %>%
  spread(ager, n, 0) %>%
  mutate(total = `18 to 34` + `35 to 49` + `50 to 64` + `65 and older`,
         young = `18 to 34` / total) %>%
  select(district, young)

college <- forecast %>%
  filter(educ != "[DO NOT READ] Don't know/Refused", educ != "[DO NOT READ] Refused") %>%
  group_by(district, educ) %>%
  count() %>%
  spread(educ, n, 0) %>%
  mutate(total = `Grade school` + `High school` + `Some college or trade school` + `Bachelors\' degree` + `Graduate or Professional Degree`,
         college = (`Bachelors\' degree` + `Graduate or Professional Degree`) / total) %>%
  select(district, college)

nonwhite <- forecast %>%
  filter(!is.na(race_eth2), race_eth2 != "[DO NOT READ] Don't know/Refused") %>%
  group_by(district, race_eth2) %>%
  count() %>%
  spread(race_eth2, n, 0) %>%
  mutate(total = Nonwhite + White, nonwhite = Nonwhite / total) %>%
  select(district, nonwhite)

likely <- forecast %>%
  filter(likely != "[DO NOT READ] Don't know/Refused") %>%
  group_by(district, likely) %>%
  count() %>%
  spread(likely, n, 0) %>%
  mutate(total = `Already voted` + `Almost certain` + `Very likely` + `Somewhat likely` + `Not very likely` + `Not at all likely`,
         likely = (`Almost certain` + `Very likely` + `Somewhat likely`) / total) %>%
  select(district, likely)

table <- forecast %>%
  group_by(district, response) %>%
  count() %>%
  spread(response, n, 0) %>%
  # calculate Republican advantage
  mutate(total = Dem + Rep + Und, rep_adv = (Rep-Dem)/total) %>%
  select(district, rep_adv) %>%
  # merge poll and actual data frames
  merge(results, by="district") %>%
  rename(poll = rep_adv.x, actual = rep_adv.y) %>%
  mutate(accuracy = 1 - abs(poll-actual)) %>%
  merge(young, by="district") %>%
  merge(college, by="district") %>%
  merge(nonwhite, by="district") %>%
  merge(likely, by="district")
  

# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Sample Demographic and Poll Accuracy"),
  
  # Sidebar with a select input
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "category",
                  label = "Select Demographic",
                  choices = c("18 to 34" = "young", 
                              "College Educated" = "college", 
                              "Nonwhite" = "nonwhite",
                              "Likely to Vote" = "likely"))
    ),
    
    # Show a plot
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    
    table <- table %>%
      select(input$category, accuracy)
    
    table %>%
      ggplot(aes(x = table[,1], y = accuracy)) +
      # create scatterplot
      geom_point() +
      # add title, subtitle
      labs(title = case_when(input$category == "young" ~ "Poll Accuracy by Share of Age 18-34 in Sample",
                             input$category == "college" ~ "Poll Accuracy by Share of College Graduates in Sample",
                             input$category == "nonwhite" ~ "Poll Accuracy by Share of Nonwhite Voters in Sample",
                             input$category == "likely" ~ "Poll Accuracy by Share of Likely to Vote Voters in Sample"),
           x = case_when(input$category == "young" ~ "Percentage of Age 18-34 Voters",
                         input$category == "college" ~ "Percentage of College Graduates",
                         input$category == "nonwhite" ~ "Percentage of Nonwhite Voters",
                         input$category == "likely" ~ "Percentage of Likely to Vote Voters"),
           y = "Poll Accuracy")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

