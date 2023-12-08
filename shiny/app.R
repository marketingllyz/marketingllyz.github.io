#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggthemes)
library(scales)
library(cowplot)
library(gtsummary)

data <- read_csv("marketing_campaign.csv")

data <- data %>%  dplyr::select(-ID, -Z_CostContact,  -Z_Revenue, -NumCatalogPurchases, -NumDealsPurchases)

data <- data %>%
  mutate(Total_amount = MntWines +  MntFruits + MntMeatProducts +  MntFishProducts +
           MntSweetProducts + MntGoldProds,
         Prefer_web_or_store = ifelse(NumStorePurchases > NumWebPurchases, "store", "web"),
         Accept_camps = AcceptedCmp1 +  AcceptedCmp2 +  AcceptedCmp3 +  AcceptedCmp4 +
           AcceptedCmp5 + Response,
         Age = as.numeric(substr(Dt_Customer,7,10)) - Year_Birth,
  ) %>% rename("WebVisits" = "NumWebVisitsMonth")


data <- data %>% dplyr::select(-MntWines, -MntFruits, -MntMeatProducts, 
                               -MntFishProducts, -MntSweetProducts, -MntGoldProds,
                               -AcceptedCmp1,-AcceptedCmp2,-AcceptedCmp3,
                               -AcceptedCmp4,-AcceptedCmp5 ,-Response,
                               -Year_Birth, -Dt_Customer,
                               -NumWebPurchases, -NumStorePurchases) %>% drop_na()

data$Marital_Status[data$Marital_Status %in% c("Absurd", "Alone","YOLO","Widow")] =  "Other"

data  %>%
  tbl_summary(by = Prefer_web_or_store, type = list(Accept_camps ~ "continuous")) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Prefer web or store**") %>%
  modify_footnote(all_stat_cols() ~ "Median (IQR) or Frequency (%)") %>%
  modify_caption("**Data descriptive statistics**") %>%
  bold_labels()

d <- data %>% 
  group_by(Prefer_web_or_store,Education ) %>% 
  count() %>%
  group_by(Education  ) %>% 
  mutate(pct= n/sum(n))
g1 <- ggplot(d, aes(Education  , pct, fill =  Prefer_web_or_store)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_economist_white() +
  labs(x = "Education level",
       y = "Percentage",
       fill = "Prefer web or store") +
  scale_fill_brewer(palette = "Pastel1")

d <- data %>% 
  group_by(Prefer_web_or_store,Marital_Status ) %>% 
  count() %>%
  group_by(Marital_Status) %>% 
  mutate(pct= n/sum(n))
g2 <- ggplot(d, aes(Marital_Status , pct, fill =  Prefer_web_or_store)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_economist_white() +
  labs(x = "Marital status",
       y = "Percentage",
       fill = "Prefer web or store") +
  scale_fill_brewer(palette = "Pastel1")

d <- data %>% 
  group_by(Prefer_web_or_store,Kidhome ) %>% 
  count() %>%
  group_by(Kidhome) %>% 
  mutate(pct= n/sum(n))
g3 <- ggplot(d, aes(factor(Kidhome) , pct, fill =  Prefer_web_or_store)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_economist_white() +
  labs(x = "Number of small children",
       y = "Percentage",
       fill = "Prefer web or store")+
  scale_fill_brewer(palette = "Pastel1")

d <- data %>% 
  group_by(Prefer_web_or_store,Teenhome ) %>% 
  count() %>%
  group_by(Teenhome) %>% 
  mutate(pct= n/sum(n))
g4 <- ggplot(d, aes(factor(Teenhome) , pct, fill =  Prefer_web_or_store)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_economist_white() +
  labs(x = "Number of teenagers",
       y = "Percentage",
       fill = "Prefer web or store")+
  scale_fill_brewer(palette = "Pastel1")

d <- data %>% 
  group_by(Prefer_web_or_store,Complain ) %>% 
  count() %>%
  group_by(Complain ) %>% 
  mutate(pct= n/sum(n))
g5 <- ggplot(d, aes(factor(Complain ) , pct, fill =  Prefer_web_or_store)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_economist_white() +
  labs(x = "Complain or not",
       y = "Percentage",
       fill = "Prefer web or store")+
  scale_fill_brewer(palette = "Pastel1")

d <- data %>% 
  group_by(Prefer_web_or_store,Accept_camps ) %>% 
  count() %>%
  group_by(Accept_camps ) %>% 
  mutate(pct= n/sum(n))
g6 <- ggplot(d, aes(factor(Accept_camps ) , pct, fill =  Prefer_web_or_store)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_economist_white() +
  labs(x = "Accept number of campaigns",
       y = "Percentage",
       fill = "Prefer web or store")+
  scale_fill_brewer(palette = "Pastel1")

# Assuming 'data' is your dataset and you have already computed the plots g1, g2, g3, g4, g5, g6
# Create a list to store the ggplot objects
plots_list <- list("Education level" = g1,
                   "Marital status" = g2,
                   "Number of small children" = g3,
                   "Number of teenagers" = g4,
                   "Complain or not" = g5,
                   "Accept number of campaigns" = g6)

# UI
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("plotType", "Choose a factor:",
                   choices = names(plots_list))
    ),
    mainPanel(
      plotOutput("selectedPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$selectedPlot <- renderPlot({
    # Render the plot based on the selection from the radio buttons
    plots_list[[input$plotType]]
  })
}

# Run the application
shinyApp(ui = ui, server = server)

