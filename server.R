# server.R
library(shiny)
library(JMbayes2)
library(dplyr)
library(ggplot2)
library(survival)
library(broom)

# Prepare data
pbc_data <- JMbayes2::pbc2 %>%
  filter(!is.na(serBilir)) %>%
  distinct(id, .keep_all = TRUE)  # one row per patient

# Server
server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    ggplot(pbc_data, aes(x = age)) +
      geom_histogram(fill = "steelblue", bins = 20, color = "white") +
      theme_minimal() +
      labs(title = "Age Distribution", x = "Age", y = "Count")
  })
  
  output$plot2 <- renderSankeyNetwork({
    sankey_df <- pbc_data %>%
      count(sex, drug) %>%
      filter(!is.na(sex), !is.na(drug))
    
    nodes <- data.frame(name = unique(c(sankey_df$sex, sankey_df$drug)))
    
    links <- sankey_df %>%
      mutate(source = match(sex, nodes$name) - 1,
             target = match(drug, nodes$name) - 1,
             value = n) %>%
      select(source, target, value)
    
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target", Value = "value",
                  NodeID = "name", fontSize = 12, nodeWidth = 30)
  })
  
  output$plot3 <- renderPlotly({
    surv_data <- pbc_data %>%
      mutate(event = ifelse(status == "alive", 0, 1)) %>%
      filter(!is.na(sex))
    
    fit <- survfit(Surv(years, event) ~ sex, data = surv_data)
    
    tidy_fit <- tidy(fit)
    
    gg <- ggplot(tidy_fit, aes(x = time, y = estimate, color = strata)) +
      geom_line(size = 1.2) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata),
                  alpha = 0.2, color = NA) +
      labs(
        title = "Kaplan-Meier Survival Curves by Sex",
        x = "Time (Years)",
        y = "Survival Probability",
        color = "Sex",
        fill = "Sex"
      ) +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  output$plot4 <- renderPlotly({
    gg <- ggplot(pbc_data, aes(x = serBilir, color = sex, fill = sex)) +
      geom_density(alpha = 0.4) +
      theme_minimal() +
      labs(title = "Density of Serum Bilirubin by Sex",
           x = "Serum Bilirubin",
           y = "Density") +
      theme(legend.title = element_blank())
    
    ggplotly(gg)
  })
}
