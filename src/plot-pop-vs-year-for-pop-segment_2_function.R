#*****************************************************
# Function for plotting Pop-vs-Year for particular population segments 
# 2019-09-19
# Nayef 

#*****************************************************

# plotting function: 
plot_trend2 <- function(df, 
                        subset1 = NULL, 
                        subset2 = NULL, 
                        site = "RHS"){
  # arguments: 
  # > df: df with cols: year, pop, ed_visits 
  # > subset1: optional, used to specify which subset of population we're 
  #   focusing on. E.g. "65-69 years old"
  # > subset2: optional, used to add a further level of segmentation; e.g. 
  #   "CTAS 1" 
  # > site: used in plot title
  
  # returns ggplot with trend line
  
  min_year <- df$year %>% min
  max_year <- df$year %>% max
  
  df %>% 
    ggplot(aes(x = pop, 
               y = ed_visits)) + 
    geom_point() + 
    geom_text(aes(label = year, 
                  size = years_from_2010),
              alpha = 0.2, 
              vjust = "top") + 
    geom_smooth(col = "grey90", 
                se = FALSE, 
                method = "lm") + 
    scale_y_continuous(limits = c(0, 2500), 
                       breaks = seq(0, 2500, 500)) +
    # scale_x_continuous(limits = c(0, 400000)) +
    labs(title = sprintf("%s: ED visits versus BC population", 
                         site), 
         subtitle = sprintf("Age group: %s \nCTAS: %s \n%i to %i", 
                            subset1, 
                            subset2, 
                            min_year, 
                            max_year), 
         caption = "\nData sources: PEOPLE 2018, BC Stats; DSDW EDMart, VCH Decision Support") + 
    theme_light() +
    theme(panel.grid.minor = element_line(colour = "grey95"), 
          panel.grid.major = element_line(colour = "grey95"))
  
  
}


# test function: 
# First run the following script until definition of `df5.nested`: 
# "src/2019-09-17_RH_Emergency-ED-Projections-Planning.R"

# df5.nested$data[[58]] %>% plot_trend2(subset1 = "<1", subset2 = "1 - Resuscitation")
