
#*****************************************************
# Function for plotting ed visits: historical and projection
# 2019-09-23
# Nayef 

#*****************************************************

# plotting function: 
plot_ed_projection <- function(df, 
                        subset1 = NULL, 
                        subset2 = NULL, 
                        site = "RHS"){
  # arguments: 
  # > df: df with cols: year, metric, value. Metric has 3 levels: ed_visits, fit_lm, and upr_lm 
  # > subset1: optional, used to specify which subset of population we're 
  #   focusing on. E.g. "65-69 years old"
  # > subset2: optional, used to add a further level of segmentation; e.g. 
  #   "CTAS 1" 
  # > site: used in plot title
  
  # returns ggplot with trend line
  
  min_year <- df$year %>% min
  max_year <- df$year %>% max
  
  df %>% 
    ggplot(aes(x = year, 
               y = value, 
               group = metric, 
               col = metric)) + 
    geom_line() + 
    geom_point() + 
    
    scale_y_continuous(limits = c(0, 6000), 
                       breaks = seq(0, 6000, 500)) +
    scale_color_manual(values = c("black", 
                                  "dodgerblue", 
                                  "firebrick")) + 
    
    labs(title = sprintf("%s: ED visits, historical and projected", 
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
# First run the following script until definition of `df11.pivoted`: 
# "src/2019-09-17_RH_Emergency-ED-Projections-Planning.R"

# df11.pivoted$data[[58]] %>% plot_ed_projection(subset1 = "<1", subset2 = "1 - Resuscitation")
