
#*****************************************************
# Function for plotting Pop-vs-Year for particular population segments 
# 2019-09-18
# Nayef 

#*****************************************************

# plotting function: 
plot_trend <- function(df, 
                       subset = NULL){
  # arguments: 
  # df: df with cols: year, pop
  # subset: optional, used to specify which subset of population we're 
  #   focusing on. E.g. "65-69 years old"
  
  # returns ggplot with trend line
  
  df %>% 
    ggplot(aes(x = year, 
               y = pop)) + 
    geom_point() + 
    geom_line() + 
    scale_y_continuous(limits = c(0, 500000), 
                       breaks = seq(0, 500000, 50000), 
                       labels = sprintf("%i K", seq(0, 500, 50))) + 
    labs(title = sprintf("%s : Population growth for all of BC", 
                         subset), 
         caption = "Data source: PEOPLE 2018, BC Stats \nRetrieved from: [DSSI].[dbo].[PEOPLE2018Complete]", 
         subtitle = "2010 to 2036") + 
    theme_light() +
    theme(panel.grid.minor = element_line(colour = "grey95"), 
          panel.grid.major = element_line(colour = "grey95"))
  
  
}


# test function: 
# First run the following script until definition of `df3.pop_nested`: 
# "src/2019-09-17_RH_Emergency-ED-Projections-Planning.R"

# df3.pop_nested$data[[1]] %>% plot_trend()
