
#'--- 
#' title: "RHS ED projections"
#' author: "Nayef Ahmad"
#' date: "2019-07-18"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     number_sections: true
#'     toc: true
#'     toc_float: 
#'       collapsed: false
#' ---
#' 

#+ lib, include = FALSE 
# 0) libraries: --------------
library(tidyverse)
library(denodoExtractor)
library(DT)

# setup_denodo()
cnx <- DBI::dbConnect(odbc::odbc(), dsn = "cnx_SPDBSCSTA001")

ed_mart <- dplyr::tbl(cnx, dbplyr::in_schema("EDMart.dbo", 
                                             "[vwEDVisitIdentifiedRegional]"))

people_2018 <- dplyr::tbl(cnx, dbplyr::in_schema("DSSI.dbo", 
                                                 "[PEOPLE2018Complete]"))

# functions: 
source(here::here("src", 
                  "plot-pop-vs-year-for-pop-segment_function.R"))
source(here::here("src", 
                  "plot-pop-vs-year-for-pop-segment_2_function.R"))
source(here::here("src", 
                  "plot-predictions_function.R"))

# matching age_group labels: 
df0.age_group_labels <- 
  read_csv(here::here("data", 
                      "2019-09-19_matching-age-group-labels-across-ED-and-population-data.csv"))

# knitr options: 
knitr::opts_chunk$set(warning=FALSE, message=FALSE)





#' # ED visits data 
#+ data 
# 1) ED visits data : ----------------
site <- "RHS"

df1.ed_visits_annual <- 
  ed_mart %>% 
  filter(FacilityShortName == site) %>% 
  select(StartDate, 
         TriageAcuityDescription, 
         Age, 
         PatientID) %>% 
  collect() %>% 
  
  mutate(year = lubridate::year(StartDate), 
         age_group = cut(Age, c(-1, 0, seq(4, 89, 5), 200)),  # 5-yr age buckets
         TriageAcuityDescription = as.factor(TriageAcuityDescription)) %>%
  
  # remove NA and "Invalid":  
  filter(!is.na(Age),
         !TriageAcuityDescription %in% c("Invalid", "8 - Disaster Acuity Only" )) %>% 
  mutate(TriageAcuityDescription = fct_drop(TriageAcuityDescription)) %>% 
  

  # group to year-ctas-age_group level: 
  count(year,
        TriageAcuityDescription, 
        Age, 
        age_group) %>%
  
  rename(ctas = TriageAcuityDescription,
         ed_visits = n)
  

# result: 
# str(df1.ed_visits_annual)
# summary(df1.ed_visits_annual)

#' Note that we remove cases where `Age` = NA (25 rows), and where `age_group` =
#' "Invalid" (255 rows)
#' 
#' We also remove `ctas` levels other than 1 to 5.
#' 
#'  Here's a random sample of 100 rows from the result:  

df1.ed_visits_annual %>% 
  sample_n(100) %>% 
  arrange(year, ctas, Age) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#' 
#' ## Exploratory plots - RHS ED visits 
# > Exploratory plots - RHS ED visits : -------------

# Annual ED Visits by Calendar Year, broken out by CTAS 
df1.ed_visits_annual %>% 
  group_by(year, 
           ctas) %>% 
  summarise(ed_visits = sum(ed_visits)) %>% 
  
  filter(year != "2019") %>% 
  ggplot(aes(x = year, 
             y = ed_visits, 
             group = ctas, 
             col = ctas)) + 
  geom_line(alpha = 0.5) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  labs(title = sprintf("%s Annual ED Visits by Calendar Year", 
                       site)) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# Annual ED Visits by Calendar Year
df1.ed_visits_annual %>% 
  filter(year != "2019") %>% 
  group_by(year) %>% 
  summarise(ed_visits = sum(ed_visits)) %>% 
  
  ggplot(aes(x = year, 
             y = ed_visits)) + 
  geom_line() + 
  geom_point() + 
  labs(title = sprintf("%s Annual ED Visits by Calendar Year", 
                       site)) +  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


#' Looks like it would make sense to look at historical data from 2010 onwards
#' 







#' # BC Population data
#' 
#' See file *`r here::here("src", "src/2019-09-17_rh_historical-ed-visits-data.sql")`*
#'  
#' Peter is using total BC population as the predictor, *not* limiting to
#' `HSDAName` = "Richmond". That's what I'll do as well.

#**********************************************
# 2) BC Population data: --------------

df2.bc_population <- 
  people_2018 %>% 
  filter(Year >= "2010", 
         Year <= "2036") %>% 
  select(Year, 
         AgeGroup, 
         Population) %>% 
  collect() %>% 
  group_by(Year, 
           AgeGroup) %>% 
  summarise(pop = sum(Population)) %>% 
  rename(year = Year, 
         age_group_pop = AgeGroup) %>% 
  as_tibble()

# str(df2.bc_population)

df2.bc_population %>% 
  datatable(extensions = 'Buttons',
          options = list(dom = 'Bfrtip', 
                         buttons = c('excel', "csv")))





#' ## Population growth by age group
#' 
#+ pop-by-age
# 3) Population growth by age group:-------------

# create nested df: 
df3.pop_nested <- 
  df2.bc_population %>% 
  group_by(age_group_pop) %>% 
  nest()


# map plotting function across all age groups: 
df3.pop_nested <- 
  df3.pop_nested %>% 
  mutate(age_group_growth = map2(data, 
                                 age_group_pop, 
                                 plot_trend))

#' Too many graphs to show here. Look in *`r here::here("results", "dst")`*
#' 
#' Here's just one example: 
#' 

# example: 
df3.pop_nested$age_group_growth[[sample(1:20, 1)]]

# save output: 
# pdf(here::here("results",
#                "dst",
#                "2019-09-24_bc_pop-growth-by-age-group.pdf"))
# df3.pop_nested$age_group_growth
# dev.off()


# for use in predictions with regressions, pull out the future populations only:
df3.pop_nested <- 
  df3.pop_nested %>% 
  mutate(pop_projection = map(data, 
                              function(df){
                                df %>% filter(df$year > "2018")
                              }))

# df3.pop_nested$pop_projection[[1]]



#' # Join ED data with population data 
#' 
# 4) Join ED data with population data -----------------

#' First match the `age_group` labels: 
#' 

df1.1.ed_visits <- 
  df1.ed_visits_annual %>% 
  left_join(df0.age_group_labels) 

#' Then join ED with population data: 
#' 

df4.ed_and_pop_data <- 
  df1.1.ed_visits %>% 
  filter(year >= "2010", 
         year != "2019") %>% 
  group_by(year, 
           age_group_pop, 
           ctas) %>% 
  summarise(ed_visits = sum(ed_visits)) %>% 
  
  select(year, 
         age_group_pop, 
         ctas, 
         ed_visits) %>% 
  
  left_join(df2.bc_population) %>% 
  
  arrange(age_group_pop,
          year, 
          ctas) %>% 
  ungroup() %>% 
  
  # prep for regression models: 
  mutate(years_from_2010 = year - 2010) %>% 
  select(year, 
         years_from_2010, 
         everything())

# view result: 
df4.ed_and_pop_data %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


# str(df4.ed_and_pop_data)
# summary(df4.ed_and_pop_data)



#' ## Plots - joined dataset
# > Plots - joined dataset: --------------

#' Distribution of num visit by age segment, in 2018
#' 

# ED visits in 2018, by age group
df4.ed_and_pop_data %>% 
  filter(year == "2018") %>% 
  
  ggplot(aes(x = as.factor(age_group_pop), 
             y = ed_visits)) + 
  geom_boxplot() + 
  facet_wrap(~ctas) + 
  labs(title = sprintf("%s ED visits in 2018, by age group", 
                       site), 
       subtitle = "todo: \"5-9\" age group is in the wrong place") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

# dist of population by age: 
df4.ed_and_pop_data %>% 
  filter(year == "2018") %>% 
  select(age_group_pop, 
         pop) %>% 
  distinct() %>% 
  
  ggplot(aes(x = as.factor(age_group_pop), 
             y = pop)) + 
  geom_col(fill = "steelblue4") + 
  scale_y_continuous(limits = c(0, 500000), 
                     breaks = seq(0, 500000, 50000), 
                     labels = sprintf("%i K", seq(0, 500, 50))) + 
  labs(title = "\"Shape\" of the BC population in 2018, by age group", 
       subtitle = "todo: \"5-9\" age group is in the wrong place") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


#' High-level view of `ed_visits` vs `pop`, across all segments: 
#' 

df4.ed_and_pop_data %>% 
  filter(year == "2018") %>% 
  ggplot(aes(x = pop, 
             y = ed_visits, 
             )) + 
  geom_jitter()
  
#' Not sure that there's much to take away from that.   
  
  


#+ nest-data
#' ## Nested dataset 
# > Nested dataset: ----------

df5.nested <- 
  df4.ed_and_pop_data %>% 
  group_by(age_group_pop, 
           ctas) %>% 
  nest()
  
# df5.nested  
# df5.nested$data[[1]]
# df5.nested$data[[2]]


#' ## Plots - ED visits-vs-pop
#'
#' Let's examine whether it is reasonable to fit a linear trend to the
#' ed_visits-vs-pop historical data. 
#' 

# > Plots - ED visits-vs-pop ------------

df5.nested <- 
  df5.nested %>% 
  mutate(ed_vs_pop = pmap(list(df = data, 
                               subset1 = age_group_pop, 
                               subset2 = ctas, 
                               site = "RHS"), 
                          plot_trend2))
# df5.nested
# df5.nested$ed_vs_pop[64]
  
#' Too many graphs to show here. Look in *`r here::here("results", "dst")`*
#' 
#' Here's just one example: 
#' 

# example: 
df5.nested$ed_vs_pop[[sample(1:100, 1)]]


# save output: 
# pdf(here::here("results",
#                "dst",
#                "2019-09-24_rhs_ed-visits-vs-pop-segmented-by-age-and-ctas.pdf"))
# df5.nested$ed_vs_pop
# dev.off()


#' # Model fitting 
#' 
#' We'll fit two models for `ed_visits` vs `pop`:
#' 
#' 
#' 1. OLS - that is, standard linear regression.
#'  
#' 2. Robust regression with `MASS::rlm()`. In rlm, the Huber fn uses squared 
#' residuals when they are "small", and the simple difference between observed 
#' and fitted values when the residuals are above a threshold.^[See **Applied Predictive Modeling** by Max Kuhn, p109]


# 5) Model fitting: ----------

# OLS regression 
segment_model <- function(df){
  lm(ed_visits ~ pop, 
     data = df)
}

# Robust regression
segment_model_rlm <- function(df){
  MASS::rlm(ed_visits ~ pop, 
            data = df)
}


# fit the models: 
df6.models <- 
  df5.nested %>% 
  mutate(model = map(data, segment_model), 
         model_rlm = map(data, segment_model_rlm))

# df6.models
segment <- sample(1:100, 1)  # random selection

# df6.models$model[[segment]] %>% summary
# df6.models$model_rlm[[segment]] %>% summary

#' 
#' ## Extracting estimates
# > extracting estimates: -----------
df6.models <- 
  df6.models %>% 
  mutate(tidy = map(model, broom::tidy), 
         glance = map(model, broom::glance), 
         
         # let's specifically extract r squared: 
         rsq = glance %>% map_dbl("r.squared"),  
         # map( ) can be used to extract all objects with a certain name, here "r.squared"
         
         # same results for rlm: 
         tidy_rlm = map(model_rlm, broom::tidy), 
         glance_rlm = map(model_rlm, broom::glance), 
         
         # get AIC specifically: 
         aic = glance_rlm %>% map_dbl("AIC")
         
         
  )


# df6.models
# df6.models %>% View()
# df6.models %>% unnest(glance)
# df6.models %>% unnest(glance_rlm)
# df6.models %>% unnest(tidy)
# df6.models %>% unnest(tidy_rlm)

#' ### OLS estimates 
df6.models %>% 
  unnest(tidy) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(3:99)

#' ### Robust regression estimates 
df6.models %>% 
  unnest(tidy_rlm) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(3:8)

#' Results from OLS and robust regression seem very close. That's a good thing.
#' 
#' Todo: We'll compare the two models later for 2018 data to see which seems to do
#' better

#'
#' ## Comparing models
# > comparing models: ----------

# OLS regression
df6.models %>% 
  unnest(tidy) %>% 
  select(term, 
         estimate, 
         age_group_pop, 
         ctas, 
         rsq) %>% 
  spread(term, estimate) %>% 
  
  ggplot(aes(x = pop, 
             y = `(Intercept)`, 
             col = ctas, 
             size = rsq)) + 
  geom_point(alpha = .5) + 
  geom_vline(xintercept = 0, col = "grey") + 
  geom_hline(yintercept = 0, col = "grey") + 
  
  facet_wrap(~ctas) +
  
  scale_color_brewer(type = "qual", 
                     palette = 3, 
                     direction = -1) +
  
  labs(x = "Coefficient of pop", 
       y = "Intercept", 
       title = "Results from OLS regression of ed_visits on pop for each [age_group-ctas] segment") + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      

# robust regression  
df6.models %>% 
  unnest(tidy_rlm) %>% 
  select(term, 
         estimate, 
         age_group_pop, 
         ctas, 
         rsq) %>% 
  spread(term, estimate) %>% 
  
  ggplot(aes(x = pop, 
             y = `(Intercept)`, 
             col = ctas, 
             size = rsq)) + 
  geom_point(alpha = .5) + 
  geom_vline(xintercept = 0, col = "grey") + 
  geom_hline(yintercept = 0, col = "grey") + 
  
  facet_wrap(~ctas) +
  
  scale_color_brewer(type = "qual", 
                     palette = 3, 
                     direction = -1) +
  
  labs(x = "Coefficient of pop", 
       y = "Intercept", 
       title = "Results from robust regression of ed_visits on pop for each \n[age_group-ctas] segment") + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))

#' ## Notes on the models
#'
#' There's a lot of useful info in the graphs above.
#'
#' 1. Firstly note the range across the y-axis: the intercepts are often very
#' different from 0, and these are fairly well-fitting models (using Rsquared as
#' a metric). This emphasizes again: **please, for crying out loud, do not just
#' assume direct proportionality between ED visits and population**. That only
#' holds when the relationship is a straight line through the origin (zero
#' intercept)
#'
#' 2. Results from OLS and robust regression are almost identical: this helps
#' counter concerns that may be raised about "outlier" years biasing the results
#'
#' 3. CTAS 3 is most correlated with population size (both positively and
#' negatively), followed by CTAS 4.
#'
#' 4. Nearly every segment with a positive intercept has a negative slope. These
#' are likely cases where population size is decreasing, but ED visits are
#' rising.
#'
#' 5. Very rough estimates of impact of each additional BC resident on ED visits, by CTAS (across all age groups): 
#' 
#'     * CTAS 1: no impact
#' 
#'     * CTAS 2: 0.01 more visits 
#' 
#'     * CTAS 3: 0.02 more visits 
#'     
#'     * CTAS 4: 0.005 more visits 
#'     
#'     * CTAS 5: no impact
#'     


#+ projections
#' # Projections 
# 7) Projections: --------- 

#' For each age_group-ctas segment, we use the specific regression model for
#' that segment, and predict future ED visits using the known projected
#' population values.
#'
#' This is implemented mainly by calling `predict(model, newdata = df)`, where
#' df is a dataframe with the projected population numbers from BC Stats.

# fist we have to pull in the projected pop for each pop segmented
df7.add_pop_projection <- 
  df6.models %>% 
  left_join(df3.pop_nested %>% 
              select(age_group_pop, 
                     pop_projection))

# df7.add_pop_projection$pop_projection[[1]]

#' The following is the function that will be mapped across the segments: 

#+ proj-fn
# functions for projections: 
lm_predict <- function(model, new_data){
  df <- new_data
  
  predict(model,
          newdata = df %>% select(pop), 
          interval = "predict") %>% as.data.frame()
  
}

#+ add-predictions
df8.add_predictions <-
  df7.add_pop_projection %>%
  mutate(ed_visits_projected_lm = map2(model,
                                       pop_projection,
                                       lm_predict), 
         
         ed_visits_projected_rlm = map2(model_rlm,
                                        pop_projection,
                                        lm_predict))


# df8.add_predictions

# df with predictions: 
df9.predictions_unnested <- 
  df8.add_predictions %>% 
  unnest(ed_visits_projected_lm, 
         ed_visits_projected_rlm) %>% 
  select(age_group_pop, 
         ctas, 
         fit:upr, 
         fit1:upr1) %>% 
  rename(fit_lm = fit, 
         lwr_lm = lwr, 
         upr_lm = upr, 
         
         fit_rlm = fit1, 
         lwr_rlm = lwr1, 
         upr_rlm = upr1) %>% 
  mutate(year = rep(2019:2036, times = 100))

# df9.predictions_unnested 

#' ## Join historical and projected data
# > join historical and projected data: ------
df10.historical_and_projection <- 
  df8.add_predictions %>% 
  unnest(data) %>% 
  select(age_group_pop, 
         ctas, 
         year, 
         pop, 
         ed_visits) %>% 
  
  full_join(df9.predictions_unnested) %>% 
  
  arrange(age_group_pop, 
          ctas, 
          year)
  
# view: 
df10.historical_and_projection %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(6:11)



#+ plot-results 
#' ## Plotting final results 
# # 8) Plotting final results: ------

# pivot data into right format: 
df11.pivoted <- 
  df10.historical_and_projection %>% 
  select(age_group_pop, 
         ctas, 
         year, 
         ed_visits, 
         fit_lm, 
         upr_lm) %>% 
  gather(key = "metric", 
         value = "value", 
         -c(age_group_pop, 
            ctas, 
            year)) %>% 
  arrange(age_group_pop, 
          ctas, 
          year) %>% 
  na.omit() %>% 
  mutate(value = ifelse(value < 0, 
                        0, 
                        value)) %>% 
  
  # group by and nest: 
  group_by(age_group_pop, 
           ctas) %>% 
  nest()

# df11.pivoted

# add in graphs by mapping over list-col

df11.pivoted <- 
  df11.pivoted %>% 
  mutate(plot_projection = pmap(list(df = data, 
                                     subset1 = age_group_pop, 
                                     subset2 = ctas, 
                                     site = "RHS"), 
                               plot_ed_projection))
  
#' There are too many graphs to show here. See *`r here::here("results", "dst")`*
#' 
#' Here's just one example: 

df11.pivoted$plot_projection[[sample(1:100, 1)]]


# save output: 
# pdf(here::here("results",
#                "dst",
#                "2019-09-24_rhs_projected-ed-visits-by-age-and-ctas-segment.pdf"))
# df11.pivoted$plot_projection
# dev.off()






#' # Adjustments to the projections
# 9) Adjustments to the projections: -----------

#' All estimates are based on trends in the historical data. There are 4 
#' population segments for which there is almost no relevant 	
#' historical data, because of a major shift in behaviour since 2016. These 
#' segments are: 	
#'  
#' * Age 40-44, CTAS 2 and CTAS 3	
#' * Age 45-49, CTAS 2 and CTAS 3	
#' 

#' These segments had decreasing population and increasing ED visits from 2010 
#' to 2015. 	
#' 
#' Then, from 2016, there is slight evidence that both population and ED visits 
#' are increasing, but the trend is not clear yet.
#' 
#' We will refit models for these segments with data only from 2016. 
#' 


#' \  
#' 
#' The plan is to go back to `df4.ed_and_pop_data`, filter specifically for the
#' problematic segments, then filter to only include last 3 years of data.
#'
#' Then follow the same process up to `df10.historical_and_projection`
#'
#' In the end, delete those segments from `df10.historical_and_projection` and
#' replace them with the new versions.
#' 
#' Let's name them all starting with `df12.xx_`
#' 
#' 

df12.1_adjustments <-
  df4.ed_and_pop_data %>%
  filter(age_group_pop %in% c("40-44", "45-49"), 
         ctas %in% c("2 - Emergency", "3 - Urgent"), 
         year >= 2016)

  
df12.2_nested <- 
  df12.1_adjustments %>% 
  group_by(age_group_pop, 
           ctas) %>% 
  nest()

df12.3_models <- 
  df12.2_nested %>% 
  mutate(model = map(data, segment_model), 
         model_rlm = map(data, segment_model_rlm)) %>% 
  
  mutate(tidy = map(model, broom::tidy), 
         glance = map(model, broom::glance), 
         
         # let's specifically extract r squared: 
         rsq = glance %>% map_dbl("r.squared"),  
         # map( ) can be used to extract all objects with a certain name, here "r.squared"
         
         # same results for rlm: 
         tidy_rlm = map(model_rlm, broom::tidy), 
         glance_rlm = map(model_rlm, broom::glance), 
         
         # get AIC specifically: 
         aic = glance_rlm %>% map_dbl("AIC"))

# view: 
df12.3_models %>% 
  unnest(tidy_rlm) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(3:9)

# add pop data: 
df12.4_add_pop <- 
  df12.3_models %>% 
  left_join(df3.pop_nested %>% 
              select(age_group_pop, 
                     pop_projection))

# add projections: 
df12.5_add_projections <- 
  df12.4_add_pop %>% 
  mutate(ed_visits_projected_lm = map2(model,
                                       pop_projection,
                                       lm_predict), 
         
         ed_visits_projected_rlm = map2(model_rlm,
                                        pop_projection,
                                        lm_predict)) %>% 
  unnest(ed_visits_projected_lm, 
         ed_visits_projected_rlm) %>% 
  select(age_group_pop, 
         ctas, 
         fit:upr, 
         fit1:upr1) %>% 
  rename(fit_lm = fit, 
         lwr_lm = lwr, 
         upr_lm = upr, 
         
         fit_rlm = fit1, 
         lwr_rlm = lwr1, 
         upr_rlm = upr1) %>% 
  mutate(year = rep(2019:2036, times = 4))


# join historical and projected data: 
df12.6_historical_and_projection <- 
  df12.4_add_pop %>% 
  unnest(data) %>% 
  select(age_group_pop, 
         ctas, 
         year, 
         pop, 
         ed_visits) %>% 
  
  full_join(df12.5_add_projections) %>% 
  
  arrange(age_group_pop, 
          ctas, 
          year)

#' Now we replace the rows corresponding to these segments in the previous
#' results. 
#' 

# remove segments: 
df10.historical_and_projection <- 
  df10.historical_and_projection %>% 
  filter(!(age_group_pop %in% c("40-44", "45-49") & 
         ctas %in% c("2 - Emergency", "3 - Urgent") &
         year >= 2016)) # %>% View("df10 filter")


df10.historical_and_projection <- 
  df10.historical_and_projection %>% 
  bind_rows(df12.6_historical_and_projection) %>% 
  arrange(age_group_pop, 
          ctas, 
          year)

#' Here's the final dataset after adjustments: 

# View result: 
df10.historical_and_projection %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(6:12)



#' ## Plotting final after adjustments: 
# > Plotting final after adjustments: -------

# pivot data into right format: 
df11.pivoted <- 
  df10.historical_and_projection %>% 
  select(age_group_pop, 
         ctas, 
         year, 
         ed_visits, 
         fit_lm, 
         upr_lm) %>% 
  gather(key = "metric", 
         value = "value", 
         -c(age_group_pop, 
            ctas, 
            year)) %>% 
  arrange(age_group_pop, 
          ctas, 
          year) %>% 
  na.omit() %>% 
  mutate(value = ifelse(value < 0, 
                        0, 
                        value)) %>% 
  
  # group by and nest: 
  group_by(age_group_pop, 
           ctas) %>% 
  nest()

# df11.pivoted

# add in graphs by mapping over list-col

df11.pivoted <- 
  df11.pivoted %>% 
  mutate(plot_projection = pmap(list(df = data, 
                                     subset1 = age_group_pop, 
                                     subset2 = ctas, 
                                     site = "RHS"), 
                                plot_ed_projection))

#' There are too many graphs to show here. See *`r here::here("results", "dst")`*
#' 
#' Here's just one example: 

df11.pivoted$plot_projection[[sample(1:100, 1)]]


# save output: 
# pdf(here::here("results",
#                "dst",
#                "2019-09-24_rhs_projected-ed-visits-by-age-and-ctas-segment.pdf"))
# df11.pivoted$plot_projection
# dev.off()


#' ## Summaries
# > Summaries ----

# with ctas breakdown: 
df13.1_summary_fcast_by_year <- 
  df10.historical_and_projection %>% 
  group_by(year, 
           ctas) %>% 
  summarise(fit_lm = sum(fit_lm)) %>% 
  spread(key = ctas, 
         value = fit_lm) 

#View: 
df13.1_summary_fcast_by_year %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(2:6)


# without ctas breakdown: 
df13.2_summary_fcast_by_year <- 
  df10.historical_and_projection %>% 
  group_by(year) %>% 
  summarise(fit_lm = sum(fit_lm))

# view: 
df13.2_summary_fcast_by_year %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(2:6)
  


# historical numbers: 
df13.3_summary_historical_by_year <- 
  df10.historical_and_projection %>% 
  group_by(year) %>% 
  summarise(ed_visits = sum(ed_visits)) 
  

# view: 
df13.3_summary_historical_by_year %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(2:6)


#'
#' # Appendix 
# Appendix -----------

# write_csv(df10.historical_and_projection,
#           here::here("results",
#                      "dst",
#                      "2019-09-24_rhs_ed-visits-projections-by-age-and-ctas.csv"))


#' ## Checks
#'
#' 1. After all processing, does 2017 and 2018 total ED visits match to within
#' 50 visits what it should be? I'll allow for a difference of up to 50 because we exclude unknown age and
#' CTAS other than 1-5.
#' 

site <- "RHS"

df14.1.actuals <- 
  ed_mart %>% 
  filter(FacilityShortName == site, 
         StartDate >= "2017-01-01", 
         StartDate <= "2018-12-31") %>% 
  select(StartDate, 
         TriageAcuityDescription, 
         Age, 
         PatientID) %>% 
  collect() %>% 
  
  mutate(year = lubridate::year(StartDate)) %>% 
  
  group_by(year) %>% 
  summarize(ed_visits = n())
  
         
df14.2.processed <- 
  df10.historical_and_projection %>% 
  filter(year %in% c("2017", "2018")) %>% 
  group_by(year) %>% 
  summarise(ed_visits = sum(ed_visits))

#' Ans: `r abs(df14.1.actuals$ed_visits - df14.2.processed$ed_visits) < 50`

#' 
#' 2. Do we have the right number of rows in the nested data set `df5.nested`?
#'
#' Ans: `r nrow(df5.nested) == 100`
#'
#'
#' 3. Do we have the right number of rows in the final dataset? Age groups \*
#' CTAS \* years. Years are from 2010 to 2036.
#' 
#' Ans: `r 20*5*(2036-2010+1) == nrow(expand(df10.historical_and_projection, age_group_pop, ctas, year) %>% left_join(df10.historical_and_projection))`
 