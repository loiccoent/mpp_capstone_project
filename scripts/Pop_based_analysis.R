library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(gridExtra)

#### Data visualisation functions ####

#plot bar function
plot_bars <- function(df, cat_cols){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in cat_cols){
    p = ggplot(df, aes_string(col)) +
      geom_bar(alpha = 0.6) +
      theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.title = element_text(size= 15),
          legend.text = element_text(size = 15))
    print(p)
  }
}

#plot box function
plot_box <- function(df, col_y, cat_cols){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in cat_cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            legend.title = element_text(size= 15),
            legend.text = element_text(size = 15))
    print(p)
  }
}

#plot violin function
plot_violin <- function(df, col_y, cat_cols, bins = 30){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in cat_cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_violin() +
      theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            legend.title = element_text(size= 15),
            legend.text = element_text(size = 15))
    print(p)
  }
}

#plot hist function
plot_hist <- function(df, num_cols, bins = 10){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in num_cols){
    bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
    p = ggplot(df, aes_string(col)) +
      geom_histogram(alpha = 0.6, binwidth = bw)
    print(p)
  }
}

#plot kde function
plot_dist <- function(df, num_cols){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in num_cols){
    p = ggplot(df, aes_string(col)) +
      geom_density(color= 'blue') +
      geom_rug(na.rm = TRUE)
    print(p)
  }
}

#plot combo kde hist function
plot_hist_dens <- function(df, num_cols, bins = 10){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in num_cols){
    bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
    p = ggplot(df, aes_string(col)) +
      geom_histogram(alpha = 0.5, binwidth = bw, aes(y=..density..)) +
      geom_density(aes(y=..density..),color= 'blue') +
      geom_rug()
    print(p)
  }
}

#plot combo kde hist facet function
plot_hist_dens_facet <- function(df, facet, num_cols, bins = 10){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in num_cols){
    bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
    p = ggplot(df, aes_string(col)) +
      geom_histogram(alpha = 0.5, binwidth = bw, aes(y=..density..)) +
      geom_density(aes(y=..density..),color= 'blue') +
      geom_rug() +
      facet_grid(. ~df[[facet]])
    print(p)
  }
}

#plot scatter function
plot_scatter <- function(df, col_y, shape, color, num_cols, alpha = 1){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in num_cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_point(aes(shape = factor(.data[[shape]]), color = factor(.data[[color]])), alpha = alpha)
    print(p)
  }
}

#plot scatter grid function
plot_scatter_grid <- function(df, col_y, facet1, facet2, color, num_cols, alpha = 1){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in num_cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_point(aes(color = factor(.data[[color]])), alpha = alpha) +
      facet_grid(df[[facet1]] ~ df[[facet2]])
    print(p)
  }
}

#plot 2D density function
plot_2D_density <- function(df, col_y, shape, color, num_cols, alpha = 1){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in num_cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_density_2d() +
      geom_point(aes(shape = factor(.data[[shape]]), color = factor(.data[[color]])), alpha = alpha) +
    print(p)
  }
}

#pair plot function
plot_pair <- function(df, color, num_cols, alpha = 0.1){
  options(repr.plot.width = 6, repr.plot.height = 6) #set the initial plot area dimensions
  p = ggpairs(df, 
              columns = num_cols,
              aes(color = factor(.data[[color]]), alpha = alpha),
              lower = list(continuous = 'points'),
              upper = list(continous = wrap(ggally_density,  alignPercent = 1, size = 15))) + 
  theme(text = element_text(size = 15))
  print(p)
}
#### Feature data Checking #####

#train values check
train_values <- read.csv("data/train_values.csv")
str(train_values)

#features definitions
ID_num_cols <- c("population")
ID_fac_cols <- c("state", "year")
ethnicity_cols <- c("pct_white", "pct_af_am", "pct_hispanic", "pct_am_ind", "pct_asian", "pct_nh_pi", "pct_multiple", "pct_other")
housing_cols <- c("renter_occupied_households", "pct_renter_occupied", "median_gross_rent", "median_household_income", "median_property_value", "rent_burden")
economic_fac_cols <- c("rucc", "urban_influence", "economic_typology")
economic_num_cols <- c("poverty_rate", "pct_civilian_labor", "pct_unemployment")
health_cols <- c("pct_uninsured_adults", "pct_uninsured_children", "pct_adult_obesity", "pct_adult_smoking", "pct_diabetes", "pct_low_birthweight", "pct_excessive_drinking", "pct_physical_inactivity", "air_pollution_particulate_matter_value", "homicides_per_100k", "motor_vehicle_crash_deaths_per_100k", "heart_disease_mortality_per_100k", "pop_per_dentist", "pop_per_primary_care_physician")
demographic_cols <- c("pct_female", "pct_below_18_years_of_age", "pct_aged_65_years_and_older", "pct_adults_less_than_a_high_school_diploma", "pct_adults_with_high_school_diploma", "pct_adults_with_some_college", "pct_adults_bachelors_or_higher", "birth_rate_per_1k", "death_rate_per_1k")

#summary of each set of numeric columns
summary(train_values[,ID_num_cols])
sapply(train_values[,ethnicity_cols],summary)
sapply(train_values[,housing_cols],summary)
sapply(train_values[,economic_num_cols],summary)
sapply(train_values[,health_cols],summary)
sapply(train_values[,demographic_cols],summary)

sd(train_values[,ID_num_cols])
sapply(train_values[,ethnicity_cols],sd)
sapply(train_values[,housing_cols],sd)
sd(train_values$median_household_income, na.rm = TRUE)
sd(train_values$median_property_value, na.rm = TRUE)
sapply(train_values[,economic_num_cols],sd)
sapply(train_values[,health_cols],sd)
sd(train_values$pct_adult_smoking, na.rm = TRUE)
sd(train_values$pct_low_birthweight, na.rm = TRUE)
sd(train_values$pct_excessive_drinking, na.rm = TRUE)
sd(train_values$air_pollution_particulate_matter_value, na.rm = TRUE)
sd(train_values$homicides_per_100k, na.rm = TRUE)
sd(train_values$motor_vehicle_crash_deaths_per_100k, na.rm = TRUE)
sd(train_values$pop_per_dentist, na.rm = TRUE)
sd(train_values$pop_per_primary_care_physician, na.rm = TRUE)
sapply(train_values[,demographic_cols],sd)

unique(train_values$county_code)
unique(train_values$state)

train_values %>%
  group_by(rucc) %>%
  summarize(count=n())

train_values %>%
  group_by(urban_influence) %>%
  summarize(count=n())

train_values %>%
  group_by(economic_typology) %>%
  summarize(count=n())

#plot distribution 

#bars
plot_bars(train_values, ID_fac_cols)
plot_bars(train_values, economic_fac_cols)

#plot histogrames
plot_hist(train_values, ID_num_cols)
plot_hist(train_values, ethnicity_cols)
plot_hist(train_values, housing_cols)
plot_hist(train_values, economic_num_cols)
plot_hist(train_values, health_cols)
plot_hist(train_values, demographic_cols)

#combo histogrames kde
plot_hist_dens(train_values, ID_num_cols)
plot_hist_dens(train_values, ethnicity_cols)
plot_hist_dens(train_values, housing_cols)
plot_hist_dens(train_values, economic_num_cols)
plot_hist_dens(train_values, health_cols)
plot_hist_dens(train_values, demographic_cols)

#combo histogrames kde facet
plot_hist_dens_facet(train_values, facet = 'economic_typology', ID_num_cols)
plot_hist_dens_facet(train_values, facet = 'economic_typology', ethnicity_cols)
plot_hist_dens_facet(train_values, facet = 'economic_typology', housing_cols)
plot_hist_dens_facet(train_values, facet = 'economic_typology', economic_num_cols)
plot_hist_dens_facet(train_values, facet = 'economic_typology', health_cols)
plot_hist_dens_facet(train_values, facet = 'economic_typology', demographic_cols)

#### Labels data checking and merging #####

train_labels <- read.csv("data/train_labels.csv")
summary(train_labels)
str(train_labels)
glimpse(train_labels)

#eviction distribution
sd(train_labels$evictions)
plot_hist_dens(train_labels, 'evictions')
plot(density(train_labels$evictions))
plot(density(sqrt(train_labels$evictions)))
plot(density(log(train_labels$evictions)))

#gathering data
train_data <- merge(train_values, train_labels, by="row_id")

#### Test data checking ####

#train values check
test_values <- read.csv("data/test_values.csv")
str(train_values)

#### Data transformation 1 : Apply all pct to population ####

train_data_mod <- train_data %>%
  mutate(white = pct_white * population) %>%
  mutate(af_am = pct_af_am * population) %>%
  mutate(hispanic = pct_hispanic * population) %>%
  mutate(am_ind = pct_am_ind * population) %>%
  mutate(asian = pct_asian * population) %>%
  mutate(nh_pi = pct_nh_pi * population) %>%
  mutate(multiple = pct_multiple * population) %>%
  mutate(other = pct_other * population) %>%
  mutate(poors = poverty_rate * population) %>%
  mutate(civilian_employees = pct_civilian_labor * population) %>%
  mutate(unemployed = pct_unemployment * population) %>%
  mutate(below_18 = pct_below_18_years_of_age * population) %>%
  mutate(above_18 = (1 - pct_below_18_years_of_age) * population) %>%
  mutate(above_65 = pct_aged_65_years_and_older * population) %>%
  mutate(below_65 = (1 - pct_aged_65_years_and_older) * population) %>%
  mutate(births = birth_rate_per_1k/1000 * population) %>%
  mutate(deaths = death_rate_per_1k/1000 * population) %>%
  mutate(homicides = homicides_per_100k/100000 * population) %>%
  mutate(motor_vehicle_crash_deaths = motor_vehicle_crash_deaths_per_100k/100000 * population) %>%
  mutate(heart_disease_deaths = heart_disease_mortality_per_100k/100000 * population) %>%
  mutate(uninsured_adults = above_18 * pct_uninsured_adults ) %>%
  mutate(uninsured_children = below_18 * pct_uninsured_children) %>%
  mutate(adult_obese = above_18 * pct_adult_obesity) %>%
  mutate(adult_smoker = above_18 * pct_adult_smoking) %>%
  mutate(adult_excessive_drinker = above_18 * pct_excessive_drinking) %>%
  mutate(adult_physically_inactive = above_18 * pct_physical_inactivity) %>%
  mutate(diabetes = pct_diabetes * population) %>%
  mutate(low_birthweight = births * pct_low_birthweight * population) %>%
  mutate(female = pct_female * population) %>%
  mutate(male = (1 - pct_female) * population) %>%
  mutate(adults_less_than_a_high_school_diploma = above_18 * pct_adults_less_than_a_high_school_diploma) %>%
  mutate(adults_with_high_school_diploma = above_18 * pct_adults_with_high_school_diploma) %>%
  mutate(adults_with_some_college = above_18 * pct_adults_with_some_college) %>%
  mutate(adults_bachelors_or_higher = above_18 * pct_adults_bachelors_or_higher) %>%
  mutate(dentists = population / pop_per_dentist) %>%
  mutate(physicians = population / pop_per_primary_care_physician)

train_data_mod <- train_data_mod %>%
  mutate(evictions_per_1k = evictions / population * 1000)

#new features definitions
ethnicity_cols_pop <- c("white", "af_am", "hispanic", "am_ind", "asian", "nh_pi", "multiple", "other")
economic_num_cols_pop <- c("poors", "civilian_employees", "unemployed")
health_cols_pop <- c("uninsured_adults", "uninsured_children", "adult_obese", "adult_smoker", "diabetes", "low_birthweight", "adult_excessive_drinker", "adult_physically_inactive", "homicides", "motor_vehicle_crash_deaths", "heart_disease_deaths", "dentists", "physicians")
demographic_cols_pop <- c("female", "male", "below_18", "above_18", "above_65", "below_65", "adults_less_than_a_high_school_diploma", "adults_with_high_school_diploma", "adults_with_some_college", "adults_bachelors_or_higher", "births", "deaths")
housing_cols_pop <- c("renter_occupied_households", "median_gross_rent", "median_household_income", "median_property_value", "rent_burden")


#plot histogrames
plot_hist(train_data_mod, ethnicity_cols_pop)
plot_hist(train_data_mod, economic_num_cols_pop)
plot_hist(train_data_mod, health_cols_pop)
plot_hist(train_data_mod, demographic_cols_pop)

#plot kde
plot_dist(train_data_mod, ethnicity_cols_pop)
plot_dist(train_data_mod, economic_num_cols_pop)
plot_dist(train_data_mod, health_cols_pop)
plot_dist(train_data_mod, demographic_cols_pop)

#check new correlation ratio
sapply(train_data_mod[,ethnicity_cols_pop], function(r){cor(r, y = train_data_mod$evictions)})
sapply(train_data_mod[,economic_num_cols_pop], function(r){cor(r, y = train_data_mod$evictions)})
sapply(train_data_mod[,health_cols_pop], function(r){cor(r, y = train_data_mod$evictions, use = 'complete')})
sapply(train_data_mod[,demographic_cols_pop], function(r){cor(r, y = train_data_mod$evictions)})

#### Data transformation 2 : combine rucc and urban influence, group under 3 level ####

#Check which urban categories are related
cat_count <- train_data_mod %>%
  group_by(rucc, urban_influence, economic_typology) %>%
  summarize(count = n())

ggplot(cat_count, aes(x = rucc, y = urban_influence, colour = economic_typology, size = count ^2)) +
  geom_point() +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size= 15),
        legend.text = element_text(size = 15))

#create new category with only 3 factors
levels(train_data_mod$urban_influence)

Nonmetro_not_adjacent_to_metro <- c(10, 11)
Nonmetro_adjacent_to_metro <- c(2, 3, 4, 5, 6, 7, 8, 9)
Metro <- c(1, 12)

train_data_mod <- train_data_mod %>%
  mutate(geo_simple = ifelse(as.numeric(urban_influence) %in% Nonmetro_not_adjacent_to_metro, 1, ifelse(as.numeric(urban_influence) %in% Nonmetro_adjacent_to_metro, 2, 3)))

train_data_mod$geo_simple <- factor(train_data_mod$geo_simple,
                    levels = c(1,2,3),
                    labels = c("Nonmetro not adjacent to metro", "Nonmetro adjacent to metro", "Metro"))

str(train_data_mod$geo_simple)

cat_count_2 <- train_data_mod %>%
  group_by(geo_simple, economic_typology) %>%
  summarize(count = n(), average_evictions_per_1k = mean(evictions_per_1k)) %>%
  arrange(desc(average_evictions_per_1k))

ggplot(cat_count_2, aes(x = geo_simple, y = economic_typology, size = average_evictions_per_1k ^2)) +
  geom_point() +
  theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size= 15),
        legend.text = element_text(size = 15))

very_high <- c("Metro.Federal/State government-dependent")
high <- c("Metro.Manufacturing-dependent", "Metro.Nonspecialized", "Metro.Mining-dependent", "Nonmetro adjacent to metro.Manufacturing-dependent", "Metro.Recreation", "Nonmetro adjacent to metro.Nonspecialized")
medium <- c("Nonmetro not adjacent to metro.Recreation", "Nonmetro adjacent to metro.Federal/State government-dependent", "Nonmetro adjacent to metro.Recreation")
low <- c("Nonmetro adjacent to metro.Mining-dependent", "Nonmetro not adjacent to metro.Nonspecialized", "Metro.Farm-dependent", "Nonmetro not adjacent to metro.Federal/State government-dependent", 
         "Nonmetro not adjacent to metro.Manufacturing-dependent", "Nonmetro adjacent to metro.Farm-dependent", "Nonmetro not adjacent to metro.Mining-dependent", "Nonmetro not adjacent to metro.Farm-dependent")

train_data_mod <- cbind(train_data_mod, unite(train_data_mod[,c('geo_simple', 'economic_typology')],'geo.typo',c('geo_simple','economic_typology'), sep ="."))

train_data_mod <- train_data_mod %>% 
  mutate(area_evictions_per_1k = ifelse(geo.typo %in% very_high, 4, 
                                 ifelse(geo.typo %in% high, 3,
                                 ifelse(geo.typo %in% medium, 2,
                                 ifelse(geo.typo %in% low, 1,
                                 'NA')))))

train_data_mod$area_evictions_per_1k <- factor(train_data_mod$area_evictions_per_1k,
                                               levels = c(1,2,3,4),
                                               labels = c("low",
                                                          "medium",
                                                          "high",
                                                          "very_high"))



#scatter
plot_scatter(train_data_mod, col_y = 'evictions', shape = 'geo_simple', color = 'economic_typology', ID_num_cols)
plot_scatter(train_data_mod, col_y = 'evictions', shape = 'geo_simple', color = 'economic_typology', ethnicity_cols_pop)
plot_scatter(train_data_mod, col_y = 'evictions', shape = 'geo_simple', color = 'economic_typology', housing_cols)
plot_scatter(train_data_mod, col_y = 'evictions', shape = 'geo_simple', color = 'economic_typology', economic_num_cols_pop)
plot_scatter(train_data_mod, col_y = 'evictions', shape = 'geo_simple', color = 'economic_typology', health_cols_pop)
plot_scatter(train_data_mod, col_y = 'evictions', shape = 'geo_simple', color = 'economic_typology', demographic_cols_pop)

#box plots
plot_box(train_data_mod, col_y = 'evictions', ID_fac_cols)
plot_box(train_data_mod, col_y = 'evictions', economic_fac_cols)
plot_box(train_data_mod, col_y = 'evictions', 'geo_simple')
#Violin plots
plot_violin(train_data_mod, col_y = 'evictions', ID_fac_cols)
plot_violin(train_data_mod, col_y = 'evictions', economic_fac_cols)
plot_violin(train_data_mod, col_y = 'evictions', 'geo_simple')


#### Data transformation 4 : Remove extremes ####

summary(train_data$population)
quantile(train_data$population,0.99)
train_data_mod_short <- train_data_mod[which(train_data_mod$population < 1322349),]

plot_hist(train_data_mod, ID_num_cols)

#### Data transformation 5 : transform some features ####

plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(ID_num_cols, 'evictions_per_1k'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(ethnicity_cols, 'evictions_per_1k'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(housing_cols, 'evictions_per_1k'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(economic_num_cols, 'evictions_per_1k'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(health_cols, 'evictions_per_1k'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(demographic_cols, 'evictions_per_1k'))

plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(ID_num_cols, 'evictions'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c('population', ethnicity_cols_pop, 'evictions'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(housing_cols_pop, 'evictions'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(economic_num_cols_pop, 'evictions'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(health_cols_pop, 'evictions'))
plot_pair(train_data_mod_short, color = 'geo_simple', num_cols= c(demographic_cols_pop, 'evictions'))

#### Data transformation 6 : Group states ####

state_list <- unique(train_data_mod$state)
str(state_list)

state_count <- train_data_mod %>%
  group_by(state) %>%
  summarize(m = mean(evictions_per_1k)) %>%
  arrange(desc(m))

#box plots
plot_box(train_data_mod, 'evictions_per_1k', c('state'))

state_count$state <- as.vector(state_count$state) #get rid of factors
state_count$state = factor(state_count$state,state_count$state) #add ordered factors back

ggplot(state_count, aes(x = state, y = m)) +
  geom_point()

state_count <- state_count %>%
  mutate(interval_state_eviction = cut(state_count$m, c(-0.5,1.5,2.5,3.5,5.5,10.5), labels = c('0-1', '1-2', '2-3','3-5', '5-10')))

state_count$state <- as.vector(state_count$state) #get rid of factors
state_count$state = factor(state_count$state,state_count$state) #add ordered factors back

ggplot(state_count, aes(x = state, y = m, color = interval_state_eviction)) +
  geom_point()

train_data_mod <- merge(train_data_mod, state_count[,c('interval_state_eviction','state')], by = 'state')

state_list <- unique(train_data_mod[,c('interval_state_eviction', 'state')]) %>%
  arrange(desc(interval_state_eviction))

#scatter grid
plot_scatter_grid(train_data_mod, col_y = 'evictions', facet1 = 'geo_simple', facet2 = 'economic_typology', color = 'interval_state_eviction', ID_num_cols)
plot_scatter_grid(train_data_mod, col_y = 'evictions', facet1 = 'geo_simple', facet2 = 'economic_typology', color = 'interval_state_eviction', ethnicity_cols)
plot_scatter_grid(train_data_mod, col_y = 'evictions', facet1 = 'geo_simple', facet2 = 'economic_typology', color = 'interval_state_eviction', housing_cols)
plot_scatter_grid(train_data_mod, col_y = 'evictions', facet1 = 'geo_simple', facet2 = 'economic_typology', color = 'interval_state_eviction', economic_num_cols)
plot_scatter_grid(train_data_mod, col_y = 'evictions', facet1 = 'geo_simple', facet2 = 'economic_typology', color = 'interval_state_eviction', health_cols)
plot_scatter_grid(train_data_mod, col_y = 'evictions', facet1 = 'geo_simple', facet2 = 'economic_typology', color = 'interval_state_eviction', demographic_cols)


plot_box(train_data_mod, 'evictions', economic_fac_cols)

#### Data transformation 7 : Group years ####

plot_box(train_data_mod, 'evictions_per_1k', c('year'))

ggplot(train_data_mod, aes(x = population, y = evictions, color = year)) + 
  geom_point()

state_year_count <- unite(train_data_mod,'state.year',c('state','year'), sep =".") %>%
  group_by(state.year) %>%
  summarize(m = mean(evictions_per_1k)) %>%
  arrange(desc(m))

state_year_count <- state_year_count %>%
  mutate(interval_state_year_eviction = cut(state_year_count$m, c(-0.5,1.5,2.5,3.5,5.5,10.5), labels = c('0-1', '1-2', '2-3','3-5', '5-10')))

state_year_count$state.year <- as.vector(state_year_count$state.year) #get rid of factors
state_year_count$state.year = factor(state_year_count$state.year,state_year_count$state.year) #add ordered factors back

ggplot(state_year_count, aes(x = state.year, y = m, color = interval_state_year_eviction)) +
  geom_point()


train_data_mod <- cbind(train_data_mod, unite(train_data_mod[,c('state', 'year')],'state.year',c('state','year'), sep ="."))

train_data_mod <- merge(train_data_mod, state_year_count[,c('interval_state_year_eviction','state.year')], by = 'state.year')

state_year_list <- unique(state_year_count[,c('interval_state_year_eviction', 'state.year')]) %>%
  arrange(desc(interval_state_year_eviction))