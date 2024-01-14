library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(gridExtra)

############################################ Data visualisation functions ##########################################

#plot bar function
plot_bars <- function(df, cat_cols){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in cat_cols){
    p = ggplot(df, aes_string(col)) +
      geom_bar(alpha = 0.6) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
      geom_rug()
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
      geom_point(aes(shape = factor(.data[[shape]]), color = factor(.data[[color]])), alpha = alpha) +
      ggtitle(paste('Scatter plot of', col_y, ' vs ', col,
                    '\n conditioned on ', shape, ' and ', color))
    print(p)
  }
}

#plot scatter grid function
plot_scatter_grid <- function(df, col_y, facet1, facet2, color, num_cols, alpha = 1){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in num_cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_point(aes(color = factor(.data[[color]])), alpha = alpha) +
      ggtitle(paste('Scatter plot of', col_y, ' vs ', col,
                    '\n conditioned on ', color,
                    '\n faceted on ', facet1, ' and ', facet2)) +
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
      ggtitle(paste('2D density plot of ', col_y, ' vs ', col,
                    '\n conditioned on ', shape, ' and ', color))
    print(p)
  }
}

#plot box function
plot_box <- function(df, col_y, cat_cols){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in cat_cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_boxplot() +
      ggtitle(paste('Box plot of', col_y, ' vs ', col)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(p)
  }
}

#plot box function
plot_violin <- function(df, col_y, cat_cols, bins = 30){
  options(repr.plot.width = 4, repr.plot.height = 3.5) #set the initial plot area dimensions
  for(col in cat_cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_violin() +
      ggtitle(paste('Violin plot of', col_y, ' vs ', col)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(p)
  }
}

#pair plot function
plot_pair <- function(df, color, num_cols, alpha = 0.1){
  options(repr.plot.width = 6, repr.plot.height = 6) #set the initial plot area dimensions
  for(col in num_cols){
    p = ggpairs(df, 
                columns = col,
                aes(color = color, alpha = alpha),
                lower = list(continuous = 'points'),
                upper = list(continous = ggally_density))
    print(p)
  }
}

############################################ Feature data Checking #########################################################

#train values check
train_values <- read.csv("data/train_values.csv")
str(train_values)

#features definitions
ID_num_cols <- c("population")
ID_fac_cols <- c("county_code", "state", "year")
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

#plot distribution

#bars
plot_bars(train_values, ID_fac_cols)
plot_bars(train_values, economic_fac_cols)

#density by ethnicity
plot(density(sqrt(train_values$pct_white)))
plot(density(sqrt(train_values$pct_af_am)))
plot(density(sqrt(train_values$pct_hispanic)))
plot(density(sqrt(train_values$pct_asian)))
plot(density(sqrt(train_values$pct_am_ind)))
plot(density(sqrt(train_values$pct_nh_pi)))
plot(density(sqrt(train_values$pct_multiple)))
plot(density(sqrt(train_values$pct_other)))

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

########################################### Labels data checking ######################################

train_labels <- read.csv("data/train_labels.csv")
summary(train_labels)
str(train_labels)
glimpse(train_labels)

#eviction distribution
sd(train_labels$evictions)
plot_hist_dens(train_labels, 'evictions')
plot(density(train_labels$evictions))
plot(density(log(train_labels$evictions)))
plot(density(sqrt(train_labels$evictions)))


######################################### Data merging #############################################################

#gathering data
train_data <- merge(train_values, train_labels, by="row_id")

#by county
train_data_by_county <- train_data %>%
  group_by(county_code) %>%
  summarize(mean(evictions))
summary(train_data_by_county)

#by state
train_data_by_state <- train_data %>%
  group_by(state) %>%
  summarize(mean(evictions))
summary(train_data_by_state)

#for state d725a95
train_data_d725a95 <- train_data %>%
  filter(state == "d725a95")
cor(train_data_d725a95$population, train_data_d725a95$evictions)

#by county and by ethnicity
train_data_by_county_all <- train_data %>%
  group_by(county_code) %>%
  summarise_all(funs(mean))

cor(train_data_by_county_all$pct_white, train_data_by_county_all$evictions)
cor(train_data_by_county_all$pct_hispanic, train_data_by_county_all$evictions)
cor(train_data_by_county_all$pct_af_am, train_data_by_county_all$evictions)
cor(train_data_by_county_all$pct_asian, train_data_by_county_all$evictions)
cor(train_data_by_county_all$pct_nh_pi, train_data_by_county_all$evictions)
cor(train_data_by_county_all$pct_am_ind, train_data_by_county_all$evictions)
cor(train_data_by_county_all$pct_multiple, train_data_by_county_all$evictions)
cor(train_data_by_county_all$pct_other, train_data_by_county_all$evictions)

#check all correlations factors
cor(train_data$population, y = train_data$evictions)
sapply(train_data[,ethnicity_cols], function(r){cor(r, y = train_data$evictions)})
sapply(train_data[,housing_cols], function(r){cor(r, y = train_data$evictions, use = "complete")})
sapply(train_data[,economic_num_cols], function(r){cor(r, y = train_data$evictions, use = "complete")})
sapply(train_data[,health_cols], function(r){cor(r, y = train_data$evictions, use = "complete")})
sapply(train_data[,demographic_cols], function(r){cor(r, y = train_data$eviction, use = "complete")})


#by year and by state
train_data_by_state_year <- train_data %>%
  group_by(state,year) %>%
  summarize(median(evictions))

#for states de1c9a5 and e602fb0
train_data_by_year_de1c9a5_e602fb0 <- train_data_by_state_year %>%
  filter(state %in% c("de1c9a5","e602fb0"))
train_data_by_year_de1c9a5_e602fb0

########################################### Data transformation ########################################################

# Mod 1 : Normalize by population

cor(train_data$population, train_data$evictions)

train_data_mod1 <- train_data %>%
  mutate(evictions_ratio = (evictions/population)) %>%
  mutate(evictions_ratio_renters = (evictions_ratio/pct_renter_occupied))

plot(density(train_data$evictions))
plot(density(train_data_mod1$evictions_ratio))
plot(density(train_data_mod1$evictions_ratio_renters))

#check correlation new ratio
cor(train_data_mod1$population, train_data_mod1$evictions_ratio)
sapply(train_data_mod1[,ethnicity_cols], function(r){cor(r, y = train_data_mod1$evictions_ratio)})
sapply(train_data_mod1[,housing_cols], function(r){cor(r, y = train_data_mod1$evictions_ratio, use = "complete")})
sapply(train_data_mod1[,economic_num_cols], function(r){cor(r, y = train_data_mod1$evictions_ratio, use = "complete")})
sapply(train_data_mod1[,health_cols], function(r){cor(r, y = train_data_mod1$evictions_ratio, use = "complete")})
sapply(train_data_mod1[,demographic_cols], function(r){cor(r, y = train_data_mod1$evictions_ratio, use = "complete")})

#Mod 2: combine rucc and urban influence, group under 3 levels

#scatter grid
plot_scatter_grid(train_data_mod1, col_y = 'evictions_ratio', facet1 = 'rucc', facet2 = 'urban_influence', color = 'economic_typology', ID_num_cols)
plot_scatter_grid(train_data_mod1, col_y = 'evictions_ratio', facet1 = 'rucc', facet2 = 'urban_influence', color = 'economic_typology', ethnicity_cols)
plot_scatter_grid(train_data_mod1, col_y = 'evictions_ratio', facet1 = 'rucc', facet2 = 'urban_influence', color = 'economic_typology', housing_cols)
plot_scatter_grid(train_data_mod1, col_y = 'evictions_ratio', facet1 = 'rucc', facet2 = 'urban_influence', color = 'economic_typology', economic_num_cols)
plot_scatter_grid(train_data_mod1, col_y = 'evictions_ratio', facet1 = 'rucc', facet2 = 'urban_influence', color = 'economic_typology', health_cols)
plot_scatter_grid(train_data_mod1, col_y = 'evictions_ratio', facet1 = 'rucc', facet2 = 'urban_influence', color = 'economic_typology', demographic_cols)

#Check which urban categories are related
cat_count <- train_data_mod1 %>%
  group_by(rucc, urban_influence, economic_typology) %>%
  summarize(count = n())

ggplot(cat_count, aes(x = rucc, y = urban_influence, colour = economic_typology, size = count ^2)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#create new category with only 3 factors
levels(train_data_mod1$urban_influence)

as.numeric(train_data_mod4$urban_influence)

Nonmetro_not_adjacent_to_metro <- c(10, 11)
Nonmetro_adjacent_to_metro <- c(2, 3, 4, 5, 6, 7, 8, 9)
Metro <- c(1, 12)

train_data_mod2 <- train_data_mod1 %>%
  mutate(geo_simple = ifelse(as.numeric(urban_influence) %in% Nonmetro_not_adjacent_to_metro, 1, ifelse(as.numeric(urban_influence) %in% Nonmetro_adjacent_to_metro, 2, 3)))

train_data_mod2$geo_simple <- factor(train_data_mod2$geo_simple,
                    levels = c(1,2,3),
                    labels = c("Nonmetro not adjacent to metro", "Nonmetro adjacent to metro", "Metro"))

str(train_data_mod2$geo_simple)

#scatter
plot_scatter(train_data_mod2, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', ID_num_cols)
plot_scatter(train_data_mod2, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', ethnicity_cols)
plot_scatter(train_data_mod2, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', housing_cols)
plot_scatter(train_data_mod2, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', economic_num_cols)
plot_scatter(train_data_mod2, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', health_cols)
plot_scatter(train_data_mod2, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', demographic_cols)

#2D density
plot_2D_density(train_data_mod2, col_y = 'evictions_ratio', shape = 'economic_typology', color = 'geo_simple', ID_num_cols)
plot_2D_density(train_data_mod2, col_y = 'evictions_ratio', shape = 'economic_typology', color = 'geo_simple', ethnicity_cols)
plot_2D_density(train_data_mod2, col_y = 'evictions_ratio', shape = 'economic_typology', color = 'geo_simple', housing_cols)
plot_2D_density(train_data_mod2, col_y = 'evictions_ratio', shape = 'economic_typology', color = 'geo_simple', economic_num_cols)
plot_2D_density(train_data_mod2, col_y = 'evictions_ratio', shape = 'economic_typology', color = 'geo_simple', health_cols)
plot_2D_density(train_data_mod2, col_y = 'evictions_ratio', shape = 'economic_typology', color = 'geo_simple', demographic_cols)
#box plots
plot_box(train_data_mod2, col_y = 'evictions_ratio', ID_fac_cols)
plot_box(train_data_mod2, col_y = 'evictions_ratio', economic_fac_cols)
plot_box(train_data_mod2, col_y = 'evictions_ratio', 'geo_simple')
#Violin plots
plot_violin(train_data_mod2, col_y = 'evictions_ratio', ID_fac_cols)
plot_violin(train_data_mod2, col_y = 'evictions_ratio', economic_fac_cols)
plot_violin(train_data_mod2, col_y = 'evictions_ratio', 'geo_simple')
#pair plots
plot_pair(train_data_mod2, color = 'geo_simple', ID_num_cols)
plot_pair(train_data_mod2, color = 'geo_simple', ethnicity_cols)
plot_pair(train_data_mod2, color = 'geo_simple', housing_cols)
plot_pair(train_data_mod2, color = 'geo_simple', economic_num_cols)
plot_pair(train_data_mod2, color = 'geo_simple', health_cols)
plot_pair(train_data_mod2, color = 'geo_simple', demographic_cols)

# Mod3: education level gathering
train_data_mod3 <- train_data_mod2 %>%
  mutate(pct_adults_high_school_or_less = pct_adults_less_than_a_high_school_diploma + pct_adults_with_high_school_diploma)

cor(train_data_mod3$pct_adults_high_school_or_less, train_data_mod3$evictions_ratio)

# Mod 4: sqrt of ethnicity

train_data_mod4 <- train_data_mod3 %>%
  mutate(pct_white_rev = 1/(pct_white)) %>%
  mutate(pct_af_am_sqrt = sqrt(pct_af_am)) %>%
  mutate(pct_hispanic_sqrt = sqrt(pct_hispanic)) %>%
  mutate(pct_am_ind_sqrt = sqrt(pct_am_ind)) %>%
  mutate(pct_asian_sqrt = sqrt(pct_asian)) %>%
  mutate(pct_nh_pi_sqrt = sqrt(pct_nh_pi)) %>%
  mutate(pct_multiple_sqrt = sqrt(pct_multiple)) %>%
  mutate(pct_other_sqrt = sqrt(pct_other)) %>%
  mutate(evictions_ratio_sqrt = sqrt(evictions_ratio))

sapply(train_data_mod4[,ethnicity_cols], function(r){cor(r, y = train_data_mod4$evictions_ratio)})

ethnicity_cols_sqrt <- c("pct_white_rev", "pct_af_am_sqrt", "pct_hispanic_sqrt", "pct_am_ind_sqrt", "pct_asian_sqrt", "pct_nh_pi_sqrt", "pct_multiple_sqrt", "pct_other_sqrt")
sapply(train_data_mod4[,ethnicity_cols_sqrt],summary)
sapply(train_data_mod4[,ethnicity_cols], function(r){cor(r, y = train_data_mod4$evictions_ratio_renters)})

plot_scatter(train_data_mod4, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', 'pct_af_am')
plot_scatter(train_data_mod4, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', 'pct_af_am_sqrt')
plot_scatter(train_data_mod4, col_y = 'evictions_ratio_sqrt', shape = 'geo_simple', color = 'economic_typology', 'pct_af_am_sqrt')

plot_scatter(train_data_mod4, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', 'pct_white')
plot_scatter(train_data_mod4, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', 'pct_white_rev')
plot_scatter(train_data_mod4, col_y = 'evictions_ratio_sqrt', shape = 'geo_simple', color = 'economic_typology', 'pct_white_rev')

cor(train_data_mod4$pct_af_am, train_data_mod4$evictions_ratio_renters, use = 'complete')
cor(train_data_mod4$pct_af_am_sqrt, train_data_mod4$evictions_ratio_renters, use = 'complete')




#Mod5 : sqrt of pop per dentist/doctor

train_data_mod5 <- train_data_mod4 %>%
  mutate(pop_per_dentist_sqrt = sqrt(pop_per_dentist)) %>%
  mutate(pop_per_primary_care_physician_sqrt = sqrt(pop_per_primary_care_physician))

summary(train_data_mod5$pop_per_dentist_sqrt)
summary(train_data_mod5$pop_per_primary_care_physician_sqrt)

cor(train_data_mod5$pop_per_dentist, train_data_mod5$evictions_ratio, use = 'complete')
cor(train_data_mod5$pop_per_primary_care_physician, train_data_mod5$evictions_ratio, use = 'complete')

cor(train_data_mod5$pop_per_dentist_sqrt, train_data_mod5$evictions_ratio, use = 'complete')
cor(train_data_mod5$pop_per_primary_care_physician_sqrt, train_data_mod5$evictions_ratio, use = 'complete')

plot_scatter(train_data_mod5, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', 'pop_per_dentist_sqrt')
plot_scatter(train_data_mod5, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', 'pop_per_dentist')
plot_scatter(train_data_mod5, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', 'pop_per_primary_care_physician')
plot_scatter(train_data_mod5, col_y = 'evictions_ratio', shape = 'geo_simple', color = 'economic_typology', 'pop_per_primary_care_physician_sqrt')
plot_scatter(train_data_mod5, col_y = 'evictions_ratio_sqrt', shape = 'geo_simple', color = 'economic_typology', 'pop_per_primary_care_physician_sqrt')


