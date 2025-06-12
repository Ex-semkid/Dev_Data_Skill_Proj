# Loading necessary packages on R script. My first project on R. 
## It was quite rough.. Lol ##

#if(!require(pacman)) install.packages('pacman')
pacman:: p_load(
  "rmarkdown", "tinytex", "tidyverse", "remotes", 
  "knitr", "flexdashboard","prettydoc", "rmdformats", 
  "kableExtra", "inspectdf", "plotly", "visdat", "here", 
  "janitor", "flexdashboard", "patchwork", "finalfit")



# Importing and loading Hospital Aggregates Dataset in a directory
Hospital_Aggregates_raw <- read_csv("Data/HU_HA.csv")
head(Hospital_Aggregates_raw)


list.files("Scripts")


# familiarizing and understanding the dataset
head(Hospital_Aggregates_raw, 7)
str(Hospital_Aggregates_raw) # variable type
vis_dat(Hospital_Aggregates_raw) # Visualize variable types
distinct(Hospital_Aggregates_raw , Variable) # No. of Variables present 


# Tabulating Variable Column as observations and Year as Variables
Hospital_Aggregates_raw %>% 
tabyl(Variable, Year)



# names(Hospital_Aggregates_raw)
# names(Hospital_Aggregates_raw) [1]


data_hg <- Hospital_Aggregates_raw %>% 
  select(Year, Country, Variable, Measure, Value) %>%
  filter(Variable %in% c("Inpatient care average length of stay (all hospitals)", 
                         "Curative care average length of stay")) %>% 
  clean_names() %>% 
  filter(!(year %in% c(2021:2022)) & !(country %in% c("Brazil",
                                                      "Bulgaria",
                                                      "China (People's Republic of)", 
                                                      "Croatia",
                                                      "Romania",
                                                      "Russia",
                                                      "South Africa"))) %>%# exclude all non-oecd countries
  drop_na() %>% 
  arrange(year)

data_hg %>% 
  distinct(measure)

data_hg %>% 
  tabyl(country) %>%
  filter(n != 22)
   

mean_year <- data_hg %>% 
  filter(!(country %in% c("Canada",
                   "Chile",
                   "Colombia", 
                   "Denmark",
                   "Greece",
                   "Mexico",
                   "Netherlands",
                   "Slovak Republic"))) %>%
  group_by(year, variable, measure) %>% 
  summarize(mean_value = mean(value)) %>% 
  mutate(mean_value = round(mean_value, 1)) %>% 
  ungroup()


gap_years <- seq(from = 2010, to = 2020, by = 1)

mean_year %>% 
  ggplot(mean_year, mapping = aes(x = year, y = mean_value, color = variable)) +
  geom_line(linewidth = 1) + geom_point(color = "black") + 
  scale_x_continuous(breaks = gap_years) + 
  geom_text(aes(label = mean_value, vjust = -0.3)) + 
  theme(legend.title = element_blank()) +
  labs(title = "AVERAGE LENGTH OF STAY",
       subtitle = "Curative care ALOS vs Impatient care ALOS in the Last Decade",
       x = "Year", y = "Average Days",
       caption = "source: https://stats.oecd.org
       Accessed Sunday 7th November, 2023")



# Pivoting from long to wide data
wide_data <- Hospital_Aggregates_raw %>%
  pivot_wider(names_from = c(Variable, Measure),
              values_from = Value, 
              id_cols = c(Year, Country)) %>% 
  clean_names() # clean names to lower and snake case for easy manipulation


clean_data <- wide_data %>%
  select(year, country, 
         inpatient_care_alos = inpatient_care_average_length_of_stay_all_hospitals_days, 
         curative_care_alos = curative_care_average_length_of_stay_days) %>%
  filter(!(year %in% c(2021:2022)) & !(country %in% c("Brazil",
                                                      "Bulgaria",
                                                      "China (People's Republic of)", 
                                                      "Croatia",
                                                      "Romania",
                                                      "Russia",
                                                      "South Africa"))) %>% # exclude year: 2021-2022 a& all non-oecd countries
  drop_na() %>% # Drop all rows with NA values
  arrange(country) # Arrange by ascending order by default


# Check and view for missing data either either in Impatient ALOS or Curative ALOS
clean_data %>% 
  tabyl(country, year) %>% 
  View()

oecd_countries <- clean_data %>% # Both countries had empty values in 1 or more years 
filter(!(country %in% c("Greece", "Slovak Republic"))) 

oecd_countries %>%
  tabyl(country, year) %>% 
  View()


oecd_countries %>%
  filter(country %in% c(TC<rkiye, Japan, Korea, Portugal))







# Average Length of Stay----

# Time series representation of Inpatient care ALOS in the OECD Economies with the last decade(2010-2021)
gap_years <- seq(from = 2010, to = 2020, by = 1)

ggplot(oecd_countries, mapping = aes(x = year, y = inpatient_care_alos,
                                     color = country)) +
  geom_line(linewidth = 1) + geom_point() + scale_x_continuous(breaks = gap_years) +
  geom_text(aes(label = inpatient_care_alos, vjust = -0.3)) + 
  labs(title = "Inpatient Care Average Length of Stay", 
       subtitle = "Inpatient Care ALOS across the OECD Countries in Days (2010-2021)",
       x = "Year", y = "Inpatient Care Average Lenght of Stay", 
       caption = "Source: https://stats.oecd.org")


# Curative Care Average Length of Stay----

# Time series representation of Curative care ALOS in the OECD Economies with the last decade(2010-2021)
ggplot(oecd_countries, mapping = aes(x = year, y = curative_care_alos,
                                     color = country)) + 
  geom_line(size = 1) + geom_point() + scale_x_continuous(breaks = gap_years) + 
  geom_text(aes(label = curative_care_alos, vjust = -0.3)) + 
  labs(title = "Inpatient Care Average Length of Stay", 
       subtitle = "Inpatient Care ALOS across the OECD Countries in Days (2010-2021)",
       x = "Year", y = "Curative Care Average Lenght of Stay", 
       caption = "Source: https://stats.oecd.org")
    



##  Finding the Mean Inpatient care Alos and Mean Curative care Alos across the OECD Economies
##  Grouping by country and summarizing  
mean_oecd_countries <- oecd_countries %>% 
  group_by(country) %>% 
  summarise(mean_inpatient_care_alos = mean(inpatient_care_alos), 
            mean_curative_care_alos = mean(curative_care_alos)) %>% 
  mutate(mean_inpatient_care_alos = round(mean_inpatient_care_alos, 1), 
         mean_curative_care_alos = round(mean_curative_care_alos, 1)) %>% 
  arrange(country)



oecd_countries %>% 
summary_factorlist(dependent = "inpatient_care_alos", 
                   explanatory = "curative_care_alos",
                   p = TRUE,
                   column = TRUE)


# Mean Inpatient care Average Length of Stay----

## plot representing the Mean Inpatient care Alos across the OECD Economies from 2010-2021 
ggplot(mean_oecd_countries, mapping = aes(x = reorder(country, mean_inpatient_care_alos), 
                                          y = mean_inpatient_care_alos, 
                                    group = country, fill = country)) + 
    geom_bar(position = "dodge", stat = "identity") + coord_flip() + 
    geom_text(aes(label = mean_inpatient_care_alos, vjust = 0.4, hjust = -0.1)) +
    labs(title = "Inpatient Care Average Length of Stay", 
         subtitle = "Mean Inpatient Care ALOS across the OECD Countries in Days",
         x = "OECD Countries", y = "Mean Inpatient Care Average Lenght of Stay")


# Mean Curative Care Average Length of Stay----

## plot representing the Mean Curative care Alos across the OECD Economies from 2010-2021 
  ggplot(mean_oecd_countries, mapping = aes(x = reorder(country, mean_curative_care_alos), 
                                            y = mean_curative_care_alos, 
                                            group = country, fill = country)) +
    geom_bar(position = "dodge", stat = "identity") + coord_flip() + 
    geom_text(aes(label = mean_curative_care_alos, vjust = 0.4, hjust = -0.1)) +  
    labs(title = "Curative Care Average Length of Stay", title.hjust = 2,
         subtitle = "Mean Curative Care ALOS across the OECD Countries in Days", 
         x = "OECD Countries", y = "Mean Curative Care Average Lenght of Stay")

#----



mean_match <- mean_oecd_countries %>% 
  filter(mean_inpatient_care_alos > 8.9 & mean_curative_care_alos > 6.8)


# Mean matched Inpatient care Average Length of Stay----

## plot representing the Mean Inpatient care Alos across the OECD Economies from 2010-2021 
mmi <- ggplot(mean_match, mapping = aes(x = reorder(country, mean_inpatient_care_alos), 
                                          y = mean_inpatient_care_alos, 
                                          group = country, fill = country)) + 
  geom_col() + 
  geom_text(aes(label = mean_inpatient_care_alos, vjust = -0.4)) +
  theme(legend.position = "none") +
  labs(title = "Inpatient Care Average Length of Stay", 
       subtitle = "Mean Inpatient Care ALOS in Days with the last decade",
       x = "OECD Countries", y = "Mean Inpatient Care ALOS")


# Mean matched Curative Care Average Length of Stay----

## plot representing the Mean Curative care Alos across the OECD Economies from 2010-2021 
mmc <- ggplot(mean_match, mapping = aes(x = reorder(country, mean_curative_care_alos), 
                                          y = mean_curative_care_alos, 
                                          group = country, fill = country)) +
  geom_col() + 
  theme(legend.position = "none") +
  geom_text(aes(label = mean_curative_care_alos, vjust = -0.4)) +  
  labs(title = "Curative Care Average Length of Stay",
       subtitle = "Mean Curative Care ALOS in Days with the last decade", 
       x = "OECD Countries", y = "Mean Curative Care ALOS")


mmi + mmc +
  plot_annotation(title = "High Average Length of Stay",
                  subtitle = "Countries with the Above Normal ALOS in the last decade", 
                  tag_levels = "A") +
  plot_layout(guides = "collect")





match_year <- oecd_countries %>% 
  filter(country %in% c("Japan", "Korea", "Portugal", "Luxembourg", "Germany")) %>% 
  group_by(year, country) %>% 
  summarise(mean_inpatient_care_alos = mean(inpatient_care_alos), 
            mean_curative_care_alos = mean(curative_care_alos)) %>% 
  mutate(mean_inpatient_care_alos = round(mean_inpatient_care_alos, 1), 
         mean_curative_care_alos = round(mean_curative_care_alos, 1)) %>% 
  arrange(country)


# Line graph inpatient

miy <- ggplot(match_year, mapping = aes(x = year, y = mean_inpatient_care_alos,
                                     color = country)) +
  geom_line(linewidth = 1) + geom_point() + scale_x_continuous(breaks = gap_years) +
  geom_text(aes(label = mean_inpatient_care_alos, vjust = -0.3)) + 
  labs(title = "Inpatient Care Average Length of Stay", 
       subtitle = "Inpatient Care ALOS in Days (2010-2020)",
       x = "Year", y = "Inpatient Care Average Lenght of Stay", 
       caption = "Source: https://stats.oecd.org")


# linegraph Curative

## plot representing the Mean Curative care Alos across the OECD Economies from 2010-2021 

mcy <- ggplot(match_year, mapping = aes(x = year, y = mean_curative_care_alos,
                                     color = country)) +
  geom_line(linewidth = 1) + geom_point() + scale_x_continuous(breaks = gap_years) +
  geom_text(aes(label = mean_curative_care_alos, vjust = -0.3)) + 
  labs(title = "Curative Care Average Length of Stay", 
       subtitle = "Curative Care ALOS in Days (2010-2020)",
       x = "Year", y = "Curative Care Average Lenght of Stay", 
       caption = "Source: https://stats.oecd.org")


miy + mcy +
  plot_annotation(title = "Changes in countries with high ALOS",
                  subtitle = "Countries with the Above Normal ALOS in the last decade", 
                  tag_levels = "A") +
  plot_layout(guides = "collect")

#-----

mmi + mmc + miy + mcy +
  plot_annotation(title = "Changes in countries with high ALOS",
                  subtitle = "Countries with the Above Normal ALOS in the last decade", 
                  tag_levels = "A") +
  plot_layout(guides = "collect")



# Including data from both oecd and non-oecd countries in the last decade without dropping any NA or Missing values

allclean_data <- wide_data %>%
  select(year, country, 
         inpatient_care_alos_days = inpatient_care_average_length_of_stay_all_hospitals_days, 
         curative_care_alos_days = curative_care_average_length_of_stay_days) %>%
  filter(!(year %in% c(2021:2022)))


#----
#   Scatter plot

##  Understanding the trend Between average length of stay and curative care average length of stay 
### locally weighted scatter plot smoothing for a trend line in a scatter plot



ggplotly(ggplot(allclean_data, mapping = aes(x = inpatient_care_alos_days, 
                              y = curative_care_alos_days)) +
  geom_point(aes(color = country)) + geom_smooth(method = "glm", se = F) + 
  scale_y_log10() + scale_x_log10() +
    labs(title = "Impatient Care ALOS vs Curative Care ALOS", 
         subtitle = "Trend Between the Curative Care ALOS and Inpatient Care ALOS in the last decade", 
         x = "Curative Care ALOS", y = "Inpatient Care ALOS", 
         caption = "Source: https://stats.oecd.org"))

# A trend line to fit a smooth curve between the curative care average length of stay 
# and the inpatient average length of stay for all OECD Countries in the last decade.

allclean_data %>% 
  summary_factorlist(dependent = "curative_care_alos_days", 
                     explanatory = "inpatient_care_alos_days",
                     p = TRUE,
                     column = TRUE) %>% 
  kable()


######

install.packages("gt")

