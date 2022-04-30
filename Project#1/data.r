library(tidyverse)

covid <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
hospital <- read_csv("data.csv")
demo <- read_csv("demographics.csv")

# Data wrangling for vaccinations data. Created column for dates and 
# confirmed vaccinations shots
covid %>% 
  pivot_longer(cols = -one_of('UID', 'iso2', 'iso3', 'code3', 'Admin2',
                              'Province_State', 'Country_Region', 'Lat', 
                              'Long_', 'Combined_Key', 'Population'),
               names_to='date',values_to= 'confirmed') -> covid
covid

# New variable to calculate the vaccination rate (confirmed/ Population)
vacRate <- covid %>% mutate(vacRate = confirmed / Population)

# Removed unnecessary data
hello <- vacRate %>% filter(!is.na(confirmed), !is.na(vacRate), vacRate > 0, 
                            confirmed > 0)

# New variable to calculate number of days since last non-zero vaccination shot
value <- hello %>% 
  group_by(Combined_Key, 
           group = cumsum(date = lag(date, default = "12/31/2021"))) %>% 
  mutate(daysSinceStart = row_number()) %>% ungroup %>% select(-group)

# Completed data wrangling vaccination data
value

demo_tidy <- demo %>% select(-`Series Name`) %>% 
  pivot_wider(names_from = "Series Code", values_from = YR2015)

demo_merged <- demo_tidy %>% 
  mutate(SP.POP.80UP=SP.POP.80UP.FE+SP.POP.80UP.MA) %>% 
  mutate(SP.POP.1564.IN=SP.POP.1564.MA.IN+SP.POP.1564.FE.IN) %>% 
  mutate(SP.POP.0014.IN=SP.POP.0014.MA.IN+SP.POP.0014.FE.IN) %>% 
  mutate(SP.DYN.AMRT=SP.DYN.AMRT.MA+SP.DYN.AMRT.FE) %>% 
  mutate(SP.POP.TOTL.IN=SP.POP.TOTL.FE.IN+SP.POP.TOTL.MA.IN) %>% 
  mutate(SP.POP.65UP.IN=SP.POP.65UP.FE.IN+SP.POP.65UP.MA.IN) %>% 
  select(-contains(".FE")) %>% select(-contains(".MA"))

names(demo_merged) <- gsub(" ", "_", names(demo_merged))

# Completed demographics wrangling vaccination data
demo_merged

#hospital <- filter(hospital, Year == 2019)

# Completed hospital wrangling vaccination data
hospital


# Changed name to South Korea
demo_merged <- demo_merged %>% 
  mutate(Country_Name = replace(Country_Name , Country_Name  == 
                                  "Korea, Dem. People's Rep.", "South Korea"))
demo_merged <- demo_merged %>% 
  mutate(Country_Name = replace(Country_Name, Country_Name == "Korea, Rep.", 
                                "South Korea"))

# Changed name to Bahamas
demo_merged <- demo_merged %>% 
  mutate(Country_Name = replace(Country_Name, Country_Name == "Bahamas, The", 
                                "Bahamas"))

# Changed name to Iran
demo_merged <- demo_merged %>% 
  mutate(Country_Name = replace(Country_Name, 
                                Country_Name == "Iran, Islamic Rep.", "Iran"))

# Changed name to Hong Kong
demo_merged <- demo_merged %>% 
  mutate(Country_Name = replace(Country_Name, 
                                Country_Name == "Hong Kong SAR, China", 
                                "Hong Kong"))

# Merged demographics data into value
value_joined <- value %>% 
  inner_join(demo_merged, by=c(Country_Region = "Country_Name"))

# Merged hospital data into value
value_joined <- value_joined %>% 
  inner_join(hospital, by=c(Country_Region = "Country"))

# Completed merge of all 3 tables
value_joined

# Linear modeling data, Population vs SP.DYN.LE00.IN
modPopulationDyn <- lm(data=value_joined, formula=Population~SP.DYN.LE00.IN)
summary(modPopulationDyn)

# Linear modeling data, Population vs SP.URB.TOTL
modPopulationUrb <- lm(data=value_joined, formula=Population~SP.URB.TOTL)
summary(modPopulationUrb)

# Linear modeling data, Population vs SP.POP.TOTL
modPopulationUrbPop <- lm(data=value_joined, formula=Population~SP.POP.TOTL)
summary(modPopulationUrbPop)

# Linear modeling data, Population vs SP.POP.80UP
modPopulationUrb80UP <- lm(data=value_joined, formula=Population~SP.POP.80UP)
summary(modPopulationUrb80UP)

# Linear modeling data, Population vs SP.POP.1564.IN
modPopulationUrb80In <- lm(data=value_joined, formula=Population~SP.POP.1564.IN)
summary(modPopulationUrb80In)


# Calculates the most recent vaccination rate per country
valueTable <- value_joined %>% group_by(Country_Region) %>% 
  summarize(daysSinceStart = max(daysSinceStart), vacRate = first(vacRate))

valueTable


# Scatterplot
ggplot(data=valueTable) + geom_point(mapping = aes(x=daysSinceStart, y=vacRate))

ggsave("scatterplot.pdf")

Model <- c(0.06027, 0.3993, 0.5123, 0.2982, 0.4891)

df <- data.frame(Model)

# Bar graph
ggplot(data=df) + geom_bar(mapping = aes(x=Model))

ggsave("bar_graph.pdf")