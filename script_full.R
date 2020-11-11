#-----
# Wuhan
# If you don't have the "leaflet" package installed yet, uncomment and run the line below
#install.packages("leaflet")
library(leaflet)
# Initialize and assign us as the leaflet object
leaflet() %>%
  # add tiles to the leaflet object
  addTiles() %>%  
  # setting the centre of the map and the zoom level
  setView(lng = 114.3055, lat = 30.5928 , zoom = 10) %>%
  # add a popup marker 
  addMarkers(lng = 114.3055, lat = 30.5928, popup = "<b>Wuhan, capital of Central China’s Hubei province</b><br><a href='https://www.ft.com/content/82574e3d-1633-48ad-8afb-71ebb3fe3dee'>China and Covid-19: what went wrong in Wuhan?</a>")
#-----


#install.packages(c("dplyr", "stringr")) # install multiple packages by passing a vector of package names to the function; this function will install the requested packages, along with any of their non-optional dependencies
suppressPackageStartupMessages(library(readxl)) 
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(dbplyr))
suppressPackageStartupMessages(library(tmap))
suppressPackageStartupMessages(library(tmaptools))

###
url2ecdc <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-11-11.xlsx"
suppressMessages(GET(url2ecdc, write_disk(tf <- tempfile(fileext = ".xlsx"))))
covid_world <- read_excel(tf)


#set up the database connection to work on `covid_world` data.
SQLcon <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWriteTable(SQLcon, "covid", covid_world, overwrite=TRUE)


#see what tables are in the database
dbListTables(SQLcon)

#list the fields in a table: 
dbListFields(SQLcon, name = "covid")


#run a query to obtain distinct values for the field "continentExp".
dbFetch(dbSendQuery(SQLcon, "Select distinct continentExp from covid"))


#run a query to count how many entries we have for each continent
dbFetch(
  dbSendQuery(SQLcon,
              "Select continentExp, count(*) as Count  
                 from covid
                 group by continentExp"))


#see how many entries are there for the UK, using a `where` clause.
dbFetch(
  dbSendQuery(SQLcon,
              "Select continentExp, count(*) as Count  
                  from covid
                  Where countriesAndTerritories = 'United_Kingdom'
                  group by continentExp"))


#declare covid as a `tbl` for use with `dplyr`; call it `covid_ecdc` to avoid any confusion 
covid_ecdc <- tbl(SQLcon, "covid")

#glance at data set structure to find out how information it containers is structured
covid_ecdc %>%
  glimpse()


#replicate the above queries using the `dplyr` functions; select `countriesAndTerritories` and continentExp` from `covid_ecdc` data.
head(covid_ecdc %>% 
       select(countriesAndTerritories, continentExp)) # returns first six rows of the vector, i.e. tibble

#the counts of entries for each continent
covid_ecdc %>% 
  group_by(continentExp) %>% 
  tally()
  
#look for the number of entries for the UK
covid_ecdc %>%
  filter(countriesAndTerritories == "United_Kingdom") %>%
  tally()

#total number of readings for each country and present it in a table using the `DT` package. `
if (!require("DT")) install.packages('DT') # returns a logical value say, FALSE if the requested package is not found and TRUE if the package is loaded
tt <- covid_ecdc %>%
  group_by(countriesAndTerritories) %>%
  summarise(no_readings = n()) %>%
  arrange(no_readings)

DT::datatable(data.frame(tt))

# -----------------------
## Tidying Data

#select European countries and Turkey
covid_eu <- rbind(covid_world %>% filter(continentExp == "Europe"), 
                  covid_world %>% filter(countriesAndTerritories == "Turkey"))  

DT::datatable(covid_eu)


#pull the data from the server into R's memory and do required manipulations  
#covid_eu <- covid_ecdc %>% 
#  filter(continentExp == "Europe") %>% 
#  collect()
#DT::datatable(covid_eu)

# -----------------------
# --- tidy data ---
glimpse(covid_eu)
#covid_eu <- covid_eu[, -c(2:4)] # remove redundant information
covid_eu <- covid_eu %>% 
  separate(dateRep, c("dateRep"), sep = "T") %>%
  group_by(countriesAndTerritories) %>% 
  arrange(dateRep) %>% 
  mutate(total_cases = cumsum(cases), 
         total_deaths = cumsum(deaths)) %>% 
  mutate(Diff_cases = total_cases - lag(total_cases),  # 1st derivative (same as cases)
         Rate_pc_cases = round(Diff_cases/lag(total_cases) * 100, 2)) %>% # rate of change
  mutate(second_der = Diff_cases - lag(Diff_cases)) %>% # 2nd derivative
  rename(country = countriesAndTerritories) %>% 
  rename(country_code = countryterritoryCode) %>% 
  rename(Fx14dper100K = "Cumulative_number_for_14_days_of_COVID-19_cases_per_100000") %>% 
  mutate(Fx14dper100K = round(Fx14dper100K))

covid_eu$dateRep <- as.Date(covid_eu$dateRep)
head(covid_eu) # returns first six rows of the df

# -----------------------
# Writing Functions

# function for filltering a country from the given df
sep_country <- function(df, ccode){ 
  df_c <- df %>% 
    filter(country_code == as.character(ccode))
  return(df_c)
}

# plotting the 2nd derivative
sec_der_plot <- function(df){
  df %>% 
  filter(!is.na(second_der)) %>% 
    ggplot(aes(x = dateRep, y = second_der)) +
    geom_line() + geom_point(col = "#00688B") +
    xlab("") + ylab("") +
    labs (title = "2nd derivative of F(x)", 
          caption = "Data from: https://www.ecdc.europa.eu") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}

# -----------------------
# Data Visualisation
## plotly and ggplot

# the time series of daily number of new infection cases and deaths
# plot cases and deaths day-by-day; created using the `plotly` package
covid_uk <- sep_country(covid_eu, "GBR")
x <- list(title = "date reported")
fig <- plot_ly(covid_uk, x = ~  dateRep) 
fig <- fig %>% add_trace(y = ~cases, name = 'cases', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~deaths, name = 'deaths', type = 'scatter', mode = 'lines')   
fig <- fig %>% layout(xaxis = x)
fig


#The plot: dynamic changes based on the F(x) 
covid_uk %>% 
  ggplot(aes(x = dateRep, y = total_cases)) +
  geom_bar(stat="identity", fill = "#00688B") + 
  labs (title = "Cumulative number of cases F(x)", 
        caption = "Data from: https://www.ecdc.europa.eu", 
        x = "Date", y = "number of cases") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  theme(legend.position="none") 


# using the line plot; integrates interactivity (`ggplotly()`) 
pl1 <- covid_uk %>% 
  ggplot(aes(x = dateRep, y = total_cases)) +
  geom_line() + geom_point(col = "#00688B") +
  xlab("Date") + ylab("Number of Cases") +
  labs (title = "F(x)", 
        caption = "Data from: https://www.ecdc.europa.eu") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
ggplotly(pl1)


#the cumulative number of covid-19 cases using a logarithmic scale 
pl_log <- covid_uk %>% 
  mutate(log_total_cases = log(total_cases)) %>% 
  ggplot(aes(x = dateRep, y = log_total_cases)) +
  geom_line() + geom_point(col = "#00688B") +
  xlab("") + ylab("") +
  labs (title = "F(x) on log scale", 
        caption = "Data from: https://www.ecdc.europa.eu") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) 
pl_log


#present several plots next to each other using the`plot_grid()` function from the `cowplot` package  
plot_grid(pl1, pl_log)


# the cumulative number of cases for all selected European countries
all_plot <- covid_eu %>% 
  filter(country_code %in% c("GBR", "FRA", "DEU", "ITA", "ESP", "SWE")) %>% 
  filter(dateRep > (max(dateRep) - 21)) %>% 
  ggplot(aes(x = dateRep, y = total_cases, colour = country_code)) +
  geom_line() + 
  xlab("") + ylab("") +
  labs (title = "F(x) in the last three weeks", 
        caption = "Data from: https://www.ecdc.europa.eu") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_date(labels = date_format("%m-%d"),
               breaks = 'day') +
  scale_colour_brewer(palette = "Set1") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90)) 
ggplotly(all_plot)


# same as above using the log scale
covid_eu %>% 
  filter(country_code %in% c("GBR", "FRA", "DEU", "ITA", "ESP", "SWE")) %>% 
  filter(dateRep > (max(dateRep) - 21)) %>% 
  mutate(log_total_cases = log(total_cases)) %>% 
  ggplot(aes(x = dateRep, y = log_total_cases, colour = country_code)) +
  geom_line() + 
  xlab("") + ylab("") +
  labs (title = "logF(x) in the last three weeks", 
        caption = "Data from: https://www.ecdc.europa.eu") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  scale_x_date(labels = date_format("%m-%d"),
               breaks = 'day') +
  scale_colour_brewer(palette = "Set1") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45)) 


#plot the change in the acceleration in relation to the governmental measures
covid_uk %>% 
  filter(!is.na(second_der)) %>% 
  ggplot(aes(x = dateRep, y = second_der)) +
  geom_line() + geom_point(col = "#00688B") +
  xlab("") + ylab("") +
  labs (title = "2nd derivative of F(x) for SR", 
        caption = "Data from: https://www.ecdc.europa.eu") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), linetype = 3, colour = "red", alpha = 0.5) +
  geom_vline(xintercept = as.numeric(as.Date("2020-05-10")), linetype = 3, colour = "dodgerblue4", alpha = 0.5) +
  geom_vline(xintercept = as.numeric(as.Date("2020-07-04")), linetype = 3, colour = "chartreuse4", alpha = 0.5) +
  geom_vline(xintercept = as.numeric(as.Date("2020-11-05")), linetype = 3, colour = "red", alpha = 0.5) +
  annotate(geom="text", x=as.Date("2020-03-23"), y = 8000, 
           label="UK wide lockdown", col = "red") +
  annotate(geom="text", x=as.Date("2020-05-21"), y = 5000, 
           label="lockdown lifting plan", col = "dodgerblue4") +
  annotate(geom="text", x=as.Date("2020-07-04"), y = -5000, 
           label="wide-ranging changes" , col = "chartreuse4") +
  annotate(geom="text", x=as.Date("2020-11-05"), y = 8000, 
           label="UK wide lockdown", col = "red")  
# same plot for France
covid_fr <- sep_country(covid_eu, "FRA") 
sdfr <- sec_der_plot(covid_fr)
ggplotly(sdfr)
# same plot for Germany
covid_de <- sep_country(covid_eu, "DEU") 
sdde <- sec_der_plot(covid_de)
ggplotly(sdde)


#visualise a comparison between these three countries of the total number of deaths month by month
covid_eu %>% 
   filter(country %in% c("United_Kingdom", "Germany", "France")) %>% 
   mutate(mon = month(dateRep, label = TRUE, abbr = TRUE)) %>% 
   group_by(country, mon) %>% 
   summarise(no_readings = n(), tdeath = max(total_deaths)) %>% 
   ggplot(aes(x = mon, y = tdeath, fill = country)) +
   geom_bar(stat="identity", position = "dodge", color = "black") +
   theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5)) +
   labs (title = "total number of deaths by month", 
         caption = "Data from: https://www.ecdc.europa.eu/en", 
         x = "month", y = "number of deaths") +
   scale_fill_brewer(palette="Paired") + 
   theme(legend.position="bottom") 


#the same comparison for the total number of infections
#the spread of the pandemic has started last December
covid_eu %>% 
  filter(country %in% c("United_Kingdom", "Germany", "France")) %>% 
  mutate(mon = month(dateRep, label = TRUE, abbr = TRUE)) %>% 
  mutate(mon = factor(mon, levels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))) %>% 
  group_by(country, mon) %>% 
  summarise(no_readings = n(), tcases = max(total_cases)) %>% 
  ggplot(aes(x = mon, y = tcases, fill = country)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  coord_flip() +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5)) +
  labs (title = "total number of infections by month", 
        caption = "Data from: https://www.ecdc.europa.eu/en", 
        x = "month", y = "number of infections") +
  scale_fill_brewer(palette="Set1") + 
  theme(legend.position="bottom") 


#the total number of infections for each month
covid_eu %>% 
  filter(country %in% c("United_Kingdom", "Germany", "France")) %>% 
  mutate(mon = month(dateRep, label = TRUE, abbr = TRUE)) %>% 
  mutate(mon = factor(mon, levels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))) %>% 
  group_by(country, mon) %>% 
  summarise(month_cases = sum(cases)) %>% 
  ggplot(aes(x = mon, y = month_cases, fill = country)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5)) +
  scale_y_continuous(breaks = seq(0, 800000, 200000), labels = c("0", "200K", "400K", "600K", "800K")) +
  labs (title = "total number of infections each month", 
        caption = "Data from: https://www.ecdc.europa.eu/en", 
        x = "month", y = "number of deaths") +
  scale_fill_brewer(palette="Dark2") + 
  theme(legend.position="bottom") 

#the total number of deaths for each month 
covid_eu %>% 
  filter(country %in% c("United_Kingdom", "Germany", "France")) %>% 
  mutate(mon = month(dateRep, label = TRUE, abbr = TRUE)) %>% 
  mutate(mon = factor(mon, levels=c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))) %>% 
  group_by(country, mon) %>% 
  summarise(month_deaths = sum(deaths)) %>% 
  ggplot(aes(x = mon, y = month_deaths, fill = country)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  theme(plot.title = element_text(size = 14, vjust = 2, hjust=0.5)) +
  #   geom_text(aes(label = month_cases), size = 3, hjust = 0.5) +  
  labs (title = "total number of deaths each month", 
        caption = "Data from: https://www.ecdc.europa.eu/en", 
        x = "month", y = "number of cases") +
  scale_fill_brewer(palette="Accent") + 
  theme(legend.position="bottom") 

# ------------------------------------------
## Spatial Visualisation

# a choropleth: colours the EU countries according to the most current value 
# of cumulative numbers for 14 days of COVID-19 cases per 100000
#points to the shape file
bound <- "shapes/eu_countries_simplified.shp"

#used the st_read() function to import it
bound <- st_read(bound)

# plot the shape file
ggplot(bound) + 
  geom_sf()

covid_EU <- covid_eu %>% 
  filter(dateRep == max(dateRep))


# tidy up
# Make the country names correspond to ecdc data 
bound$country <- gsub(" ", "_", bound$country)
bound <- bound %>% 
  mutate(country = fct_recode(country,
                              "Czechia" = "Czech_Republic",
                              "North_Macedonia" = "Macedonia"))

# join data from the two data frames  
my_map <- left_join(bound, covid_EU,
                    by = c("country" = "country"))

# plot the choropleth
ggplot(my_map) +
  geom_sf(aes(fill = Fx14dper100K)) +
  scale_fill_distiller(direction = 1, name = "Fx14per100K") +
  labs(title="Cumulative number for 14 days of COVID-19 cases per 100000", caption="Source: ecdc")

# have a look at the joined data
DT::datatable(my_map)

# the same using the `tmap` package
my_map <- my_map %>% 
  mutate(ln_deaths = log(deaths)^10)

tmap_mode(mode =  "view")

tm_shape(my_map) +
  tm_polygons("Fx14dper100K", 
              id = "country", 
              palette = "YlGn", 
              popup.vars=c("cases", 
                           "deaths")) +
  tm_layout(title = "Covid-19 EU</b><br>data source: <a href='https://www.ecdc.europa.eu/en/covid-19-pandemic'>ECDC</a>",
            frame = FALSE,
            inner.margins = c(0.1, 0.1, 0.05, 0.05)) 



#disconnect from the database. 
dbDisconnect(SQLcon)