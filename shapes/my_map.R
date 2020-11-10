url2 <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx"

# Interactive graph: hower to get date and figure

GET(url2, write_disk(tf <- tempfile(fileext = ".xlsx")))

covid_ecdc <- read_excel(tf)

#pointed to the shape file
nuts_eu <- "NUTS_RG_60M_2021_4326.shp"

#used the st_read() function to import it
nuts_eu <- st_read(nuts_eu)

# take a look at the file
View(nuts_eu)

# plot the disstricts 
ggplot(nuts_eu) + 
  geom_sf()

names(covid_ecdc)

df_eu <- covid_ecdc %>% 
  filter(continentExp == "Europe")

my_map <- left_join(nuts_eu, df_eu,
                    by = c("CNTR_CODE" = "geoId"))

ggplot(my_map) +
  geom_sf(aes(fill=popData2019)) +
  scale_fill_distiller(direction = 1, name = "Population") +
  labs(title="Population of Europian Countries", caption="Source: ecdc")


