library(tidycensus); library(tigris); library(sf); library(dplyr); library(tidyr); library(leaflet); library(ggplot2)
options(tigris_use_cache = TRUE)
YR <- 2024   # 2020-2024 5-year ACS

# --- 1. Find the Central PUMA ----------------------------------------------
pumas <- pumas("DC", year = 2022) |>
  rename(any_of(c(PUMA = "PUMACE20", NAME = "NAMELSAD20")))
central <- filter(pumas, grepl("(Central", NAME, fixed = T))
print(st_drop_geometry(central)[, c("PUMA", "NAME")])   # confirm it's the one you mean

# --- 2. PUMS households in that PUMA ---------------------------------------
pums <- get_pums(variables = c("PUMA", "VEH", "HINCP", "ADJINC", "BLD", "AGEP"),
                 state = "DC", survey = "acs5", year = YR)

hh <- pums |>
  filter(SPORDER == 1, WGTP > 0, PUMA %in% central$PUMA) |>      # one row per household
  mutate(veh = suppressWarnings(as.numeric(VEH)),
         adj = as.numeric(ADJINC), adj = ifelse(adj > 100, adj / 1e6, adj),
         inc = as.numeric(HINCP) * adj,                           # 2024 dollars
         cars = ifelse(veh == 0, "No vehicle", "1+ vehicles"),
         bld = case_when(
           as.numeric(BLD) == 2      ~ "Single-family detached",
           as.numeric(BLD) == 3      ~ "Single-family attached (rowhouse)",
           as.numeric(BLD) %in% 4:5  ~ "2-4 units",
           as.numeric(BLD) %in% 6:7  ~ "5-19 units",
           as.numeric(BLD) %in% 8:9  ~ "20+ units",
           TRUE                      ~ "Mobile home / boat / RV / other"),
         inc_grp = cut(inc, c(-Inf, 25, 50, 75, 100, 150, 200, Inf) * 1e3, right = FALSE,
                       labels = c("<$25k", "$25-50k", "$50-75k", "$75-100k",
                                  "$100-150k", "$150-200k", "$200k+"))) |>
  filter(!is.na(veh), !is.na(inc))

# % of households in each income group, within each vehicle group
hh |> count(cars, inc_grp, wt = WGTP) |>
  group_by(cars) |> mutate(pct = round(100 * n / sum(n), 1)) |> ungroup() |>
  pivot_wider(id_cols = inc_grp, names_from = cars, values_from = pct)


# Among HH with less than $50k in income, how many have no car?
hh |> filter(inc_grp %in% c("<$25k", "$25-50k")) |>
  count(cars, wt = WGTP) |>
  mutate(pct = round(100 * n / sum(n), 1)) |> ungroup()

# Among HH with more than $150k in income, how many have no car?
hh |> filter(inc_grp %in% c("$150-200k", "$200k+")) |>
  count(cars, wt = WGTP) |>
  mutate(pct = round(100 * n / sum(n), 1)) |> ungroup()


# Among HH with less than $50k in income and a car, how many lived in each type of unit?
low_inc_and_car_and_big_blg <- 
  hh |> filter(inc_grp %in% c("<$25k", 
                              "$25-50k", "$50-75k"
                              ), cars == "1+ vehicles", AGEP >= 55) |>
  group_by(bld) |>
  summarise(households = sum(WGTP), sample_n = n()) |>
  mutate(pct = round(100 * households / sum(households), 1)) |>
  arrange(desc(households))

low_inc_and_car_and_big_blg

low_inc_and_car_and_big_blg_hh <- 
  low_inc_and_car_and_big_blg |> filter(bld %in% c("20+ units", "5-19 units")) |>
  summarise(hh = sum(households)) |> pull(hh)

total_number_of_hh <- hh |> summarise(households = sum(WGTP)) |> pull(households)
total_number_of_hh

low_inc_and_car_and_big_blg_hh / total_number_of_hh

to_plot <- data.frame(var=c("All households", 'low-income seniors with a car\nlikely w/o free parking'), val=c(total_number_of_hh, low_inc_and_car_and_big_blg_hh))

ggplot(to_plot, aes(x=var, y=val)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=format(val, big.mark=",")), vjust=-.5) +
  theme_minimal() +
  labs(x="", y="Number of households", 
       title = "In Ward 1, low-income, car-owning seniors lacking off-street parking\nare 2.7% of households") +
  theme(axis.text.x = element_text(size = 14))



# Household counts, weighted medians, and unweighted sample sizes
wmed <- function(x, w) { o <- order(x); x[o][which(cumsum(w[o]) >= sum(w) / 2)[1]] }
to_plot <- hh |> group_by(cars) |>
  summarise(households = sum(WGTP), median_income = wmed(inc, WGTP), sample_n = n())


ggplot(to_plot, aes(x=cars, y=median_income)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=paste0("$", format(round(median_income), big.mark = ",", scientific = FALSE))), vjust = -.5) +
  theme_minimal() +
  labs(x="", y="Median household income", title = "Car-owning households' typical income is almost twice\nas high as car-free households' typical income")



# --- 3. Map: Central PUMA vs. Ward 1 ---------------------------------------
ward1 <- st_read(paste0(
  "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/",
  "Administrative_Other_Boundaries_WebMercator/MapServer/53/query",
  "?where=WARD%3D1&outFields=WARD&outSR=4326&f=geojson"), quiet = TRUE)

leaflet() |> addProviderTiles(providers$Esri.WorldGrayCanvas) |>
  addPolygons(data = st_transform(pumas, 4326), weight = 1, color = "#888",
              fillOpacity = 0.05, popup = ~paste(PUMA, NAME)) |>
  addPolygons(data = st_transform(central, 4326), color = "#d7301f", weight = 3,
              fillOpacity = 0.25, popup = ~paste(PUMA, NAME)) |>
  addPolylines(data = ward1, color = "black", weight = 2, dashArray = "6") |>
  addLegend(colors = c("#d7301f", "black"),
            labels = c("Central PUMA (data used)", "Ward 1 (2022), for reference"))



















# DC-wide income-vehicle-ownership distribution:

pums <- get_pums(variables = c("VEH", "HINCP", "ADJINC", "TYPEHUGQ"),
                 state = "DC", survey = "acs5", year = 2024)

brks <- c(-Inf, 25, 50, 75, 100, 150, 200, 300, Inf) * 1e3
labs <- c("<$25k", "$25–50k", "$50–75k", "$75–100k", "$100–150k",
          "$150–200k", "$200–300k", "$300k+")

hh <- pums |>
  filter(SPORDER == 1, TYPEHUGQ == "1", WGTP > 0) |>       # one row per occupied household
  mutate(veh = suppressWarnings(as.numeric(VEH)),
         adj = as.numeric(ADJINC), adj = if_else(adj > 100, adj / 1e6, adj),
         inc = as.numeric(HINCP) * adj,                     # 2024 dollars
         inc_grp = cut(inc, brks, labs, right = FALSE),
         veh_grp = factor(pmin(veh, 3), 0:3,
                          c("No vehicle", "1 vehicle", "2 vehicles", "3+"))) |>
  filter(!is.na(veh), !is.na(inc))

avg <- hh |>
  group_by(inc_grp) |>
  summarise(avg = weighted.mean(veh, WGTP)) |>
  mutate(xlab = paste0(inc_grp, "\navg ", sprintf("%.2f", avg)))

shares <- hh |>
  count(inc_grp, veh_grp, wt = WGTP) |>
  group_by(inc_grp) |> mutate(share = n / sum(n)) |> ungroup() |>
  left_join(avg, by = "inc_grp") |>
  mutate(xlab = forcats::fct_reorder(xlab, as.integer(inc_grp)),
         veh_grp = forcats::fct_rev(veh_grp))                       # "No vehicle" stacks at bottom

ggplot(shares, aes(xlab, share, fill = veh_grp, group = veh_grp)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.5) +
  geom_text(aes(label = if_else(share >= 0.07, scales::percent(share, 1), ""),
                color = veh_grp == "No vehicle"),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("No vehicle" = "#86b6ef", "1 vehicle" = "#3987e5",
                               "2 vehicles" = "#1c5cab", "3+" = "#0d366b"),
                    breaks = c("No vehicle", "1 vehicle", "2 vehicles", "3+"), name = NULL) +
  scale_color_manual(values = c(`TRUE` = "#1a1a1a", `FALSE` = "white"), guide = "none") +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(x = NULL, y = NULL,
       title = "Higher-income DC households own more cars",
       subtitle = "Share of households by number of vehicles, by household income (2024 dollars); avg = vehicles per household",
       caption = "Source: ACS 2020–2024 5-year PUMS, DC. Occupied housing units, weighted by WGTP.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
