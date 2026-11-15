

library(readxl); library(dplyr); library(tidyr); library(purrr)
library(stringr); library(tibble); library(ggplot2); library(scales)
library(forcats)

root_path  <- "~/Github/car-ownership/make_model_analysis"
foia_path  <- file.path(root_path, "FOIA R013826-090926 Responsive Document.xlsx")
codes_path <- file.path(root_path, "Vehicle Model Codes.xlsx")
src <- "Source: FOIA of DC Department of Motor Vehicles."

# One normalizer for everything: squish whitespace, uppercase, drop hyphens
norm <- function(x) str_to_upper(str_remove_all(str_squish(x), "-"))

# ---- DMV counts: one tab per year, pivot layout with blank "same as above" cells
read_year <- function(sheet) {
  x <- read_excel(foia_path, sheet = sheet, skip = 2, col_types = "text",
                  .name_repair = "minimal")[, 1:6]
  names(x) <- c("weight_class", "make_cd", "make", "model_cd", "model_year", "n")
  x |>
    fill(weight_class, make_cd, make, model_cd) |>
    filter(str_detect(model_year, "^\\d{4}$")) |>            # drops Total + blank rows
    mutate(year       = as.integer(sheet),
           model_year = as.integer(model_year),
           n          = as.numeric(n),
           model_cd   = str_remove(str_squish(model_cd), "\\.0$"))
}

dmv <- map_dfr(excel_sheets(foia_path), read_year)

# ---- Model code lookup (drop junk code "0" and duplicate spellings)
codes <- read_excel(codes_path, col_types = "text") |>
  transmute(model_cd = str_trim(VEH_MODEL_CD), model_desc = VEH_MODEL_DESC) |>
  filter(!is.na(model_cd), model_cd != "0") |>
  distinct(model_cd, .keep_all = TRUE)

# ---- Fixes for truncated 2015-17 text codes (write them however; norm() handles hyphens/case)
fixes <- tribble(
  ~make,           ~raw,     ~fixed,
  "TOYOTA",        "COROLL", "COROLLA",
  "TOYOTA",        "4RUNNE", "4RUNNER",
  "HYUNDAI",       "ELANTR", "ELANTRA",
  "SUBARU",        "FOREST", "FORESTER",
  "SUBARU",        "IMPREZ", "IMPREZA",
  "SUBARU",        "OBK",    "OUTBACK",
  "FORD",          "EXPLOR", "EXPLORER",
  "FORD",          "ECONOL", "ECONOLINE",
  "JEEP",          "GRAND",  "GRAND CHEROKEE",
  "JEEP",          "WRANGL", "WRANGLER",
  "JEEP",          "RENEGA", "RENEGADE",
  "MERCEDES-BENZ", "C",      "C-CLASS",
  "MERCEDES-BENZ", "E",      "E-CLASS",
  "LAND ROVER",    "RANGE",  "RANGE ROVER"
) |>
  mutate(across(everything(), norm)) |>
  distinct(make, raw, .keep_all = TRUE)

# ---- Merge, normalize, apply fixes, collapse
dmv_clean <- dmv |>
  left_join(codes, by = "model_cd", relationship = "many-to-one") |>
  mutate(
    # 2015-17 tabs often hold free-text names ("CIVIC", "COROLL") instead of numeric codes
    model_desc = coalesce(model_desc,
                          if_else(str_detect(model_cd, "^\\d+$"), NA_character_, model_cd)),
    make_clean = case_match(str_squish(make),
                            "KIA MOTORS CORPORATION"    ~ "KIA",
                            "GENERAL MOTORS CORP."      ~ "GMC",
                            "AMERICAN GENERAL (HUMMER)" ~ "HUMMER",
                            "RIVA"                      ~ "RIVIAN",
                            .default = str_squish(make)),
    across(c(make_clean, model_desc), norm)
  ) |>
  left_join(fixes, by = c("make_clean" = "make", "model_desc" = "raw"),
            relationship = "many-to-one") |>
  mutate(model_desc = coalesce(fixed, model_desc),
         make_model = str_c(make_clean, " ", model_desc)) |>    # NA stays NA (paste() gives "X NA")
  count(year, weight_class, make_clean, model_desc, make_model, model_year, wt = n)

# ---- Checks
dmv_clean |> count(year, wt = n)                                 # should match each tab's Grand Total
dmv_clean |> summarise(pct_decoded = sum(n[!is.na(model_desc)]) / sum(n), .by = year)




total_2025 <- dmv_clean |> filter(year == 2025) |> summarize(n=sum(n, na.rm=T))
top_val    <- dmv_clean |> filter(year == 2025) |> 
              count(make_model, wt = n, sort = TRUE) |> head(1) |> pull(n)
top_mod    <- dmv_clean |> filter(year == 2025) |> 
  count(make_model, wt = n, sort = TRUE) |> head(1) |> pull(make_model) |>
  stringr::str_to_title()

top_10_mod    <- dmv_clean |> filter(year == 2025) |> 
  count(make_model, wt = n, sort = TRUE) |> head(10) |> pull(make_model)





totals <- dmv_clean |>
  summarise(n = sum(n, na.rm = TRUE), .by = year) |>
  arrange(year)

a <- ggplot(totals, aes(x = year, y = n)) +
  geom_col(fill = "grey30", width = 0.7) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.4, size = 3.6) +
  scale_x_continuous(breaks = totals$year) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.08))) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 10, hjust = 0)) +
  labs(x = "", y = "Vehicles",
       # caption = "Source: FOIA of DC Department of Motor Vehicles."
       title = "Fewer newly registered vehicles over time")



b <- dmv_clean |> filter(year == 2025) |> count(make_model, wt = n, sort = TRUE) |> head(10) |>
  mutate(make_model = ifelse(make_model %in% c("TOYOTA RAV4", "HONDA CRV", "SUBARU FORESTER"), 
                             paste0(make_model, " 🚗"), make_model)) |>
  ggplot(aes(x=n, y=forcats::fct_reorder(make_model, n))) +
  geom_bar(stat="identity") +
  
  labs(y="", 
       x="Number of newly registered vehicles",
       title="Top 10 most common vehicle types among newly registered vehicles in 2025",
       caption=paste0("SUVs are shown with little SUV icons. For scale: in 2025, there were ", top_val, " new ",
                      top_mod, " registrations,\nwhich was ",
                      round(top_val/total_2025*100), "% of new registrations that year.",
                      "Source: FOIA of DC Department of Motor Vehicles")
       ) +
  theme_minimal() +
  lims(x=c(0, 1800)) +
  theme(
    plot.caption = element_text(size=10, hjust = 0),
    plot.title   = element_text(size=14),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
    )

a / b








dmv_clean |>
  filter(stringr::str_detect(pattern = "TESLA", string = make_clean)) |>
  group_by(year) |>
  summarise(n = sum(n, na.rm=T)) |>
  ungroup() |>
  arrange(year) |>
  mutate(pct_change = (n-lag(n)) / lag(n) * 100) |>
  ggplot(aes(x=year, y=n)) +
  geom_bar(stat="identity")






write.csv(x = dmv_clean, 
          file = "make_model_analysis/dmv_data_cleaned.csv", 
          row.names = F)








df <- dmv_clean |>
  group_by(year, make_model) |>
  summarise(n=sum(n, na.rm=T)) |>
  ungroup()



plot_df <- df |>
  filter(make_model %in% top_10_mod, year > 2016)

# Facet order + labels by total volume ("1. Honda Civic", ...)
rank_order <- plot_df |>
  filter(year == 2025) |>
  arrange(desc(n)) |>
  pull(make_model)
plot_df <- plot_df |>
  mutate(panel = factor(make_model, levels = rank_order,
                        labels = paste0(seq_along(rank_order), ". ", str_to_title(rank_order))))

bg <- plot_df |> select(-panel)   # no facet column -> drawn in every panel

ggplot(plot_df, aes(year, n)) +
  geom_line(data = bg, aes(group = make_model), colour = "grey88", linewidth = 0.5) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.4) +
  geom_text(data = \(d) filter(d, year == max(year)), aes(label = comma(n)),
            hjust = -0.25, size = 3.2) +
  geom_text(data = \(d) filter(d, year == min(year)), aes(label = comma(n)),
            vjust = -1.1, size = 3.2) +
  facet_wrap(~panel, ncol = 5) +
  scale_x_continuous(breaks = c(2017, 2021, 2025),
                     expand = expansion(mult = c(0.06, 0.28))) +
  scale_y_continuous(labels = comma, limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "DC's 10 most common make-models, vehicles per year, 2017–2025:\nSUVs up, sedans generally down",
       x = NULL, y = NULL,
       caption = "Source: FOIA of DC Department of Motor Vehicles. Gray lines = the other nine models.") +
  theme_minimal(base_size = 11) +
  coord_cartesian(clip = "off") +
  theme(plot.background    = element_rect(fill = "white", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(colour = "grey94"),
        axis.line.x        = element_line(colour = "grey30"),
        strip.text         = element_text(face = "bold", hjust = 0, size = 11),
        panel.spacing.x = unit(2, "lines"),      
        plot.margin     = margin(10, 25, 10, 10),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "grey40"))




























































## LOAD VEHICLE WEIGHT DATA (v2)

weights_path <- file.path(root_path, "dc_vehicle_weights_epa_v2.xlsx")

# One row per DMV make + model + model year
wts <- read_excel(weights_path, sheet = "Recommended_Weights") |>
  transmute(make_clean = norm(`Make (DMV)`),
            model_desc = norm(`Model (DMV)`),
            model_year = as.integer(`Model year`),
            curb_lb    = `Recommended curb weight (lb)`,
            wt_source  = Source) |>
  distinct(make_clean, model_desc, model_year, .keep_all = TRUE)

# EV flag lives at the make-model level
ev_flag <- read_excel(weights_path, sheet = "Model_Summary") |>
  transmute(make_clean = norm(`Make (DMV)`),
            model_desc = norm(`Model (DMV)`),
            ev         = `EV?` == "Yes") |>
  distinct(make_clean, model_desc, .keep_all = TRUE)

dmv_w <- dmv_clean |>
  mutate(model_year = as.integer(model_year)) |>
  left_join(wts,     by = c("make_clean", "model_desc", "model_year")) |>
  left_join(ev_flag, by = c("make_clean", "model_desc")) |>
  mutate(ev = coalesce(ev, FALSE))   # models with no EPA match are pre-EV-era or gas

# Check: coverage and average curb weight, Class I-III
dmv_w |>
  filter(weight_class %in% c("Class I", "Class II", "Class III"), year >= 2018) |>
  summarise(coverage = sum(n[!is.na(curb_lb)]) / sum(n),
            avg_curb = weighted.mean(curb_lb, n, na.rm = TRUE),
            .by = year) |>
  arrange(year)

# Biggest unmatched models (look for spelling/normalization mismatches)
dmv_w |>
  filter(weight_class != "All Others", year >= 2018, is.na(curb_lb)) |>
  count(make_clean, model_desc, wt = n, sort = TRUE) |>
  head(20)


















ev_plot <- dmv_w |>
  filter(year >= 2018, model_year >= year,
         weight_class %in% c("Class I", "Class II", "Class III")) |>
  summarise(ev_share = sum(n[ev]) / sum(n), .by = year) |>
  arrange(year)

ggplot(ev_plot, aes(x = year, y = ev_share)) +
  geom_col(fill = "grey30", width = 0.7) +
  geom_text(aes(label = scales::percent(ev_share, accuracy = 0.1)),
            vjust = -0.4, size = 3.8) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.08))) +
  scale_x_continuous(breaks = ev_plot$year) +
  theme_minimal() +
  labs(x = "", y = "Share of new vehicles",
       title = "Battery-electric share of new vehicles registered in DC",
       caption = paste("Source: FOIA of DC Department of Motor Vehicles; EV flag from EPA Test Car Lists.",
                       "Class I-III vehicles; new = model year ≥ registration year.")) +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        plot.title   = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank())
















yr0 <- c(2018, 2019)   # baseline
yr1 <- c(2024, 2025)   # end

shares <- dmv_clean |>
  filter(!is.na(model_desc), year >= 2018) |>
  summarise(n = sum(n), .by = c(year, make_model)) |>
  mutate(share = n / sum(n), .by = year)

growth <- shares |>
  mutate(period = case_when(year %in% yr0 ~ "start", year %in% yr1 ~ "end")) |>
  filter(!is.na(period)) |>
  summarise(n = sum(n) / 2, share = sum(share) / 2,          # /2 so a missing year counts as 0
            .by = c(make_model, period)) |>
  pivot_wider(names_from = period, values_from = c(n, share), values_fill = 0) |>
  mutate(share_chg_pp = (share_end - share_start) * 100,     # percentage-point change in share
         ratio        = (n_end + 10) / (n_start + 10))       # +10 so brand-new models aren't Inf

# Two rankings: biggest absolute gain in share vs. fastest relative growth (min. volume filter)
top_abs <- growth |> slice_max(share_chg_pp, n = 15)
top_rel <- growth |> filter(n_end >= 200) |> slice_max(ratio, n = 15)

# Dumbbell plot: share at start (gray) -> end (black)
top_rel |>
  mutate(lab = fct_reorder(str_to_title(make_model), share_end)) |>
  ggplot(aes(y = lab)) +
  geom_segment(aes(x = share_start, xend = share_end, yend = lab), colour = "grey75", linewidth = 1) +
  geom_point(aes(x = share_start), colour = "grey60", size = 2.5) +
  geom_point(aes(x = share_end), colour = "black", size = 2.5) +
  scale_x_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(title = "Make-models that gained the most share of DC vehicles",
       subtitle = "Share of vehicles in the DMV file, 2018–19 average (gray) vs. 2024–25 average (black)",
       x = NULL, y = NULL,
       caption = "Source: DC DMV FOIA R013826.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        plot.title.position = "plot", plot.caption = element_text(hjust = 0, colour = "grey40"))



















yrs  <- 2018:2025                                   # first fully numeric-coded year onward
pass <- c("Class I", "Class II", "Class III")       # cars + light trucks

base <- dmv_clean |> filter(year %in% yrs, weight_class %in% pass)
total_vehicles <- sum(base$n)                       # denominator includes undecoded vehicles

# ---- Top 50 make-models and how much of the fleet they cover
top50 <- base |>
  filter(!is.na(model_desc)) |>
  count(make_clean, model_desc, make_model, wt = n, name = "vehicles", sort = TRUE) |>
  mutate(rank      = row_number(),
         share     = vehicles / total_vehicles,
         cum_share = cumsum(share)) |>
  slice_head(n = 50)

top50 |> select(rank, make_model, vehicles, share, cum_share) |> print(n = 50)
























body <- readr::read_csv("body_type_lookup.csv")         |> mutate(nameplate = norm(nameplate))
harm <- readr::read_csv("nameplate_harmonization.csv")  |> mutate(across(everything(), norm))

dmv_b <- dmv_clean |>
  left_join(harm, by = "make_model") |>
  mutate(nameplate = coalesce(nameplate, make_model)) |>
  left_join(body, by = "nameplate") |>
  mutate(body = replace_na(body, "Unclassified"))





library(dplyr); library(tidyr); library(ggplot2); library(ggrepel)
library(patchwork); library(scales); library(stringr)

# Display names (your norm() stripped hyphens: "HONDA CRV" -> "Honda CR-V")
nice <- function(x) str_to_title(x) |>
  str_replace_all(c("Crv" = "CR-V", "Hrv" = "HR-V", "Rav4" = "RAV4", "Cx(\\d)" = "CX-\\1",
                    "Xc(\\d)" = "XC\\1", "Bmw" = "BMW", "Gmc" = "GMC", "Id\\.4" = "ID.4",
                    "Mercedesbenz" = "Mercedes-Benz", "\\bGlc\\b" = "GLC", "\\bGle\\b" = "GLE",
                    "\\bNx\\b" = "NX", "\\bRx\\b" = "RX", "\\bMdx\\b" = "MDX", "\\bRdx\\b" = "RDX"))



# =====================================================================
# 1. Rank climbers: 2018-19 vs 2024-25, among the 2024-25 top 50
# =====================================================================
dec <- dmv_b |> filter(!is.na(model_desc), year >= 2018)

per <- function(yrs) dec |>
  filter(year %in% yrs) |>
  summarise(n = sum(n) / length(yrs), .by = nameplate)

ranks <- full_join(rename(per(2018:2019), n0 = n),
                   rename(per(2024:2025), n1 = n), by = "nameplate") |>
  mutate(across(c(n0, n1), \(x) replace_na(x, 0)),
         rank0 = if_else(n0 > 0, min_rank(desc(n0)), NA),
         rank1 = min_rank(desc(n1)),
         jump  = rank0 - rank1) |>
  left_join(distinct(dmv_b, nameplate, body), by = "nameplate")

top50    <- ranks |> filter(rank1 <= 50)
new_ones <- top50 |> filter(is.na(rank0) | n0 < 20) |> arrange(rank1)
climbers <- top50 |> anti_join(new_ones, by = "nameplate") |> slice_max(jump, n = 15)
fallers  <- top50 |> anti_join(new_ones, by = "nameplate") |> slice_min(jump, n = 8)

cl_long <- climbers |>
  mutate(label = nice(nameplate), suv = body == "SUV") |>
  pivot_longer(c(rank0, rank1), names_to = "period", values_to = "rank") |>
  mutate(x   = if_else(period == "rank0", 0, 1),
         lab = if_else(x == 0, paste0(label, "  #", rank),
                       paste0("#", rank, "  ", label, "  (+", jump, ")")))

new_note <- paste0("New to the top 50 (no 2018–19 baseline): ",
                   paste0(nice(new_ones$nameplate), " (#", new_ones$rank1, ")", collapse = ", "))

p_rank <- ggplot(cl_long, aes(x, rank, group = nameplate)) +
  geom_line(aes(linetype = suv, colour = suv), linewidth = 0.9) +
  geom_point(shape = 21, fill = "white", size = 2) +
  geom_text_repel(data = \(d) filter(d, x == 0), aes(label = lab),
                  hjust = 1, nudge_x = -0.05, direction = "y", size = 3.3,
                  segment.colour = "grey75", min.segment.length = 0) +
  geom_text_repel(data = \(d) filter(d, x == 1), aes(label = lab),
                  hjust = 0, nudge_x = 0.05, direction = "y", size = 3.3,
                  segment.colour = "grey75", min.segment.length = 0) +
  scale_y_reverse() +
  scale_x_continuous(breaks = 0:1, labels = c("2018–19 rank", "2024–25 rank"),
                     limits = c(-0.85, 1.95)) +
  scale_linetype_manual(values = c(`TRUE` = "solid", `FALSE` = "dashed"), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey45"), guide = "none") +
  labs(title    = "SUV's Biggest climbers in DC's popularity ranking",
       subtitle = "Among the 2024–25 top 50; solid = SUV, dashed = car/other",
       caption  = paste0(new_note, "\n", 
                         "Ranks by average vehicles per year; renamed/variant nameplates combined.\n", src),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, colour = "black"),
        plot.title.position = "plot", plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "grey30"),
        plot.background = element_rect(fill = "white", colour = NA))

p_rank


# =====================================================================
# 2. SUVs vs cars: counts and shares, 2018-2025
# =====================================================================
body_lab <- c(SUV = "SUVs & crossovers", Car = "Cars\n(sedans, hatchbacks, coupes)",
              Pickup = "Pickups", `Van/minivan` = "Vans & minivans")

body_yr <- dmv_b |>
  filter(year >= 2018, body %in% c(names(body_lab), "Other")) |>
  summarise(n = sum(n), .by = c(year, body)) |>
  mutate(share = n / sum(n), .by = year) |>                       # share of classified vehicles
  filter(body %in% names(body_lab)) |>
  mutate(body = factor(body, levels = names(body_lab)),
         main = body %in% c("SUV", "Car"))

line_scales <- list(
  scale_linetype_manual(values = c(SUV = "solid", Car = "dashed", Pickup = "dotted",
                                   `Van/minivan` = "dotdash"), guide = "none"),
  scale_colour_manual(values = c(SUV = "black", Car = "black", Pickup = "grey50",
                                 `Van/minivan` = "grey50"), guide = "none"),
  scale_shape_manual(values = c(SUV = 21, Car = 22, Pickup = 24, `Van/minivan` = 23), guide = "none"),
  scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.7), guide = "none"),
  scale_x_continuous(breaks = 2018:2025),
  theme_minimal(base_size = 12),
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.line.x = element_line(colour = "black"),
        plot.title = element_text(face = "bold", size = 13), plot.title.position = "plot")
)

base_lines <- function(yvar) list(
  geom_line(aes(y = {{ yvar }}, linetype = body, colour = body, linewidth = main)),
  geom_point(aes(y = {{ yvar }}, shape = body, colour = body), fill = "white", size = 2.3)
)

ends <- body_yr |> filter(main, year == max(year))
starts <- body_yr |> filter(main, year == min(year))

p_count <- ggplot(body_yr, aes(year)) +
  base_lines(n) +
  geom_text(data = ends, aes(y = n, label = body_lab[as.character(body)]),
            hjust = 0, nudge_x = 0.2, size = 3.6) +
  annotate("text", x = 2025.2, y = 2600, hjust = 0, size = 3.3, colour = "grey35",
           label = "Pickups (dotted) and vans/minivans\n(dash-dot), ~1,500 each") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  coord_cartesian(xlim = c(2018, 2028.3), clip = "off") +
  labs(title = "Number of newly registered vehicles", x = NULL, y = NULL) +
  line_scales

p_share <- ggplot(body_yr, aes(year)) +
  base_lines(share) +
  geom_text(data = ends,   aes(y = share, label = percent(share, 1)), hjust = 0, nudge_x = 0.2, size = 3.6) +
  geom_text(data = starts, aes(y = share, label = percent(share, 1)), hjust = 1, nudge_x = -0.2, size = 3.6) +
  annotate("text", x = 2025.2, y = 0.06, hjust = 0, size = 3.3, colour = "grey35",
           label = "Pickups and vans,\n~3% each") +
  scale_y_continuous(labels = percent, limits = c(0, 0.7)) +
  coord_cartesian(xlim = c(2017.3, 2026.6), clip = "off") +
  labs(title = "Share of vehicles", x = NULL, y = NULL) +
  line_scales

p_body <- (p_count | p_share) +
  plot_annotation(
    title   = "New registrations of SUVs overtook cars in DC around 2023",
    caption = paste0("Body type hand-classified for the 250 most common nameplates ",
                    "(~84% of vehicles); the rest are excluded from both panels.\n",
                    src),
    theme   = theme(plot.title = element_text(size = 15),
                    plot.caption = element_text(hjust = 0, colour = "grey30"),
                    plot.background = element_rect(fill = "white", colour = NA)))

p_body




























## PLOT: average curb weight of brand-new vehicles

to_plot <- dmv_w |>
  filter(year >= 2018, model_year >= year,
         weight_class %in% c("Class I", "Class II", "Class III")) |>
  summarise(avg_curb_lb = weighted.mean(curb_lb, w = n, na.rm = TRUE), .by = year) |>
  arrange(year) |>
  mutate(myl = if_else(year %in% range(year), scales::comma(round(avg_curb_lb)), NA))

start_val <- first(to_plot$avg_curb_lb)
end_val   <- last(to_plot$avg_curb_lb)

a <- ggplot(to_plot, aes(x = year, y = avg_curb_lb)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_label(aes(label = myl), vjust = -0.5, border.color = NA, na.rm = TRUE) +
  coord_cartesian(ylim = c(3.5e3, 4100), clip = "off") +
  scale_x_continuous(breaks = to_plot$year) +
  theme_minimal() +
  labs(y = "Average curb weight (lbs)", x = "",
       title = paste0("Between ", min(to_plot$year), " and ", max(to_plot$year),
                      ", the average new vehicle registered in DC got ",
                      scales::comma(round(end_val - start_val)), " lbs (~",
                      round((end_val - start_val) / start_val * 100), "%) heavier")
       # caption = paste("Source: FOIA of DC Department of Motor Vehicles; curb weights from EPA",
       #                 "Test Car Lists and carspecs.org. \nClass I-III vehicles; new = model year",
       #                 "≥ registration year.")
       ) +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        plot.title   = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())










# Weighted median (midpoint interpolation, same method as my charts)
wmed <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  approx((cumsum(w) - w / 2) / sum(w), x, xout = 0.5, ties = "ordered")$y
}

hood <- read_excel(file.path(root_path, "hood_heights_v4.xlsx"), sheet = "Hood_heights") |>
  select(rank = 1, make_model = 2, hood_in = 4) |>
  filter(!is.na(rank), !is.na(hood_in)) |>              # drops unmeasured and summary rows
  mutate(key = recode(norm(make_model),
                      "JEEP WRANGLER UNLIMITED" = "JEEP WRANGLER",
                      "MERCEDESBENZ CCLASS"     = "MERCEDESBENZ C",
                      "MERCEDESBENZ ECLASS"     = "MERCEDESBENZ E")) |>
  distinct(key, .keep_all = TRUE)

med <- dmv_b |>
  filter(year >= 2018, weight_class %in% c("Class I", "Class II", "Class III")) |>
  mutate(key = norm(nameplate)) |>
  inner_join(hood, by = "key") |>
  summarise(median_in = wmed(hood_in, n), .by = year) |>
  arrange(year) |>
  mutate(lab = if_else(year %in% range(year), paste0(round(median_in, 1), " in"), NA))

rise <- round(last(med$median_in) - first(med$median_in), 1)

b <- ggplot(med, aes(year, median_in)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  geom_text(aes(label = lab), vjust = -1.2, size = 4.2, na.rm = TRUE) +
  scale_x_continuous(breaks = 2018:2025) +
  scale_y_continuous(labels = \(x) paste0(x, " in")) +
  coord_cartesian(ylim = c(30, 38), clip = "off") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 14),
        plot.caption = element_text(size = 9, hjust = 0)) +
  labs(x = "", y = "Median hood height",
       title = paste0("The median vehicle registered in DC has a hood ", rise,
                      " inches higher than in ", min(med$year)),
       caption = paste("Hood leading-edge height (Euro NCAP 50° straight-edge point),\nmeasured from carsized.com photos",
                       "for the 195 most common nameplates (~82% of Class I–III registrations).",
                       "\nSource: FOIA of DC Department of Motor Vehicles.")
       )




a / b







