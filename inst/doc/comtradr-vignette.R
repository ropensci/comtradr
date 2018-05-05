## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE, fig.width = 9, fig.height = 6)

## ----eval = FALSE--------------------------------------------------------
#  install.packages("comtradr")

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("ChrisMuir/comtradr")

## ------------------------------------------------------------------------
library(comtradr)

## ---- echo = FALSE-------------------------------------------------------
v_data_1 <- system.file("extdata", "vignette_data_1.rda", package = "comtradr")
if (!file.exists(v_data_1)) {
  stop("internal vignette data set '~/extdata/vignette_data_1.rda' not found", call. = FALSE)
}
load(v_data_1)

## ---- eval = FALSE-------------------------------------------------------
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports")

## ------------------------------------------------------------------------
str(q)

## ---- eval = FALSE-------------------------------------------------------
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports",
#                 start_date = 2010,
#                 end_date = 2014)

## ---- eval = FALSE-------------------------------------------------------
#  # Get all monthly data for a single year (API max of 12 months per call).
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports",
#                 start_date = 2012,
#                 end_date = 2012,
#                 freq = "monthly")
#  
#  # Get monthly data for a specific span of months (API max of five months per call).
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports",
#                 start_date = "2012-03",
#                 end_date = "2012-07",
#                 freq = "monthly")

## ------------------------------------------------------------------------
ct_country_lookup("korea", "reporter")
ct_country_lookup("bolivia", "partner")

## ---- eval = FALSE-------------------------------------------------------
#  q <- ct_search(reporters = "Rep. of Korea",
#                 partners = "Bolivia (Plurinational State of)",
#                 trade_direction = "all")

## ------------------------------------------------------------------------
ct_commodity_lookup("tomato")

## ---- eval = FALSE-------------------------------------------------------
#  tomato_codes <- ct_commodity_lookup("tomato",
#                                      return_code = TRUE,
#                                      return_char = TRUE)
#  
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Mexico"),
#                 trade_direction = "all",
#                 commod_codes = tomato_codes)

## ---- eval = FALSE-------------------------------------------------------
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Mexico"),
#                 trade_direction = "all",
#                 commod_codes = c("0702", "070200", "2002", "200210", "200290"))

## ------------------------------------------------------------------------
# The url of the API call.
attributes(q)$url
# The date-time of the API call.
attributes(q)$time_stamp

# The total duration of the API call, in seconds.
attributes(q)$req_duration

## ------------------------------------------------------------------------
ct_country_lookup(c("Belgium", "vietnam", "brazil"), "reporter")

ct_commodity_lookup(c("tomato", "trout"), return_char = TRUE)

## ------------------------------------------------------------------------
ct_commodity_lookup(c("tomato", "trout"), return_char = FALSE)

## ------------------------------------------------------------------------
ct_commodity_lookup(c("tomato", "sldfkjkfdsklsd"), verbose = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  ct_update_databases()
#  #> All DB's are up to date, no action required

## ------------------------------------------------------------------------
ct_commodity_db_type()

## ------------------------------------------------------------------------
# Column headers returned from function ct_search
colnames(q)

## ------------------------------------------------------------------------
# Apply polished column headers
q <- ct_use_pretty_cols(q)

# Print new column headers.
colnames(q)

## ---- echo = FALSE-------------------------------------------------------
v_data_2 <- system.file("extdata", "vignette_data_2.rda", package = "comtradr")
if (!file.exists(v_data_2)) {
  stop("internal vignette data set '~/extdata/vignette_data_2.rda' not found", call. = FALSE)
}
load(v_data_2)

## ---- eval = FALSE-------------------------------------------------------
#  # Comtrade api query.
#  df <- ct_search(reporters = "China",
#                  partners = c("Rep. of Korea", "USA", "Mexico"),
#                  trade_direction = "exports")

## ---- warning = FALSE, message = FALSE-----------------------------------
library(ggplot2)

# Apply polished col headers.
df <- ct_use_pretty_cols(df)

# Create plot.
ggplot(df, aes(Year, `Trade Value usd`, color = factor(`Partner Country`), 
               shape = factor(`Partner Country`))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(min(df$Year), max(df$Year)), 
                     breaks = seq.int(min(df$Year), max(df$Year), 2)) +
  scale_color_manual(values = c("orange", "blue", "red"), 
                     name = "Destination\nCountry") +
  scale_shape_discrete(name = "Destination\nCountry") +
  labs(title = "Total Value (USD) of Chinese Exports, by Year") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## ---- echo = FALSE-------------------------------------------------------
v_data_3 <- system.file("extdata", "vignette_data_3.rda", package = "comtradr")
if (!file.exists(v_data_3)) {
  stop("internal vignette data set '~/extdata/vignette_data_3.rda' not found", call. = FALSE)
}
load(v_data_3)

## ---- eval = FALSE-------------------------------------------------------
#  # First, collect commodity codes related to shrimp.
#  shrimp_codes <- ct_commodity_lookup("shrimp",
#                                      return_code = TRUE,
#                                      return_char = TRUE)
#  
#  # Comtrade api query.
#  df <- ct_search(reporters = "Thailand",
#                  partners = "All",
#                  trade_direction = "exports",
#                  start_date = 2007,
#                  end_date = 2011,
#                  commod_codes = shrimp_codes)

## ---- warning = FALSE, message = FALSE-----------------------------------
library(ggplot2)
library(dplyr)

# Apply polished col headers.
df <- ct_use_pretty_cols(df)

# Create country specific "total weight per year" dataframe for plotting.
plotdf <- df %>% 
  group_by_(.dots = c("`Partner Country`", "Year")) %>% 
  summarise(kg = as.numeric(sum(`Net Weight kg`, na.rm = TRUE))) %>% 
  as_data_frame()

# Get vector of the top 8 destination countries/areas by total weight shipped 
# across all years, then subset plotdf to only include observations related 
# to those countries/areas.
top8 <- plotdf %>% 
  group_by(`Partner Country`) %>% 
  summarise(kg = as.numeric(sum(kg, na.rm = TRUE))) %>% 
  top_n(8, kg) %>%
  arrange(desc(kg)) %>% 
  .[["Partner Country"]]
plotdf <- plotdf %>% filter(`Partner Country` %in% top8)

# Create plots (y-axis is NOT fixed across panels, this will allow us to ID 
# trends over time within each country/area individually).
qplot(Year, kg, data = plotdf) + 
  geom_line(data = plotdf[plotdf$`Partner Country` %in% names(which(table(plotdf$`Partner Country`) > 1)), ]) + 
  xlim(min(plotdf$Year), max(plotdf$Year)) + 
  labs(title = "Weight (KG) of Thai Shrimp Exports, by Destination Area, 2007 - 2011") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.text = element_text(size = 7)) + 
  facet_wrap(~factor(`Partner Country`, levels = top8), scales = "free", nrow = 2, ncol = 4)

