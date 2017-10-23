## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE, fig.width = 9, fig.height = 6)

## ----eval = FALSE--------------------------------------------------------
#  install.packages("comtradr")

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("ChrisMuir/comtradr")

## ------------------------------------------------------------------------
library(comtradr)

## ------------------------------------------------------------------------
q <- ct_search(reporters = "USA", 
               partners = c("Germany", "France", "Japan", "Mexico"), 
               trade_direction = "imports")

# API calls return a tidy data frame.
str(q)

## ---- eval = FALSE-------------------------------------------------------
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports",
#                 start_date = "2010-01-01",
#                 end_date = "2014-01-01")

## ---- eval = FALSE-------------------------------------------------------
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports",
#                 start_date = "2012-03-01",
#                 end_date = "2012-07-01",
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

## ------------------------------------------------------------------------
q <- ct_search(reporters = "USA", 
               partners = c("Germany", "France", "Mexico"), 
               trade_direction = "all", 
               commod_codes = c("0702", "070200", "2002", "200210", "200290"))

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

## ------------------------------------------------------------------------
ct_update_databases()

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

## ---- warning = FALSE, message = FALSE-----------------------------------
library(ggplot2)

# Comtrade api query.
df <- ct_search(reporters = "China", 
                partners = c("Rep. of Korea", "USA", "Mexico"), 
                trade_direction = "exports")

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

## ---- warning = FALSE, message = FALSE-----------------------------------
library(ggplot2)
library(dplyr)

# First, collect commodity codes related to shrimp.
shrimp_codes <- ct_commodity_lookup("shrimp", 
                                    return_code = TRUE, 
                                    return_char = TRUE)

# Comtrade api query.
df <- ct_search(reporters = "Thailand", 
                partners = "All", 
                trade_direction = "exports", 
                start_date = "2007-01-01", 
                end_date = "2011-01-01", 
                commod_codes = shrimp_codes)

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
  arrange(desc(kg)) %>% 
  magrittr::extract2("Partner Country") %>% 
  magrittr::extract(1:8)
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

