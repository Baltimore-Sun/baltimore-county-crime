#LOAD LIBRARIES
  
library(tidyverse)
library(janitor)
library(tidyr)
library(lubridate)
library(purrr)


#get data

baltcocrime <- read_csv("Public_Crime_Data.csv") %>% clean_names()


#create date type column

baltcocrime$occurredon <- sub(" .*", "", baltcocrime$occurredon)


baltcocrime$occurredon <- as.Date(baltcocrime$occurredon, format = "%Y/%m/%d")


#Get date of last record in  data that occurs at least 30 times

crime_counts <- baltcocrime %>% count(occurredon, name = "n")
frequent_crime_dates <- crime_counts %>% filter(n >= 30) %>% pull(occurredon)


#control for reporting lag


thisyearend <- max(frequent_crime_dates)-days(7)



baltcocrime <- baltcocrime %>%
  filter(
    occurredon >= make_date(year(thisyearend), 1, 1) & occurredon <= thisyearend
    |
      occurredon >= make_date(year(thisyearend)-1, 1, 1) & occurredon <= update(thisyearend, year = year(thisyearend)-1)
  ) 


#create most recent 30 days vs. 30 days before that split



bc_prevmonth <- baltcocrime %>% filter(occurredon >= thisyearend - days(59) & occurredon <= thisyearend - days(30)) %>% arrange(occurredon)




bc_thismonth <- baltcocrime %>% filter(occurredon >= thisyearend - days(29) & occurredon <= thisyearend) %>% arrange(occurredon)


#pivot month data




#pivot year data

baltcocrimepivot <- baltcocrime %>% tabyl(year, ibr_description) %>% clean_names()





###sum certain columns to create property, violent, burglary columns

#VIOLENT CRIME

baltcocrimepivot <- baltcocrimepivot %>%  mutate(violent = rowSums(across(c(aggravated_assault,murder_and_nonnegligent_manslaughter,rape,robbery))))


#LARCENY

baltcocrimepivot <- baltcocrimepivot %>%  mutate(all_larceny = rowSums(across(c(all_other_larceny, pocket_picking, purse_snatching, shoplifting, theft_from_building, theft_from_coin_operated_machine_or_device, theft_of_motor_vehicle_parts_or_accessories, theft_from_motor_vehicle))))



#PROPERTY CRIME

baltcocrimepivot <- baltcocrimepivot %>%  mutate(property = rowSums(across(c(all_larceny,arson,motor_vehicle_theft,burglary_breaking_entering))))



#Export pivot with crimes we want


baltcocrimepivot <- baltcocrimepivot  %>%  mutate(total = rowSums(across(c(violent,property))))




bc_pivot_export <-baltcocrimepivot[, c("year", "violent", "property", "total", "aggravated_assault", "murder_and_nonnegligent_manslaughter", "rape", "robbery", "all_larceny", "arson", "burglary_breaking_entering", "motor_vehicle_theft", "shoplifting", "theft_from_building", "theft_from_motor_vehicle", "theft_of_motor_vehicle_parts_or_accessories", "drug_equipment_violations", "drug_narcotics_violations", "human_trafficking_commercial_sex_acts", "prostitution", "purchasing_prostitution", "negligent_manslaughter"
)]





# Find the rows for 2024 and 2025
row_2024 <- bc_pivot_export[ bc_pivot_export$year == 2024, ]
row_2025 <- bc_pivot_export[ bc_pivot_export$year == 2025, ]

# Compute change for each column except 'year'
# First, get the column names to change
other_cols <- setdiff(names(bc_pivot_export), "year")

# Initialize a new row list
change_row <- vector("list", length = length(names(bc_pivot_export)))
names(change_row) <- names(bc_pivot_export)

# Fill in the year for the change row
change_row[["year"]] <- "Change"

# Compute the change percentage for each other column
for (col in other_cols) {
  # get numeric values
  val2024 <- as.numeric(row_2024[[col]])
  val2025 <- as.numeric(row_2025[[col]])
  # guard against division by zero, NAs, etc
  if (!is.na(val2024) && val2024 != 0) {
    pct <- (val2025 - val2024) / val2024
  } else {
    pct <- NA
  }
  # format as "0.0%"
  change_row[[col]] <- if (!is.na(pct)) sprintf("%.1f%%", pct * 100) else NA
}

# Turn the change_row into a one‑row data.frame with same column types (or character for numeric ones)
change_df <- as.data.frame(change_row, stringsAsFactors = FALSE)

# If original columns are numeric, you might want to keep numeric in 2024/2025 rows and character in change row, so the whole column becomes character. 
# Or you can convert change_df columns to match.

# Finally bind the new row
bc_new <- rbind(bc_pivot_export, change_df)







#zip codes 


bczips <- baltcocrime %>% filter(zip > 20999 & zip < 22000) %>% arrange(zip)



bczips25 <- bczips %>% filter(year == "2025" ) %>% tabyl(zip, ibr_description) %>% clean_names()





bczips24 <- bczips %>% filter(year == "2024" ) %>% tabyl(zip, ibr_description) %>% clean_names()





bczips24 <- bczips24  %>%  mutate(violent = rowSums(across(c(aggravated_assault,murder_and_nonnegligent_manslaughter,rape,robbery))))

bczips24  <- bczips24  %>%  mutate(all_larceny = rowSums(across(c(all_other_larceny, pocket_picking, purse_snatching, shoplifting, theft_from_building, theft_from_coin_operated_machine_or_device, theft_of_motor_vehicle_parts_or_accessories, theft_from_motor_vehicle))))

bczips24  <- bczips24  %>%  mutate(property = rowSums(across(c(all_larceny,arson,motor_vehicle_theft,burglary_breaking_entering))))

bczips24  <- bczips24  %>%  mutate(all = rowSums(across(c(violent,property))))



bczips25 <- bczips25  %>%  mutate(violent = rowSums(across(c(aggravated_assault,murder_and_nonnegligent_manslaughter,rape,robbery))))

bczips25  <- bczips25  %>%  mutate(all_larceny = rowSums(across(c(all_other_larceny, pocket_picking, purse_snatching, shoplifting, theft_from_building, theft_from_coin_operated_machine_or_device, theft_of_motor_vehicle_parts_or_accessories, theft_from_motor_vehicle))))

bczips25  <- bczips25  %>%  mutate(property = rowSums(across(c(all_larceny,arson,motor_vehicle_theft,burglary_breaking_entering))))

bczips25  <- bczips25  %>%  mutate(all = rowSums(across(c(violent,property))))




bczips25 <- bczips25[, c("zip", "violent", "property", "all", "aggravated_assault", "murder_and_nonnegligent_manslaughter", "rape", "robbery", "all_larceny", "arson", "burglary_breaking_entering", "motor_vehicle_theft", "shoplifting", "theft_from_building", "theft_from_motor_vehicle", "theft_of_motor_vehicle_parts_or_accessories", "drug_equipment_violations", "drug_narcotics_violations", "human_trafficking_commercial_sex_acts", "prostitution", "purchasing_prostitution", "negligent_manslaughter"
)]





bczips24 <- bczips24[, c("zip", "violent", "property", "all", "aggravated_assault", "murder_and_nonnegligent_manslaughter", "rape", "robbery", "all_larceny", "arson", "burglary_breaking_entering", "motor_vehicle_theft", "shoplifting", "theft_from_building", "theft_from_motor_vehicle", "theft_of_motor_vehicle_parts_or_accessories", "drug_equipment_violations", "drug_narcotics_violations", "human_trafficking_commercial_sex_acts", "prostitution", "purchasing_prostitution", "negligent_manslaughter"
)]



bczips25 <- bczips25 %>% rename_with(.fn = ~ paste0("2025_", .x), .cols = -1)
bczips24 <- bczips24 %>% rename_with(.fn = ~ paste0("2024_", .x), .cols = -1)




zipyrcompare <- left_join(bczips25, bczips24, by = "zip")




# First gather the base names programmatically, by looking at column names
base_vars <- names(zipyrcompare) %>%
  # find those that start with "2025_"
  str_subset("^2025_") %>%
  # remove the prefix to get the base
  str_remove("^2025_")

# Now mutate to add change columns
zipyrcompare_change <- zipyrcompare %>%
  mutate(
    across(
      .cols = all_of(paste0("2025_", base_vars)),   # for each of the 2025_ prefixed columns
      .fns = ~ {
        # For column .x (which is a 2025 column), find corresponding 2024 column
        varname_5 <- cur_column()
        base <- str_remove(varname_5, "^2025_")
        varname_4 <- paste0("2024_", base)
        v2025 <- .x
        v2024 <- zipyrcompare[[varname_4]]
        pct <- (v2025 - v2024) / v2024
        # Format as 0.0%; guard for div by zero or NA
        # If v2024 is zero or NA, produce NA or some default
        if_else(!is.na(pct) & v2024 != 0,
                sprintf("%.1f%%", pct * 100),
                NA_character_)
      },
      .names = "{base_vars}_change"   # this will give columns like violent_change, rape_change, etc.
    )
  )


#filter for zips with at least 100 total 2025 crimes

zipyrcompare_change <- zipyrcompare_change %>% filter(`2025_all` > 99) %>% arrange(desc(`2025_all`))

#get zip code key; make sure zip col is number
zip_key <- read_csv("zip_key.csv") %>% clean_names()

#join with zip code area names
zipyrcompare_change <- left_join(zipyrcompare_change, zip_key, by = "zip")

zipyrcompare_change <- zipyrcompare_change %>% relocate(last_col(), .after = 1)


#collecting results for report
#bc_new is countywide ytd comparison
#zipyrcompare_change is zip ytd comparison






#transpose for presentation
bc_transposed <- t(bc_new)
bc_transposed <- as.data.frame(bc_transposed)




# Assign the first row as column names
colnames(bc_transposed) <- bc_transposed[1, ]

# Remove the first row from the data frame
bc_transposed <- bc_transposed[-1, ]

# Give the first column a name
colnames(bc_transposed)[1] <- "offense"





write_csv(zipyrcompare_change,"zipyrcompare_change.csv")




write_csv(bc_transposed,"bc_transposed.csv")

#to show when the data ends
write_csv(thisyearend,"thisyearend.csv")

