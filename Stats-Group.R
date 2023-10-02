#author - Meghana Vasanth Shettigar

# Install packages
install.packages("readr")
devtools::install_github("r-lib/conflicted")
install.packages("plotly")

# Load the readr package
library(readr)
library(tidyverse)
library(plotly)

#3. Inspect Health Violations Data by Brand - Los Angeles City, CA
####################################################################
dh <- read_csv('/Users/meghanavs/Desktop/STATS-GroupProject/restaurant-and-market-health-violations.csv')
cols <- c('facility_name', 'violation_code',
          'violation_description', 'violation_status', 'points', 'grade',
          'facility_address', 'facility_city', 'facility_id', 'facility_state',
          'facility_zip','score', 'service_code',
          'service_description', 'row_id')
dh <- dh[, cols]
dh <- unique(dh[!duplicated(dh), ])

# Cleaning up facility names by removing special characters, white spaces and converting to lower
dh$facility_name <- str_to_lower(str_remove_all(dh$facility_name, "\\W+"))

dh[c('facility_name', 'violation_code', 'violation_description')]

print("data summary")
summary(dh)

head(dh)

view(dh)

# Obtain Top Health Violations
dh1 <- dh %>% 
  count(violation_description) %>% 
  arrange(n) %>% 
  rename("Violation Description" = "violation_description", 
         "Number of Violations" = "n") %>% 
  mutate(`Violation Description` = fct_reorder(`Violation Description`, `Number of Violations`)) 

# Plot the bar chart
fig11 <- dh1 %>%
  slice_tail(n = 10) %>%
  ggplot(aes(x = `Number of Violations`, y = `Violation Description`, fill = `Number of Violations`)) +
  geom_col() +
  scale_fill_gradient(low = "white", high = "darkcyan") +
  xlab("Number of Violations") +
  ylab("Violation Description") +
  ggtitle("Common Health Violations in LA") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

# Display the plot
ggplotly(fig11)

#####################

# Plot Top Health Violations
fig11 <- v_code1 %>% 
  slice_tail(n = 10) %>% 
  plot_ly(x = ~Number_of_Violations, y = ~Violation_Description, type = 'bar', orientation = 'h') %>% 
  layout(title = 'Common Health Violations in LA', xaxis = list(title = 'Number of Violations'), yaxis = list(title = 'Violation Code'), showlegend = FALSE) %>% 
  update_traces(marker = list(color = 'darkcyan'))

# Obtain and clean up brand-specific data
dh2 <- v_code %>% 
  count(facility_name) %>% 
  arrange(desc(n)) %>% 
  rename(Number_of_Violations = n) %>%
  mutate(Brand = case_when(
    str_detect(facility_name, "burgerking") ~ "BurgerKing",
    str_detect(facility_name, "wendys") ~ "Wendys",
    str_detect(facility_name, "mcdonalds") ~ "McDonalds",
    str_detect(facility_name, "subway") ~ "Subway",
    str_detect(facility_name, "kfc") ~ "KFC",
    str_detect(facility_name, "tacobell") ~ "TacoBell",
    str_detect(facility_name, "arbys") ~ "Arbys"
  ))

# group at brand level
dh3 <- dh2 %>% 
  group_by(Brand) %>% 
  summarize(Nbr_of_Violations = sum(Number_of_Violations), Stores = n(), Violations_per_store = Nbr_of_Violations / Stores) %>% 
  arrange(desc(Violations_per_store)) %>% 
  rename(Brands = Brand)

dh3
  
# create subplots
fig5 <- plot_ly(dh3, x = ~Brands, showlegend = FALSE, height = 450) %>% 
  add_trace(y = ~Nbr_of_Violations, name = 'Total Violations', type = 'bar', 
            subplot = 'row 1, col 1') %>% 
  add_trace(y = ~Stores, name = 'Store Counts', type = 'bar', 
            subplot = 'row 1, col 2') %>% 
  add_trace(y = ~Violations_per_store, name = 'Violations Per Store', type = 'bar', 
            subplot = 'row 1, col 3') %>% 
  layout(title = 'Health Inspection Results by Brands- Los Angeles', 
         xaxis = list(title = ''), yaxis = list(title = ''))

#display plot
fig5
#####################

#3. Inspect Health Violations Data by Brand - Chicago City, IL
####################################################################

library(readr)

# read csv file
ch <- read_csv('/Users/meghanavs/Desktop/STATS-GroupProject/food-inspections.csv')

# select required columns
ch <- ch[c('DBA Name', 'AKA Name', 'Facility Type', 'Address', 'Risk', 'City', 'State', 'Zip', 'Inspection Date', 'Inspection Type', 'Results', 'Violations')]

# print first 5 rows
head(ch)
# Print the number of unique violations
cat(paste("the violations description is very specific. There are", length(unique(ch$Violations)), "unique violations"))

# Create a new column 'Violations2' with the first 75 characters of 'Violations'
ch$Violations2 <- substr(ch$Violations, 1, 75)

# Remove the 'Violations' column
ch$Violations <- NULL

# Combine 'DBA Name' and 'Address' columns
ch$`DBA Name` <- paste(ch$`DBA Name`, ch$Address)

# Subset the dataframe to only include failed results
ch <- subset(ch, Results == "Fail")

# Count the number of violations and store in a new dataframe 'df1'
df1 <- aggregate(ch$Violations2, by=list(ch$Violations2), FUN=length)
colnames(df1) <- c("Violations", "Counts")
df1 <- df1[order(df1$Counts),]

# Print the sorted dataframe
print(df1)

library(plotly)

fig6 <- plot_ly(df1[(nrow(df1)-9):nrow(df1),], x=~Counts, y=~Violations, type="bar", orientation="h", height=500, width=1500,
                marker = list(color="darkcyan")) %>%
  layout(xaxis = list(title="Number of Violations"), yaxis = list(title="Violation Code"), title="Common Health Violations in Chicago")

fig6

library(stringr)

#########################

library(tidyverse)
library(plotly)

df2 <- ch %>%
  mutate(`DBA Name` = tolower(`DBA Name`),
         `DBA Name` = str_replace_all(`DBA Name`, "\\W+", "")) %>%
  count(`DBA Name`, sort = TRUE) %>%
  rename(`Nbr of Violations` = n) %>%
  filter(str_detect(`DBA Name`, paste(c("burgerki", "donalds", "subway", "tacobell", "kfc", "arbys", "wendys"), collapse = "|"))) %>%
  mutate(Brand = case_when(
    str_detect(`DBA Name`, "burgerk") ~ "BurgerKing",
    str_detect(`DBA Name`, "donalds") ~ "McDonalds",
    str_detect(`DBA Name`, "arby") ~ "Arbs",
    str_detect(`DBA Name`, "subway") ~ "Subway",
    str_detect(`DBA Name`, "tacobe") ~ "TacoBell",
    str_detect(`DBA Name`, "wendy") ~ "Wendys",
    str_detect(`DBA Name`, "kfc") ~ "KFC",
    TRUE ~ NA_character_
  )) %>%
  select(Brand, `Nbr of Violations`) %>%
  group_by(Brand) %>%
  summarise(`Nbr_of_Violations` = sum(`Nbr of Violations`), Stores = n()) %>%
  ungroup() %>%
  mutate(Violations_per_store = Nbr_of_Violations / Stores) %>%
  arrange(desc(Violations_per_store))

fig7 <- plot_ly() %>%
  add_trace(x = df3$Brand, y = df3$Nbr_of_Violations, name = "Total Violations", type = "bar", marker = list(color = "rgb(0, 0, 153)")) %>%
  add_trace(x = df3$Brand, y = df3$Stores, name = "Store Counts", type = "bar", marker = list(color = "rgb(0, 153, 0)")) %>%
  add_trace(x = df3$Brand, y = df3$Violations_per_store, name = "Violations Per Store", type = "bar", marker = list(color = "rgb(153, 0, 0)")) %>%
  layout(title = "Health Inspection Results by Brands - Chicago",
         xaxis = list(title = "Brand"),
         yaxis = list(title = "Number of Violations / Store Counts / Violations per Store"),
         barmode = "group",
         showlegend = FALSE,
         height = 450)

fig7


#################################################


#####################









