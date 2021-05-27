library(readxl)
library(tidyverse)
library(lubridate)
library(stringr)
library(janitor)
library(plyr)
library(arules)
library(arulesViz)
library(htmlwidgets)
library(ggplot2)
library(gt)
library(tidyr)
library(paletteer)
library(viridis)
library(devtools)
devtools::install_github('bbc/bbplot')
library(RColorBrewer)
library(sysfonts)
install.packages("extrafont")

font_files()

font_add_google("Reforma")
#Read both transaction records
#If not initialised, set your working directory by using setwd()
retail_old <- read_excel("online_retail_II.xlsx")
retail_new <- read_excel("Online Retail.xlsx")

#Use standardized naming conventions
retail_old <- retail_old %>%
  rename(c("Invoice" = "InvoiceNo", "Price" = "UnitPrice" , `Customer ID` = "CustomerID"))

#Combine rows, this is a base R function with the tidyverse version being rbind()
retail <- bind_rows(retail_new, retail_old)

#Find summary statistics
summary(retail)

#Include only positive values
retail_clean <- retail %>%
  filter(Quantity > 0 & UnitPrice > 0)

#Check for presence of NAs
retail_clean %>%
  filter(is.na(Description))

retail_clean <- retail_clean %>%
  mutate(StockCode = toupper(StockCode))

#Data's clean but not clean enough, there are still StockCodes such as M, Amazonfee etc. that need to be removed
#Let's see all the non-numeric StockCodes
misc_StockCode <- retail_clean %>%
  filter(!grepl('^[0-9]', StockCode))

misc_StockCode %>%
  select(StockCode, Description) %>%
  distinct() %>%
  head(n = 20) %>%
  gt() %>%
  tab_header(title = md("**Non-Numeric StockCodes**"),
                  subtitle = md("Observing Meaningful and Meaningless StockCodes")) %>%
  tab_options(table.border.top.width = 10,
              heading.border.bottom.width = 10,
              heading.border.bottom.color = "#A8A8A8") %>%
  cols_align(align = "center",
             columns = "Description") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_style(cell_borders(sides = "left", color = "#A8A8A8", weight = px(3)),
            locations = cells_body(columns = "Description"))

gt_misc_StockCode <- misc_StockCode %>%
  select(StockCode, Description) %>%
  distinct() %>%
  head(n = 20) %>%
  gt() %>%
  tab_header(title = md("**Differentiating StockCodes**"),
             subtitle = md("Segregating StockCodes into Good and Bad")) %>%
  tab_options(table.border.top.width = 10,
              heading.border.bottom.width = 10,
              heading.border.bottom.color = "#A8A8A8") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_style(cell_borders(sides = "left", color = "#A8A8A8", weight = px(3)),
            locations = cells_body(columns = "Description")) %>%
  tab_row_group(label = "Bad StockCodes",
    rows = !grepl('^[0-9]|DCGS|gift|C2|SP', StockCode)) %>%
  tab_row_group(label = "Good StockCodes",
    rows = grepl('^[0-9]|DCGS|gift|C2|SP', StockCode)) %>%
  tab_style(style = list(cell_fill(color = "#259A00"), cell_text(weight = "bold" , color = "#FFFFFF")),
            cells_row_groups("Good StockCodes")) %>%
  tab_style(style = list(cell_fill(color = "#AA0000"), cell_text(weight = "bold" , color = "#FFFFFF")),
            cells_row_groups("Bad StockCodes")) %>%
  cols_width("StockCode" ~ px(200),
             "Description" ~ px(400))

gt_misc_StockCode

gtsave(gt_misc_StockCode, "gt_misc_StockCode.png")

#We'll update the REGEX to exlcude anything with DCGS, gift, C2 and SP as we believe
#they are important products
bad_StockCode <- retail_clean %>%
  filter(!grepl('^[0-9]|DCGS|gift|C2|SP', StockCode))

#Filter out bad stock codes
retail_clean <- retail_clean %>%
  filter(!StockCode %in% bad_StockCode$StockCode)

#Objective is to get only TransactionID and Description; lets see how many NAs are there for each
retail_clean %>%
  filter(is.na(Description))

#Show disparity between naming conventions

#Fill NA's with the fill function
#This might not be needed if we filter correctly for Quantity and UnitPrice

#Figure out how to display columns where StockCode != Description

#Find duplicates with janitor's get_dupes() function
dups_Description <- retail_clean %>%
  select(StockCode, Description) %>%
  distinct() %>%
  get_dupes(Description) %>%
  arrange(desc(dupe_count)) %>%
  relocate(StockCode, Description, dupe_count)

gt_dups_Description <- dups_Description[1:20,] %>%
  gt() %>%
  tab_header(title = md("**Duplicate Descriptions**"),
             subtitle = md("Same Descriptions with Different StockCodes")) %>%
  tab_options(table.border.top.width = 10,
              heading.border.bottom.width = 10,
              heading.border.bottom.color = "#A8A8A8") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) %>%
  tab_style(style = cell_fill(color = "#BCFFC0"),
            locations = cells_body(rows = grepl("METAL SIGN,CUPCAKE SINGLE HOOK", Description))) %>%
  tab_style(style = cell_fill(color = "#FEFFBB"),
            locations = cells_body(rows = grepl("COLUMBIAN CANDLE ROUND", Description))) %>%
  tab_style(style = cell_fill(color = "#BCC5FF"),
            locations = cells_body(rows = grepl("SET OF 4 FAIRY CAKE PLACEMATS", Description))) %>%
  tab_style(style = cell_fill(color = "#C5987B"),
            locations = cells_body(rows = grepl("COLOURING PENCILS BROWN TUBE", Description))) %>%
  tab_style(style = cell_fill(color = "#FFDAFD"),
            locations = cells_body(rows = grepl("MODERN CHRISTMAS TREE CANDLE", Description))) %>%
  cols_label(dupe_count = "Number of Duplicates")

gt_dups_Description

gtsave(gt_dups_Description, "dups_Description.png")

dups_StockCode <- retail_clean %>%
  select(StockCode, Description) %>%
  distinct() %>%
  get_dupes(StockCode) %>%
  arrange(desc(dupe_count)) %>%
  relocate(StockCode, Description, dupe_count)

gt_dups_StockCode <- dups_StockCode[1:20,] %>%
  gt() %>%
  tab_header(title = md("**Duplicate StockCodes**"),
             subtitle = md("Same StockCodes with Different Descriptions")) %>%
  tab_options(table.border.top.width = 10,
              heading.border.bottom.width = 10,
              heading.border.bottom.color = "#A8A8A8") %>%
  tab_style(style = cell_fill(color = "#FF7F7F"),
            locations = cells_body(rows = grepl("20685", StockCode))) %>%
  tab_style(style = cell_fill(color = "#FFA7F8"),
            locations = cells_body(rows = grepl("22344", StockCode))) %>%
  tab_style(style = cell_fill(color = "#B9BEFF"),
            locations = cells_body(rows = grepl("22345", StockCode))) %>%
  tab_style(style = cell_fill(color = "#BDFFB9"),
            locations = cells_body(rows = grepl("22346", StockCode))) %>%
  tab_style(style = cell_fill(color = "#FFD0F9"),
            locations = cells_body(rows = grepl("22384", StockCode))) %>%
  cols_label(dupe_count = "Number of Duplicates") %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()))

gtsave(gt_dups_StockCode, "dups_StockCode.png")

#Get the distinct items and anti-join from dups
unique_StockCode <- dups %>%
  distinct(StockCode)

unique_Description <- dups %>%
  distinct(Description)

retail_clean %>%
  mutate(StockCode = toupper(StockCode)) %>%
  distinct(StockCode)

#There are two types of dulplicates: StockCode duplicates and Description duplicates.
#We can find the dupes for each and arrange columns accordingly


#Transactions of all items with duplciate Stock Codes
sales_dups_IDs <- retail_clean %>%
  filter(StockCode %in% dups_StockCode$StockCode) %>%
  arrange(StockCode)

sales_dups_IDs[1:20,] %>%
  gt()

#Transactions of all items with duplciate Descriptions
sales_dups_Descriptions <- retail_clean %>%
  filter(Description %in% dups_Description$Description)


#Exploratory Data Analysis
#Top products in sales amount
#There are no NAs in description, no neeed to fill for missing NAs
retail_clean %>%
  filter(!is.na(Description)) %>%
  group_by(StockCode, Description)

retail_clean %>%
  filter(!StockCode %in% misc_StockCode$StockCode)

#Exploratory data analysis: most sold item
gg_top_sold <- retail_clean %>% 
  group_by(StockCode, Description) %>% 
  dplyr::summarize(count = n(), .groups = "drop") %>%
  ungroup() %>%
  arrange(desc(count))

retail_clean %>%
  select(StockCode, Description, Quantity) %>% 
  group_by(StockCode, Description) %>%
  dplyr::summarise(count = n())

ggplot(gg_top_sold[1:10,], aes(y = count, x = fct_reorder(Description, count))) +
  geom_bar(stat='identity', aes(fill = Description)) +
  scale_fill_brewer(palette = "Paired") +
  coord_flip() +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  labs(title = "Most Frequently Purchased Items") +
  scale_y_continuous(breaks = c(1000,2000,3000,4000,5000))+
  theme(aspect.ratio = 3/4,
        panel.grid.major.x = element_line(color="#CBCBCB"), 
        panel.grid.major.y=element_blank(),
        legend.position = "none",
        text = element_text(family = "Arial"),
        axis.text.y = element_text(face = "bold", margin = margin(t = 0, r = -30, b = 0, l = 0)),
        axis.text.x = element_text(face = "bold"))

ggsave("Most_Frequently_Purchased_Items.png", width = 20, height = 15, dpi = 300)

top_revenue_products <- retail_clean %>%
  mutate(Item_Total = UnitPrice * Quantity) %>%
  select(StockCode, Description, Item_Total) %>%
  group_by(StockCode, Description) %>%
  dplyr::summarize(Item_Total = sum(Item_Total)) %>%
  arrange(desc(Item_Total))

ggplot(top_revenue_products[1:10,], aes(y = Item_Total, x = fct_reorder(Description, Item_Total))) +
  geom_bar(stat='identity', fill = c("#A6CEE3", "#B1DF8A", "#320505", "#33A02C", "#6A3E9A",
                                      "#2578B4", "#EB7ECD", "#790000", "#FEF337", "#C0C0C0")) +
  scale_fill_brewer(palette = "Paired") +
  coord_flip() +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  labs(title = "Top Grossing Items") +
  scale_y_continuous(breaks = seq(50000, 450000, by=50000),
                     labels=scales::dollar_format()) +
  theme(aspect.ratio = 3/4,
        panel.grid.major.x = element_line(color="#CBCBCB"), 
        panel.grid.major.y=element_blank(),
        legend.position = "none",
        text = element_text(family = "Serif"),
        axis.text.y = element_text(face = "bold", margin = margin(t = 0, r = -30, b = 0, l = 0)),
        axis.text.x = element_text(face = "bold", angle = 30))

ggsave("Top_Revenue_Items.png", width = 20, height = 15, dpi = 300)

#Find the % of events this covers...how much % of revenue and how much % of items ordered
freq_purchases <- retail_clean %>%
  group_by(StockCode, Description) %>%
  dplyr::summarize(Quantity = sum(Quantity), .groups = "drop") %>%
  arrange(desc(Quantity))

tmp <- retail_clean %>%
  group_by(StockCode, Description) %>%
  dplyr::summarize(count = n(), .groups = "drop")

freq_purchases_differences <- freq_purchases %>%
  inner_join(tmp, by = c("StockCode", "Description")) %>%
  mutate(Difference = Quantity - count) %>%
  rename(c("Quantity" = "Total Item Sold", "count" = "Total Transactions")) %>%
  arrange(desc(`Total Item Sold`), desc(`Total Transactions`)) %>%
  ungroup() %>%
  select(-StockCode) %>%
  head(n = 20)


#gt
gt_freq_purchases_difference <- freq_purchases_differences %>%
  gt() %>%
  tab_style(style = cell_fill(color = "#FF0000"),
            locations = cells_body(columns = vars("Total Transactions"),
                                   rows = `Total Transactions` < 500)) %>%
  tab_style(style = cell_fill(color = "#FF6C6C"),
            locations = cells_body(columns = vars("Total Transactions"),
                                   rows = `Total Transactions` > 500 & `Total Transactions` < 1550)) %>%
  tab_style(style = cell_fill(color = "#FFAAAA"),
            locations = cells_body(columns = vars("Total Transactions"),
                                   rows = `Total Transactions` > 1550 & `Total Transactions` < 2550)) %>%
  tab_style(style = cell_fill(color = "#FFE2E2"),
            locations = cells_body(columns = vars("Total Transactions"),
                                   rows = `Total Transactions` > 2550)) %>%
  cols_align(align = "center",
             columns = c("Total Item Sold", "Difference")) %>%
  tab_style(style = cell_text(weight = "bold"),
           locations = cells_column_labels(everything()))

gt_freq_purchases_difference
  
gtsave(gt_freq_purchases_difference, "freq_purchases_difference_table.png")

#Total number of invoices; be sure to bring up how quantity doesn't affect itemsets
#Notice how sometimes there's multiple commas because of the description--you can see them here
commas_retail <- retail_clean %>%
  filter(grepl(",", Description)) %>%
  head(n = 20) %>%
  gt() %>%
  cols_align(align = "center",
             columns = "Quantity")

gtsave(commas_retail, "commas_retail_table.png")

check <- retail_clean %>%
  select(InvoiceNo, Description) %>%
  unique()

##A comma throws everything off but we can still keep other punctuation
no_comma <- apply(retail_clean, 2, FUN = function(y) gsub(',','', y))

#Before this step ensure you don't want to put a unique() function to get only 1 item per itemset
transactionData <- ddply(data.frame(no_comma), "InvoiceNo",
                         function(x) paste(x$Description,
                                            collapse = ","))
getwd()
#Remove unneeded colnames
transactionData$InvoiceNo <- NULL
colnames(transactionData) <- "items"

#convert to csv and convert back to basket
write.csv(transactionData,"transactiondata.csv", quote = FALSE, row.names = FALSE)

tr <- read.transactions('transactiondata.csv', format = 'basket', sep=',')

#Note rows, columns & density figures, sizes (itemset) by popularity, distriubtion of skewness
summary(tr)

itemFrequencyPlot(tr, topN=20, type='absolute')

association.rules <- apriori(tr, parameter = list(supp=0.005, conf=0.8, maxlen=100))

summary(association.rules)

inspect(association.rules[1:50])

rules_by_lift <- sort(association.rules, by = "lift")

df_lift_rules <- as(rules_by_lift, 'data.frame')

check <- as(rules_by_lift[!is.redundant(rules_by_lift)], 'data.frame')

gt_lift_rules <- df_lift_rules %>%
  head(n = 20) %>%
  separate(col = rules, into = c("lhs", "rhs"), sep = "=>") %>%
  rowid_to_column() %>%
  dplyr::rename(rule = rowid) %>%
  gt() %>%
  fmt_number(columns = vars(support),
             decimals = 4) %>%
  fmt_number(columns = vars(confidence),
             decimals = 2) %>%
  fmt_number(columns = vars(coverage),
             decimals = 4) %>%
  fmt_number(columns = vars(lift),
             decimals = 2) %>%
  fmt(columns = vars(lhs),
      fns = function(x) {
        gsub(',',', ', x)
        }) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything()))

gtsave(gt_lift_rules, "lift_rules_table.png")

inspect(rules_by_lift[1:50])

retail_clean %>%
  filter(Description == "PAPER CRAFT , LITTLE BIRDIE")

topRules <- association.rules[1:10]

inspectDT(rules_by_lift)

#Use argument `verbose = TRUE` inside the list function to see all other applicable arguments for 
plot(rules_by_lift[1:10], method = "graph", con = list(cex = 0.6, arrowSize = 0.4, precision = 2))

png("lift_rules_graph.png", width=1400, height=960, res=300)

#Create a different type of graph using the htmlwidgets engine
rules_widget <- plot(rules_by_lift[1:20], method="graph", engine="htmlwidget",
     igraphLayout = "layout_in_circle")

#Save widget to file
saveWidget(rules_widget, file = "ruleswidget.html")

plot(rules_by_lift[1:30], method = "two-key plot")

plot(rules_by_lift[1:30], method = "paracoord")

jpeg(
  filename="networkgraph.jpeg",
  width=8,
  height=8,
  units="in",
  res=500)

dev.off()

plot(rules_by_lift[1:30], method = "graph",control = list(verbose = TRUE))

plot(association.rules[1:20], method="graph", engine="htmlwidget")
