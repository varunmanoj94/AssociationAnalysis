Association Analysis
================
Varun Manoj
20/05/2021

## Introduction

Following last month’s blog post, I’ll look to provide some additional
analytical methods that we can apply on sales transaction data. The
method that we’ll explore in this post can be applied to any form of
sales data, from e-commerce sites or traditional brick and mortar
stores. Association analysis can be used to derive crucial insights to
inform business decisions concerning three of the four P’s of
marketing–product, positioning and price–and can be used to guide
decision-making in the digital era.

> [**Association
> Analysis**](https://www.jmp.com/support/help/en/15.2/index.shtml#page/jmp/association-analysis.shtml)
> enables you to identify items that have an affinity for each other. It
> is frequently used to analyze transactional data (also called market
> baskets) to identify items that often appear together in transactions.
> For example, grocery stores and online merchants use association
> analysis to strategically organize and recommend products that tend to
> be purchased together.

## Data Preprocessing

This blog’s dataset is actually an extension from last month’s dataset.
I wanted to test myself with a larger dataset but couldn’t seem to find
an appropriate one until I stumbled upon [retail
data](https://archive.ics.uci.edu/ml/datasets/Online+Retail+II) of the
two years prior to the previously used dataset. In case you missed the
last blog post, here’s a
[recap](https://varunmanoj.blog/2021/01/27/clustering-from-sales-trancation-data/)
of the dataset from the last blog post.

The goal of data preprocessing often differs with the choice of
analytical model. We need a very particular format of data for
association analysis: two columns, one with an invoice or transaction
ID, and the other with a column of items purchased by description.
Keeping that goal in mind, we can manipulate our dataset accordingly to
transform it to the desired format. We must note that this format is
inconsistent with [tidy data
principles](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html),
however it’s required for this type of analysis. We begin the analysis
by importing the two different transaction record files and combining
them into one data frame. We need to be careful about naming
conventions, as we can already see some inconsistency between the two
sets of transaction data. We can fix the problem by using the `rename()`
function, and rename the columns ‘Price’, ‘Invoice’ and ‘Customer ID’ to
‘UnitPrice’, ‘InvoiceNo’ and ‘CustomerID’ respectively.

``` r
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
```

    ##   InvoiceNo          StockCode         Description           Quantity        
    ##  Length:1067370     Length:1067370     Length:1067370     Min.   :-80995.00  
    ##  Class :character   Class :character   Class :character   1st Qu.:     1.00  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :     3.00  
    ##                                                           Mean   :     9.94  
    ##                                                           3rd Qu.:    10.00  
    ##                                                           Max.   : 80995.00  
    ##                                                                              
    ##   InvoiceDate                    UnitPrice           CustomerID    
    ##  Min.   :2009-12-01 07:45:00   Min.   :-53594.36   Min.   :12346   
    ##  1st Qu.:2010-07-09 09:46:00   1st Qu.:     1.25   1st Qu.:13975   
    ##  Median :2010-12-07 15:28:00   Median :     2.10   Median :15255   
    ##  Mean   :2011-01-02 21:13:27   Mean   :     4.65   Mean   :15325   
    ##  3rd Qu.:2011-07-22 10:23:00   3rd Qu.:     4.15   3rd Qu.:16797   
    ##  Max.   :2011-12-09 12:50:00   Max.   : 38970.00   Max.   :18287   
    ##                                                    NA's   :243007  
    ##    Country         
    ##  Length:1067370    
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##                    
    ## 

The combined dataset 1,067,370 observations but the data’s far from
clean. We can see a couple of problems from the results of the
`summary()` function: over 240,000 records have NA’s in the CustomerID
variable, at least 1 record has a negative UnitPrice and at least 1
record has a negative Quantity. We don’t need to be worried about the
CustomerID variable for association analysis, but we do need to ensure
our dataset has only positive values for UnitPrice and Quantity. We can
filter for our requirements using the `filter()` function and confirm we
have a complete datatset by checking the `is.na()` function:

``` r
#Include only positive values
retail_clean <- retail %>%
  filter(Quantity > 0 & UnitPrice > 0)

#Check for presence of NAs
retail_clean %>%
  filter(is.na(Description))
```

    ## # A tibble: 0 x 8
    ## # … with 8 variables: InvoiceNo <chr>, StockCode <chr>, Description <chr>,
    ## #   Quantity <dbl>, InvoiceDate <dttm>, UnitPrice <dbl>, CustomerID <dbl>,
    ## #   Country <chr>

This data frame seems to have complete data for all the variables that
we require. Next, let’s explore the StockCode column to check if there
are any problems. We can see that there are three main types of
StockCode variables: numeric, alpha-numeric and alphabets. This is a
perfect opportunity to use some REGEX to differentiate observations and
determine whether or not to include certain observations. We can begin
by using REGEX to filter out all non-numeric StockCodes by using the
`grepl()` function to specify our regular expression.

``` r
#Data's clean but not clean enough, there are still StockCodes such as M, Amazonfee etc. that need to be removed
#Let's see all the non-numeric StockCodes
misc_StockCode <- retail_clean %>%
  filter(!grepl('^[0-9]', StockCode))
```

There are only 4,793 observations with these ‘miscellaneous’ StockCodes,
however they can still affect our association analysis and must examine
these variables. Upon inspection of these observations, we can see that
there are StockCodes that resemble ‘Postage’, ‘Manual’ and ‘Dotcom
Postage’, all of which aren’t items that are sold by the retailer. On
the contrary, there are StockCodes such as ‘C2’ and ‘gift’ which
resemble ‘Carriage’ and ‘Gift card’ respectively and are items that the
retailer sells. We need to use our own judgement here to determine
whether to include certain observations, and I feel a good rule of thumb
is keep items that have multiple observations with the same StockCode,
Description and UnitPrice, and discard the rest. This shows that an item
is consistently priced for the same description, hence must be an item
sold by the retailer. We can modify the previous REGEX to include
non-numerics, along with StockCodes that begin with ‘DCGS’, ‘gift’, ‘C2’
and ‘SP’. We’ve display some of these ‘good’ and ‘bad’ StockCodes in a
tabular format below. We can subsequently update the retail\_clean data
frame to exclude those observations.

<div id="jkfbfgzzhb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jkfbfgzzhb .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 10px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jkfbfgzzhb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jkfbfgzzhb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jkfbfgzzhb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jkfbfgzzhb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 10px;
  border-bottom-color: #A8A8A8;
}

#jkfbfgzzhb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jkfbfgzzhb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jkfbfgzzhb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jkfbfgzzhb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jkfbfgzzhb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jkfbfgzzhb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jkfbfgzzhb .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#jkfbfgzzhb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jkfbfgzzhb .gt_from_md > :first-child {
  margin-top: 0;
}

#jkfbfgzzhb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jkfbfgzzhb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jkfbfgzzhb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#jkfbfgzzhb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jkfbfgzzhb .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jkfbfgzzhb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jkfbfgzzhb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jkfbfgzzhb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jkfbfgzzhb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jkfbfgzzhb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jkfbfgzzhb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jkfbfgzzhb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jkfbfgzzhb .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jkfbfgzzhb .gt_left {
  text-align: left;
}

#jkfbfgzzhb .gt_center {
  text-align: center;
}

#jkfbfgzzhb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jkfbfgzzhb .gt_font_normal {
  font-weight: normal;
}

#jkfbfgzzhb .gt_font_bold {
  font-weight: bold;
}

#jkfbfgzzhb .gt_font_italic {
  font-style: italic;
}

#jkfbfgzzhb .gt_super {
  font-size: 65%;
}

#jkfbfgzzhb .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table" style="table-layout: fixed;; width: 0px">
  <colgroup>
    <col style="width:200px;"/>
    <col style="width:400px;"/>
  </colgroup>
  <thead class="gt_header">
    <tr>
      <th colspan="2" class="gt_heading gt_title gt_font_normal" style><strong>Differentiating StockCodes</strong></th>
    </tr>
    <tr>
      <th colspan="2" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Segregating StockCodes into Good and Bad</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;">StockCode</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="font-weight: bold;">Description</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading" style="background-color: #259A00; color: #FFFFFF; font-weight: bold;">Good StockCodes</td>
    </tr>
    <tr><td class="gt_row gt_left">C2</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">CARRIAGE</td></tr>
    <tr><td class="gt_row gt_left">DCGS0076</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">SUNJAR LED NIGHT NIGHT LIGHT</td></tr>
    <tr><td class="gt_row gt_left">DCGS0003</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">BOXED GLASS ASHTRAY</td></tr>
    <tr><td class="gt_row gt_left">gift_0001_40</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">Dotcomgiftshop Gift Voucher £40.00</td></tr>
    <tr><td class="gt_row gt_left">DCGS0070</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">CAMOUFLAGE DOG COLLAR</td></tr>
    <tr><td class="gt_row gt_left">gift_0001_50</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">Dotcomgiftshop Gift Voucher £50.00</td></tr>
    <tr><td class="gt_row gt_left">gift_0001_30</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">Dotcomgiftshop Gift Voucher £30.00</td></tr>
    <tr><td class="gt_row gt_left">gift_0001_20</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">Dotcomgiftshop Gift Voucher £20.00</td></tr>
    <tr><td class="gt_row gt_left">DCGS0069</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">OOH LA LA DOGS COLLAR</td></tr>
    <tr><td class="gt_row gt_left">DCGSSBOY</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">BOYS PARTY BAG</td></tr>
    <tr><td class="gt_row gt_left">DCGSSGIRL</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">GIRLS PARTY BAG</td></tr>
    <tr><td class="gt_row gt_left">gift_0001_10</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">Dotcomgiftshop Gift Voucher £10.00</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading" style="background-color: #AA0000; color: #FFFFFF; font-weight: bold;">Bad StockCodes</td>
    </tr>
    <tr><td class="gt_row gt_left">POST</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">POSTAGE</td></tr>
    <tr><td class="gt_row gt_left">DOT</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">DOTCOM POSTAGE</td></tr>
    <tr><td class="gt_row gt_left">M</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">Manual</td></tr>
    <tr><td class="gt_row gt_left">BANK CHARGES</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">Bank Charges</td></tr>
    <tr><td class="gt_row gt_left">AMAZONFEE</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">AMAZON FEE</td></tr>
    <tr><td class="gt_row gt_left">m</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">Manual</td></tr>
    <tr><td class="gt_row gt_left">S</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">SAMPLES</td></tr>
    <tr><td class="gt_row gt_left">PADS</td>
<td class="gt_row gt_left" style="border-left-width: 3px; border-left-style: solid; border-left-color: #A8A8A8;">PADS TO MATCH ALL CUSHIONS</td></tr>
  </tbody>
  
  
</table>
</div>

## Including Plots

You can also embed plots, for example:

![](Association_Analysis_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
