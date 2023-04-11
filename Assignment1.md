---
title: "Assignment1"
author: "Nikola Baci"
date: "2023-02-02"
output: 
  html_document:
    toc: true
    theme: united
---




# License Applications in New York

In this notebook you will be presented with an exploratory analysis of the
license applications in the NYC metroploitan area.
The data is downloaded from NYC Data and that could be found [here](https://data.cityofnewyork.us/Business/License-Applications/ptev-4hud).



### Import the data

The zipcode data is to provide a unified pairing between zipcode and county because 
counties differ depending on what the applicant wrote which might not be correct.

We also do some cleaning of the column names and subset the data.
The data that we will focus is all the licenses that are issued in the 5 boroughs
in NYC.


```r
license_applications <-  read.csv("data.csv")
zipcodes <- read.csv("zipcodes.csv")

names(zipcodes) <- tolower(gsub("[.]", "_", names(zipcodes)))
zipcodes <- rename(zipcodes, zip = zip_code, city = common_cities)
zipcodes$zip <- as.character(zipcodes$zip)


df_orig <- license_applications

df_orig <- df_orig %>%
  select(Application.ID:End.Date, License.Category, Application.Category, City:Latitude)

#change names
names(df_orig) <- tolower(gsub("[.]", "_", names(df_orig)))
zipcodes <- select(zipcodes, -type)


df <- df_orig

#Limit the analysis of only businesses in from NY with an valid issued licence
df <- df %>%
  filter(state == "NY") %>%
  filter(status == "Issued") %>%
  mutate(contact_phone = case_when(contact_phone == "" ~ NA_character_,
                                   TRUE ~ contact_phone)) %>%
  mutate(end_date = mdy(end_date), start_date = mdy(start_date))

# Join the tables and remove duplicated columns
df <- df %>%
  left_join(zipcodes, by ="zip") %>%
  select(-city.x) %>%
  rename(city = city.y) %>%
  filter(!duplicated(application_id)) %>%
  filter(county %in% c("Queens County", "Bronx County","Kings County", "Richmond County", "New York County"))
```


### Understanding the dataset

Looking at only the 5 boroughs, the data shrinks by almost 90K observations which 
are licenses issued on other counties upstate or in Long Island.

About a third (see below) of those observations do not have either contact_phone, and/or longitude,
and/or latitude data. The last two can be approximated using the full business 
physical address, but this will be left as an add-on improvement feature.


```r
# check for duplicates
colSums(is.na(df) / nrow(df))
```

```
##         application_id         license_number           license_type application_or_renewal          business_name 
##              0.0000000              0.0000000              0.0000000              0.0000000              0.0000000 
##                 status             start_date               end_date       license_category   application_category 
##              0.0000000              0.0000000              0.0000000              0.0000000              0.0000000 
##                  state                    zip          contact_phone              longitude               latitude 
##              0.0000000              0.0000000              0.3313588              0.3168868              0.3168868 
##                   city                 county 
##              0.0000000              0.0000000
```

### Top 10 Zipcodes
Here are 10 zip codes with the most licenses issued. The first-place zip code is located
in Queens County, while Kings County has more zip codes in the top ten places.


```r
df %>%
  group_by(zip, county) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  top_n(10) %>% 
  ggplot() +
  geom_col(aes(fct_reorder(zip, count), count)) +
  coord_flip() +
  theme_bw() +
  ylab("Count") +
  xlab("Zips") +
  ggtitle("Top 10 zipcodes by number of applications")
```
![Top 10 zips](www\\topzips.png)



### Overall Distribution

Investigating the total number of issued licenses over the years, we see an
enormous jump from 2013 with roughly 10,000 licenses to 2014 with almost 40,000
licenses issued. Did people become more entrepreneurial in just one year? Or is this
some anomaly with the data?


```r
df %>%
  ggplot() +
  geom_bar(aes(year(end_date))) +
  theme_bw() +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Number of Applications 2000-2023")
```

![Overall distribution](www\\alldist.png)


### What is going on?

As you can see from the table below, the majority of the licenses issued before 2014
regarded the Tobacco shops, and the rest of the categories had very little records.

Either NY passed a law that required businesses to have a license to operate (a hypothesis
I could not prove due to the lack of information online) or New Yorkers embarked on what
seems to be an entrepreneurial journey that continues to thrive despite Covid-19 pandemic.
However, the second option is highly unlikely due to very low number and limited categories
as seen in the table


```r
df %>%
  filter(year(end_date) < 2014) %>% 
  group_by(license_category) %>%
  summarise(count = n()) %>%
  gt()
```

<!--html_preserve--><div id="rnchchagua" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#rnchchagua .gt_table {
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
  border-top-width: 2px;
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

#rnchchagua .gt_heading {
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

#rnchchagua .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rnchchagua .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rnchchagua .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rnchchagua .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnchchagua .gt_col_headings {
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

#rnchchagua .gt_col_heading {
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

#rnchchagua .gt_column_spanner_outer {
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

#rnchchagua .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rnchchagua .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rnchchagua .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rnchchagua .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
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
  text-align: left;
}

#rnchchagua .gt_empty_group_heading {
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

#rnchchagua .gt_from_md > :first-child {
  margin-top: 0;
}

#rnchchagua .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rnchchagua .gt_row {
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

#rnchchagua .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#rnchchagua .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#rnchchagua .gt_row_group_first td {
  border-top-width: 2px;
}

#rnchchagua .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnchchagua .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rnchchagua .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rnchchagua .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnchchagua .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnchchagua .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rnchchagua .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rnchchagua .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rnchchagua .gt_footnotes {
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

#rnchchagua .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnchchagua .gt_sourcenotes {
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

#rnchchagua .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rnchchagua .gt_left {
  text-align: left;
}

#rnchchagua .gt_center {
  text-align: center;
}

#rnchchagua .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rnchchagua .gt_font_normal {
  font-weight: normal;
}

#rnchchagua .gt_font_bold {
  font-weight: bold;
}

#rnchchagua .gt_font_italic {
  font-style: italic;
}

#rnchchagua .gt_super {
  font-size: 65%;
}

#rnchchagua .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#rnchchagua .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rnchchagua .gt_indent_1 {
  text-indent: 5px;
}

#rnchchagua .gt_indent_2 {
  text-indent: 10px;
}

#rnchchagua .gt_indent_3 {
  text-indent: 15px;
}

#rnchchagua .gt_indent_4 {
  text-indent: 20px;
}

#rnchchagua .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="license_category">license_category</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="count">count</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="license_category" class="gt_row gt_left">Cabaret</td>
<td headers="count" class="gt_row gt_right">1</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Catering Establishment</td>
<td headers="count" class="gt_row gt_right">1</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Garage</td>
<td headers="count" class="gt_row gt_right">59</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Garage and Parking Lot</td>
<td headers="count" class="gt_row gt_right">2</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">General Vendor</td>
<td headers="count" class="gt_row gt_right">25</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Home Improvement Contractor</td>
<td headers="count" class="gt_row gt_right">2</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Home Improvement Salesperson</td>
<td headers="count" class="gt_row gt_right">4</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Laundry</td>
<td headers="count" class="gt_row gt_right">11</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Laundry Jobber</td>
<td headers="count" class="gt_row gt_right">2</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Parking Lot</td>
<td headers="count" class="gt_row gt_right">6</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Secondhand Dealer - Auto</td>
<td headers="count" class="gt_row gt_right">4</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Secondhand Dealer - General</td>
<td headers="count" class="gt_row gt_right">1</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Sidewalk Cafe</td>
<td headers="count" class="gt_row gt_right">4</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Special Sale</td>
<td headers="count" class="gt_row gt_right">1</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Stoop Line Stand</td>
<td headers="count" class="gt_row gt_right">1</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Tobacco Retail Dealer</td>
<td headers="count" class="gt_row gt_right">55929</td></tr>
  </tbody>
  
  
</table>
</div><!--/html_preserve-->


### Processing Time

The Department of Consumer and Worker Protection (DCWP) seems to be very
efficient with the applications since half of the applications are closed
only one day after the application is submitted, 75% of applications are 
closed not later than 3 days, and 98.5% of all the applications is closed 
within 3 months.

By looking at the table below, we can see in descending order the license 
categories that take the most time to get processed and issued. 

Pedicab businesses take the most time on average to get a license with about 
34-35 days to get a license. The second category that takes the longest for a 
license issued are the Tow Truck companies at 31 days and then the Car Wash 
businesses at 17 days. 

This time is highly possible to be due to applicant submitting an incomplete
application and need to retain those documents before the license is issued.


```r
# create the data frame
period <- df %>%
  filter(end_date >= start_date) %>% #some data have a end_date before start_date
  filter(year(end_date) > 2013) %>% #only look at data from 2014 and on
  mutate(days = as.integer(end_date - start_date)) #calculate the days

# Quartiles and min/max
summary(period$days)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##    0.000    1.000    1.000    6.952    3.000 3787.000
```

```r
# % of issued licenses before 3 months = 98.5%
nrow(filter(period, days < 90)) / nrow(period)
```

```
## [1] 0.9855194
```

```r
# table the top 10 slowest
period %>%
  group_by(license_category) %>%
  summarise(days = median(days)) %>%
  arrange(desc(days)) %>%
  top_n(10) %>%
  gt()
```

```
## Selecting by days
```

<!--html_preserve--><div id="lrftwtlhmc" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#lrftwtlhmc .gt_table {
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
  border-top-width: 2px;
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

#lrftwtlhmc .gt_heading {
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

#lrftwtlhmc .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#lrftwtlhmc .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#lrftwtlhmc .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#lrftwtlhmc .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lrftwtlhmc .gt_col_headings {
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

#lrftwtlhmc .gt_col_heading {
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

#lrftwtlhmc .gt_column_spanner_outer {
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

#lrftwtlhmc .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#lrftwtlhmc .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#lrftwtlhmc .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#lrftwtlhmc .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
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
  text-align: left;
}

#lrftwtlhmc .gt_empty_group_heading {
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

#lrftwtlhmc .gt_from_md > :first-child {
  margin-top: 0;
}

#lrftwtlhmc .gt_from_md > :last-child {
  margin-bottom: 0;
}

#lrftwtlhmc .gt_row {
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

#lrftwtlhmc .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#lrftwtlhmc .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#lrftwtlhmc .gt_row_group_first td {
  border-top-width: 2px;
}

#lrftwtlhmc .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lrftwtlhmc .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#lrftwtlhmc .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#lrftwtlhmc .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lrftwtlhmc .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#lrftwtlhmc .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#lrftwtlhmc .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#lrftwtlhmc .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#lrftwtlhmc .gt_footnotes {
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

#lrftwtlhmc .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lrftwtlhmc .gt_sourcenotes {
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

#lrftwtlhmc .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#lrftwtlhmc .gt_left {
  text-align: left;
}

#lrftwtlhmc .gt_center {
  text-align: center;
}

#lrftwtlhmc .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#lrftwtlhmc .gt_font_normal {
  font-weight: normal;
}

#lrftwtlhmc .gt_font_bold {
  font-weight: bold;
}

#lrftwtlhmc .gt_font_italic {
  font-style: italic;
}

#lrftwtlhmc .gt_super {
  font-size: 65%;
}

#lrftwtlhmc .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#lrftwtlhmc .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#lrftwtlhmc .gt_indent_1 {
  text-indent: 5px;
}

#lrftwtlhmc .gt_indent_2 {
  text-indent: 10px;
}

#lrftwtlhmc .gt_indent_3 {
  text-indent: 15px;
}

#lrftwtlhmc .gt_indent_4 {
  text-indent: 20px;
}

#lrftwtlhmc .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="license_category">license_category</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="days">days</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="license_category" class="gt_row gt_left">Pedicab Business</td>
<td headers="days" class="gt_row gt_right">34.5</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Tow Truck Company</td>
<td headers="days" class="gt_row gt_right">31.0</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Car Wash</td>
<td headers="days" class="gt_row gt_right">17.0</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Sightseeing Bus</td>
<td headers="days" class="gt_row gt_right">13.5</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Commercial Lessor</td>
<td headers="days" class="gt_row gt_right">7.0</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Laundries</td>
<td headers="days" class="gt_row gt_right">7.0</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Ticket Seller</td>
<td headers="days" class="gt_row gt_right">7.0</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Amusement Arcade</td>
<td headers="days" class="gt_row gt_right">3.5</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Bingo Game Operator</td>
<td headers="days" class="gt_row gt_right">3.0</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Construction Labor Provider</td>
<td headers="days" class="gt_row gt_right">3.0</td></tr>
    <tr><td headers="license_category" class="gt_row gt_left">Ticket Seller Business</td>
<td headers="days" class="gt_row gt_right">3.0</td></tr>
  </tbody>
  
  
</table>
</div><!--/html_preserve-->

### Distribution for every year

Below are the graphs from 2014 to 2022 with the total applications submitted for
each month (bars-left y axis) and the average timeto process for each month 
(red line - right y axis). Generally,the distributions are bimodal, which lets 
us know that the winter months are the busiest for the DCWP.



```r
over_months <- df %>%
  filter(year(start_date) > 2013, year(start_date) < 2023) %>%
  mutate(year = year(start_date), month = month(start_date, label = T)) %>%
  mutate(days = as.integer(end_date - start_date)) %>%
  filter(days > 0) %>%
  group_by(year, month) %>%
  summarise(count = n(), avg = mean(days)) %>%
  ungroup()


over_months %>%
  ggplot() +
  geom_col(aes(x = month, y = count)) +
  geom_line(aes(x = month, y = avg * 100 , group = 1), color = "red") +
  scale_y_continuous("Applications Count", 
                     breaks = seq(0, 10000, 2000),
                     sec.axis = sec_axis(~ . /100, name = "Average Days")) +
  facet_wrap(~year) +
  theme_bw() +
  xlab("Month") +
  ggtitle("Year by Year Distribution and Average Processing Time") +
  theme(axis.text.x = element_text(angle = 45))
```

![Distributions by year](www\\inddist.png)

### Forecasting Until 2025

In this last analysis piece, I fit the data through the Holt-Winter's exponential
smoothing forecasting model with the indent to capture the number of applications
in the next 2 years to come. The solid blue line represents the mean, the dark
purple area indicates 50% confidence interval and the lighter purple area 
indicates 90% confidence interval.


```r
dates <- df %>%
  filter(year(end_date) > 2013) %>%
  group_by(year(end_date), month(end_date)) %>%
  summarise(count = n())

dfts <- ts(dates[, 3], start = c(2014, 0), frequency = 12)

autoplot(hw(dfts, h = 24, seasonal = "multiplicative", level = c(50, 90))) +
  theme_bw() +
  xlab("Time") +
  ylab("Application Count") +
  ggtitle("Forecast For The Next 2 Years ")
```
![Forecast](www\\forecast.png)






















