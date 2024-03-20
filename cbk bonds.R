#clear working environment 
rm(list = ls(all = T))

#set working emvironment 
setwd('E:/Archives/The Point Sets')


#libraries 
library(tidyverse)
library(janitor)
library(splitstackshape)
library(lubridate)
library(scales)
library(showtext)
library(ggrepel)



#add fonts
font_add_google("Quattrocento Sans", "raleway")
showtext_auto()

#get the fonts 
font_family1 = 'Quattrocento Sans'
font_family2 = 'raleway'

caption_label = 'Average Coupon Rates for Bonds \nsource: Central Bank of Kenya'
caption_label = 'Data Visualizations by Point Analytics\n Data Source:Central Bank of Kenya(CBK)\n Email: datapowereddecisons@gmail.com \n X: @_PointAnalytics Instagram:_pointanalytics'


#read df
df = readr::read_csv("data/cbk datasets/Issues of Treasury Bonds.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(date = as.Date(issue_date, format = "%d/%m/%Y")) %>% 
  dplyr::filter(!is.na(date)) %>% 
  dplyr::mutate(issue_no = str_replace_all(issue_no, "-","/")) %>% 
  splitstackshape::cSplit('issue_no', sep = "/", drop = F) %>% 
  dplyr::select(-issue_no_2, -issue_no_3) %>% 
  dplyr::rename(cat = issue_no_1) %>% 
  dplyr::mutate(cat = gsub("\\s+", "", cat), 
                cat = case_when(str_detect(cat, "FXD")~"Fixed Rate Bond", 
                                 str_detect(cat, 'IFB')~"Infrastructure Bond",
                                 str_detect(cat, "MAB")~"Monetary Absorption Bond",
                                 str_detect(cat, "SDB")~"Special Development Bond.",
                                 # str_detect(cat, "SDB")~"Special Development Bond.",
                                 TRUE~NA), 
                year = lubridate::year(date)) %>% 
  dplyr::filter(!is.na(cat))

#get outlier bonds 
outliers = df %>% 
  dplyr::filter(year != 2024) %>%
  dplyr::filter(cat == "Fixed Rate Bond"|cat == "Infrastructure Bond") %>% 
  dplyr::select(date=issue_date, issue_no, isin_number, face_value_kshs_millions, coupon_rate, tenor, cat, year) %>% 
  dplyr::group_by(isin_number,issue_no, year, cat) %>%
  dplyr::summarise(isin_number = paste(unique(isin_number), collapse = ", "),
                   cat = paste(unique(cat), collapse = ", "),
                   issue_no = paste(unique(issue_no), collapse = ", "),
                  face_value_kshs_millions = round(sum(face_value_kshs_millions, na.rm = T),2), 
                  coupon = round(mean(coupon_rate, na.rm = T)/100,4), 
                  tenor = round(mean(tenor, na.rm = T),2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cat) %>% 
  dplyr::slice_max(coupon, n=6) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year, cat) %>% 
  dplyr::slice_max(coupon, n=1) 

outliers_low = df %>% 
  # dplyr::filter(year != 2024) %>%
  dplyr::filter(cat == "Fixed Rate Bond"|cat == "Infrastructure Bond") %>% 
  dplyr::select(date=issue_date, issue_no, isin_number, face_value_kshs_millions, coupon_rate, tenor, cat, year) %>% 
  dplyr::group_by(isin_number,issue_no, year, cat) %>%
  dplyr::summarise(isin_number = paste(unique(isin_number), collapse = ", "),
                   cat = paste(unique(cat), collapse = ", "),
                   issue_no = paste(unique(issue_no), collapse = ", "),
                   face_value_kshs_millions = round(sum(face_value_kshs_millions, na.rm = T),2), 
                   coupon = round(mean(coupon_rate, na.rm = T)/100,4), 
                   tenor = round(mean(tenor, na.rm = T),2)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(cat) %>% 
  dplyr::slice_min(coupon, n=2) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year, cat) %>% 
  dplyr::slice_min(coupon, n=1) 



avg_coupon = df %>% 
  dplyr::group_by(year, cat) %>% 
  dplyr::summarise(face_value_kshs_millions = round(sum(face_value_kshs_millions, na.rm = T),2), 
                   coupon = round(mean(coupon_rate, na.rm = T)/100,4), 
                   tenor = round(mean(tenor, na.rm = T),2))


avg_coupon %>% 
  dplyr::filter(cat == "Fixed Rate Bond"|cat == "Infrastructure Bond") %>% 
  ggplot(aes(year, coupon, color = cat))+
  geom_point(size = 2)+
  #plot for the outliers 
  geom_point(data = outliers %>% 
              dplyr::filter(cat =="Fixed Rate Bond"),
            aes(year, coupon), color = '#2279aa',shape = 18, size = 3.5)+
  geom_point(data = outliers %>% 
              dplyr::filter(cat =="Infrastructure Bond"),
            aes(year, coupon), color = '#e4ae0f',shape = 18, size = 3.5)+
  geom_point(data = outliers_low,
             aes(year, coupon), color = 'red',shape = 18, size = 3.5)+
  #plot the general data
  geom_line(data = avg_coupon %>% 
               dplyr::filter(cat =="Fixed Rate Bond"),
             aes(year, coupon), color = '#2279aa', linetype = "dashed", size = .8)+
  geom_line(data = avg_coupon %>% 
              dplyr::filter(cat =="Infrastructure Bond"),
            aes(year, coupon), color = '#e4ae0f', size = .8)+
  geom_text_repel(data = outliers,aes(label = paste(issue_no, "\n",scales::dollar_format(prefix = "Ksh.", suffix = "B", accuracy = 0.1)(face_value_kshs_millions/1000), " (CR:", scales::percent(coupon, accuracy = 0.1),")", sep = "")),
                                      size = 4, #fontface = "bold",
                                      family = font_family2)+
  geom_text_repel(data = outliers_low,aes(label = paste(issue_no, "\n",scales::dollar_format(prefix = "Ksh.", suffix = "B", accuracy = 0.1)(face_value_kshs_millions/1000), " (CR:", scales::percent(coupon, accuracy = 0.1),")", sep = "")),
                  size = 4, 
                  color = "red",
                  family = font_family2)+
  # geom_hline(yintercept = 0.126, linetype = "dashed", color = 'grey30')+
  scale_y_continuous(labels = percent_format())+
  scale_colour_manual(values = c('#2279aa', '#e4ae0f'))+
  scale_x_continuous(breaks = seq(2006, 2025, by = 2),
                     labels = seq(2006, 2025, by = 2))+
  
  labs(caption = caption_label,
       y = "Avg Coupon Rate", 
       x = "Year")+
  theme(legend.position = 'bottom', 
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.grid.major = element_line(colour = "#c8c8c8", size = .2),
        legend.title = element_blank(),
        
        axis.title.x = element_blank(),
        legend.text = element_text(family = font_family2, size = 12), 
        axis.ticks = element_line(color = "#fafafa"),
        axis.line = element_line(color = "#fafafa", size = .3),
        axis.text = element_text(family = "raleway", lineheight = 0.4, size = 13.5), 
        plot.title = element_text(family = "passion", size = 80),
        plot.subtitle = element_text(margin = margin(b = 20), family = "raleway", lineheight = 0.4, size = 24),
        plot.margin = margin(10, 15, 0, 10),
        axis.title = element_text(family = font_family2, size = 15),
        plot.caption = element_text(size = 8, face = 'italic', color = 'grey30'),
        strip.text.x = element_text(family = "raleway", lineheight = 0.4, size = 20, face = "bold"))

