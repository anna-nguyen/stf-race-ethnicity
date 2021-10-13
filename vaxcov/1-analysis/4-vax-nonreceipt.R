##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Find percentages of each reported reason
# for vaccine non-receipt
##########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))
library(purrr)

load(district_demographics_path)

data = read.csv(data_path_2017)
data$dist=factor(data$dist,levels=c("WCCUSD","OUSD"))

data %>% filter(vx1617 != 9 & vx1516 != 9 & vx1415 != 9) %>% 
  filter(!(vx1617 == vx1516 & vx1516 == vx1415)) %>%
  filter(vx1617 == vx1516 | vx1516 == vx1415) %>% 
  group_by(dist) %>% count()

data.y4 = read.csv(data_path_2018)
data.y4$dist=factor(data.y4$dist,levels=c("WCCUSD","OUSD"))

# drop Wilson because it has no matched pair
data.y4=data.y4[data.y4$schoolname!="Wilson Elementary",]

data.y4$vx1718yn=ifelse(data.y4$vx1718==1,1,0)

white.y4 = data.y4 %>% filter(race == "White")
API.y4 = data.y4 %>% filter(race == "Asian" | race == "Pacific islander")
multi.y4 = data.y4 %>% filter(race == "Multi")
black.y4 = data.y4 %>% filter(race == "Black")
latino.y4 = data.y4 %>% filter(race == "Latino")

white.y4.no.missing = white.y4 %>% filter(whynot != "Missing")
black.y4.no.missing = black.y4 %>% filter(whynot != "Missing")
api.y4.no.missing = API.y4 %>% filter(whynot != "Missing")
latino.y4.no.missing = latino.y4 %>% filter(whynot != "Missing")
multi.y4.no.missing = multi.y4 %>% filter(whynot != "Missing")


#nonreceipt
white_nonreceipt = as.data.frame(prop.table(table(white.y4.no.missing$whynot,white.y4.no.missing$dist), 2))
colnames(white_nonreceipt) = c("Reason", "District", "Proportion")
white_nonreceipt$Race = "White"

black_nonreceipt = as.data.frame(prop.table(table(black.y4.no.missing$whynot,black.y4.no.missing$dist), 2))
colnames(black_nonreceipt) = c("Reason", "District", "Proportion")
black_nonreceipt$Race = "African American"
 
api_nonreceipt = as.data.frame(prop.table(table(api.y4.no.missing$whynot,api.y4.no.missing$dist), 2))
colnames(api_nonreceipt) = c("Reason", "District", "Proportion")
api_nonreceipt$Race = "Asian/Pacific Islander"
 
latino_nonreceipt = as.data.frame(prop.table(table(latino.y4.no.missing$whynot,latino.y4.no.missing$dist), 2))
colnames(latino_nonreceipt) = c("Reason", "District", "Proportion")
latino_nonreceipt$Race = "Hispanic/Latino"
 
multi_nonreceipt = as.data.frame(prop.table(table(multi.y4.no.missing$whynot,multi.y4.no.missing$dist), 2))
colnames(multi_nonreceipt) = c("Reason", "District", "Proportion")
multi_nonreceipt$Race = "Multiple"
 
nonreceipt_tbl = bind_rows(white.y4.no.missing, black.y4.no.missing, api.y4.no.missing, latino.y4.no.missing, multi.y4.no.missing)
nonreceipt_tbl = nonreceipt_tbl %>% bind_rows() %>% 
  mutate(reason_cat = case_when(whynot == "It costs too much" ~ "Logistics",
                                whynot == "I didn’t have time to take my student to the doctor" ~ "Logistics",
                                whynot == "I thought my student needed health insurance to get it" ~ "Logistics",
                                whynot == "I didn’t know where to get it" ~ "Logistics", 
                                whynot == "I didn’t receive the consent form to get the vaccine at school" ~ "SLIV-specific concerns", 
                                whynot == "I forgot to return the consent form to get the vaccine at school" ~ "SLIV-specific concerns",
                                whynot == "I didn’t want to share my insurance information on the consent form to get the vaccine at school" ~ "SLIV-specific concerns",
                                whynot == "I don’t believe in it" ~ "Non-belief", 
                                whynot == "I believe it might make my student sick" ~ "Non-belief",
                                whynot == "My student is afraid of needles" ~ "Logistics",
                                whynot == "I didn’t trust schools to vaccinate my student" ~ "SLIV-specific concerns", 
                                whynot == "Our doctor did not recommend it" ~ "Non-belief"),
         Logistics = ifelse(reason_cat == "Logistics", 1, 0), 
         Nonbelief = ifelse(reason_cat == "Non-belief", 1, 0), 
         Distrust = ifelse(reason_cat == "Distrust", 1, 0),
         SLIVreason = ifelse(reason_cat == "SLIV-specific concerns", 1, 0),
         race = case_when(race == "Asian" ~ "Asian/Pacific Islander",
                          race == "Pacific islander" ~ "Asian/Pacific Islander",
                          race == "Black" ~ "Black/African American",
                          race == "Multi" ~ "Multiple Races",
                          T ~ as.character(race)))


saveRDS(nonreceipt_tbl, paste0(local_res_path, "/reasons_for_nonreceipt.RDS"))



nonbelief_specific_tbl = nonreceipt_tbl %>% 
  filter(reason_cat == "Non-belief") %>% 
  group_by(district, whynot) %>% 
  summarize(count = n()) %>%  
  mutate(percent = round(count/sum(count) * 100, 1), 
         "N (%)" = paste0(count, " (", percent, "%)"), 
         district = ifelse(district == "OUSD", "Intervention", "Comparison")) %>% 
  ungroup() %>% 
  dplyr::select(District = district, 
                "Reason for Vaccine Non-Receipt" = whynot, 
                `N (%)`)

write.csv(nonbelief_specific_tbl, paste0(local_tab_path, "/nonbelief_specific_tbl.csv"))