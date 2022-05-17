library(unvotes)
library(tidyverse)

joined <- un_votes %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  ungroup()

joined$year <- format(joined$date, format = "%Y")


joined %>% 
  filter(issue == "Palestinian conflict") -> pal

#### Important votes ####### 
joined %>% 
  # Peaceful settlement of the question of Palestine 	
  mutate(importantvote = ifelse(unres == "A/RES/74/11", 1, importantvote))

joined %<>% 
  # Committee on the Exercise of the Inalienable Rights of the Palestinian People 	
  mutate(importantvote = ifelse(unres == "A/RES/74/10", 1, importantvote)) 
joined %<>% 
  # Division for Palestinian Rights of the Secretariat 	
  mutate(importantvote = ifelse(unres == "A/RES/74/12", 1, importantvote)) 

joined %<>% 
  # Further practical measures for the prevention of an arms race in outer space 	
  mutate(importantvote = ifelse(unres == "A/RES/74/34", 1, importantvote))

joined %<>% 
  # Implementation of the Convention on the Prohibition  Use of Chemical Weapons
  mutate(importantvote = ifelse(unres == "A/RES/74/40", 1, importantvote)) 

joined %<>% 
  # Treaty on the Prohibition of Nuclear Weapons
  mutate(importantvote = ifelse(unres == "A/RES/74/41", 1, importantvote))

# Nuclear disarmament
joined %<>% 
  mutate(importantvote = ifelse(unres == "A/RES/74/45", 1, importantvote))  

joined %<>% 
  # Convention on the Prohibition of the Use of Nuclear Weapons 
  mutate(importantvote = ifelse(unres == "A/RES/74/68", 1, importantvote)) 

joined %<>% 
  mutate(importantvote = ifelse(unres == "A/RES/74/75", 1, importantvote)) 

joined %<>% 
  # Assistance to Palestine Refugees
  mutate(importantvote = ifelse(unres == "A/RES/74/83", 1, importantvote)) 

# Work of the Special Committee to Investigate Israeli Practices Affecting the Human
#Rights of the Palestinian People and Other Arabs of the Occupied Territories 
joined %<>% 
  mutate(importantvote = ifelse(unres == "A/RES/74/87", 1, importantvote)) 

joined %<>%
  # Office of the United Nations High Commissioner for Refugees
  mutate(importantvote = ifelse(unres == "A/RES/74/130", 1, importantvote)) 

joined %<>%
  # Report of the Human Rights Council 
  mutate(importantvote = ifelse(unres == "A/RES/74/132", 1, importantvote))

joined %<>%
  # Combating glorification of Nazism, neo-Nazism 
  mutate(importantvote = ifelse(unres == "A/RES/74/136", 1, importantvote)) 

joined %<>%
  # The right of the Palestinian people to self-determination
  mutate(importantvote = ifelse(unres == "A/RES/74/139", 1, importantvote)) 

joined %<>%
  # Situation of human rights in the Islamic Republic of Iran
  mutate(importantvote = ifelse(unres == "A/RES/74/167", 1, importantvote))

joined %<>%
  # Situation of human rights in the Autonomous Republic of Crimea
  mutate(importantvote = ifelse(unres == "A/RES/74/168", 1, importantvote)) 

joined %<>%
  # Situation of human rights in the Syrian Arab Republic
  mutate(importantvote = ifelse(unres == "A/RES/74/169", 1, importantvote))

joined %<>%
  # Commodities
  mutate(importantvote = ifelse(unres == "A/RES/74/204", 1, importantvote))

# Agricultural technology for sustainable development
joined %<>%
  mutate(importantvote = ifelse(unres == "A/RES/74/215", 1, importantvote))

# Role of the United Nations in promoting development
joined %<>%
  mutate(importantvote = ifelse(unres == "A/RES/74/228", 1, importantvote))

# Eradicating rural poverty to implement the 2030 Agenda for Sustainable 
joined %<>%
  mutate(importantvote = ifelse(unres == "A/RES/74/237", 1, importantvote)) 

# Situation of human rights of Rohingya Muslims and other minorities in Myanmar
joined %<>%
  mutate(importantvote = ifelse(unres == "A/RES/74/246", 1, importantvote)) 

# Countering the use of information and communications technologies for criminal 
joined %<>%
  mutate(importantvote = ifelse(unres == "A/RES/74/247", 1, importantvote)) 




############### 2018 ################

#Assistance to Palestine refugees
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/92", 1, importantvote)) 

# Promotion and protection of human rights and fundamental freedoms
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/173", 1, importantvote)) 

# Global Compact for Safe, Orderly and Regular Migration
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/195", 1, importantvote)) 

# Judgement of the International Court of Justice of 31 March 2004
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/257", 1, importantvote)) 

#Cooperation between the United Nations and the Organization for the Prohibition of Chemical Weapons
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/258", 1, importantvote)) 

#Situation of human rights in the Autonomous Republic of Crimea
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/263", 1, importantvote)) 

# Advancing responsible State behaviour in cyberspace security
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/266", 1, importantvote)) 

# Ending the economic, commercial and financial embargo 
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/8", 1, importantvote)) 

# Exercise of the Inalienable Rights of the Palestinian People
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/18", 1, importantvote)) 

# Division for Palestinian Rights of the Secretariat
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/21", 1, importantvote)) 

# Persons displaced as a result of the June 1967 and subsequent hostilities
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/93", 1, importantvote)) 

# Operations of the United Nations Relief and Works Agency for Palestine Refugees
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/95", 1, importantvote)) 

# Palestine refugees' properties and their revenues
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/94", 1, importantvote)) 

# Investigate Israeli Practices Affecting the Human rights
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/96", 1, importantvote)) 

# Geneva Convention relative to the Protection of Civilian
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/97", 1, importantvote)) 

# Israeli settlements in the Occupied Palestinian Territory, including East Jerusalem
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/98", 1, importantvote)) 

#  Israeli practices affecting the human rights of the Palestinian people in the Occupied
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/99", 1, importantvote)) 

# Situation of human rights in the Islamic Republic of Iran 
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/181", 1, importantvote)) 

# Situation of human rights in the Syrian Arab Republic
joined %<>%
  mutate(importantvote = ifelse(unres == "R/73/182", 1, importantvote)) 

########################

joined$importantvote[is.na(joined$importantvote)] <- 0


### Looking at important votes - USA

important <- joined %>%
  filter(importantvote == 1) %>%
  select(rcid, country, vote, year)

important %>%
  mutate(vote_fac = as.factor(vote)) %>%
  mutate(vote_num = as.numeric(vote_fac)) %>%
  distinct(year, country, rcid, vote_num, .keep_all = "TRUE") %>%
  select(rcid, country, vote_num, year) %>%
  pivot_wider(names_from = country, values_from = "vote_num" ) -> imp_wide

imp_wide[is.na(imp_wide)] = 0

for(j in 1:nrow(imp_wide))
{
  for(i in 4:ncol(imp_wide))
  {
    if (imp_wide[j,3] == imp_wide[j,i])
    {
      imp_wide[j,i] = 1
    }else{
      imp_wide[j,i] = 0}
  }
}

imp_long <- imp_wide %>% 
  gather(country, vote,`Canada`:`South Sudan`) %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same_us = sum(vote), 
         vote_percent_same_us = vote_same_us / n) %>% 
  distinct(year, country, vote_percent_same_us, .keep_all = TRUE) %>% 
  select(year, country, important_votes = n, everything()) %>% 
  select(!c(rcid, vote, `United States`)) %>% 
  ungroup() -> us_imp_votes


View(us_imp_votes)
#### Looking at all votes =  Similar to US ####


joined %>%
  mutate(vote_fac = as.factor(vote)) %>%
  mutate(vote_num = as.numeric(vote_fac)) %>%
  distinct(year, session, country, rcid, vote_num, .keep_all = "TRUE") %>% 
  select(rcid, year, country, vote_num) %>%
  pivot_wider(names_from = country, values_from = "vote_num" ) -> all_wide


all_wide[is.na(all_wide)] = 0

View(all_wide)

for(j in 1:nrow(all_wide))
{
  for(i in 4:ncol(all_wide))
  {
    if (all_wide[j,3] == all_wide[j,i])
    {
      all_wide[j,i] = 1
    }else{
      all_wide[j,i] = 0}
  }
}

# Percentage total votes
all_long <- all_wide %>% 
  gather(country, vote,`Canada`:`South Sudan`) %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same_us_all = sum(vote), 
         vote_percent_same_us_all = vote_same_us_all / n) %>% 
  distinct(year, country, vote_percent_same_us_all, .keep_all = TRUE) %>% 
  select(year, session, country, all_votes = n, everything()) %>% 
  select(!c(rcid, vote, `United States`)) %>% 
  ungroup() -> us_all_votes

un_year <- us_all_votes %>% 
  select(year, session, country)  

un_votes2 <- cbind(un_year, un_votes)

un_votes2 <- merge(un_votes, un_year, by = c("session", "country"), all.y = TRUE)

un_votes2 %<>% 
  select(session, year, country, everything())

#### China #### ALL VOTES
  
all_wide_china <- all_wide %>%  
  select(rcid, year, China, everything())

all_wide_china[is.na(all_wide_china)] <- 0
  
for(j in 1:nrow(all_wide_china))
  {
    for(i in 4:ncol(all_wide_china))
    {
      if (all_wide_china[j,3] == all_wide_china[j,i])
      {
        all_wide_china[j,i] = 1
      }else{
        all_wide_china[j,i] = 0}
    }
  }

# Percentage total votes
all_long_china <- all_wide_china %>% 
  gather( country, vote,`United States`:`South Sudan`) %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same = sum(vote), 
         vote_percent = vote_same / n) %>% 
  distinct(year, country, vote_percent, .keep_all = TRUE) %>% 
  select(year, country, all_votes = n, everything()) %>% 
  select(!c(rcid, vote, `China`)) %>% 
  ungroup() -> all_china_votes

View(all_china_votes)

all_china_votes %<>% 
  select(year, country, all_votes, vote_same_china = vote_same,
         vote_same_china_percent = vote_percent)



####

all_wide_russia <- all_wide %>%  
  select(rcid, year, Russia, everything())

all_wide_russia[is.na(all_wide_russia)] <- 0

View(all_wide_russia)

for(j in 1:nrow(all_wide_russia))
{
  for(i in 4:ncol(all_wide_russia))
  {
    if (all_wide_russia[j,3] == all_wide_russia[j,i])
    {
      all_wide_russia[j,i] = 1
    }else{
      all_wide_russia[j,i] = 0}
  }
}

View(all_russia_votes)
# Wide to long

# Percentage total votes
all_long_russia <- all_wide_russia %>% 
  gather(country, vote,`United States`:`South Sudan`) %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same = sum(vote), 
         vote_percent = vote_same / n) %>% 
  distinct(year, country, vote_percent, .keep_all = TRUE) %>% 
  select(year, country, all_votes = n, everything()) %>% 
  select(!c(rcid, vote, `Russia`)) %>% 
  ungroup() -> all_russia_votes

all_russia_votes %<>% 
  select(year, country, all_votes, vote_same_russia = vote_same,
         vote_same_russia_percent = vote_percent)

total_all_votes <- merge(all_russia_votes, all_china_votes, all.x = TRUE)

total_all_votes <- merge(total_all_votes, us_all_votes, all.x = TRUE)

total_all_votes %<>% 
  select(year, country, 
         vote_same_us_all, 
         vote_percent_same_us_all, 
         
         vote_same_china_all = vote_same_china,
         vote_percent_same_china_all = vote_same_china_percent,
         
         vote_same_russia_all = vote_same_russia,
         vote_percent_same_russia_all = vote_same_russia_percent)

View(total_all_votes)


write.csv(total_all_votes, "total_all_votes.csv")

########################################################################

important %>%
  mutate(vote_fac = as.factor(vote)) %>%
  mutate(vote_num = as.numeric(vote_fac)) %>%
  distinct(year, country, rcid, vote_num, .keep_all = "TRUE") %>%
  select(rcid, country, vote_num, year) %>%
  pivot_wider(names_from = country, values_from = "vote_num" ) -> imp_wide

imp_try = imp_wide

imp_try[is.na(imp_try)] = 0

for(j in 1:nrow(imp_try))
{
  for(i in 4:ncol(imp_try))
  {
    if (imp_try[j,3] == imp_try[j,i])
    {
      imp_try[j,i] = 1
    }else{
      imp_try[j,i] = 0}
  }
}


# Wide to long
imp_try_long <- gather(imp_try, country, vote,`Canada`:`South Sudan`)

# Percentage total votes
imp_try_long %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same = sum(vote), 
         vote_percent = vote_same / n) %>% 
  distinct(year, country, vote_percent, .keep_all = TRUE) %>% 
  select(year, country, important_votes = n, everything()) %>% 
  select(!c(rcid, vote, `United States`)) %>% 
  ungroup() -> us_imp_votes

View(us_imp_votes)

##### CHINA IMPORTANT VOITES
imp_wide_china <- imp_wide %>%  
  select(rcid, year, China, everything())

imp_wide_china[is.na(imp_wide_china)] <- 0

for(j in 1:nrow(imp_wide_china))
{
  for(i in 4:ncol(imp_wide_china))
  {
    if (imp_wide_china[j,3] == imp_wide_china[j,i])
    {
      imp_wide_china[j,i] <- 1
    }else{
      imp_wide_china[j,i] <- 0}
  }
}

# Wide to long
imp_long_china <- gather(imp_wide_china, country, vote,`United States`:`South Sudan`)

# Percentage total votes
imp_long_china %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same_imp_china = sum(vote), 
         vote_percent_imp_china = vote_same_imp_china / n) %>% 
  distinct(year, country, vote_percent_imp_china, .keep_all = TRUE) %>% 
  select(year, country, important_votes = n, everything()) %>% 
  select(!c(rcid, vote, `China`)) %>% 
  ungroup() -> china_imp_votes


View(china_imp_votes)


###########
imp_wide_russia <- imp_wide %>%  
  select(rcid, year, Russia, everything())

imp_wide_russia[is.na(imp_wide_russia)] <- 0

View(imp_wide_russia)

for(j in 1:nrow(imp_wide_russia))
{
  for(i in 4:ncol(imp_wide_russia))
  {
    if (imp_wide_russia[j,3] == imp_wide_russia[j,i])
    {
      imp_wide_russia[j,i] <- 1
    }else{
      imp_wide_russia[j,i] <- 0}
  }
}

# Wide to long
imp_long_russia <- tidyr::gather(data = imp_wide_russia,
                                 key = country, 
                                 value = vote,
                                 `United States`:`South Sudan`)

View(imp_long_russia)

# Percentage total votes
imp_long_russia %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same_imp_russia = sum(vote), 
         vote_percent_imp_russia = vote_same_imp_russia / n) %>% 
  distinct(year, country, vote_percent_imp_russia, .keep_all = TRUE) %>% 
  select(year, country, important_votes = n, everything()) %>% 
  select(!c(rcid, vote, `Russia`)) %>% 
  ungroup() -> russia_imp_votes

total_imp_votes <- merge(us_imp_votes, russia_imp_votes, all.x = TRUE)

total_imp_votes <- merge(total_imp_votes, china_imp_votes, all.x = TRUE)


total_all_votes <- merge(all_russia_votes, all_china_votes, all.x = TRUE)
total_all_votes <- merge(total_all_votes, us_all_votes, all.x = TRUE)

total_votes <- merge(total_all_votes, total_imp_votes, all.x = TRUE)


library(skimr)
skim(total_imp_votes)

total_imp_votes$cow_code <- countrycode::countrycode(total_imp_votes$country, "country.name", "cown")
write.csv(total_imp_votes, "total_imp_votes.csv")

pd %>% 
  select(!c(vote_percent_imp_us:vote_percent_imp_china)) -> pd2

pd3 <- merge(pd2, total_imp_votes, all.x = TRUE)
pd4 <- merge(pd, total_votes, by = c("cow_code", "year"), all.x = TRUE)

total_votes$cow_code <- countrycode::countrycode(total_votes$country, "country.name", "cown")

########################################


# IRELAND AT THE UN


library(rnaturalearth)
library(countrycode)
library(unvotes)

joined$year <- format(joined$date, format = "%Y")

map <- ne_countries(scale = "medium", returnclass = "sf")

regions <- map %>% 
  select(name, economy, income_grp, continent:region_wb)

hi <- merge(regions, joined, by.x = c("name"), by.y = c("country"), all.y = TRUE)

joined %>%
  mutate(vote_fac = as.factor(vote)) %>%
  mutate(vote_num = as.numeric(vote_fac)) %>%
  distinct(year, year, country, rcid, vote_num, .keep_all = "TRUE") %>% 
  select(rcid, year, country, vote_num, year) %>%
  pivot_wider(names_from = country, values_from = "vote_num" ) -> all_wide

all_wide_ire <- all_wide %>%  
  select(rcid, year, Ireland, everything())

all_wide_ire[is.na(all_wide_ire)] <- 0

for(j in 1:nrow(all_wide_ire))
{
  for(i in 4:ncol(all_wide_ire))
  {
    if (all_wide_ire[j,3] == all_wide_ire[j,i])
    {
      all_wide_ire[j,i] = 1
    }else{
      all_wide_ire[j,i] = 0}
  }
}

# Percentage total votes
all_long_ire <- all_wide_ire %>% 
  gather( country, vote,`United States`:`South Sudan`) %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same = sum(vote), 
         vote_percent = vote_same / n) %>% 
  distinct(year, country, vote_percent, .keep_all = TRUE) %>% 
  select(year, country, all_votes = n, everything()) %>% 
  select(!c(rcid, vote, `Ireland`)) %>% 
  ungroup() 


all <- merge(all_long_ire, regions, by.y = c("name"), by.x = c("country"), all.x = TRUE)

plyr::count(all$subregion)
all %>% 
  group_by(subregion, year) %>% 
  mutate(avg_vote = mean(vote_percent, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(year > 1955) %>% 
  filter(subregion == c("Western Europe", "Northern Europe", "Eastern Europe", "Southern Europe")) %>% 
  ggplot(aes(x = year, y = avg_vote, group = subregion)) +
  geom_line(aes(color = subregion), size = 4) + ggthemes::theme_pander()

####### PALESTINE 
palestine_votes <- joined %>% 
  filter(issue == "Palestinian conflict")

palestine_votes %>%
  mutate(vote_fac = as.factor(vote)) %>%
  mutate(vote_num = as.numeric(vote_fac)) %>%
  distinct(year, year, country, rcid, vote_num, .keep_all = "TRUE") %>% 
  select(rcid, year, country, vote_num, year) %>%
  pivot_wider(names_from = country, values_from = "vote_num" ) -> all_wide

all_wide_ire <- all_wide %>%  
  select(rcid, year, Ireland, everything())

all_wide_ire[is.na(all_wide_ire)] <- 0

for(j in 1:nrow(all_wide_ire))
{
  for(i in 4:ncol(all_wide_ire))
  {
    if (all_wide_ire[j,3] == all_wide_ire[j,i])
    {
      all_wide_ire[j,i] = 1
    }else{
      all_wide_ire[j,i] = 0}
  }
}

palestine_ire <- all_wide_ire %>% 
  gather( country, vote,`United States`:`South Sudan`) %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same = sum(vote), 
         vote_percent = vote_same / n) %>% 
  distinct(year, country, vote_percent, .keep_all = TRUE) %>% 
  select(year, country, all_votes = n, everything()) %>% 
  select(!c(rcid, vote, `Ireland`)) %>% 
  ungroup() 




all_palestine  <- merge(palestine_ire, regions, by.y = c("name"), by.x = c("country"), all.x = TRUE)

all_palestine %>% 
  group_by(subregion, year) %>% 
  mutate(avg_vote = mean(vote_percent, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(year > 1955) %>% 
  filter(subregion == c("Western Europe", "Northern Europe", "Eastern Europe", "Southern Europe")) %>% 
  ggplot(aes(x = year, y = avg_vote, group = subregion)) +
  geom_line(aes(color = subregion), size = 4) + ggthemes::theme_pander()

colonial_votes <- hi %>% 
  filter(issue == "Colonialism")

arms_votes <- hi %>% 
  filter(issue == "Arms control and disarmament")

economic_votes <- hi %>% 
  filter(issue == "Economic development")

nuclear_votes <- hi %>% 
  filter(issue == "Nuclear weapons and nuclear material")

human_rights_votes <- hi %>% 
  filter(issue == "Human rights")

names(hi)
hi %>% as_data_frame() %>% 
  filter(year > 1955) %>% 
  filter(issue == "Palestinian conflict") %>% 
  filter(subregion == c("Western Europe", "Northern Europe", 
                        "Eastern Europe", "Southern Europe")) 


mutate(voting_yes = ifelse(vote == "yes", 1, 0)) %>%
  mutate(sum_yes = sum(voting_yes)) %>% 
  mutate(voting_percent = sum_yes / n) %>%
  # select(name, year, voting_percent) %>% 
  # arrange(desc(voting_percent))
  summarise(voting_percent = sum_yes / n) %>%
  select(name, year, voting_percent) %>% 
  arrange(desc(voting_percent))

# mutate(avg_vote = sum(voting_yes, na.rm = TRUE)) %>% 
# ungroup() %>% 

filter(year > 1955) %>% 
  filter(subregion == c("Western Europe", "Northern Europe", 
                        "Eastern Europe", "Southern Europe")) %>% 
  ggplot(aes(x = year, y = voting_yes, group = subregion)) +
  geom_line(aes(color = subregion), size = 4) + ggthemes::theme_pander()

View(all_aid10)

write.csv(df3, "civ_aid_df_1.csv")

rm(foraid)

# Ireland and Palestine

all_wide_ire <- all_wide %>% 
  select(!c(rcid, session)) %>% 
  select(year, Ireland, Israel, everything())

all_wide_ire[is.na(all_wide_ire)] <- 0

View(all_wide_ire)

for(j in 1:nrow(all_wide_ire))
{
  for(i in 4:ncol(all_wide_ire))
  {
    if (all_wide_ire[j,3] == all_wide_ire[j,i])
    {
      all_wide_ire[j,i] = 1
    }else{
      all_wide_ire[j,i] = 0}
  }
}

# Percentage total votes
all_long_ire <- all_wide_ire %>% 
  gather(country, vote,`United States`:`South Sudan`) %>% 
  group_by(country, year) %>% 
  add_count() %>% 
  mutate(vote_same = sum(vote), 
         vote_percent = vote_same / n) %>% 
  distinct(year, country, vote_percent, .keep_all = TRUE) %>% 
  select(year, country, all_votes = n, everything()) %>% 
  select(!c(rcid, vote, `Ireland`)) %>% 
  ungroup() -> all_ire_votes

View(all_ire_votes)

all_ire_votes %<>% 
  select(year, country, all_votes, vote_same_ireland = vote_same,
         vote_same_ire_percent = vote_percent)


##################################################

pal %>% 
  select(country, year, vote, descr, session, rcid) -> hi

library(widyr)


pal %>%
  filter(country == "Ireland" | country == "United States") %>% 
  group_by(country, year) %>% 
  mutate(vote = as.numeric(vote)) %>%
  summarise(mean_vote = mean(vote, na.rm = TRUE)) %>% 
  ungroup() -> hi 
pairwise_cor(country, mean_vote, 
             use = "complete.obs",
             sort = TRUE)

cors %>% 
  arrange(correlation)

pal %>% 
  filter(country == "United States") %>% 
  
  mutate(year = as.numeric(year)) %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_point() +
  geom_line() + 
  bbplot::bbc_style()

cors <- joined %>%
  filter(issue == "Palestinian conflict") %>% 
  filter(year > 1955) %>%
  mutate(vote = as.numeric(vote)) %>%
  pairwise_cor(country, rcid, vote, use = "pairwise.complete.obs", sort = TRUE)

cors %>% filter(item1 == "Ireland") -> ire_cor

plyr::count(joined$issue)


if (require("maps", quietly = TRUE) &&
    require("fuzzyjoin", quietly = TRUE) &&
    require("countrycode", quietly = TRUE) &&
    require("ggplot2", quietly = TRUE)) {
  world_data <- map_data("world") %>%
    regex_full_join(iso3166, by = c("region" = "mapname")) %>%
    filter(region != "Antarctica")
  
  ire_cor %>%
    mutate(a2 = countrycode(item2, "country.name", "iso2c")) %>%
    full_join(world_data, by = "a2") %>%
    ggplot(aes(long, lat, group = group, fill = correlation)) +
    geom_polygon(color = "gray", size = .1) +
    scale_fill_gradient2() +
    coord_quickmap() +
    theme_void() +
    labs(title = "Correlation of each country's UN votes with the United States",
         subtitle = "Blue indicates agreement, red indicates disagreement",
         fill = "Correlation w/ US")
}

