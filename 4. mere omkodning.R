# Pakker og data
library(pakken); pakr(haven, adviceverse, labelled, plm)
dd <- readRDS("data_long_1.RDS")

######################
##  Lidt mere rens  ##
######################

# Okay, sidste rens (slet folk hvis de ikke har job i mindst to af runderne)
dd <- dd %>% mutate(employ = ifelse(is.na(as.numeric(employ)), 0, as.numeric(employ)))
x1 <- dd %>% group_by(id) %>% summarise(hmmm = mean(employ)) %>% filter(hmmm > 1/13)
dd <- dd %>% semi_join(x1, by = "id") %>% 
  select(-jbft_dv) %>% # ... og fjern fuldtid/deltid
  select(-aidhrs) # ... og fjern care work

# Nye navne til alle variable:
names(dd) <- c("id", "wave", "køn", "fødselsår", "børn", "ukborn", "sygdom",
               "helbred_generelt", "rolig", "energi", "depri", "løntype",
               "tilf_job", "arbtid", "indk_kap", "tilf_helbred", "tilf_indkomst",
               "tilf_fritid", "tilf_life", "jobbis", "subj_wellb_1", "subj_wellb_2",
               "helbred_fysisk", "helbred_mentalt", "jobtype", "tilf_parforhold",
               "tid_husholdning", "tid_frivillig", "tidsautonomi", "tid_tv",
               "tid_modfys", "tid_kraftfys", "bopartner", "løn")

d <- dd

########################
##  Bortfaldsanalyse  ##
########################

# Bortfaldsanalyse
# w1 <- w1 %>% mutate_all(~ if_else(.x<0,NA,.x)) # NAs som NA
# 
# mean(dd$birthy, na.rm=T)
# mean(w1$a_birthy, na.rm=T)
# 
# tabl(sex, dset=dd)
# tabl(a_sex, dset=w1)
# 
# tabl(ukborn, dset=dd)
# tabl(a_ukborn, dset=w1)


#########################
##  Fire nye variable  ##
#########################

# Nyt barn?
d2 <- dd %>%
  arrange(id, wave) %>%
  group_by(id) %>%
  mutate(temp = dplyr::lag(børn, order_by=wave)) %>%
  ungroup()

d3 <- d2 %>% mutate(nytbarn = if_else(børn > temp, 1, 0))

# Sidste års månedsløn
d4 <- d3 %>% 
  arrange(id, wave) %>% 
  group_by(id) %>% 
  mutate(løn_lagged = dplyr::lag(løn, order_by=wave)) %>% 
  ungroup()

# Nyligt fraflyttet?
d5 <- d4 %>% 
  arrange(id, wave) %>% 
  group_by(id) %>% 
  mutate(temp = dplyr::lag(bopartner, order_by=wave)) %>% 
  ungroup()

d6 <- d5 %>% mutate(fraflyttet = if_else(bopartner < temp, 1, 0))

# Ændring i arbejdstid
d7 <- d6 %>% 
  arrange(id, wave) %>% 
  group_by(id) %>% 
  mutate(temp = dplyr::lag(arbtid, order_by=wave)) %>% 
  ungroup()

d8 <- d7 %>% mutate(ændring_arbtid = arbtid-temp)

# Kald det dd igen
dd <- d8

saveRDS(dd, "data_long_2.RDS")





















