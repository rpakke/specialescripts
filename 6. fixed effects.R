# Pakker og data
library(pakken); pakr(haven, adviceverse, labelled, fixest, scales, modelsummary, gt)
dd <- readRDS("data/long3.RDS")

d <- dd
d %>% llookup()


##################################################################
##                            Indeks                            ##
##################################################################

d2 <- dd %>% mutate(
  indeks_fysisk_helbred = (helbred_generelt + helbred_fysisk +
    energi + tilf_helbred)/4,
  indeks_mentalt = (helbred_mentalt + rolig + depri)/3,
  indeks_tilf_generelt = (tilf_life + subj_wellb_1 + subj_wellb_2)/3,
  indeks_fys_akt = (tid_modfys + tid_kraftfys)/2
)

# Og arbejdstid^2
d3 <- d2 %>% mutate(arbtid2 = arbtid^2)

# Og fuck arbejdstidsautonomi
d4 <- d3 %>% select(-tidsautonomi)

# Og forælder som binær
d5 <- d4 %>% mutate(forælder = if_else(børn>0, 1, 0))

# Og z-standardiserede variable ganges med 100 (sd = 100)
d6 <- d5 %>% mutate(
  helbred_generelt = helbred_generelt*100,
  rolig = rolig*100, energi = energi*100, depri = depri*100,
  tilf_job = tilf_job*100, tilf_helbred = tilf_helbred*100,
  tilf_indkomst = tilf_indkomst*100, tilf_fritid = tilf_fritid*100,
  tilf_life = tilf_life*100, subj_wellb_1 = subj_wellb_1*100,
  subj_wellb_2 = subj_wellb_2*100, helbred_fysisk = helbred_fysisk*100,
  helbred_mentalt = helbred_mentalt*100, tilf_parforhold = tilf_parforhold*100,
  indeks_fysisk_helbred = indeks_fysisk_helbred*100,
  indeks_mentalt = indeks_mentalt*100, indeks_tilf_generelt = indeks_tilf_generelt*100
)

# Og indkomst i 1000'er
d7 <- d6 %>% mutate(
  løn_lagged = løn_lagged/1000
)

# Management/professional over for routine/intermediate
d8 <- d7 %>% mutate(
  jobtype = as.character(jobtype),
  jobtype = if_else(jobtype == "Intermediate", "Routine", jobtype)
)

# Og ikke nogen løntypeOther
d9 <- d8 %>% mutate(
  løntype = as.character(løntype),
  løntype = if_else(løntype == "Other", NA, løntype)
)

# Og indvandrer byttet om
d10 <- d9 %>% mutate(
  ukborn = if_else(ukborn==0, 1, 0)
)

# Og tv-sening per uge
d11 <- d10 %>% mutate(
  tid_tv = tid_tv*7
)

dd <- d11
d <- dd


#################################################################
##                          8 datasæt                          ##
#################################################################

# Liste over folk, der har job i alle runder
a <- dd %>% group_by(id) %>% summarise(jobbo = mean(jobbis)) %>% filter(jobbo==1)
altidjob <- a$id

d01 <- dd
d02 <- dd %>% filter(arbtid >= 20)
d03 <- dd %>% filter(arbtid >= 25)


#################################################################
##                        Modelfunktion                        ##
#################################################################

add_stars <- function(p_value) {
  if (p_value < 0.001) {
    return("<0.001")
  } else if (p_value < 0.01) {
    return("<0.01")
  } else if (p_value < 0.05) {
    return("<0.05")
 #} else if (p_value < 0.1) {
  #  return("<0.1")
  } else {
    return("")
  }
}

# Funktion til basic FE-modeller
nemt <- function(dep_var, ind_vars) {
  ind_vars_str <- ind_vars
  
  preds1 <- c(ind_vars_str, "børn", "sygdom", "bopartner",
                 "jobtype", "løntype", "indk_kap")
  preds2 <- c("nytbarn", "fraflyttet", "løn_lagged")
  formstr1 <- paste(dep_var, "~", paste(preds1, collapse = "+"),
                          "| id + wave")
  formstr2 <- paste(dep_var, "~", paste(c(preds1, preds2), collapse = "+"),
                    "| id + wave")
  formula1 <- as.formula(formstr1)
  formula2 <- as.formula(formstr2)
  
  m1a <- feols(formula1, data = d01); m1b <- feols(formula2, data = d01)
  m2a <- feols(formula1, data = d02); m2b <- feols(formula2, data = d02)
  m3a <- feols(formula1, data = d03); m3b <- feols(formula2, data = d03)

  models <- list(m1a, m1b, m2a, m2b, m3a, m3b)
  msum <- lapply(models, broom::tidy) %>%
    purrr::map_df(bind_rows, .id = "Model") %>%
    dplyr::filter(!term %in% c("børn", "sygdom", "bopartner", "jobtypeManagement & professional",
                               "jobtypeRoutine", "løntypeOther", "løntypeSalaried",
                               "indk_kap", "nytbarn", "fraflyttet",
                               "løn_lagged"))
  
  # Tilføj signifikanskolonne (med stjerner)
  msum <- msum %>%
    mutate(Significance = sapply(p.value, add_stars)) %>% 
    arrange(term)
  
  spredning = msum %>% group_by(term) %>% summarise(
    min = min(estimate),
    middel = mean(estimate),
    max = max(estimate),
    #x = "---",
    #p_min = min(p.value),
    p_middel = mean(p.value),
    #p_max = max(p.value),
    n_modeller = n())
  
  print(spredning)
  print(cat("\n\n"))
  return(msum)
}
#

##################################################################
##                        Mange modeller                        ##
##################################################################

d %>% llookup()

# For hver afhængig variabel (i alt: 23 gange):
outcome <- "tid_frivillig"

nemt(outcome, "arbtid") %>% select(term,estimate,Significance)

nemt(outcome, c("arbtid", "ændring_arbtid")) %>% select(term,estimate,Significance)

nemt(outcome, c("arbtid", "arbtid2")) %>% select(term,estimate,Significance)

nemt(outcome, "arbtid*køn") %>% select(term,estimate,Significance)
nemt(outcome, "arbtid*løn_lagged") %>% select(term,estimate,Significance)
nemt(outcome, "arbtid*jobtype") %>% select(term,estimate,Significance)
nemt(outcome, "arbtid*løntype") %>% select(term,estimate,Significance)
nemt(outcome, "arbtid*ukborn") %>% select(term,estimate,Significance)
nemt(outcome, "arbtid*forælder") %>% select(term,estimate,Significance)
nemt(outcome, "arbtid*alder") %>% select(term,estimate,Significance)




# Når der er mange:
#rm(a1, a2, a3, a4, a5)

a1 <- nemt("tid_modfys", "arbtid")
a2 <- nemt("tid_kraftfys", "arbtid")
a3 <- nemt("indeks_fys_akt", "arbtid")

a1 <- nemt("tid_modfys", c("arbtid", "ændring_arbtid"))
a2 <- nemt("tid_kraftfys", c("arbtid", "ændring_arbtid"))
a3 <- nemt("indeks_fys_akt", c("arbtid", "ændring_arbtid"))

a1 <- nemt("tid_modfys", c("arbtid", "arbtid2"))
a2 <- nemt("tid_kraftfys", c("arbtid", "arbtid2"))
a3 <- nemt("indeks_fys_akt", c("arbtid", "arbtid2"))


a1 <- nemt("tid_modfys", 
           "arbtid*alder")
a2 <- nemt("tid_kraftfys", 
           "arbtid*alder")
a3 <- nemt("indeks_fys_akt", 
           "arbtid*alder")


a1 <- nemt("helbred_generelt", "arbtid")
a2 <- nemt("rolig", "arbtid")
a3 <- nemt("energi", "arbtid")
a4 <- nemt("depri", "arbtid")
a5 <- nemt("tilf_job", "arbtid")
a6 <- nemt("tilf_helbred", "arbtid")
a7 <- nemt("tilf_indkomst", "arbtid")
a8 <- nemt("tilf_fritid", "arbtid")
a9 <- nemt("tilf_life", "arbtid")
a10 <- nemt("subj_wellb_1", "arbtid")
a11 <- nemt("subj_wellb_2", "arbtid")
a12 <- nemt("helbred_fysisk", "arbtid")
a13 <- nemt("helbred_mentalt", "arbtid")
a14 <- nemt("tilf_parforhold", "arbtid")
a15 <- nemt("indeks_fysisk_helbred", "arbtid")
a16 <- nemt("indeks_mentalt", "arbtid")
a17 <- nemt("indeks_tilf_generelt", "arbtid")



# Eksekvér
print(a1) %>% select(term)
aa <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,
            a11,a12,a13,a14,a15,a16,a17) %>% 
  filter(term=="arbtid") %>% select(term,estimate,p.value,Significance)
print(aa, n=1000)
mean(aa$estimate) %>% round(2)
min(aa$estimate) %>% round(2)
max(aa$estimate) %>% round(2)
mean(aa$p.value) %>% round(3)
#


# Helt overordnet for alle z-outcomes
ab <- aa %>% filter(Significance!="") %>% print(n=100) # 102 outcomes
ab %>% filter(estimate > 0) %>% nrow() # sign. positive: 10
ab %>% filter(estimate < 0) %>% nrow() # sign. negative: 30


sd(d$indeks_tilf_generelt, na.rm=T)
tabl(forælder)







