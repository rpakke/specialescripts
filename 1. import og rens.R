library(pakken)
pakr(haven, adviceverse)

# "Funktion" til at slette alt undtagen et bestemt objekt
# rm(list = setdiff(ls(), "w1"))

# Importér alle 13 datasæt
w1 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/a_indresp.dta")
w2 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/b_indresp.dta")
w3 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/c_indresp.dta")
w4 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/d_indresp.dta")
w5 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/e_indresp.dta")
w6 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/f_indresp.dta")
w7 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/g_indresp.dta")
w8 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/h_indresp.dta")
w9 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/i_indresp.dta")
w10 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/j_indresp.dta")
w11 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/k_indresp.dta")
w12 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/l_indresp.dta")
w13 <- read_dta("C:/Users/45285/Videos/Specialedatasæt/ukhls/UKDA-6614-stata/stata/stata13_se/ukhls/m_indresp.dta")

# Behold relevante variable
ends3 <- c("pidp",
          "_sex",
          "birthy",
          "nchunder16",
          "ukborn",
          "_health",
          "sf1", "sf6a", "sf6b", "sf6c",
          "aidhrs",
          "paytyp", "paynl", "paynwc",
          "jbsat",
          "jbhrs",
          "ccare",
          "fiyrdia",
          "lfsat1", "lfsat2", "lfsat7", "lfsato",
          "marstat", "livesp", "livewith",
          "employ",
          "jbft_dv",
          "ghq1_dv", "ghq2_dv",
          "sf12pcs_dv", "sf12mcs_dv", 
          "jbnssec3_dv",
          "_tvhours",
          "_mday", "_vday",
          "_mdhrs", "_vdhrs",
          "_mdmin", "_vdmin",
          "_howlng",
          "_volhrs",
          "_wkaut5",
          "_screlhappy")

# ... og slet nogle enkeltvis, hvis de kun er i nogle waves
ends3no <- c("_ff_jbstat", "_ff_ukborn",
             "_nxtjbhrs", "memploy", "pbirthy")

test1 <- w1 %>% select(ends_with(ends3), -ends_with(ends3no))
test2 <- w2 %>% select(ends_with(ends3), -ends_with(ends3no))
test3 <- w3 %>% select(ends_with(ends3), -ends_with(ends3no))
test4 <- w4 %>% select(ends_with(ends3), -ends_with(ends3no))
test5 <- w5 %>% select(ends_with(ends3), -ends_with(ends3no))
test6 <- w6 %>% select(ends_with(ends3), -ends_with(ends3no))
test7 <- w7 %>% select(ends_with(ends3), -ends_with(ends3no))
test8 <- w8 %>% select(ends_with(ends3), -ends_with(ends3no))
test9 <- w9 %>% select(ends_with(ends3), -ends_with(ends3no))
test10 <- w10 %>% select(ends_with(ends3), -ends_with(ends3no))
test11 <- w11 %>% select(ends_with(ends3), -ends_with(ends3no))
test12 <- w12 %>% select(ends_with(ends3), -ends_with(ends3no))
test13 <- w13 %>% select(ends_with(ends3), -ends_with(ends3no))

# INNER JOIN
ya <- inner_join(test1, test2, by = "pidp") %>% 
  inner_join(test3, by = "pidp") %>% 
  inner_join(test4, by = "pidp") %>% 
  inner_join(test5, by = "pidp") %>% 
  inner_join(test6, by = "pidp") %>% 
  inner_join(test7, by = "pidp") %>% 
  inner_join(test8, by = "pidp") %>% 
  inner_join(test9, by = "pidp") %>% 
  inner_join(test10, by = "pidp") %>% 
  inner_join(test11, by = "pidp") %>% 
  inner_join(test12, by = "pidp") %>% 
  inner_join(test13, by = "pidp")

# Generelt helbred er i to variable, de skal bare have den højeste som værdi
ya <- ya %>% mutate(
  b_sf1 = if_else(b_scsf1 > b_sf1, b_scsf1, b_sf1),
  c_sf1 = if_else(c_scsf1 > c_sf1, c_scsf1, c_sf1),
  d_sf1 = if_else(d_scsf1 > d_sf1, d_scsf1, d_sf1),
  e_sf1 = if_else(e_scsf1 > e_sf1, e_scsf1, e_sf1),
  f_sf1 = if_else(f_scsf1 > f_sf1, f_scsf1, f_sf1),
  g_sf1 = if_else(g_scsf1 > g_sf1, g_scsf1, g_sf1),
  h_sf1 = if_else(h_scsf1 > h_sf1, h_scsf1, h_sf1),
  i_sf1 = if_else(i_scsf1 > i_sf1, i_scsf1, i_sf1),
  j_sf1 = if_else(j_scsf1 > j_sf1, j_scsf1, j_sf1),
  k_sf1 = if_else(k_scsf1 > k_sf1, k_scsf1, k_sf1),
  l_sf1 = if_else(l_scsf1 > l_sf1, l_scsf1, l_sf1),
  m_sf1 = if_else(m_scsf1 > m_sf1, m_scsf1, m_sf1)
) %>% select(-ends_with("_scsf1"))


# Tjek at alt er i orden
(ncol(ya)-1)/13
ya

# Alle NAs (negative værdier) som NA
ya <- ya %>%
  mutate(across(everything(), ~if_else(. < 0, NA, .)))

# Gem
saveRDS(ya, "data_clean_1.RDS")









