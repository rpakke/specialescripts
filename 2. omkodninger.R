# Pakker og data
library(pakken)
pakr(haven, adviceverse, labelled)

dd <- readRDS("data_clean_1.RDS")
d <- dd
codebook <- function() {d %>% llookup(print_max=(ncol(d)-1)/13)}

# Kodebog (delvis) ya:
codebook()

# Value labels for en variabel:
val_labels(d$b_sf1)



##########################
##  Simple omkodninger  ##
##########################

# Frivilligtimer: omkod NA til 0
d2 <- dd %>%
  mutate(across(ends_with("_volhrs"), 
         ~ if_else(is.na(.x), 0, .x)))

# Standardisér skala-outcomes 1
d3 <- d2 %>% mutate(
  across(ends_with("sf1"), ~ scale(.x)),
  across(ends_with("sf1"), ~ .x*(-1)),
  across(ends_with("sf6a"), ~ scale(.x)),
  across(ends_with("sf6a"), ~ .x*(-1)),
  across(ends_with("sf6b"), ~ scale(.x)),
  across(ends_with("sf6b"), ~ .x*(-1)),
  across(ends_with("sf6c"), ~ scale(.x)),
  across(ends_with("lfsat1"), ~ scale(.x)),
  across(ends_with("sf12pcs_dv"), ~ scale(.x)),
  across(ends_with("sf12mcs_dv"), ~ scale(.x))
)

# Standardisér skala-outcomes 2
d4 <- d3 %>% mutate(
  across(ends_with("jbsat"), ~ scale(.x)),
  across(ends_with("lfsat2"), ~ scale(.x)),
  across(ends_with("lfsat7"), ~ scale(.x)),
  across(ends_with("lfsato"), ~ scale(.x)),
  across(ends_with("ghq1_dv"), ~ scale(.x)),
  across(ends_with("ghq1_dv"), ~ .x*(-1)),
  across(ends_with("ghq2_dv"), ~ scale(.x)),
  across(ends_with("ghq2_dv"), ~ .x*(-1)),
  across(ends_with("relhappy"), ~ scale(.x))
)

d <- d4

# Motion
d5 <- d4 %>% mutate(
  g_mod = g_mday * (g_mdhrs + g_mdmin/60),
  i_mod = i_mday * (i_mdhrs + i_mdmin/60),
  k_mod = k_mday * (k_mdhrs + k_mdmin/60),
  l_mod = l_mday * (l_mdhrs + l_mdmin/60),
  m_mod = m_mday * (m_mdhrs + m_mdmin/60),
  
  g_vig = g_vday * (g_vdhrs + g_vdmin/60),
  i_vig = i_vday * (i_vdhrs + i_vdmin/60),
  k_vig = k_vday * (k_vdhrs + k_vdmin/60),
  l_vig = l_vday * (l_vdhrs + l_vdmin/60),
  m_vig = m_vday * (m_vdhrs + m_vdmin/60),
  
  across(ends_with("_mod"), ~ if_else(is.na(.x), 0, .x)),
  across(ends_with("_vig"), ~ if_else(is.na(.x), 0, .x))
)

# Husholdningsarbejde fungerer ikke i wave 1
d6 <- d5 %>% select(-a_howlng)

# Langvarig sygdom skal være 0 og 1
d7 <- d6 %>% mutate(
  across(ends_with("_health"), ~ if_else(.x == 2, 0, .x))
)

# Care work som binær (over 4 timer eller ej)
d8 <- d7 %>% mutate(
  across(ends_with("_aidhrs"),
         ~ if_else((is.na(.x) | .x==1), 0, 1))
)

# Bor med partner
d9 <- d8 %>% mutate(
  across(ends_with("_livesp"), ~ if_else(is.na(.x), 2, .x)),
  across(ends_with("_livewith"), ~ if_else(is.na(.x), 2, .x)),
  
  a_bopartner = if_else((a_livesp==1 | a_livewith==1), 1, 0),
  b_bopartner = if_else((b_livesp==1 | b_livewith==1), 1, 0),
  c_bopartner = if_else((c_livesp==1 | c_livewith==1), 1, 0),
  d_bopartner = if_else((d_livesp==1 | d_livewith==1), 1, 0),
  e_bopartner = if_else((e_livesp==1 | e_livewith==1), 1, 0),
  f_bopartner = if_else((f_livesp==1 | f_livewith==1), 1, 0),
  g_bopartner = if_else((g_livesp==1 | g_livewith==1), 1, 0),
  h_bopartner = if_else((h_livesp==1 | h_livewith==1), 1, 0),
  i_bopartner = if_else((i_livesp==1 | i_livewith==1), 1, 0),
  j_bopartner = if_else((j_livesp==1 | j_livewith==1), 1, 0),
  k_bopartner = if_else((k_livesp==1 | k_livewith==1), 1, 0),
  l_bopartner = if_else((l_livesp==1 | l_livewith==1), 1, 0),
  m_bopartner = if_else((m_livesp==1 | m_livewith==1), 1, 0)
)

# Jobkategorier bliver kategorisk her
d10 <- d9 %>% mutate(
  across(ends_with("_jbnssec3_dv"),
         ~ case_when(
           .x == 1 ~ "Management & professional",
           .x == 2 ~ "Intermediate",
           .x == 3 ~ "Routine",
           T ~ NA
         ))
)
d <- d10

# Køn
d11 <- d10 %>% mutate(
  across(ends_with("_sex"),
         ~ if_else(.x == 1, "Mand", "Kvinde"))
)

# Født i UK?
d12 <- d11 %>% mutate(
  across(ends_with("ukborn"),
         ~ if_else(a_ukborn==5,0,1))
)
d <- d12

# Måneds- eller timelønnet?
d13 <- d12 %>% mutate(
  across(ends_with("_paytyp"),
         ~ case_when(
           .x == 1 ~ "Salaried",
           .x == 3 ~ "By the hour",
           .x == 2 ~ "Other",
           .x == 97 ~ "Other",
           T ~ NA
         ))
)

# Seneste månedsløn ----
d14 <- d13 %>% mutate(
  a_løn = case_when(
    a_paynwc == 1 ~ round(a_paynl*30.4/7,0),
    a_paynwc == 2 ~ round(a_paynl*30.4/14,0),
    a_paynwc == 4 ~ round(a_paynl*30.4/28,0),
    a_paynwc == 52 ~ round(a_paynl/12,0),
    a_paynwc == 5 ~ a_paynl,
    T ~ NA
  ),
  b_løn = case_when(
    b_paynwc == 1 ~ round(b_paynl*30.4/7,0),
    b_paynwc == 2 ~ round(b_paynl*30.4/14,0),
    b_paynwc == 4 ~ round(b_paynl*30.4/28,0),
    b_paynwc == 52 ~ round(b_paynl/12,0),
    b_paynwc == 5 ~ b_paynl,
    T ~ NA
  ),
  c_løn = case_when(
    c_paynwc == 1 ~ round(c_paynl*30.4/7,0),
    c_paynwc == 2 ~ round(c_paynl*30.4/14,0),
    c_paynwc == 4 ~ round(c_paynl*30.4/28,0),
    c_paynwc == 52 ~ round(c_paynl/12,0),
    c_paynwc == 5 ~ c_paynl,
    T ~ NA
  ),
  d_løn = case_when(
    d_paynwc == 1 ~ round(d_paynl*30.4/7,0),
    d_paynwc == 2 ~ round(d_paynl*30.4/14,0),
    d_paynwc == 4 ~ round(d_paynl*30.4/28,0),
    d_paynwc == 52 ~ round(d_paynl/12,0),
    d_paynwc == 5 ~ d_paynl,
    T ~ NA
  ),
  e_løn = case_when(
    e_paynwc == 1 ~ round(e_paynl*30.4/7,0),
    e_paynwc == 2 ~ round(e_paynl*30.4/14,0),
    e_paynwc == 4 ~ round(e_paynl*30.4/28,0),
    e_paynwc == 52 ~ round(e_paynl/12,0),
    e_paynwc == 5 ~ e_paynl,
    T ~ NA
  ),
  f_løn = case_when(
    f_paynwc == 1 ~ round(f_paynl*30.4/7,0),
    f_paynwc == 2 ~ round(f_paynl*30.4/14,0),
    f_paynwc == 4 ~ round(f_paynl*30.4/28,0),
    f_paynwc == 52 ~ round(f_paynl/12,0),
    f_paynwc == 5 ~ f_paynl,
    T ~ NA
  ),
  g_løn = case_when(
    g_paynwc == 1 ~ round(g_paynl*30.4/7,0),
    g_paynwc == 2 ~ round(g_paynl*30.4/14,0),
    g_paynwc == 4 ~ round(g_paynl*30.4/28,0),
    g_paynwc == 52 ~ round(g_paynl/12,0),
    g_paynwc == 5 ~ g_paynl,
    T ~ NA
  ),
  h_løn = case_when(
    h_paynwc == 1 ~ round(h_paynl*30.4/7,0),
    h_paynwc == 2 ~ round(h_paynl*30.4/14,0),
    h_paynwc == 4 ~ round(h_paynl*30.4/28,0),
    h_paynwc == 52 ~ round(h_paynl/12,0),
    h_paynwc == 5 ~ h_paynl,
    T ~ NA
  ),
  i_løn = case_when(
    i_paynwc == 1 ~ round(i_paynl*30.4/7,0),
    i_paynwc == 2 ~ round(i_paynl*30.4/14,0),
    i_paynwc == 4 ~ round(i_paynl*30.4/28,0),
    i_paynwc == 52 ~ round(i_paynl/12,0),
    i_paynwc == 5 ~ i_paynl,
    T ~ NA
  ),
  j_løn = case_when(
    j_paynwc == 1 ~ round(j_paynl*30.4/7,0),
    j_paynwc == 2 ~ round(j_paynl*30.4/14,0),
    j_paynwc == 4 ~ round(j_paynl*30.4/28,0),
    j_paynwc == 52 ~ round(j_paynl/12,0),
    j_paynwc == 5 ~ j_paynl,
    T ~ NA
  ),
  k_løn = case_when(
    k_paynwc == 1 ~ round(k_paynl*30.4/7,0),
    k_paynwc == 2 ~ round(k_paynl*30.4/14,0),
    k_paynwc == 4 ~ round(k_paynl*30.4/28,0),
    k_paynwc == 52 ~ round(k_paynl/12,0),
    k_paynwc == 5 ~ k_paynl,
    T ~ NA
  ),
  l_løn = case_when(
    l_paynwc == 1 ~ round(l_paynl*30.4/7,0),
    l_paynwc == 2 ~ round(l_paynl*30.4/14,0),
    l_paynwc == 4 ~ round(l_paynl*30.4/28,0),
    l_paynwc == 52 ~ round(l_paynl/12,0),
    l_paynwc == 5 ~ l_paynl,
    T ~ NA
  ),
  m_løn = case_when(
    m_paynwc == 1 ~ round(m_paynl*30.4/7,0),
    m_paynwc == 2 ~ round(m_paynl*30.4/14,0),
    m_paynwc == 4 ~ round(m_paynl*30.4/28,0),
    m_paynwc == 52 ~ round(m_paynl/12,0),
    m_paynwc == 5 ~ m_paynl,
    T ~ NA
  )
)
d <- d14
# ----

# Kapitalismen
d15 <- d14 %>% mutate(
  across(ends_with("fiyrdia"),
         ~ if_else(is.na(.x), 0, .x))
)

# Arbejdstidsautonomi som kategorisk
d16 <- d15 %>% mutate(
  across(ends_with("wkaut5"),
         ~ case_when(
           .x == 1 ~ "Megen",
           .x == 2 ~ "Nogen",
           .x == 3 ~ "Lidt",
           .x == 4 ~ "Ingen",
           T ~ NA
         ))
)

# I betalt besk.?
d17 <- d16 %>% mutate(
  across(ends_with("_employ"),
         ~ if_else(.x==1,1,0))
)

# Fuld tid eller deltid?
d18 <- d17 %>% mutate(
  across(ends_with("_jbft_dv"),
         ~ case_when(
           .x == 1 ~ "Fuld tid",
           .x == 2 ~ "Deltid",
           T ~ NA
         ))
)

ddd <- d18

saveRDS(ddd, "data_clean_2.RDS")













