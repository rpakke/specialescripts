# Pakker og data
library(pakken)
pakr(haven, adviceverse, labelled, plm)

dd <- readRDS("data_clean_2.RDS")
d <- dd
df <- dd %>% rename(id = pidp)

# Langt format!
df_long <- df %>%
  pivot_longer(
    cols = -id,
    names_to = c("wave", ".value"),
    names_pattern = "^([a-m])_(.*)$"
  ) %>%
  mutate(wave = recode(wave, 
                       "a" = 1, "b" = 2, "c" = 3, "d" = 4, "e" = 5, 
                       "f" = 6, "g" = 7, "h" = 8, "i" = 9, "j" = 10, 
                       "k" = 11, "l" = 12, "m" = 13))

df_long %>% llookup()

# Jeg sletter altså lige nogle variable, fordi jeg har omkodet dem til andre variable
long <- df_long %>% select(-c("marstat", "livesp", "livewith", "paynl", "paynwc",
                              "mday", "vday", "mdhrs", "vdhrs", "mdmin", "vdmin"))

# Og sf6'erne skal lægges sammen
long <- long %>% mutate(
  sf6a = if_else(is.na(sf6a), scsf6a, sf6a),
  sf6b = if_else(is.na(sf6b), scsf6b, sf6b),
  sf6c = if_else(is.na(sf6c), scsf6c, sf6c)
) %>% select(-c("scsf6a", "scsf6b", "scsf6c"))

long %>% llookup()

# Hov, ville egentlig også have fjernet ccare
long <- long %>% select(-ccare)

d <- long


# Nu får de altså lige nye labels
pakr(expss)

long <- long %>% apply_labels(
  id = "id",
  wave = "wave",
  sex = "der er to køn",
  birthy = "fødselsår",
  nchunder16 = "antal børn under 16 år",
  ukborn = "ja/nej",
  health = "langvarig sygdom",
  sf1 = "obs: z",
  sf6a = "obs: z",
  sf6b = "obs: z",
  sf6c = "obs: z",
  aidhrs = "over 4 timers plejearbejde om ugen?",
  paytyp = "aflønningstype",
  jbsat = "obs: z",
  jbhrs = "typiske arbejdstimer per uge",
  fiyrdia = "sidste års renter/udbytte",
  sclfsat1 = "obs: z",
  sclfsat2 = "obs: z",
  sclfsat7 = "obs: z",
  sclfsato = "obs: z",
  employ = "i betalt beskæftigelse?",
  jbft_dv = "Fuld tid eller deltid?",
  scghq1_dv = "obs: z",
  scghq2_dv = "obs: z",
  sf12pcs_dv = "obs: z",
  sf12mcs_dv = "obs: z",
  jbnssec3_dv = "jobkategori",
  screlhappy = "obs: z",
  howlng = "ugentlige timer på husholdningsarbejde",
  volhrs = "frivilligtimer seneste 4 uger",
  wkaut5 = "arbejdstidsautonomi (kat.)",
  tvhours = "daglige tv-timer",
  mod = "moderat fys akt seneste uge",
  vig = "kraftig fys akt seneste uge",
  bopartner = "bor med partner?",
  løn = "seneste måneds løn (omkodet)"
)

long %>% llookup()

# Gem ny datafil
saveRDS(long, "data_long_1.RDS")


#




