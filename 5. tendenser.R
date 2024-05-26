# Pakker og data
#devtools::install_github("rpakke/pakken")
#install.packages("gridExtra")
library(pakken); pakr(haven, adviceverse, labelled, scales, gridExtra)
dd <- readRDS("data/long3.RDS")

d <- dd
d %>% llookup()


#################################################################
##                Hvem går op/ned i arbejdstid?                ##
#################################################################

###################
##  Omkodninger  ##
###################
# 
# # Alder
# d2 <- dd %>% mutate(alder = (wave+2008)-fødselsår)
# 
# # Aldersgruppe og børn grupperet
# d3 <- d2 %>% mutate(
#   aldersgruppe = cut(alder, breaks = c(-Inf, 30, 40, 50, 60, Inf),
#                      include.lowest = TRUE,
#                      labels = c("Under 30 år", "30-39 år", "40-49 år", "50-59 år", "60+ år")),
#   børn2 = if_else(børn > 2, "3+", as.character(børn))
# )
# 
# # Indkomstgruppe
# d4 <- d3 %>% group_by(wave) %>% 
#   mutate(indkgruppe = ntile(løn, 4)) %>% ungroup()
# 
# saveRDS(d4, "data/long3.RDS")
# 
# d <- d4


################
##  Funktion  ##er
################

# Funktion til plot og lineær reg
plotfun <- function(group_var_name, yvar_name = "arbtid", 
                    ylabel = "Ugentlig arbejdstid", dset=d, titel = " ") {
  group_var <- sym(group_var_name)
  yvar <- sym(yvar_name)
  prepared_data <- dset %>% 
    filter(!is.na(!!group_var)) %>% 
    group_by(wave, !!group_var) %>% 
    summarise(Arbejdstid = mean(!!yvar, na.rm=T), 
              N = n(), .groups = "drop") %>% filter(N>50)
  
  mini1 <- min(prepared_data$Arbejdstid, na.rm=T)
  maxi1 <- max(prepared_data$Arbejdstid, na.rm=T)
  
  mini <- mini1 - (maxi1-mini1)*.1
  maxi <- maxi1 + (maxi1-mini1)*.1
  
  p <- ggplot(prepared_data, aes(x = wave, y = Arbejdstid)) +
    geom_line() + 
    geom_point() + 
    theme_bw() + 
    ylim(mini, maxi) +
    ylab(ylabel) +
    geom_smooth(method = "lm", color = "maroon", formula = y ~ x) +
    #scale_size(guide = 'none') +
    scale_x_continuous(breaks = c(1,7,13)) +
    facet_wrap(as.formula(paste0("~", group_var_name))) +
    ggtitle(paste(titel,"\n")) + theme(plot.title = element_text(hjust = 0.5))
    
  lm_formula <- as.formula(paste(yvar_name ,"~ wave + wave *", group_var_name))
  lm_result <- lm(lm_formula, data = dset) %>% broom::tidy()
  
  tab <- dset %>% filter(!is.na(!!group_var)) %>% tabyl(!!group_var) %>%  
    adorn_totals() %>% 
    select(!!group_var, n) %>%
    mutate(n_gns = round(n/13,0)) %>% 
    mutate_if(is.numeric, ~scales::comma(.))
  
  print(tab)
  print(lm_result)
  return(p)
}

plotfun2 <- function(yy, dset=d, ylabel="", titel = " ") {
  yyy <- sym(yy)
  prep_dat <- dset %>% group_by(wave) %>% 
    summarise(y4 = mean(!!yyy, na.rm=T), .groups="drop")
  
  p <- ggplot(prep_dat, aes(x = wave, y = y4)) +
    geom_line() + geom_point() + theme_bw() +
    geom_smooth(method = "lm", color="maroon", formula=y~x) +
    scale_x_continuous(breaks = c(1,7,13)) +
    ylab(ylabel) +
    ggtitle(paste(titel,"\n")) + theme(plot.title = element_text(hjust = 0.5))
  
  lm_formula <- as.formula(paste(yy, "~ wave"))
  lm_result <- lm(lm_formula, data=dset) %>% broom::tidy()
  
  print(lm_result)
  return(p)
}


#################
##  Plots mv.  ##
#################

# Her bare et par ekstra variable :)
d2 <- dd %>% mutate(over20 = if_else(arbtid > 20, 1, 0),
                    over25 = if_else(arbtid > 25, 1, 0),
                    over30 = if_else(arbtid > 30, 1, 0),
                    over40 = if_else(arbtid > 40, 1, 0),
                    
                    under40 = if_else(arbtid < 40, 1, 0),
                    under30 = if_else(arbtid < 30, 1, 0),
                    under25 = if_else(arbtid < 25, 1, 0),
                    under20 = if_else(arbtid < 20, 1, 0),
                    
                    mindst40 = if_else(arbtid >= 40, 1, 0),
                    højst20 = if_else(arbtid<= 20, 1, 0))

d <- d2

##################
##  Overordnet  ##
##################

plotfun2("arbtid", ylabel = "Ugentlig arbejdstid", 
         titel="Gennemsnitlig ugentlig arbejdstid i timer (alle panelister)")

plotfun2("over20", ylabel = "Andel over 20 timer")
plotfun2("over40", ylabel = "Andel over 40 timer")

d3 <- d2 %>% filter(arbtid > 30)
d4 <- d2 %>% filter(arbtid <= 30)

p1 <- plotfun2("arbtid", ylabel = "Ugentlig arbejdstid", dset=d3,
         titel = "Gennemsnitlig ugentlig arbejdstid blandt panelister, 
der arbejder mere end 30 timer om ugen")

p2 <- plotfun2("arbtid", ylabel = "Ugentlig arbejdstid", dset=d4,
               titel = "Gennemsnitlig ugentlig arbejdstid blandt
panelister, der arbejder højst 30 timer om ugen")

grid.arrange(p1, p2, ncol=2)

p3 <- plotfun2("højst20", ylabel = "Andel der arbejder under 20 timer om ugen",
               titel = "... højst 20 timer om ugen") + ylim(.15, .23)
p4 <- plotfun2("mindst40", ylabel = "Andel der arbejder over 40 timer om ugen",
               titel = "... mindst 40 timer om ugen") + ylim(.15, .23)
grid.arrange(p4, p3, ncol=2, 
             top = "Andelen af alle panelister, der arbejder...")

d <- d2 %>% filter(wave == 1 | wave==13) %>% mutate(wave = as.factor(wave))
ggplot(d, aes(x = arbtid, fill = wave, y = ..density..)) +
  geom_histogram(position="dodge", binwidth=1, alpha=.7) +
  geom_density(alpha = .2) +
  theme_minimal() + xlim(10,50)


# Her er plot og lineær reg for standardafvigelsen i arbtid:
dd %>% group_by(wave) %>% summarise(sd = sd(arbtid, na.rm=T)) %>% 
  ggplot(aes(x = wave, y = sd)) + 
  geom_line() + geom_point() + theme_bw() +
  ylab("Standardafvigelse i ugentlig arbejdstid") +
  geom_smooth(method = "lm", color = "maroon", formula = y ~ x) +
  ggtitle("Standardafvigelse i ugentlige arbejdstimer (alle panelister) \n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = c(1,7,13))
  

dd %>% group_by(wave) %>% summarise(sd = sd(arbtid, na.rm=T)) %>% 
  lm(data=., sd ~ wave) %>% broom::tidy()

# Og her med gini-koefficient:
dd %>% group_by(wave) %>% summarise(gini = Gini(arbtid, na.rm=T)) %>% 
  ggplot(aes(x = wave, y = gini)) +
  geom_line() + geom_point() + theme_bw() +
  ylab("Gini-koefficient for arbejdstid") +
  geom_smooth(method = "lm", color = "maroon", formula = y ~ x)

dd %>% group_by(wave) %>% summarise(gini = DescTools::Gini(arbtid, na.rm=T)) %>% 
  lm(data=., gini ~ wave) %>% broom::tidy()



##############
##  Opdelt  ##
##############

# Regressioner sådan her:
d %>% filter(løntype=="By the hour") %>% 
  lm(data=., arbtid ~ wave) %>% 
  broom::tidy()

# Indvandrer
plotfun("ukborn", titel = "Ugentlig arbejdstid fordelt på indvandrerstatus")
plotfun("ukborn", "under20")

# Sygdom
d <- d2 %>% mutate(sygdom = case_when(
  sygdom==1 ~ "Langvarig sygdom",
  sygdom==0 ~ "Ingen langvarig sygdom",
  T ~ NA
))
d <- d %>% filter(arbtid > 20)
plotfun("sygdom", titel = "Ugentlig arbejdstid fordelt på sygdomsstatus
(kun panelister på over 20 timer om ugen)")

# Løn, job, indk
d <- d2
d <- d2 %>% filter(løntype!="Other")
d <- d %>% filter(arbtid > 20)

plotfun("løntype", titel = "Ugentlig arbejdstid fordelt på aflønningstype
(kun panelister på over 20 timer om ugen)")

d <- d2 %>% mutate(jobtype = case_when(
  jobtype == "Routine" ~ "1. Routine", 
  jobtype == "Intermediate" ~ "2. Intermediate",
  jobtype == "Management & professional" ~ "3. Management & professional",
  T ~ NA
))
d <- d %>% filter(arbtid > 20)

plotfun("jobtype", titel = "Ugentlig arbejdstid fordelt på jobtype
(kun panelister på over 20 timer om ugen)")


d <- d2 %>% mutate(indkgruppe = case_when(
  indkgruppe == 1 ~ "1. kvartil",
  indkgruppe == 2 ~ "2. kvartil",
  indkgruppe == 3 ~ "3. kvartil",
  indkgruppe == 4 ~ "4. kvartil",
  T ~ NA
))
d <- d %>% filter(arbtid>20)

plotfun("indkgruppe", titel = "Ugentlig arbejdstid fordelt på indkomstkvartil")



# Regressioner sådan her (igen):
d %>% filter(aldersgruppe=="30-39 år") %>% 
  lm(data=., arbtid ~ wave) %>% 
  broom::tidy()


# Fam
d <- d2
d <- d2 %>% filter(arbtid > 20)

plotfun("børn2", titel = "Ugentlig arbejdstid fordelt på antal børn under 16 år
(kun panelister på over 20 timer om ugen)")


# Alder
plotfun("aldersgruppe", titel = "Ugentlig arbejdstid fordelt på aldersgrupper")











