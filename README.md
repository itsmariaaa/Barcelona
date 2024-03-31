# Barcelona
## Analysis of "Barometre de Barcelona 2023". A survey was carried out after the municipal elections in Barcelona 2023.

The motivation of this project is to know the different voting behaviours of the main parties in the elections.

The database used is as follows: [ Baròmetre de Barcelona - Primera onada 2023 - Post-Electoral](https://w10.bcn.cat/APPS/riswestudis/editEstudi.do?reqCode=inspectById&estudiid=7360&set-locale=ca_ES)

### First step:
The first thing to do is to load the packages from Rstudio that are used and clean the data. 
```` r
library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
install.packages("treemap")
library(treemap)
library(dplyr)
library(moderndive) # linal regressions
post_electoral_2023 <- read.csv("file.csv")

# clean data
# eliminate columns 
post_electoral_2023 <- post_electoral_2023 %>%
  select(SEXE, EDAT1899_1A6, DISTRICTE, SITLABORAL_1A7, EVO1A_BCN, PROB_BCN_AGR, 
         VAL_GESTMUNI_1A3, MUNI_PARTIT, DECIDIR_VOTAR_SI_1A2, DECIDIR_VOTAR_PARTIT_1A2,
         MOMENT_DECIDIR_VOTAR_SI_1A4, MOMENT_DECIDIR_VOTAR_PARTIT_1A3, MUNI_PARTIT01_DUB,
         MOT_MUNI_PARTIT_1A3, VAL_BCN_ECON_1A3, IDEO_1A6, SENT_PERTI_1A5, LLOC_NAIX_1A5)
# change values from "MUNI_PARTIT" variable
post_electoral_2023 <- post_electoral_2023 %>%
  mutate(MUNI_PARTIT = case_when(
    MUNI_PARTIT %in% c("ALTRES", "PACMA", "BCNETSTU", "NOVAPRIMARIES", "VALENTS") ~ "ALTRES",
    MUNI_PARTIT %in% c("EN BLANC", "ESCONS EN BLANC") ~ "EN BLANC",
    MUNI_PARTIT == " " ~ "NO CONTESTA",
    MUNI_PARTIT == "TRIAS PER BARCELONA" ~ "TRIAS PER BARCELONA",
    MUNI_PARTIT == "BARCELONA EN COMÚ" ~ "BARCELONA EN COMÚ",
    MUNI_PARTIT == "PSC" ~ "PSC",
    MUNI_PARTIT == "CUP" ~ "CUP",
    MUNI_PARTIT == "PP" ~ "PP",
    MUNI_PARTIT == "ERC" ~ "ERC",
    MUNI_PARTIT == "VOX" ~ "VOX"
  ))
# change values from MOMENT_DECIDIR_VOTAR_PARTIT_1A3
post_electoral_2023 <- post_electoral_2023 %>%
  mutate(MOMENT_DECIDIR_VOTAR_PARTIT_1A3 = if_else(
    MOMENT_DECIDIR_VOTAR_PARTIT_1A3 == "HO TENIA DECIDIT DES DE FEIA BASTANT DE TEMPS (ABANS DE LA CAMPANYA ELECTORAL)", "ABANS DE LA CAMPANYA ELECTORAL", MOMENT_DECIDIR_VOTAR_PARTIT_1A3  ))
# change values MUNI_PARTIT01_DUB
post_electoral_2023 <- post_electoral_2023 %>%
  mutate(MUNI_PARTIT01_DUB = case_when(
    MUNI_PARTIT01_DUB %in% c("ALTRES", "PACMA", "NUL", "CIUTADANS", "VALENTS") ~ "ALTRES",
    MUNI_PARTIT01_DUB %in% c("EN BLANC", "NO HO SAP") ~ "EN BLANC",
    MUNI_PARTIT01_DUB == " " ~ "NO CONTESTA",
    MUNI_PARTIT01_DUB == "TRIAS PER BARCELONA" ~ "TRIAS PER BARCELONA",
    MUNI_PARTIT01_DUB == "BARCELONA EN COMÚ" ~ "BARCELONA EN COMÚ",
    MUNI_PARTIT01_DUB == "PSC" ~ "PSC",
    MUNI_PARTIT01_DUB == "CUP" ~ "CUP",
    MUNI_PARTIT01_DUB == "PP" ~ "PP",
    MUNI_PARTIT01_DUB == "ERC" ~ "ERC",
    MUNI_PARTIT01_DUB == "VOX" ~ "VOX"
  ))
# change values MOMENT_DECIDIR_VOTAR_SI_1A4
post_electoral_2023 <- post_electoral_2023 %>%
  mutate(MOMENT_DECIDIR_VOTAR_SI_1A4 = if_else(
    MOMENT_DECIDIR_VOTAR_SI_1A4 == "HO TENIA DECIDIT DES DE FEIA BASTANT DE TEMPS (ABANS DE LA CAMPANYA ELECTORAL)", "ABANS DE LA CAMPANYA ELECTORAL", MOMENT_DECIDIR_VOTAR_SI_1A4))
colores <- c("ALTRES" = "brown", 
             "EN BLANC" = "gray", 
             "TRIAS PER BARCELONA" = "turquoise",
             "BARCELONA EN COMÚ" = "purple",
             "PSC" = "red",
             "CUP" = "yellow",
             "PP" = "blue",
             "ERC" = "orange",
             "VOX" = "green")
````

### Second step:
Creation of differents charts.
```` r
#TREMAP ON PROBLEMS
treemap(observacions_prob,
        index="PROB_BCN_AGR",
        vSize="porcentaje",
        type="index")
observacions_prob <- post_electoral_2023 %>%
  group_by(PROB_BCN_AGR) %>%
  summarize(observaciones = n()) %>%
  mutate(porcentaje = round((observaciones / sum(observaciones)) * 100, 2))

# GRAPH ON MUNICIPAL MANAGEMENT EVALUATION
post_electoral_2023 %>%
  filter(!is.na(MUNI_PARTIT)) %>%
  ggplot(aes(x = VAL_GESTMUNI_1A3, fill = MUNI_PARTIT)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Percentatge", x = "Valoració gestió municipal", 
       title = "Valoració de la gestió municipal en funció del seu vot",
       fill = "Partit polític") +
  theme_minimal() +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank())

# GRAPH ON MUNICIPAL ECONOMY EVALUATION
post_electoral_2023 %>%
  ggplot(aes(x = VAL_BCN_ECON_1A3, fill = MUNI_PARTIT)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Percentatge", x = "Valoració gestió econòmica", 
       title = "Valoració de la economia de la ciutat en funció del seu vot",
       fill = "Partit polític") +
  theme_minimal() +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank())

# GRAPH ON BARCELONA EVOLUTION
post_electoral_2023 %>%
  ggplot(aes(x = EVO1A_BCN, fill = MUNI_PARTIT)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Percentatge", x = "Valoració evolució de la ciutat", 
       title = "Valoració de la evolució de Barcelona en funció del seu vot",
       fill = "Partit polític") +
  theme_minimal() +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major.x = element_blank(),  # Eliminar las líneas verticales
        panel.grid.minor.x = element_blank())

# GRAPH ON WHEN THEY DECIDED TO VOTE
post_electoral_2023 %>%
  filter(!is.na(MUNI_PARTIT)) %>%
  ggplot(aes(y = MOMENT_DECIDIR_VOTAR_SI_1A4, fill = MUNI_PARTIT)) +
  geom_bar(aes(x = (..count..)/sum(..count..))) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "Percentatge", y = " ",
       title = "Moment en que van decidir anar a votar",
       fill = "Partit polític") +
  theme_minimal() +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major.y = element_blank(),  # Eliminar las líneas verticales
        panel.grid.minor.y = element_blank())

# GRAPH ON WHEN THEY DECIDED TO VOTE FOR THE PARTY
post_electoral_2023 %>%
  filter(!is.na(MUNI_PARTIT) & MUNI_PARTIT != "EN BLANC") %>%
  ggplot(aes(y = MOMENT_DECIDIR_VOTAR_PARTIT_1A3, fill = MUNI_PARTIT)) +
  geom_bar(aes(x = (..count..)/sum(..count..))) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "Percentatge", y = "Moment escollir partit", 
       title = "Moment de la decició del partit polític",
       fill = "Partit polític") +
  theme_minimal() +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major.y = element_blank(),  
        panel.grid.minor.y = element_blank())

# GRAPH ON WHICH WAS THEIR SECOND OPTION FOR VOTE (PSC VOTERS)
post_electoral_2023 %>%
  filter(MUNI_PARTIT == "PSC" & !is.na(MUNI_PARTIT01_DUB)) %>%
  ggplot(aes(x = MUNI_PARTIT, fill = MUNI_PARTIT01_DUB)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Percentatge", x = "Vots a PSC", 
       title = "Les segones opcions del PSC",
       fill = "Partit polític") +
  theme_minimal() +
  scale_fill_manual(values = colores) +
  theme(panel.grid.major.x = element_blank(),  
        panel.grid.minor.x = element_blank())
````
The final visualization

**Graph 1: GRAPH ON MUNICIPAL MANAGEMENT EVALUATION**
![graph_1](https://github.com/itsmariaaa/Barcelona/assets/165412651/b32353e8-9f70-42cf-a950-422be2d17ffd)
**Graph 2: GRAPH ON MUNICIPAL ECONOMY EVALUATION**
![graph_2](https://github.com/itsmariaaa/Barcelona/assets/165412651/33763776-ac5d-4fc3-855d-4d7dd1cd0166)
**Graph 3: GRAPH ON BARCELONA EVOLUTION**
![graph_3 (2)](https://github.com/itsmariaaa/Barcelona/assets/165412651/886be8fa-5ab8-4ea7-bd0c-1ca46d3089d4)
**Graph 4: GRAPH ON WHEN THEY DECIDED TO VOTE**
![graph_4](https://github.com/itsmariaaa/Barcelona/assets/165412651/230b48bb-de99-4365-abec-3c1e05c8806c)
**Graph 5: GRAPH ON WHEN THEY DECIDED TO VOTE FOR THE PARTY**
![graph_5](https://github.com/itsmariaaa/Barcelona/assets/165412651/682b28f8-312e-40c7-b42d-5a3b78e2ffc7)
**Graph 6: GRAPH ON WHICH WAS THEIR SECOND OPTION FOR VOTE (PSC VOTERS)**
![graph_6](https://github.com/itsmariaaa/Barcelona/assets/165412651/70135de5-2b3e-4783-91f3-10bc6288073f)



### Third step:
Make some linear regressions. But first we've to create another dataframe for each political party 
````r
psc <- post_electoral_2023 %>%
  select(MUNI_PARTIT, PROB_BCN_AGR) %>%
  mutate(MUNI_PARTIT = case_when(
    MUNI_PARTIT == "PSC" ~ 1,
    MUNI_PARTIT %in% c("TRIAS PER BARCELONA", "BARCELONA EN COMÚ", "", "CUP", "PP", "ERC", "EN BLANC", "ALTRES", "VOX") ~ 0))
trias <- post_electoral_2023 %>%
  select(MUNI_PARTIT, PROB_BCN_AGR) %>%
  mutate(MUNI_PARTIT = case_when(
    MUNI_PARTIT == "TRIAS PER BARCELONA" ~ 1,
    MUNI_PARTIT %in% c("PSC", "BARCELONA EN COMÚ", "", "CUP", "PP", "ERC", "EN BLANC", "ALTRES", "VOX") ~ 0))
comuns <- post_electoral_2023 %>%
  select(MUNI_PARTIT, PROB_BCN_AGR) %>%
  mutate(MUNI_PARTIT = case_when(
    MUNI_PARTIT == "BARCELONA EN COMÚ" ~ 1,
    MUNI_PARTIT %in% c("TRIAS PER BARCELONA", "PSC", "", "CUP", "PP", "ERC", "EN BLANC", "ALTRES", "VOX") ~ 0))
pp <- post_electoral_2023 %>%
  select(MUNI_PARTIT, PROB_BCN_AGR) %>%
  mutate(MUNI_PARTIT = case_when(
    MUNI_PARTIT == "PP" ~ 1,
    MUNI_PARTIT %in% c("TRIAS PER BARCELONA", "BARCELONA EN COMÚ", "", "CUP", "PSC", "ERC", "EN BLANC", "ALTRES", "VOX") ~ 0))
erc <- post_electoral_2023 %>%
  select(MUNI_PARTIT, PROB_BCN_AGR) %>%
  mutate(MUNI_PARTIT = case_when(
    MUNI_PARTIT == "ERC" ~ 1,
    MUNI_PARTIT %in% c("TRIAS PER BARCELONA", "BARCELONA EN COMÚ", "", "CUP", "PP", "PSC", "EN BLANC", "ALTRES", "VOX") ~ 0))
vox <- post_electoral_2023 %>%
  select(MUNI_PARTIT, PROB_BCN_AGR) %>%
  mutate(MUNI_PARTIT = case_when(
    MUNI_PARTIT == "PSC" ~ 1,
    MUNI_PARTIT %in% c("TRIAS PER BARCELONA", "BARCELONA EN COMÚ", "", "CUP", "PP", "ERC", "EN BLANC", "ALTRES", "PSC") ~ 0))
````
Then we've to change the values of teh problems, and select the five's most voted, based on the previous chart about problems. 
```` r
psc_1 <- psc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "INSEGURETAT", 1, 0
  ))
psc_2 <- psc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "ACCÉS A L'HABITATGE", 1, 0
  ))
psc_3 <- psc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "NETEJA", 1, 0
  ))
psc_4 <- psc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "TURISME", 1, 0
  ))
psc_5 <- psc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "VALORS SOCIALS NEGATIUS / MANCA D'EDUCACIÓ, CIVISME", 1, 0
  ))
comuns_1 <- comuns %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "INSEGURETAT", 1, 0
  ))
comuns_2 <- comuns %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "ACCÉS A L'HABITATGE", 1, 0
  ))
comuns_3 <- comuns %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "NETEJA", 1, 0
  ))
comuns_4 <- comuns %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "TURISME", 1, 0
  ))
comuns_5 <- comuns %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "VALORS SOCIALS NEGATIUS / MANCA D'EDUCACIÓ, CIVISME", 1, 0
  ))
erc_1 <- erc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "INSEGURETAT", 1, 0
  ))
erc_2 <- erc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "ACCÉS A L'HABITATGE", 1, 0
  ))
erc_3 <- erc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "NETEJA", 1, 0
  ))
erc_4 <- erc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "TURISME", 1, 0
  ))
erc_5 <- erc %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "VALORS SOCIALS NEGATIUS / MANCA D'EDUCACIÓ, CIVISME", 1, 0
  ))
pp_1 <- pp %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "INSEGURETAT", 1, 0
  ))
pp_2 <- pp %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "ACCÉS A L'HABITATGE", 1, 0
  ))
pp_3 <- pp %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "NETEJA", 1, 0
  ))
pp_4 <- pp %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "TURISME", 1, 0
  ))
pp_5 <- pp %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "VALORS SOCIALS NEGATIUS / MANCA D'EDUCACIÓ, CIVISME", 1, 0
  ))
trias_1 <- trias %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "INSEGURETAT", 1, 0
  ))
trias_2 <- trias %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "ACCÉS A L'HABITATGE", 1, 0
  ))
trias_3 <- trias %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "NETEJA", 1, 0
  ))
trias_4 <- trias %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "TURISME", 1, 0
  ))
trias_5 <- trias %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "VALORS SOCIALS NEGATIUS / MANCA D'EDUCACIÓ, CIVISME", 1, 0
  ))
vox_1 <- vox %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "INSEGURETAT", 1, 0
  ))
vox_2 <- vox %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "ACCÉS A L'HABITATGE", 1, 0
  ))
vox_3 <- vox %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "NETEJA", 1, 0
  ))
vox_4 <- vox %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "TURISME", 1, 0
  ))
vox_5 <- vox %>%
  mutate(PROB_BCN_AGR = if_else(
    PROB_BCN_AGR == "VALORS SOCIALS NEGATIUS / MANCA D'EDUCACIÓ, CIVISME", 1, 0
  ))
````
And finally, we can make our linear regressions
```` r
# INSEGURITY MODEL
lmpsc_1<- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = psc_1)
lmpp_1 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = pp_1)
lmcomuns_1 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = comuns_1)
lmerc_1 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = erc_1)
lmvox_1 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = vox_1)
texreg::screenreg(list(lmpsc_1, lmpp_1, lmcomuns_1, lmerc_1,lmvox_1))

# HOUSING MODEL
lmpsc_2<- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = psc_2)
lmpp_2 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = pp_2)
lmcomuns_2 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = comuns_2)
lmerc_2 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = erc_2)
lmvox_2 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = vox_2)
texreg::screenreg(list(lmpsc_2, lmpp_2, lmcomuns_2, lmerc_2,lmvox_2))

# CLEANING SERVICE MODEL
lmpsc_3<- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = psc_3)
lmpp_3 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = pp_3)
lmcomuns_3 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = comuns_3)
lmerc_3 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = erc_3)
lmvox_3 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = vox_3)
texreg::screenreg(list(lmpsc_3, lmpp_3, lmcomuns_3, lmerc_3,lmvox_3))

# TURSIM MODEL
lmpsc_4<- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = psc_4)
lmpp_4 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = pp_4)
lmcomuns_4 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = comuns_4)
lmerc_4 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = erc_4)
lmvox_4 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = vox_4)
texreg::screenreg(list(lmpsc_4, lmpp_4, lmcomuns_4, lmerc_4,lmvox_4))

# CIVIC VALUES MODEL
lmpsc_5<- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = psc_5)
lmpp_5 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = pp_5)
lmcomuns_5 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = comuns_5)
lmerc_5 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = erc_5)
lmvox_5 <- lm(MUNI_PARTIT ~ PROB_BCN_AGR, data = vox_5)
texreg::screenreg(list(lmpsc_5, lmpp_5, lmcomuns_5, lmerc_5, lmvox_5))
````




