library(dplyr)
library(readxl)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(zoo)

## set system local so that R can read accents and other special characters
Sys.setlocale('LC_ALL', locale = 'es_ES')  

## open files for each state and bind them in a single data frame 
number <- paste(1:32, "xlsx", sep =".")

pres_munis <- lapply(number, function(x) read_excel(paste("INAFED/presidentes_municipales_historico", x, sep ="_"), skip = 2))

pres_munis <- as.data.frame(bind_rows(pres_munis))

pres_munis <- tbl_df(pres_munis)

## paste together articles to last names, separate names.

pres_munis <- pres_munis %>% 
  filter(!is.na(Municipio)) %>%
  rename(party = Partido, sex = Sexo) %>%
  mutate(mun_name = paste(Municipio, Estado, sep = ", ")) %>%
  mutate(Periodo = gsub("del ", "", Periodo)) %>%
  mutate(Periodo = gsub("'", "", Periodo)) %>%
  mutate(Periodo = gsub("de ", "", Periodo)) %>%
  mutate(Periodo = gsub("al ", "", Periodo)) %>%
  mutate(Periodo = gsub("a ", "", Periodo)) %>%
  separate(Periodo, c("start", "finish"), sep = " ", remove = TRUE, extra = "merge") %>%
  mutate(`Presidente Municipal` = gsub("DE ", "DE", `Presidente Municipal`)) %>%
  mutate(`Presidente Municipal` = gsub("DEL ", "DEL", `Presidente Municipal`)) %>%
  mutate(`Presidente Municipal` = gsub("DELA ", "DELA", `Presidente Municipal`)) %>%
  separate(`Presidente Municipal`, c("title", "name1", "name2", "lastn1", "lastn2"),
           sep = " ", remove = TRUE, extra = "merge")

## extract year number from date as character in case of having full date in column

pres_munis$year <- ifelse(nchar(pres_munis$start) == 11, 
                    as.numeric(substr(pres_munis$start, 8, nchar(pres_munis$start))), as.numeric(pres_munis$start))

##remove accents and special characters

pres_munis$mun_name <- gsub("`|\\'", "", iconv(pres_munis$mun_name, to="ASCII//TRANSLIT"))
pres_munis$mun_name <- gsub("~","", pres_munis$mun_name)
pres_munis$mun_name <- gsub("\"","", pres_munis$mun_name)
pres_munis$mun_name <- toupper(pres_munis$mun_name)

## correct municipality names to match with INEGI catalog and get muncodes

pres_munis$mun_name <- gsub("DR.", "DOCTOR", pres_munis$mun_name, fixed = TRUE)
pres_munis$mun_name <- gsub("GRAL.", "GENERAL",pres_munis$mun_name)
pres_munis$mun_name <- gsub("SILAO", "SILAO DE LA VICTORIA",pres_munis$mun_name)
pres_munis$mun_name <- gsub("ACAMBAY", "ACAMBAY DE RUIZ CASTANEDA",pres_munis$mun_name)
pres_munis$mun_name <- gsub("TEZOATLAN DE SEGURA Y LUNA", "HEROICA VILLA TEZOATLAN DE SEGURA Y LUNA, CUNA DE LA INDEPENDENCIA DE OAXACA",pres_munis$mun_name)
pres_munis$mun_name <- gsub("MEDELLIN", "MEDELLIN DE BRAVO",pres_munis$mun_name)
pres_munis$mun_name <- gsub("DOCTOR BELISARIO DOMINGUEZ, CHIHUAHUA", "DR. BELISARIO DOMINGUEZ, CHIHUAHUA",pres_munis$mun_name)
pres_munis$mun_name <- gsub("TLALTIZAPAN", "TLALTIZAPAN DE ZAPATA",pres_munis$mun_name)
pres_munis$mun_name <- gsub("ZACUALPAN, MORELOS", "ZACUALPAN DE AMILPAS, MORELOS",pres_munis$mun_name)
pres_munis$mun_name <- gsub("ZAPOTITLAN DEL RIO", "SAN MATEO YUCUTINDOO",pres_munis$mun_name)
pres_munis$mun_name <- gsub("TLAQUEPAQUE", "SAN PEDRO TLAQUEPAQUE",pres_munis$mun_name)
pres_munis$mun_name <- gsub("EL PARRAL, CHIAPAS", "VILLA CORZO, CHIAPAS",pres_munis$mun_name)
pres_munis$mun_name <- gsub("EMILIANO ZAPATA, CHIAPAS", "VILLA COMALTITLAN, CHIAPAS",pres_munis$mun_name)

## standardize PANAL for partido nueva alianza

pres_munis$party <- ifelse(grepl("PNA", pres_munis$party), "PANAL", pres_munis$party)

## load inegi name catalog and join

inegi_name <- read_csv("data/muncodeMAY2015.csv") %>%
  mutate(mun_name = paste(NOM_MUN, NOM_ENT, sep =", "))

inegi_name$mun_name <- toupper(inegi_name$mun_name)


pres_munis <- full_join(pres_munis, inegi_name, by = "mun_name") %>%
  select(mun_name, CVE_ENT, CVE_MUN, title:party, year) %>%
  mutate(muncode = CVE_ENT*1000+CVE_MUN)

## coalition catalog, includes the composition of some recent coalitions as used by INAFED

party_cat <- read_excel("data/partidos.xlsx") %>%
  select(party = partido, coalition = integrantes)

generic <- unique(party_cat$party[duplicated(party_cat$party)])

party_cat <-  filter(party_cat, !party %in% generic, !is.na(coalition))

pres_munis <- left_join(pres_munis, party_cat, by = "party")


## latest list. most years are already inclueded in historical list, except for Nayarit in 2014
## and the rest of states after 2014

pres_munis2016 <- read_excel("data/presidentesmunicipales2016.xlsx", skip = 2) %>%
  select(estado, municipio, CVE_ENT = id_estado, CVE_MUN = id_municipio, title = titulo, nombre, 
         lastn1 = ap_paterno, lastn2 = ap_materno, sex = sexo, party = partido,
         party_INAFED = integrantes, start = pdo_gob_ini, finish = pdo_gob_fin) %>%
  mutate(year = year(start), mun_name = paste(municipio, estado, sep = ", "), 
         muncode = as.numeric(paste0(CVE_ENT*1000 + CVE_MUN)), 
         start = as.character(start), finish = as.character(finish)) %>%
  separate(nombre, c("name1", "name2"), sep = " ", remove = TRUE, extra = "merge") %>%
  select(-estado, -municipio)

pres_munis2016$mun_name <- gsub("`|\\'", "", iconv(pres_munis2016$mun_name, to="ASCII//TRANSLIT"))
pres_munis2016$mun_name <- gsub("~","", pres_munis2016$mun_name)
pres_munis2016$mun_name <- gsub("\"","", pres_munis2016$mun_name)
pres_munis2016$mun_name <- toupper(pres_munis2016$mun_name)


pres_munis2012 <- filter(pres_munis, year > 2011)


a <- left_join(pres_munis2016, pres_munis2012, by = c("muncode", "year"))

a <- filter(a, !is.na(mun_name.y))

a <- select(a, muncode, year)

pres_munis <- filter(pres_munis, !muncode %in% unique(a$muncode) | !year %in% unique(a$year))


pres_munis2016$party_INAFED <- ifelse(is.na(pres_munis2016$party_INAFED), pres_munis2016$party, pres_munis2016$party_INAFED) 
pres_munis2016$party_INAFED <- ifelse(pres_munis2016$party_INAFED == "0", pres_munis2016$party, pres_munis2016$party_INAFED) 

pres_munis2016 <- select(pres_munis2016, -party)


## Distrito Federal 2000 - 2006 list since data is not included in historical data base. 

pres_munis_DF <- read_excel("data/df2000-06.xlsx") %>%
  mutate(name = gsub("de ", "de", name)) %>%
  mutate(name = gsub("dela ", "dela", name)) %>%
  mutate(name = gsub("delos ", "delos", name)) %>%
  mutate(name = gsub("Von ", "Von", name)) %>%
  mutate(name = gsub("aus den ", "ausden", name)) %>%
  separate(name, c("name1", "name2", "lastn1", "lastn2"),
           sep = " ", remove = TRUE, extra = "merge") %>%
  mutate(mun_name = paste(NOM_MUN, NOM_ENT, sep =", "), muncode = CVE_ENT*1000+CVE_MUN) %>%
  mutate(start = as.character(year), finish = as.character(year + 3)) %>%
  select(-NOM_MUN, NOM_ENT) %>%
  rename(party_INAFED = party)


## electoral results database by CIDAC, loaded to match with INAFED's historical database to fill 
## missing party data and party coalition members. goes from 1985 to part of 2012. It has some mistakes
## like in Arroyo Seco Quertaro when total votes exceeed by more than double previous years as well as
## anulled vots. Other mistakes in Estado de M?xico 2012 where only PRI votes were registered. Or in the BCS
##election in 2008 where votes for PRD-CONV-PT are set at zero, mistakenly giving victory to the PRI in some
## municipalities. The INAFED data may be more reliable in some cases like in El Salvador, 
## Zacatecas 2004 where therewas a tie. the CIDAC database only considers votes counted from election day, 
## it does not include,overruled results. 


files <- list.files(path = "CIDAC/", pattern = "*.csv")
files <- paste0("CIDAC/", files)

## set column types in order to be able to bind all files, since each read a different column type

cols = "cccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn"


elec_mun <- lapply(files, function(x) read_csv(x, col_types = cols))

elec_mun <- as.data.frame(bind_rows(elec_mun))

elec_mun <- tbl_df(elec_mun) %>%
  select(-ANULADOS, -NO.REG.) %>%
  mutate(muncode = gsub("[[:blank:]]", "", CVE_INEGI)) %>%
  separate(CVE_INEGI, c("CVE_ENT", "CVE_MUN"), sep = " ") %>%
  rename(year = `A. Electoral`, PAN = PAN.) %>%
  mutate(CVE_ENT = as.numeric(CVE_ENT), CVE_MUN = as.numeric(CVE_MUN))
  
## calculate percent of vote per party and melt database

all_pct <- elec_mun %>%
  mutate_each_(funs(pct = . / TOTAL*100), names(elec_mun[6:229])) %>%
  gather("party_CIDAC", "percent", 6:229, na.rm = TRUE)

##change dots to dashes in coalition composition
all_pct$party_CIDAC <- gsub("\\.", "-", all_pct$party_CIDAC)
all_pct$party_CIDAC <- gsub("PANAL", "PNA", all_pct$party_CIDAC)

##select parties with hieghts percentage, exclude Estado de M?xico mistake
winners <- all_pct %>%
  mutate(muncode = as.numeric(as.character(muncode))) %>%
  group_by(year, muncode) %>%
  filter(percent == max(percent)) %>%
  filter((year != 2012) | (CVE_ENT != 15))

## identify state where elections take place in the year previous to actual access to power
mes <- c("Ene|Feb")
prev <- unique(pres_munis[which(grepl(mes,  pres_munis$start)),]$CVE_ENT)

## shift year for states where elections take place in the previous year
pres_munis$year <- ifelse(pres_munis$CVE_ENT %in% prev, pres_munis$year-1, pres_munis$year)
pres_munis2016$year <- ifelse(month(pres_munis2016$start) < 3, pres_munis2016$year-1, pres_munis2016$year)
pres_munis2016$start <- as.character(year(pres_munis2016$start)) 
pres_munis2016$finish <- as.character(year(pres_munis2016$finish)) 

##adjust election year for state that changed election dates
mes1 <- c("Mar|Abr|May|Jun|Jul|Ago|Sep|Oct|Nov|Dic")

pres_munis[pres_munis$CVE_ENT %in% prev, ]$year <- ifelse(grepl(mes1,
                                                                pres_munis[pres_munis$CVE_ENT %in% prev, ]$start),
                                                          pres_munis[pres_munis$CVE_ENT %in% prev, ]$year+1,
                                                          pres_munis[pres_munis$CVE_ENT %in% prev, ]$year)

pres_munis$coalition <- ifelse(is.na(pres_munis$coalition), pres_munis$party, pres_munis$coalition) 

pres_munis <- pres_munis %>%
  rename(party_INAFED = coalition) %>%
  select(-party)

##bind historical database, with most current one and ad DF data

pres_munis <- bind_rows(pres_munis, pres_munis_DF)
pres_munis_final <- bind_rows(pres_munis, pres_munis2016)

pres_munis_final$year <- ifelse(pres_munis_final$CVE_ENT == 30 & pres_munis_final$year < 1991, pres_munis_final$year+1, pres_munis_final$year)

## join INAFED's database with CIDAC's
all_pres_munis <- full_join(pres_munis_final, winners, by = c("muncode", "year")) %>%
  select(mun_name, CVE_ENT = CVE_ENT.x, CVE_MUN = CVE_MUN.x, muncode, title:sex, 
         year_start = start, year_finish = finish, year_elec = year, party_INAFED, party_CIDAC)

# save(all_pres_munis, file = "allmunicipalities.RData")
# 
# load("allmunicipalities.RData")



##generate a dataframe for every year in power between 2005 and 2016, and clean party names. 
  
expan <- all_pres_munis 

expan$year_start <- ifelse(nchar(expan$year_start) == 11, 
                          as.numeric(substr(expan$year_start, 8, nchar(expan$year_start))), as.numeric(expan$year_start))

expan$year_finish <- ifelse(nchar(expan$year_finish) == 11, 
                           as.numeric(substr(expan$year_finish, 8, nchar(expan$year_finish))), as.numeric(expan$year_finish))


#expan$party_INAFED <- ifelse(expan$year_start == 2014 & expan$CVE_ENT == "1" & expan$party_INAFED == "UPT", "PAN-PRD", expan$party_INAFED)

expan <- expan %>% 
  filter(year_start > 2004)


expan$party_CIDAC <- ifelse(is.na(expan$party_CIDAC), expan$party_INAFED, expan$party_CIDAC)

noparty <- c("COALICI?N", "AXB", "COAL.", "C.C.", 
             "AXC", "CXBCH", "CXBDT", "CXBCHS", "CXBDC", "DESCONOCIDOS",
             "ALIANZA_", "CXBDCH", "CXBNT", "CXBNDT", "AXM", "CXBP",  "CQRA",
             "CPSL", "CSA", "ALIANZA", "CPBT", "CUXT", 
             "ASD", "CAFV", "AVE", "CXBNCH", "APM", "ATT", "UPT", "CUPG")

expan$party_mixed <- ifelse(expan$party_INAFED  %in% noparty, expan$party_CIDAC, expan$party_INAFED)


year <- data.frame(muncode = rep((unique(expan$muncode)), 12))

year <- arrange(year, muncode)

year$year_start <- rep(2005:2016, 2461)

expan1 <- left_join(year, expan, by = c("muncode", "year_start"))

expan1 <- na.locf(expan1)

expan1 <- tbl_df(expan1)


expan1$party_mixed <- ifelse(grepl("PRI", expan1$party_mixed), "PRI", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PAN-PRD|PRD-PAN", expan1$party_mixed), "PRANRD", expan1$party_mixed)
#expan1$party_mixed <- ifelse(grepl("PANAL", expan1$party_mixed, fixed = TRUE), "PNA", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PAN", expan1$party_mixed), "PAN", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PRD", expan1$party_mixed), "PRD", expan1$party_mixed)
expan1$party_mixed <- gsub("PRANRD", "PAN-PRD", expan1$party_mixed)
expan1$party_mixed <- gsub("TRANSIN", "PRI", expan1$party_mixed)
expan1$party_mixed <- gsub("CPU", "PAN-PRD", expan1$party_mixed)
expan1$party_mixed <- gsub("CXBDT", "PRD", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PAN-PRD", expan1$party_mixed), "PAN-PRD", expan1$party_mixed)
expan1$party_mixed <- gsub("COAL.", "PRI", expan1$party_mixed , fixed = TRUE)
expan1$party_mixed <- ifelse(expan1$party_mixed == "COALICI?N" & expan1$muncode == " 5025", "PRI",  expan1$party_mixed)
expan1$party_mixed <- ifelse(expan1$party_mixed == "CANDIDATURA COM?N", "PRI",  expan1$party_mixed)

expan1$party_mixed <- ifelse(expan1$party_mixed == "COALICI?N" & expan1$CVE_ENT == "14", "PRD",  expan1$party_mixed)
expan1$party_mixed <- ifelse(expan1$party_mixed == "COALICI?N" & expan1$CVE_ENT == "30", "AVE",  expan1$party_mixed)

big_p <- c("PRI", "PAN", "PRD", "PAN-PRD", "UYC")

expan1$party_mixed <- ifelse(!expan1$party_mixed %in% big_p, "OTHER", expan1$party_mixed)

expan2010_16 <- expan1 %>%
  filter(year_start == "2012" | year_start == "2016" | year_start == "2014" | year_start == "2010") %>%
  mutate(muncode = as.numeric(muncode)) %>%
  select(year_start, muncode, party_mixed)



#write.csv(expan2012_16, "party_mixed.csv")

load("allmunicipalities.RData")

all_pres_munis
