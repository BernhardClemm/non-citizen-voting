#===============================================================================
#  File-Name:	voting-non-nationals.R
#  Date:	May 3, 2021 
#  Author: Bernhard Clemm von Hohenberg
#
# INPUT:
# * Opinium Poll April: https://www.opinium.com/resource-center/uk-scottish-parliament-voting-intention-1-april-2021/
# * Opinium Poll March: https://www.opinium.com/resource-center/uk-scottish-parliament-voting-intention-11-march-2021
# * ESS 9: https://www.europeansocialsurvey.org/download.html?file=ESS9e03_1&y=2018
#
#===============================================================================

# PACKAGES  ####
#===============================================================================
library(tidyverse)
library(ggplot2)
library(rstatix)
library(xlsx)
library(ggpubr)

# PATHS & CONSTANTS  ####
#===============================================================================
data_path <- "/data/"

# OPINIUM POLL  ####
#===============================================================================

opinium_data <- read.xlsx(
  "data/Opinium-Scottish-Parliament-Polling-1-6-April-2021-v1.1.xlsx",
  sheetIndex = 8, encoding = "UTF-8") %>%
  select(NA..29:NA..34) 

# Recode data

colnames(opinium_data) <- opinium_data[2,]
opinium_data <- opinium_data[c(5, 7),]
opinium_data <- opinium_data %>% mutate(across(everything(), as.numeric))
opinium_data$citizenship <- c("UK citizen", "Non-citizen")

# Chi2-test

opinium_test <- chisq.test(opinium_data %>% select(-citizenship),
                           simulate.p.value = TRUE)
opinium_test_chi2 <- round(opinium_test$statistic, 2)
opinium_test_p <- round(opinium_test$p.value, 4)

# Summarize data

opinium_data_summ <- opinium_data %>%
    pivot_longer(cols = c("Conservative", "Labour", "Lib Dem", "SNP" ,
                          "Scottish Greens","Alba Party"),
                 names_to = "Party", values_to = "Frequency") %>%
  mutate(Frequency = as.numeric(Frequency)) %>%
  group_by(citizenship) %>%
  mutate(n = sum(Frequency),
         proportion = (Frequency/n)*100,
         Party = factor(Party, ordered = TRUE,
                        levels = c("Alba Party", "Scottish Greens", "Lib Dem",
                                   "Labour", "Conservative","SNP")),
         citizenship = factor(citizenship, ordered = TRUE,
                              levels = c("UK citizen", "Non-citizen")))

# Plot

opinium_data_plot <- ggplot(opinium_data_summ, aes(x = Party, y = proportion)) +
  geom_bar(aes(fill = Party,
               alpha = citizenship), stat="identity", position=position_dodge()) +
  geom_text(aes(label = round(proportion, 1),
                alpha = citizenship), vjust = -0.5,
            position = position_dodge(width = 1)) +
  theme_minimal() +
  scale_y_continuous(name = "% per respondent group",
                     limits = c(0, 50)) +
  scale_fill_manual(values = c("#dd38ff", "#019600",
                                 "#ffd43d", "#e90000", 
                                 "#3b5ffc", "#f1ef37")) +
  scale_alpha_discrete(name = "Respondents", 
                       range = c(0.4, 0.9)) +
  ggtitle("Scottish Parliament voting intention (Opinium, April 2021)") +
  labs(caption = paste0("Source: Opinium/Sky, 1 - 6 April 2021, https://www.opinium.com/resource-center/uk-scottish-parliament-voting-intention-1-april-2021/.
       Sample size: 998. Differences of the contingencies (citizenship by voting intention) are statistically significant: 
       chi-square statistic = ", opinium_test_chi2, ", p < 0.001.")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12,
                                   vjust= 4),
        axis.text.y = element_text(size = 9,
                                   hjust = 2),
        panel.grid.minor = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border= element_blank(),
        plot.caption =  element_text(size = 9),
        legend.position = c(0.15, 0.78),
        legend.background = element_rect(fill = "grey90"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        title = element_text(size = 16)) +
  guides(fill = FALSE)

# SECOND OPINIUM POLL  ####
#===============================================================================

opinium_data_march <- read.xlsx(
  "/Users/bernhardclemm/Dropbox/Journalismus/Non-citizen voting/data/Opinium-Scottish-Parliament-Polling-11-16-March-2021-v1.2.xlsx",
  sheetIndex = 8, encoding = "UTF-8") %>%
  select(NA..29:NA..33) 

colnames(opinium_data_march) <- opinium_data_march[2,]
opinium_data_march <- opinium_data_march[c(5, 7),]
opinium_data_march <- opinium_data_march %>% mutate(across(everything(), as.numeric))
opinium_data$citizenship <- c("UK citizen", "Non-citizen")

prop.table(as.table(as.matrix(opinium_data_march)), margin = 1)
opinium_test <- chisq.test(opinium_data %>% select(-citizenship),
                           simulate.p.value = TRUE)

# EUROPEAN SOCIAL SURVEY ####
#===============================================================================

ess9_data <- read.csv("/data/ESS1-9e01_1.csv")

# First, exclude countries with few non-citizens respondents
## Choosing at least 5%, which is arbitrary

ess9_data_no_na <- ess9_data %>%
  filter(ctzcntr %in% c(1, 2))
ess9_data_citizen_props <- prop.table(
  table(ess9_data_no_na$ctzcntr, 
        ess9_data_no_na$cntry), margin = 2) # 1 = Yes, 2 = No

countries <- names(which(ess9_data_citizen_props[2,] > 0.05)) # 12 countries

## Further exclude CY because of many "66" values on vote
countries <- countries[countries != "CY"]

# Recode party values into party names

ess9_data <- ess9_data %>%
  filter(ctzcntr %in% c(1, 2)) %>%
  filter(cntry %in% countries) %>%
  mutate(across(
    starts_with("prtcl"), 
      ~ as.character(.))) %>%
  mutate(across(
    starts_with("prtcl"),
    ~ ifelse(
      . == "77" | . == "88" | . == "99" ,
      "Don't \nknow", .))) %>% 
  # Austria
  mutate(prtcldat = case_when(
    prtcldat == "1" ~ "Social \nDemocratic \nParty",
    prtcldat == "2" ~ "People's \nParty",
    prtcldat == "3" ~ "Freedom \nParty",
    prtcldat == "4" ~ "PILZ",
    prtcldat == "5" ~ "The \nGreens",
    prtcldat == "6" ~ "KPÖ",
    prtcldat == "7" ~ "NEOS",
    prtcldat == "6" ~ "KPÖ",
    prtcldat == "7" ~ "NEOS",
    prtcldat == "8" ~ "GILT",
    prtcldat == "9" ~ "Other",
    TRUE ~ as.character(prtcldat)),
  # Belgium
  prtcldbe = case_when(
    prtcldbe == "1" ~ "Green",
    prtcldbe == "2" ~ "Christian \nDemocratic/\nFlemish",
    prtcldbe == "3" ~ "New \nFlemish \nAlliance",
    prtcldbe == "4" ~ "Lijst Dedecker",
    prtcldbe == "5" ~ "Flemish \nSocialist \nParty",
    prtcldbe == "6" ~ "PVDA+",
    prtcldbe == "7" ~ "Vlaams Belang",
    prtcldbe == "8" ~ "Open \nFlemish \nLiberals \nand \nDemocrats",
    prtcldbe == "9" ~ "CDH",
    prtcldbe == "10" ~ "Confederated \nEcologists",
    prtcldbe == "11" ~ "Front National",
    prtcldbe == "12" ~ "Reformist \nMovement",
    prtcldbe == "13" ~ "Socialist \nParty",
    prtcldbe == "14" ~ "PTB",
    prtcldbe == "15" ~ "Parti Populaire", 
    prtcldbe == "16" ~ "DéFI",
    prtcldbe == "17" ~ "Other",
    TRUE ~ as.character(prtcldbe)),
  # Switzerland
  prtclgch = case_when(
    prtclgch == "1" ~ "Swiss \nPeople's \nParty",
    prtclgch == "2" ~ "Social \nDemocratic \nParty",
    prtclgch == "3" ~ "The \nLiberals",
    prtclgch == "4" ~ "Christian \nDemocratic \nParty",
    prtclgch == "5" ~ "Green \nParty",
    prtclgch == "6" ~ "Green \nLiberal \nParty",
    prtclgch == "7" ~ "Conservative \nDemocratic \nParty",
    prtclgch == "8" ~ "Evangelical People's Party",
    prtclgch == "9" ~ "Federal Democratic Union",
    prtclgch == "10" ~ "Ticino League",
    prtclgch == "11" ~ "Swiss Labour Party",
    prtclgch == "12" ~ "Movement of the Citizens of French-speaking Switzerland",
    prtclgch == "13" ~ "Alternative Left",
    prtclgch == "14" ~ "Pirate Party Switzerland",
    prtclgch == "15" ~ "Other",
    TRUE ~ as.character(prtclgch)),
  # Cyprus
  prtclbcy = case_when(
    prtclbcy == "1" ~ "Progressive Party of Working People (AKEL)",
    prtclbcy == "2" ~ "Democratic Party (DIKO)",
    prtclbcy == "3" ~ "Democratic Rally (DISY)",
    prtclbcy == "4" ~ "National Popular Front (ELAM)",
    prtclbcy == "5" ~ "Solidarity Movement (KINIMA ALLILEGII)",
    prtclbcy == "6" ~ "The Cyprus Green Party",
    prtclbcy == "7" ~ "Citizens' Alliance (SYMMACHIA POLITON)",
    prtclbcy == "8" ~ "Social Democrats (KS EDEK)",
    prtclbcy == "9" ~ "Other",
    TRUE ~ as.character(prtclbcy)),
  # Germany
  prtclede = case_when(
    prtclede == "1" ~ "CDU/\nCSU",
    prtclede == "2" ~ "Social \nDemocratic \nParty",
    prtclede == "3" ~ "The \nLeft",
    prtclede == "4" ~ "The \nGreens",
    prtclede == "5" ~ "Free \nDemocratic \nParty",
    prtclede == "6" ~ "Alternative \nfor Germany",
    prtclede == "7" ~ "Pirate Party",
    prtclede == "8" ~ "National Democratic Party",
    prtclede == "9" ~ "Other",
    TRUE ~ as.character(prtclede)),
  # Estonia
  prtclgee = case_when(
    prtclgee == "1" ~ "Estonian \nReform \nParty",
    prtclgee == "2" ~ "Estonian \nCentre \nParty",
    prtclgee == "3" ~ "Isamaa",
    prtclgee == "4" ~ "Social \nDemocratic \nParty",
    prtclgee == "5" ~ "Erakond Eestimaa Rohelised",
    prtclgee == "6" ~ "Conservative \nPeople's \nParty",
    prtclgee == "9" ~ "Eesti Iseseisvuspartei",
    prtclgee == "10" ~ "Üksikkandidaadid või muud",
    prtclgee == "11" ~ "Eesti Vabaerakond",
    prtclgee == "12" ~ "Rahva Ühtsuse Erakond",
    prtclgee == "13" ~ "Other",
    TRUE ~ as.character(prtclgee)),
  # Spain
  prtclfes = case_when(
    prtclfes == "1" ~ "People's \nParty",
    prtclfes == "2" ~ "Socialist \nWorkers' \nParty",
    prtclfes == "3" ~ "Unidas Podemos",
    prtclfes == "4" ~ "Podemos",
    prtclfes == "5" ~ "Ciudadanos",
    prtclfes == "6" ~ "IU",
    prtclfes == "7" ~ "En Comú Podem",
    prtclfes == "8" ~ "Más País",
    prtclfes == "9" ~ "Compromís",
    prtclfes == "10" ~ "ERC",
    prtclfes == "11" ~ "Junts per Catalunya/PDeCAT",
    prtclfes == "12" ~ "Bloque Nacionalista Galego",
    prtclfes == "13" ~ "EAJ-PNV",
    prtclfes == "14" ~ "EH-Bildu",
    prtclfes == "15" ~ "CC-PNC",
    prtclfes == "16" ~ "VOX",
    prtclfes == "17" ~ "Navarra Suma",
    prtclfes == "18" ~ "PACMA",
    prtclfes == "19" ~ "CUP",
    prtclfes == "20" ~ "PRC",
    prtclfes == "53" | prtclfes == "54" | prtclfes == "55"  ~ "Other",
    TRUE ~ as.character(prtclfes)),
  # France
  prtclffr = case_when(
    prtclffr == "1" ~ "LO (Lutte Ouvrière)",
    prtclffr == "2" ~ "NPA (Nouveau Parti Anti-Capitaliste)",
    prtclffr == "3" ~ "PCF (Parti Communiste Français)",
    prtclffr == "4" ~ "La \nFrance \nInsoumise",
    prtclffr == "5" ~ "Socialist \nParty",
    prtclffr == "6" ~ "Europe \nEcology \nThe Greens",
    prtclffr == "7" ~ "La \nRépublique \nen \nMarche",
    prtclffr == "8" ~ "MODEM (Mouvement Démocrate)",
    prtclffr == "9" ~ "The \nRepublicans",
    prtclffr == "10" ~ "Debout la France",
    prtclffr == "11" ~ "National \nFront",
    prtclffr == "12" ~ "Other",
    TRUE ~ as.character(prtclffr)),
  # UK
  prtclcgb = case_when(
    prtclcgb == "1" ~ "Conservative",
    prtclcgb == "2" ~ "Labour",
    prtclcgb == "3" ~ "Liberal \nDemocrat",
    prtclcgb == "4" ~ "Scottish National Party",
    prtclcgb == "5" ~ "Plaid Cymru",
    prtclcgb == "6" ~ "Green Party",
    prtclcgb == "7" ~ "UK Independence Party",
    prtclcgb == "9" ~ "	Ulster Unionist Party (nir)",
    prtclcgb == "10" ~ "Democratic Unionist Party (nir)",
    prtclcgb == "11" ~ "Sinn Féin (nir)",
    prtclcgb == "12" ~ "Social Democratic and Labour Party (nir)",
    prtclcgb == "13" ~ "Alliance Party (nir)",
    prtclcgb == "14" ~ "Traditional Unionist (nir)",
    prtclcgb == "15" ~ "Green Party (nir)",
    prtclcgb == "16" ~ "Independent(s) (nir)",
    prtclcgb == "17" ~ "People Before Profit Alliance (nir)",
    prtclcgb == "18" ~ "Progressive Unionist Party",
    prtclcgb == "8" | prtclcgb == "19" ~ "Other",
    TRUE ~ as.character(prtclcgb)),
  # Ireland
  prtcleie = case_when(
    prtcleie == "1" ~ "Solidarity/PBP",
    prtcleie == "2" ~ "Fianna \nFáil",
    prtcleie == "3" ~ "Fine \nGael",
    prtcleie == "4" ~ "Green \nParty",
    prtcleie == "5" ~ "Independent",
    prtcleie == "6" ~ "Labour",
    prtcleie == "7" ~ "Sinn \nFéin",
    prtcleie == "8" ~ "Social Democrats",
    prtcleie == "9" ~ "Socialist Party",
    prtcleie == "10" ~ "RENUA Ireland",
    prtcleie == "11" ~ "Independent Alliance",
    prtcleie == "12" ~ "Workers' Party",
    prtcleie == "13" ~ "Other",
    TRUE ~ as.character(prtcleie)),
  # Italy
  prtcldit = case_when(
    prtcldit == "1" ~ "Democratic \nParty",
    prtcldit == "2" ~ "+ Europa",
    prtcldit == "6" ~ "Free and Equal",
    prtcldit == "7" ~ "Five-Star \nMovement",
    prtcldit == "8" ~ "Forza \nItalia",
    prtcldit == "9" ~ "Lega \nNord",
    prtcldit == "10" ~ "Brothers \nof Italy",
    prtcldit == "14" ~ "Other",
    TRUE ~ as.character(prtcldit)),
  # Latvia
  prtclalv = case_when(
    prtclalv == "1" ~ "Latvijas Krievu savienība",
    prtclalv == "2" ~ "New \nConservative \nParty",
    prtclalv == "3" ~ "Rīcības partija",
    prtclalv == "4" ~ "National \nAlliance",
    prtclalv == "5" ~ "Progresīvie",
    prtclalv == "6" ~ "Latvijas centriskā partija",
    prtclalv == "7" ~ "LSDSP/KDS/GKL",
    prtclalv == "8" ~ "No sirds Latvijai",
    prtclalv == "9" ~ "Social \nDemocratic \nParty",
    prtclalv == "10" ~ "Development/\nFor",
    prtclalv == "11" ~ "Latvijas Reģionu Apvienība",
    prtclalv == "12" ~ "Latviešu Nacionālisti",
    prtclalv == "13" ~ "New \nUnity",
    prtclalv == "14" ~ "Par alternatīvu",
    prtclalv == "15" ~ "Politiskā partija KPV LV",
    prtclalv == "16" ~ "Union of \nGreens and \nFarmers",
    prtclalv == "17" ~ "Other",
    TRUE ~ as.character(prtclalv)),
  # Netherlands
  prtclfnl = case_when(
    prtclfnl == "1" ~ "People's Party for Freedom and Democracy",
    prtclfnl == "2" ~ "Labour Party",
    prtclfnl == "3" ~ "Party for Freedom",
    prtclfnl == "4" ~ "Socialist Party",
    prtclfnl == "5" ~ "Christian Democratic Appeal",
    prtclfnl == "6" ~ "Democrats '66",
    prtclfnl == "7" ~ "Christian Union",
    prtclfnl == "8" ~ "Green Left",
    prtclfnl == "9" ~ "Reformed Political Party",
    prtclfnl == "10" ~ "Party for the Animals",
    prtclfnl == "11" ~ "50PLUS",
    prtclfnl == "12" ~ "DENK",
    prtclfnl == "13" ~ "Forum for Democracy",
    prtclfnl == "14" ~ "Article 1",
    prtclfnl == "16" | prtclfnl == "17" ~ "Other",
    TRUE ~ as.character(prtclfnl)),
  # Norway
  prtclbno = case_when(
    prtclbno == "1" ~ "Red Party",
    prtclbno == "2" ~ "Socialist Left Party",
    prtclbno == "3" ~ "Labour Party",
    prtclbno == "4" ~ "Liberal Party (Venstre)",
    prtclbno == "5" ~ "Christian Democratic Party",
    prtclbno == "6" ~ "Centre Party",
    prtclbno == "7" ~ "Conservative Party",
    prtclbno == "8" ~ "Progress Party",
    prtclbno == "9" ~ "Coastal Party",
    prtclbno == "10" ~ "Green Party",
    prtclbno == "11"  ~ "Other",
    TRUE ~ as.character(prtclbno)),
  # Portugal
  prtclept = case_when(
    prtclept == "1" ~ "PTP-MAS - Agir",
    prtclept == "2" ~ "Left Bloc",
    prtclept == "3" ~ "Unitary Democratic Coalition",
    prtclept == "4" ~ "Together for the People",
    prtclept == "5" ~ "LIVRE",
    prtclept == "6" ~ "We, the Citizens!",
    prtclept == "7" ~ "Citizenship and Christian Democracy",
    prtclept == "8" ~ "Portuguese Workers' Communist Party",
    prtclept == "9" ~ "Earth Party",
    prtclept == "10" ~ "Democratic Republican Party",
    prtclept == "11" ~ "Democratic Renovator Party",
    prtclept == "12" ~ "People's Monarchist Party",
    prtclept == "13" ~ "Socialist Party",
    prtclept == "14" ~ "United Party of Retirees and Pensioners",
    prtclept == "15" ~ "People Animals Nature",
    prtclept == "16" ~ "Social Democratic Party",
    prtclept == "17" ~ "People's Party",
    prtclept == "18" ~ "Other",
    TRUE ~ as.character(prtclept)),
  # Sweden
  prtclcse = case_when(
    prtclcse == "1" ~ "Centre Party",
    prtclcse == "2" ~ "Liberals",
    prtclcse == "3" ~ "Christian Democrats",
    prtclcse == "4" ~ "Green Party",
    prtclcse == "5" ~ "Moderate Party",
    prtclcse == "6" ~ "Social Democratic Party",
    prtclcse == "7" ~ "Left Party",
    prtclcse == "8" ~ "Feminist Initiative",
    prtclcse == "9" ~ "Sweden Democrats",
    prtclcse == "10" ~ "Other",
    TRUE ~ as.character(prtclcse)),
  # Slovenia
  prtclfsi = case_when(
    prtclfsi == "1" ~ "Democratic Party of Pensioners of Slovenia",
    prtclfsi == "2" ~ "The Left",
    prtclfsi == "3" ~ "List of Marjan Šarec",
    prtclfsi == "4" ~ "New Slovenia – Christian Democrats",
    prtclfsi == "5" ~ "Positive Slovenia",
    prtclfsi == "6" ~ "Social Democrats",
    prtclfsi == "7" ~ "Party of Alenka Bratušek",
    prtclfsi == "8" ~ "Slovenian Democratic Party",
    prtclfsi == "9" ~ "Slovenian People's Party",
    prtclfsi == "10" ~ "Modern Centre Party",
    prtclfsi == "11" ~ "Slovenian National Party",
    prtclfsi == "12" ~ "Other",
    TRUE ~ as.character(prtclfsi)))

# Create one voting variable across countries

ess9_data <- ess9_data %>%
  mutate(vote = case_when(
    cntry == "AT" ~ prtcldat,
    cntry == "BE" ~ prtcldbe,
    cntry == "CH" ~ prtclgch,
    cntry == "CY" ~ prtclbcy,
    cntry == "DE" ~ prtclede,
    cntry == "EE" ~ prtclgee,
    cntry == "ES" ~ prtclfes,
    cntry == "FR" ~ prtclffr,
    cntry == "GB" ~ prtclcgb,
    cntry == "IE" ~ prtcleie,
    cntry == "IT" ~ prtcldit,
    cntry == "LV" ~ prtclalv,
    cntry == "NL" ~ prtclfnl,
    cntry == "NO" ~ prtclbno,
    cntry == "PT" ~ prtclept,
    cntry == "SE" ~ prtclcse,
    cntry == "SI" ~ prtclfsi)) %>%
  filter(vote != "66")

# Summarize and exclude parties with few votes

ess9_data_summ_all <- ess9_data %>% 
  group_by(cntry, vote) %>%
  summarise(party_frequency = n()) %>%
  group_by(cntry) %>%
  mutate(country_total = sum(party_frequency),
         party_prop = party_frequency / country_total)

# Function to extract above-5% parties per country

extract_parties <- function(country) {
   
  parties <- ess9_data_summ_all %>%
    filter(party_prop < 0.05 &
             vote != "Don't \nknow" &
             cntry == country) %>%
    pull(vote)
  return(parties)
  
}

ess9_data_small_AT <- extract_parties("AT")
ess9_data_small_BE <- extract_parties("BE")
ess9_data_small_CH <- extract_parties("CH")
ess9_data_small_CY <- extract_parties("CY")
ess9_data_small_DE <- extract_parties("DE")
ess9_data_small_DK <- extract_parties("DK")
ess9_data_small_EE <- extract_parties("EE")
ess9_data_small_ES <- extract_parties("ES")
ess9_data_small_FR <- extract_parties("FR")
ess9_data_small_GB <- extract_parties("GB")
ess9_data_small_IE <- extract_parties("IE")
ess9_data_small_IT <- extract_parties("IT")
ess9_data_small_LV <- extract_parties("LV")
ess9_data_small_NL <- extract_parties("NL")
ess9_data_small_NO <- extract_parties("NO")
ess9_data_small_PT <- extract_parties("PT")
ess9_data_small_SE <- extract_parties("SE")
ess9_data_small_SI <- extract_parties("SI")

ess9_data <- ess9_data %>%
  mutate(vote = case_when(
    cntry == "AT" & vote %in% ess9_data_small_AT ~ "Other",
    cntry == "BE" & vote %in% ess9_data_small_BE ~ "Other",
    cntry == "CH" & vote %in% ess9_data_small_CH ~ "Other",
    cntry == "CY" & vote %in% ess9_data_small_CY ~ "Other",
    cntry == "DE" & vote %in% ess9_data_small_DE ~ "Other",
    cntry == "EE" & vote %in% ess9_data_small_EE ~ "Other",
    cntry == "ES" & vote %in% ess9_data_small_ES ~ "Other",
    cntry == "FR" & vote %in% ess9_data_small_FR ~ "Other",
    cntry == "GB" & vote %in% ess9_data_small_GB ~ "Other",
    cntry == "IE" & vote %in% ess9_data_small_IE ~ "Other",
    cntry == "IT" & vote %in% ess9_data_small_IT ~ "Other",
    cntry == "LV" & vote %in% ess9_data_small_LV ~ "Other",
    cntry == "NL" & vote %in% ess9_data_small_NL ~ "Other",
    cntry == "NO" & vote %in% ess9_data_small_NO ~ "Other",
    cntry == "PT" & vote %in% ess9_data_small_PT ~ "Other",
    cntry == "SE" & vote %in% ess9_data_small_SE ~ "Other",
    cntry == "SI" & vote %in% ess9_data_small_SI ~ "Other",
    TRUE ~ as.character(vote)
  ))

# Summarize again

ess9_data_summ_big <- ess9_data %>%
  group_by(cntry, vote, ctzcntr) %>%
  summarise(party_frequency = n()) %>%
  group_by(cntry, ctzcntr) %>%
  mutate(
    citizenship_country_total = sum(party_frequency),
    party_prop = party_frequency / citizenship_country_total,
    party_percent = party_prop * 100
  )

# Plot per country

## Function
country_plot <- function(country,
                         values_ordered,
                         colours,
                         yhigh, 
                         title) {
  
  data_filtered <- ess9_data_summ_big %>%
    filter(cntry == country) %>%
    mutate(vote = factor(vote, 
                         ordered = TRUE,
                         levels = values_ordered))

  plot <- ggplot(
    data_filtered,
    aes(x = vote, y = party_percent)
  ) +
    geom_bar(aes(
      fill = vote,
      alpha = as.factor(ctzcntr)
    ), stat = "identity", position = position_dodge()) +
    theme_minimal() +
    scale_y_continuous(
      name = "% per respondent group",
      limits = c(0, yhigh)
    ) +
    scale_fill_manual(values = colours) +
    scale_alpha_discrete(
      name = "Respondents",
      labels = c("Citizen", "Non-citizen"),
      range = c(0.4, 0.9)
    ) +
    ggtitle(title) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 7,
                                 vjust = 0.5),
      axis.text.y = element_text(
        size = 8,
        hjust = 2
      ),
      panel.grid.minor = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.caption = element_text(size = 9),
      legend.position = c(0.15, 0.78),
      legend.background = element_rect(fill = "white"),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12),
      title = element_text(size = 12)
    ) +
    guides(fill = FALSE)

  return(plot)
}

## Plots

plot_AT <- country_plot(country = "AT", yhigh = 50, title = "Austria",
                        values_ordered = c("Don't \nknow", "Other", 
                                           "The \nGreens", "Social \nDemocratic \nParty", 
                                           "People's \nParty", "Freedom \nParty"),
                        colours = c("cornsilk2", "bisque2", "forestgreen","red", "black","blue"))
plot_BE <- country_plot(country = "BE", yhigh = 50,title = "Belgium",
                        values_ordered = c("Don't \nknow", "Other", 
                                           "Green", "Confederated \nEcologists",
                                           "Socialist \nParty", "Flemish \nSocialist \nParty",
                                           "Christian \nDemocratic/\nFlemish", "Open \nFlemish \nLiberals \nand \nDemocrats",
                                           "Reformist \nMovement", "New \nFlemish \nAlliance"),
                        colours = c("cornsilk2", "bisque2", "forestgreen", "chartreuse4", 
                                    "red", "brown3", "black", "gold", "darkgoldenrod3", "blue"))
plot_CH <- country_plot(country = "CH", yhigh = 50, title = "Switzerland",
                        values_ordered = c("Don't \nknow", "Other", 
                                           "Green \nLiberal \nParty", "Green \nParty",
                                           "Social \nDemocratic \nParty", "Christian \nDemocratic \nParty",
                                           "The \nLiberals", "Swiss \nPeople's \nParty"),
                        colours = c("cornsilk2", "bisque2", "chartreuse4", "forestgreen",  
                                    "red", "black", "gold", "blue"))
plot_DE <- country_plot(country = "DE", yhigh = 50,title = "Germany",
                        values_ordered = c("Don't \nknow", "Other", "The \nLeft", "The \nGreens", 
                                           "Social \nDemocratic \nParty", "CDU/\nCSU",
                                           "Free \nDemocratic \nParty", "Alternative \nfor Germany"),
                        colours = c("cornsilk2", "bisque2", "deeppink4", "forestgreen","red", "black","gold","blue"))
plot_EE <- country_plot(country = "EE", yhigh = 100,title = "Estonia",
                        values_ordered = c("Don't \nknow", "Other", 
                                           "Estonian \nCentre \nParty",
                                           "Social \nDemocratic \nParty", 
                                           "Conservative \nPeople's \nParty",
                                           "Estonian \nReform \nParty","Isamaa"),
                        colours = c("cornsilk2", "bisque2", "deeppink4",
                                    "red", "black", "gold", "blue"))
plot_ES <- country_plot(country = "ES", yhigh = 60,title = "Spain",
                        values_ordered = c("Don't \nknow", "Other", 
                                           "Podemos",
                                           "Socialist \nWorkers' \nParty",
                                           "People's \nParty",
                                           "Ciudadanos", "VOX"),
                        colours = c("cornsilk2", "bisque2", "deeppink4",
                                    "red", "black", "gold", "blue"))
plot_FR <- country_plot(country = "FR", yhigh = 50,title = "France",
                        values_ordered = c("Don't \nknow", "Other", 
                                           "La \nFrance \nInsoumise", 
                                           "Europe \nEcology \nThe Greens",
                                           "Socialist \nParty", 
                                           "La \nRépublique \nen Marche",
                                           "The \nRepublicans",
                                           "National \nFront"),
                        colours = c("cornsilk2", "bisque2", "deeppink4",
                                    "forestgreen", "red", "gold", "black","blue"))
plot_GB <- country_plot(country = "GB", yhigh = 60,title = "United Kingdom",
                        values_ordered = c("Don't \nknow", "Other", 
                                           "Labour", "Liberal \nDemocrat", 
                                           "Conservative"),
                        colours = c("cornsilk2", "bisque2", "red", "gold","blue"))
plot_IE <- country_plot(country = "IE", yhigh = 50,title = "Ireland",
                        values_ordered = c("Don't \nknow", "Other", 
                                           "Sinn \nFéin", "Labour", "Fine \nGael", 
                                           "Fianna \nFáil"),
                        colours = c("cornsilk2", "bisque2", "deeppink4", "red", 
                                    "gold","black"))
plot_IT <- country_plot(country = "IT", yhigh = 50,title = "Italy",
                        values_ordered = c("Don't \nknow", "Other",
                                           "Five-Star \nMovement", 
                                           "Democratic \nParty", "Forza \nItalia", 
                                           "Lega \nNord"),
                        colours = c("cornsilk2", "bisque2", "deeppink4", "red", 
                                    "blue","black"))
plot_LV <- country_plot(country = "LV", yhigh = 100,title = "Latvia",
                        values_ordered = c("Don't \nknow", "Other",
                                           "Social \nDemocratic \nParty",
                                           "Development/\nFor",
                                           "New \nConservative \nParty", 
                                           "New \nUnity",
                                           "Union of \nGreens and \nFarmers",
                                           "National \nAlliance"),
                        colours = c("cornsilk2", "bisque2", "red", "gold",
                                    "black", "darkorchid2",
                                    "darkolivegreen4",  "blue"))

title <- as_ggplot(text_grob("Party closeness by citizenship status (ESS 2018)",size = 24))
legend <- as_ggplot(get_legend(plot_LV)) + theme(plot.margin = margin(2.5, 0, 0, 5, unit = "cm"))

ggarrange(NULL, title, NULL,
          plot_AT, plot_BE, plot_CH, plot_DE, 
          plot_EE, plot_ES, plot_FR, plot_GB,
          plot_IE, plot_IT, plot_LV, legend,
          nrow = 5, ncol = 3,
          common.legend = TRUE,
          legend = "none", heights = c(1, 5, 5, 5, 5))






