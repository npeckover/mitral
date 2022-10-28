# load packages
library(tidyverse)

# import data
totalDF <- read_csv("Project Data/mitral_regurg_touse.csv")

# replace values in peak troponin
totalDF$PeakTroponin <- gsub(">10000", "10000",
                      gsub("<5", "5", totalDF$PeakTroponin))

# replace dashes in certain columns
replace_cols <- c(32:37)
dash <- "-"
totalDF[replace_cols] <- suppressWarnings(
  sapply(totalDF[replace_cols], function(x) replace(x, x %in% dash, "No")))

# replace NAs in OOHCA
totalDF$OOHCA <- replace_na(totalDF$OOHCA, 0)

# replace NAs in Severity
totalDF$Severity <- replace_na(totalDF$Severity, "NONE")

# remove protected/unprotected distinction for LMS intervention
totalDF$LMSIntervention <- gsub("yes protected", "Yes", ignore.case = T,
                                   gsub("yes unprotected", "Yes", ignore.case = T, totalDF$LMSIntervention))

# change 'n/a' and '-' to NA in BNP column
totalDF$NTproBNP <- gsub("n/a", NA,
                                gsub("-", NA, totalDF$NTproBNP))

# make sure all character entries are in uppercase
charcols <- totalDF %>% # new vector of character vector column names
  select(where(is.character)) %>%
  colnames()
totalDF[charcols] <- sapply(totalDF[charcols], function(x) toupper(x))

# remove "UNKNOWN" and replace with NA
totalDF[,c(6:15)] <- sapply(totalDF[,c(6:15)], function(x) replace(x, x %in% "UNKNOWN", NA))

# fix 'pre-existing' so 'no previous echo' becomes NA and other values become 'yes' or 'no'
totalDF$Preexisting <- gsub("YES|MILD|TRIVIAL|MODERATE|SEVERE", "YES",
                               gsub("NO|NONE", "NO",
                                    gsub("NO PREVIOUS ECHO|NO PRREVIOUS ECHO", NA, 
                                         gsub("N/A", NA, totalDF$Preexisting))))

# replace weird excel artifacts with NA values
totalDF$"30TimeFromDischargeToDeath(days)" <- gsub(
  "#NUM!|#VALUE!", NA, totalDF$"30TimeFromDischargeToDeath(days)")
totalDF$"30TimeFromSymptomOnsetToDeath(days)" <- gsub(
  "#NUM!|#VALUE!", NA, totalDF$"30TimeFromSymptomOnsetToDeath(days)")
totalDF$"12TimeFromSymptomOnsetToDeath(days)" <- gsub(
  "#NUM!|#VALUE!", NA, totalDF$"12TimeFromSymptomOnsetToDeath(days)")
totalDF$"12TimeFromSymptomOnsetToDeath(days)" <- gsub(
  "#NUM!|#VALUE!", NA, totalDF$"12TimeFromSymptomOnsetToDeath(days)")

# make appropriate columns as factors
factor <- c(3,6:11,13:16,18,19,22,27:48,50:63,69:75,81,91,114)
totalDF[factor] <- lapply(totalDF[factor], factor)
# make appropriate columns as numeric
num <- c(5,12,20,21,23,25,26,43,49,64,66:69,78,79,86:89,104,105,116,117)
totalDF[num] <- suppressWarnings(lapply(totalDF[num], as.numeric))
# make appropriate columns as date
date <- c(4,17,24,65,92,98,103,115)
totalDF <- totalDF %>% mutate_at(vars(date), as.Date, format = "%m/%d/%Y")
# drop unnecessary columns
DF <- totalDF %>% select(-c(1,2,4,50,38:42,44:48,51,55:59,70:73,76,77,80:85,90,91,93:101,106:113,118:122))

# round the values of numeric columns to two decimal places
DF <- DF %>% mutate_if(is.numeric, ~round(., 2))
# change values of 0 to 'NO' and 1 to 'YES' in select columns
zero_one_cols <- c(19,37:39,41:43,59,63)
# create a function to change values and save the result as a factor var
zero_one_change <- function(x) {
  temp <- gsub("0", "NO",
               gsub("1", "YES", x))
  as.factor(temp)
}
# apply the function across the necessary cols
DF[zero_one_cols] <- lapply(DF[zero_one_cols], zero_one_change)

DF <- DF %>%
  mutate(Severity = fct_relevel(Severity, "NONE", "MILD", "MODERATE", "SEVERE"),
         LMS = fct_relevel(LMS, "0", "1-49", "50-74", "75-94", "95-99", "100"),
         LADProx = fct_relevel(LADProx, "0", "1-49", "50-74", "75-94", "95-99", "100"),
         LADOther = fct_relevel(LADOther, "0", "1-49", "50-74", "75-94", "95-99", "100"),
         RCA = fct_relevel(RCA, "0", "1-49", "50-74", "75-94", "95-99", "100"),
         LCx = fct_relevel(LCx, "0", "1-49", "50-74", "75-94", "95-99", "100"))



### create new columns that combine factors depending on application
DF <- DF %>%
  mutate(Severity2 = fct_collapse(Severity, 
                                  NONE = "NONE",
                                  MILD = "MILD",
                                  "MODs" = c("MODERATE", "SEVERE")),
         Severity3 = fct_collapse(Severity, 
                                  "NONEm" = c("NONE", "MILD"),
                                  "MODs" = c("MODERATE", "SEVERE")))

# multiple blockages
DF_2 <- DF %>%
  select(LMS, LADProx, LADOther, RCA, LCx) %>%
  mutate(across(everything(), ~ as.numeric(fct_collapse(.,
                                                        "0" = c("0", "1-49", "50-74"),
                                                        "1" = c("75-94", "95-99", "100")))))
DF_2$Sums <- rowSums(DF_2)
DF$MultiBlock <- as.factor(ifelse(DF_2$Sums >= 7, 1, 0)) # new indicator

# multiple interventions
DF_3 <- DF %>%
  select(LMSIntervention, LADProxIntervention, LADOtherIntervention, RCAIntervention, LCxIntervention) %>%
  mutate(across(everything(), ~ as.numeric(fct_recode(.,
                                                      "0" = "NO",
                                                      "1" = "YES"))))
DF_3$Sums <- rowSums(DF_3)
DF$MultiIntervene <- as.factor(ifelse(DF_3$Sums >= 7, 1, 0)) # new indicator

# make sure that column names are valid
colnames(DF) <- make.names(colnames(DF))