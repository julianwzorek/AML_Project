library("openxlsx")
library("dplyr")
library("lubridate")
library("ggplot2")
library("skimr")
library("kableExtra")
setwd("C:/Users/Julian/Documents/dane_seb/")
df_raw <- read.xlsx("alerts.xlsx")
df_meta <- read.xlsx("additional_info.xlsx")

#TASK 1
#getting rid of unnecessary columns
df_raw <- df_raw[,colnames(df_raw) %in% df_meta$FIELD]

#basic summary tables
df_basic_info <- matrix(0, nrow = 5, ncol = 2)
df_basic_info <- as.data.frame(df_basic_info)
colnames(df_basic_info) <- c("Type of information","Value")
df_basic_info_names <- c("Name","Number of rows","Number of columns", "Number of columns with character type","Number of columns with numeric type")
df_basic_info$`Type of information`<- df_basic_info_names
basic_info <- c("df_raw",nrow(df_raw),ncol(df_raw),12,1)
df_basic_info$Value <- basic_info
df_variables_info <- skim(df_raw)
df_character_info <- df_variables_info %>% filter(skim_type=="character") %>% select(1,2,3,4)
colnames(df_character_info) <- c("Type of column","Variable name", "No. of missing values","Proportion of non-missing values")
df_numeric_info <- df_variables_info %>% filter(skim_type=="numeric") %>% select(1,2,3,4)
colnames(df_numeric_info) <- c("Type of column","Variable name", "No. of missing values","Proportion of non-missing values")

#analysis of the "NULL" values
null_summary <- df_raw %>%
  summarise_all(~ sum(. == "NULL" & !is.na(.)))

#confirmation that all clients classifed as "pb" do not have other values than "NA"
unique(df_raw[df_raw$Type == "pb", "IndustryCode", drop = TRUE])

#the data should be modified according to the analysis of "NULL" values


df <- df_raw %>%
  mutate(CusRiskCategory = ifelse(CusRiskCategory %in% c("NULL"), NA, CusRiskCategory))

df <- df %>%
  mutate(PEP = ifelse((Type == "pb" & PEP == "NULL"), NA,PEP))

df <- df %>%
  mutate(DateClosed = ifelse((DateClosed == "NULL" & (AlertState == "Closed - Not Investigated" | AlertState =="Closed - Not Investigated Data Quality" | AlertState =="Closed - Not Suspicious"| AlertState =="Closed - Processed externally")),NA,DateClosed))

df <- df %>%
  mutate(CaseOpen = ifelse((CaseOpen == "NULL" & ((CaseState == "Closed" | CaseState == "Report Confirmed") | (CaseClosed != "NULL"))), NA,CaseOpen))

df <- df %>%
  mutate(CaseClosed = ifelse(CaseClosed == "NULL" & CaseState == "Closed", NA,CaseClosed))

df <- df %>%
  mutate(CaseState = ifelse(CaseState == "NULL" & (CaseClosed != "NULL" | CaseState != "NULL"),NA, CaseState))

#analysis of the missing values
df_na_summary<- df %>%
  summarise_all(~ sum(is.na(.)))
#Regarding the missing values, through the analysis of "NULL" values, 4 columns with NA values were identified.
#In the original df_raw, only two columns were indentified as columns with missings values. Therefore the additional analysis of these two columns ("PEP" and "IndustryCode") need to be conducted.
#With respect to the column "PEP", missing values for clients classifed as "lcfi", should be treated as rather "Not Applicable".
#With respect to the column "IndustryCode", missing values for the clients classifed as "pb" should not be treated as missing. In case of clients "lcfi", since there is already a value "NULL", missing values should not be removed.
#Additionaly, there are missing information about customer risk category since there are values "Not Specified" which should be treated as missing values
df <- df %>%
  mutate(PEP = ifelse(is.na(PEP) & Type == "lcfi", "Not Applicable",PEP))

df <- df %>%
  mutate(IndustryCode = ifelse(is.na(IndustryCode) & Type == "pb", "Not Applicable",IndustryCode))
df <- df %>%
  mutate(CusRiskCategory = ifelse(CusRiskCategory=="Not Specified", NA ,CusRiskCategory))

#After the transformations applied to the dataframe, there are following missing values detected:
df_na_summary_v2<- df %>%
  summarise_all(~ sum(is.na(.)))

#There exits 6 missing values for the column "DateClosed", 938 for column "PEP" and 54 for "CusRiskCategory"
#In case of column "DateClosed", 6 missing values are assigned since the column "AlertType" suggests in this cases that alert should be closed
#In case of column "PEP", there exist 938 not assigned categories to private banking clients
#In case of column "CusRiskCategory", there exit 682 not assigned categories to customers

#In case of column "DateClosed" data imputation is rather not applicable
#In case of column "PEP" some data imputation might be possible since for instance all clients not classifed as PEP that belongs to private banking sector are also classifed to MediumRisk category and with AlertType to "Awakening account"  but it would be rather advisable to check each client's political connections and to fill in missing values
test_pep_imput_1 <- df %>%
  filter(Type == "pb") %>%
  group_by(CusRiskCategory) %>%
  summarise(
    Y_count = sum(PEP == "Y", na.rm = TRUE),
    N_count = sum(PEP == "N", na.rm = TRUE),
    NA_count = sum(is.na(PEP))
  )

test_pep_imput_2 <- df %>%
  filter(Type == "pb") %>%
  group_by(AlertType) %>%
  summarise(
    Y_count = sum(PEP == "Y", na.rm = TRUE),
    N_count = sum(PEP == "N", na.rm = TRUE),
    NA_count = sum(is.na(PEP))
  )
test_pep_imput_3 <- df %>%
  filter(Type == "pb") %>%
  group_by(AlertType,CusRiskCategory) %>%
  summarise(
    Y_count = sum(PEP == "Y", na.rm = TRUE),
    N_count = sum(PEP == "N", na.rm = TRUE),
    NA_count = sum(is.na(PEP))
  )
#In case of column "CusRiskCategory", some data imputation might be possible if there were missing values for customers that belong to private banking sector and are indentified as PEP since all such cases are classified to Higher Risk but it would rather be advisable to fill in missing CusRiskCategory
test_cusriskcategory_imput_1 <- df %>%
  filter(Type == "pb") %>%
  group_by(PEP) %>%
  summarise(
    lower_count = sum(CusRiskCategory == "Lower Risk", na.rm = TRUE),
    medium_count = sum(CusRiskCategory == "Medium Risk", na.rm = TRUE),
    higher_count = sum(CusRiskCategory == "Higher Risk", na.rm = TRUE),
    NA_count = sum(is.na(CusRiskCategory))
  )

test_cusriskcategory_imput_2 <- df %>%
  filter(Type == "pb") %>%
  group_by(AlertType) %>%
  summarise(
    lower_count = sum(CusRiskCategory == "Lower Risk", na.rm = TRUE),
    medium_count = sum(CusRiskCategory == "Medium Risk", na.rm = TRUE),
    higher_count = sum(CusRiskCategory == "Higher Risk", na.rm = TRUE),
    NA_count = sum(is.na(CusRiskCategory))
  )

test_cusriskcategory_imput_3 <- df %>%
  filter(Type == "pb") %>%
  group_by(AlertType,PEP) %>%
  summarise(
    lower_count = sum(CusRiskCategory == "Lower Risk", na.rm = TRUE),
    medium_count = sum(CusRiskCategory == "Medium Risk", na.rm = TRUE),
    higher_count = sum(CusRiskCategory == "Higher Risk", na.rm = TRUE),
    NA_count = sum(is.na(CusRiskCategory))
  )

#Summarizing the possibility of data imputation in columns "CusRiskCategory", "DataClosed" and "PEP", no data imputation processes were conducted since it is more advisable to fill in this data 

#After the final analysis of the "NULL" values and NA values, there exits missing values for columns "DataClosed" - 6 missing values, "PEP" - 938 missing values, "CusRiskCategory" - 682 missing values
#Since the data imputation in all of these cases is rather not recommended due to the context of this missing values, these missing values will be removed from the analysis
#After the cleaning, the dataframe consists of 8361 out of 10177 original rows
df_na_summary_v3<- df %>%
  summarise_all(~ sum(is.na(.)))
df_2 <- na.omit(df)


#From the contex of the data, it would be also advisable to order it chronogically

df_2<-  dplyr::arrange(df_2, as.POSIXct(DateCreated))

#TASK 2
 #analysis of AlertType
#The most frequent types of alerts are "Unusual behaviour", "Credit Cards" and "Check Countries List"
ggplot(df, aes(x = AlertType)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  labs(title = "Frequency of Alert Types", x = "Alert Type", y = "Frequency")

#anlysis of CaseState
#Excluding "NULL" values from the analysis, when the investigation goes to the second line, the most frequent action is closing the case 
df_filtered_casestate <- df_2 %>%
  filter(CaseState != "NULL")
ggplot(df_filtered_casestate, aes(x = CaseState)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  labs(title = "Frequency of Alert Types", x = "Alert Type", y = "Frequency")
#customer risk category with respect to PEP
#The analysis to assess whether clients classifed to private banking and as PEP are more likely to be in higher risk category

df_filtered_pb <- df_2 %>%
  filter(PEP == "Y" | PEP =="N")
fill_colors <- c("Lower Risk" = "green", "Medium Risk" = "yellow", "Higher Risk" = "red")

# Create a ggplot with facets for PEP status
#All of the clients classifed to private banking and as PEP are at higher risk category while for non-PEP clients the most frequent risk category is medium risk
ggplot(df_filtered_pb, aes(x = CusRiskCategory, fill = CusRiskCategory)) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  labs(title = "Risk Category Assigned to Clients from Private Banking",
       x = "Risk Category", y = "Count",
       fill = "Risk Category") +
  scale_fill_manual(values = fill_colors) +
  facet_wrap(~PEP, scales = "free_y", labeller = as_labeller(c("N" = "Non-PEP", "Y" = "PEP"))) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
#customer risk category for lcfi clients
#For the customers classifed as lcfi, the most frequent category is medium risk, while the second one is higher risk.
df_filtered_lcfi <- df_2 %>%
  filter(Type=="lcfi")
ggplot(df_filtered_lcfi, aes(x = CusRiskCategory, fill = CusRiskCategory)) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  labs(title = "Risk Category Distribution for LCFI clients",
       x = "Risk Category", y = "Count",
       fill = "Risk Category") +
  scale_fill_manual(values = fill_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


#TASK 3   
#In general the most important area in TM models to analyse it to predict whether some transactions will be turn out to be involved with some illegal activities
#Based on the current data it is possible to analyse whether transactions of clients with higher risk are more frequently reported to assess whether customer risk category is appropriately assigned
#Based on the analysis, the risk category for clients is assigned appropriately since the percentage of confirmed reports to sum of confirmed reports and closed cases is highest for the customers with the higher risk category
df_filtered_risk_casestate <- df_2 %>%
  filter(CaseState == "Report Confirmed" | CaseState == "Closed")
summary_df_risk_casestate <- df_filtered_risk_casestate %>%
  group_by(CusRiskCategory, CaseState) %>%
  summarise(count = n())
reported_to_closed <- matrix(0, nrow = 1, ncol = 3)
reported_to_closed<- as.data.frame(reported_to_closed)
colnames(reported_to_closed) <- c("High Risk","Medium Risk","Lower Risk")
reported_to_closed[1,1] <- summary_df_risk_casestate$count[2]/(summary_df_risk_casestate$count[1]+summary_df_risk_casestate$count[2])
reported_to_closed[1,2] <- summary_df_risk_casestate$count[4]/(summary_df_risk_casestate$count[3]+summary_df_risk_casestate$count[4])
reported_to_closed[1,3] <- summary_df_risk_casestate$count[6]/(summary_df_risk_casestate$count[5]+summary_df_risk_casestate$count[6])

#Additionally it is also possible to analyse whether PEP clients are more likely to be reported with respect to non-PEP clients
#For the clients classified as PEP there are only 3 cases where state of case is either closed or raport was issued, therefore it is not possible to draw any conclusions
df_filtered_risk_pep <- df_2 %>%
  filter(Type == "pb" & (CaseState == "Report Confirmed" | CaseState == "Closed"))
summary_df_risk_pep <- df_filtered_risk_pep %>%
  group_by(PEP, CaseState) %>%
  summarise(count = n())


#Since the data can be ordered chronologically, it is also possible to detect whether there is increase in alerts or reported cases based on the year, month or day of the week which could also help to detect some patterns in transactions

