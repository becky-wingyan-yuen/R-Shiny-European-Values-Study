



rm(list=ls())

library(shiny)
runApp("./EVS_shiny")

library(rsconnect)
deployApp("./EVS_shiny")






rm(list=ls())

library(haven)

rawdata = read_sav("./EVS_shiny/data/ZA7500_v5-0-0.sav")

######## variables kept ######## 
## 
#### Independent variables ####
## 
## age - age:respondent (constructed) (Q64)
## v243_r - educational level respondent: recoded (Q81)
## v225 - sex respondent (Q63)
## country - country code (ISO 3166-1 numeric code)
## 
#### Dependent variables ####
## 
## v72 - child suffers with working mother (Q25A)
##       (1-strongly agree, 2-agree, 3-disagree, or 4-strongly disagree)
## v80 - jobs are scarce: giving...(nation) priority (Q26A)
##       (1-strongly agree, 2-agree, 3-neither agree nor disagree, 4-disagree, or 5-strongly disagree)
## 
################################

EVS = rawdata[,which(colnames(rawdata)%in%c("age","v225","v243_r","country","v72","v80"))]

#### Rename variables ####

names(EVS)[which(names(EVS)=="v225")] = "sex"
names(EVS)[which(names(EVS)=="v243_r")] = "education"

#### clean data before analysis ####

EVS = EVS[-which(EVS$education==66|is.na(EVS$education)),]  # discard "other" category and missing data rows in education
EVS = EVS[-which(is.na(EVS$age)|is.na(EVS$sex)|is.na(EVS$country)|is.na(EVS$v72)|is.na(EVS$v80)),] # discord rows with missing data

write_sav(EVS,"./EVS_shiny/data/EVS_data_cleaned.sav")





















rm(list=ls())

library(haven)

rawdata = read_sav("./EVS_shiny/data/ZA7500_v5-0-0.sav")

######## variables kept ######## 
## 
#### Independent variables ####
## 
## age - age:respondent (constructed) (Q64)
## v243_r - educational level respondent: recoded (Q81)
## v225 - sex respondent (Q63)
## country - country code (ISO 3166-1 numeric code)
## 
#### Dependent variables ####
## 
## v72 - child suffers with working mother (Q25A)
##       (1-strongly agree, 2-agree, 3-disagree, or 4-strongly disagree)
## v80 - jobs are scarce: giving...(nation) priority (Q26A)
##       (1-strongly agree, 2-agree, 3-neither agree nor disagree, 4-disagree, or 5-strongly disagree)
## 
################################

EVS = rawdata[,which(colnames(rawdata)%in%c("age","v225","v243_r","country","v72","v80"))]

#### Rename variables ####

names(EVS)[which(names(EVS)=="v225")] = "sex"
names(EVS)[which(names(EVS)=="v243_r")] = "education"

#### clean data before analysis ####

EVS = EVS[-which(EVS$education==66|is.na(EVS$education)),]  # discard "other" category and missing data rows in education
EVS = EVS[-which(is.na(EVS$age)|is.na(EVS$sex)|is.na(EVS$country)|is.na(EVS$v72)|is.na(EVS$v80)),] # discord rows with missing data

#write_sav(EVS,"./data/EVS_data_cleaned.sav")

#### Code for generating dynamic reports ####

library(labelled)
country_list = val_labels(EVS$country)
null_country = c()

for( i in 1:length(country_list) ){
  if( dim(EVS[which(EVS$country==country_list[i]),])[1]==0 ){
    null_country = c(null_country,names(country_list)[i])
    print(paste("There is no data for ", names(country_list)[i]))
    next
  }else{
    if( grepl(" ", names(country_list)[i]) == FALSE ){
      rmarkdown::render("./reports/batch-report.Rmd", 
                        output_file = paste("Report-for-country-", names(country_list)[i], sep=""),
                        params = list(country_num = country_list[i], country_char = names(country_list)[i]),
                        quiet = TRUE)
    }else{
      rmarkdown::render("./reports/batch-report.Rmd", 
                        output_file = paste("Report-for-country-", paste(unlist(strsplit(names(country_list)[i], " ")), collapse="-"), sep=""),
                        params = list(country_num = country_list[i], country_char = names(country_list)[i]),
                        quiet = TRUE)
    }
  }
}

if(is.null(null_country)==FALSE){
  country_list = country_list[-which(names(country_list)%in%null_country)]
}


#### Code for generating markdown dropdown list for accessing countries' reports ####

dropdown_list = paste("<details>","\n<summary>Country List</summary> \n \n",sep="")
for( i in 1:length(country_list) ){
  if( grepl(" ", names(country_list)[i]) == FALSE ){
    dropdown_list = paste(dropdown_list,"* [",names(country_list)[i],"](./reports/Report-for-country-",names(country_list)[i],".md) \n",sep="")
  }else{
    dropdown_list = paste(dropdown_list,"* [",names(country_list)[i],"](./reports/Report-for-country-",paste(unlist(strsplit(names(country_list)[i], " ")), collapse="-"),".md) \n",sep="")
  }
}
dropdown_list = paste(dropdown_list,"\n</details>",sep="")
cat(dropdown_list)














