library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

setwd("C:/Users/dplachouras/OneDrive - European Centre for Disease Prevention and Control/Documents/HAINETICU_2023")

#function definitions --------
prc<-function(x){return(100*round(x,digits=3))}
pct<-function(x,tot){prc(sum(x,na.rm=TRUE)/tot)}
unfactor<-function(x){as.numeric(as.character(x))}
p25<-function(x){quantile(x,c(0.25),na.rm=TRUE)}
p75<-function(x){quantile(x,c(0.75),na.rm=TRUE)}


parse_mixed_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  
  # Try ISO first, then dd/mm/yyyy
  d1 <- as.Date(x, format = "%Y-%m-%d")
  d2 <- as.Date(x, format = "%d/%m/%Y")
  
  coalesce(d1, d2)
}

#top 10 microorganism by country tables function -----
build_top10_country_tables <- function(df,
                                       isolate_col = "Isolate",
                                       total_col = "total",
                                       totalpc_col = "totalpc",
                                       country_name_lut = c(
                                         AT = "Austria", BE = "Belgium", CZ = "Czech Republic",
                                         DE = "Germany", EE = "Estonia", ES = "Spain",
                                         FR = "France", HU = "Hungary", IT = "Italy",
                                         LT = "Lithuania", LU = "Luxembourg", MT = "Malta",
                                         PL = "Poland", PT = "Portugal", RO = "Romania",
                                         SK = "Slovakia", UK = "United Kingdom",
                                         "IT-SPIN-UTI" = "Italy-SPIN-UTI",
                                         "IT-GiViTI" = "Italy-GiViTI",
                                         ITSPINUTI = "Italy-SPIN-UTI",
                                         ITGiViTI = "Italy-GiViTI"
                                       )) {

  # Numeric country count columns (exclude totals and percentage columns)
  country_cols <- df %>%
    dplyr::select(where(is.numeric), -dplyr::any_of(c(total_col, totalpc_col)), -dplyr::matches("pc$")) %>%
    names()

  # Percentage columns
  country_pc_cols <- df %>%
    dplyr::select(dplyr::matches("pc$"), -dplyr::any_of(totalpc_col)) %>%
    names()

  summary_tbl <- df %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(country_cols), ~ sum(.x, na.rm = TRUE))) %>%
    dplyr::mutate(total = rowSums(dplyr::across(dplyr::all_of(country_cols)), na.rm = TRUE))

  totals_tbl <- df %>%
    dplyr::select(dplyr::all_of(country_cols), dplyr::any_of(total_col)) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE), .names = "{.col}_sum"))

  pc_tbl <- df %>%
    dplyr::select(dplyr::all_of(isolate_col), dplyr::all_of(country_pc_cols), dplyr::any_of(totalpc_col)) %>%
    dplyr::mutate(dplyr::across(-dplyr::all_of(isolate_col), ~ tidyr::replace_na(.x, 0)))

  # Rename % columns to full country names
  pc_keys <- sub("pc$", "", country_pc_cols)
  display_names <- ifelse(
    is.na(country_name_lut[pc_keys]),
    pc_keys,
    unname(country_name_lut[pc_keys])
  )

  names(pc_tbl) <- c(
    isolate_col,
    display_names,
    if (totalpc_col %in% names(pc_tbl)) "total"
  )

  list(
    summary = summary_tbl,
    totals = totals_tbl,
    pc = pc_tbl,
    country_cols = country_cols,
    country_pc_cols = country_pc_cols
  )
}

#variable check
unit<-fread("1.HAIICU.csv")
deno <- fread("2.HAIICU$Denom.csv")
ind <- fread("3.HAIICU$Denom$Ind.csv")
inf <- fread("3.HAIICU$Pt$Inf.csv", data.table = FALSE)
pat<-fread("2.HAIICU$Pt.csv",data.table=FALSE)

denom<-left_join(unit,deno,by=c("RecordId"="ParentId"))
indic<-left_join(ind,deno,by=c("ParentId"="RecordId"))
indic<-left_join(indic,select(unit,RecordId,ReportingCountry),by=c("ParentId.y"="RecordId"))
mort<-left_join(pat,select(unit,RecordId,ReportingCountry),by=c("ParentId"="RecordId"))
mort<-left_join(select(inf,ParentId),mort,by=c("ParentId"="RecordId"))


unitl<-fread("1.HAIICULIGHT.csv")
denol <- fread("2.HAIICULIGHT$Deno.csv")
indl <- fread("3.HAIICULIGHT$Deno$Ind.csv")
infl <- fread("3.HAIICULIGHT$Deno$Inf.csv", data.table = FALSE)

denoml<-left_join(unitl,denol,by=c("RecordId"="ParentId"))
indicl<-left_join(indl,denol,by=c("ParentId"="RecordId"))
indicl<-left_join(indicl,select(unitl,RecordId,ReportingCountry),by=c("ParentId.y"="RecordId"))
unitinfl<-left_join(infl,denoml,by=c("ParentId"="RecordId.y"))


table(inf$ParentId%in%pat$RecordId)
table(ind$ParentId%in%deno$RecordId)
table(inf$BSIOrigin)
table(inf$InfectionSite)

#merge dataframes
#standard$UnitId<-standard$RecordId
#standard$RecordId<-NULL
#standard_pt<-merge(standard,pt,by.x="UnitId",by.y="ParentId",all=TRUE)
#standard_pt$PatientId<-standard_pt$RecordId
#standard_pt$RecordId<-NULL
#standard_pt_inf<-merge(standard_pt,inf,by.x="PatientId",by.y="Id",all=TRUE)
#standard_pt_inf$InfectionId<-standard_pt_inf$RecordId
#standard_pt_inf$RecordId<-NULL
#standard_pt_inf_micro<-merge(standard_pt_inf,micro,by.x="InfectionId",by.y="ParentId",all=TRUE)



CountryCodeLUT<-c("BE"="Belgium","CZ"="Czech Republic","EE"="Estonia","FR"="France","DE"="Germany","HU"="Hungary","IT"="Italy","LT"="Lithuania","LU"="Luxembourg","MT"="Malta", "PL"="Poland","PT"="Portugal", "RO"="Romania","SK"="Slovakia","ES"="Spain","UK"="United Kingdom")
#haiicu_level1 cleaning --------
haiicu_level1<-read.csv("1.HAIICU.csv")   #load 1.HAIICU.csv
levels(haiicu_level1$ReportingCountry)<-c(levels(haiicu_level1$ReportingCountry),"IT-GiViTI","IT-SPIN-UTI")
haiicu_level1$ReportingCountry[haiicu_level1$ReportingCountry=="IT"]<-haiicu_level1$DataSource[haiicu_level1$ReportingCountry=="IT"]#Replace IT with network name
haiicu_level1_light<-read.csv("1.HAIICULIGHT.csv") #load 1.HAIICULIGHT.csv
haiicu_level1$ReportingCountry<-as.character(haiicu_level1$ReportingCountry)
haiicu_level1_all<-rbind(haiicu_level1,haiicu_level1_light)
haiicu_level1_all$ReportingCountry<-as.factor(haiicu_level1_all$ReportingCountry)
haiicu_level1_all<-haiicu_level1_all%>%mutate(UnitSize=replace(UnitSize,UnitSize == "99"|UnitSize=="86", "UNK"))
attach(haiicu_level1_all)
haiicu_level1_all$UnitIdGlobal<-as.factor(paste(ReportingCountry,HospitalId,UnitId,collapse=NULL)) #unique identifier for ICU
haiicu_level1_all$HospitalIdGlobal<-as.factor(paste(ReportingCountry,HospitalId,collapse=NULL)) #unique identifier for Hospital
save (haiicu_level1_all, file="haiicu_level1_all.Rda")
#haiicu_level2 cleaning -- patient level data -------
haiicu_pt<-read.csv("2.HAIICU$PT.csv") #load 2.HAIICU$Pt

haiicu_pt<-haiicu_pt%>%mutate(across(c(DateUnitAdmission, DateUnitDischarge), parse_mixed_date))
#haiicu_pt$DateUnitAdmission<-as.Date(haiicu_pt$DateUnitAdmission,format="%Y-%m-%d")
#haiicu_pt$DateUnitDischarge<-as.Date(haiicu_pt$DateUnitDischarge,format="%Y-%m-%d")
haiicu_pt<-haiicu_pt%>%filter(DateUnitDischarge>DateUnitAdmission+1,!is.na(DateUnitDischarge))
haiicu_pt$los<-as.numeric(haiicu_pt$DateUnitDischarge-haiicu_pt$DateUnitAdmission+1)
haiicu_pt<-haiicu_pt%>%filter(los<366)
haiicu_pt$UnitId<-haiicu_pt$ParentId
haiicu_level1$UnitId<-haiicu_level1$RecordId
haiicu_pt_unit<-merge(haiicu_pt,haiicu_level1,by="UnitId")
#haiicu_pt_unit$isNotification<-NULL
save (haiicu_pt_unit, file="haiicu_pt_unit.Rda")

#haiicu__pt_inf cleaning -- infection data -----------
haiicu_pt_inf<-read.csv("3.HAIICU$PT$INF.csv") #load 3.HAIICU$Pt$INf
haiicu_pt_inf$Id<-haiicu_pt_inf$ParentId
haiicu_pt_unit$Id<-haiicu_pt_unit$RecordId.x
haiicu_pt_inf_all<-merge(haiicu_pt_unit, haiicu_pt_inf,by="Id", all=TRUE) #merged file of all patients incl infections data
haiicu_pt_inf_all<-filter(haiicu_pt_inf_all,!is.na(UnitId))
haiicu_pt_inf_all<-haiicu_pt_inf_all%>%mutate(InfectionId=RecordId)
haiicu_pt_inf_all<-select(haiicu_pt_inf_all,-(ParentId.x:RecordType.x),
                          -(RecordId.y:DataSource),-(UnitSurveillanceBSI:RecordType),-DateUsedForStatistics,
                          -HospitalComment)
haiicu_pt_inf_all<-rename(haiicu_pt_inf_all,RecordId=RecordId.x)
#spread(haiicu_pt_inf_all, Id, InfectionSite)
haiicu_pt_inf_all$hasHai<-!is.na(haiicu_pt_inf_all$InfectionSite)
haiicu_pt_inf_all$dupl_pat<-duplicated(haiicu_pt_inf_all$Id)

#calculate length of stay ---------
haiicu_pt_inf_all<-haiicu_pt_inf_all%>%mutate(across(c(DateUnitAdmission, DateUnitDischarge,DateOfOnset), parse_mixed_date)) 
#haiicu_pt_inf_all$DateUnitAdmission<-as.Date(haiicu_pt_inf_all$DateUnitAdmission)
#haiicu_pt_inf_all$DateUnitDischarge<-as.Date(haiicu_pt_inf_all$DateUnitDischarge)
haiicu_pt_inf_all$lengthofstay<-haiicu_pt_inf_all$DateUnitDischarge-haiicu_pt_inf_all$DateUnitAdmission+1
haiicu_pt_inf_all<-haiicu_pt_inf_all%>%filter(lengthofstay<366)
haiicu_pt_inf_all<-haiicu_pt_inf_all%>%unique()

#GiViTI correction for patient days after first episode of infection------
haiicu_pt_inf_all<-haiicu_pt_inf_all%>%mutate(losPN=case_when (ReportingCountry=="IT-GiViTI" & grepl("PN", InfectionSite) ~ (as.Date(DateOfOnset,format="%d/%m/%Y")-DateUnitAdmission+1),
                                                               TRUE ~ (DateUnitDischarge-DateUnitAdmission+1)))




#calculate incidence of pneumonia per 1000 patient days in standad protocol
#haiicu_pt_unit$Id<-haiicu_pt_unit$RecordId.y
pn_incid<-sum(grepl("PN", haiicu_pt_inf_all$InfectionSite))/(sum(as.numeric(haiicu_pt_inf_all$lengthofstay[!haiicu_pt_inf_all$dupl_pat]),na.rm=TRUE))*1000
bsi_incid<-sum(grepl("BSI|CRI3", haiicu_pt_inf_all$InfectionSite))/(sum(as.numeric(haiicu_pt_inf_all$lengthofstay[!haiicu_pt_inf_all$dupl_pat]),na.rm=TRUE))*1000
saveRDS(haiicu_pt_inf_all,"haiicu_pt_inf_all.Rda")

#aggregate by icu ---------
haiicu_unit_inc<-select(haiicu_pt_inf_all, Id,UnitId,InfectionSite,lengthofstay,BSIOrigin)
haiicu_unit_inc$BSI<-grepl("BSI",haiicu_unit_inc$InfectionSite)
haiicu_unit_inc$PN<-grepl("PN",haiicu_unit_inc$InfectionSite)
haiicu_unit_inc$UTI<-grepl("UTI",haiicu_unit_inc$InfectionSite)
haiicu_unit_inc$CRI<-grepl("CRI3",haiicu_unit_inc$InfectionSite)
haiicu_unit_inc$PRBSI<-grepl("BSI|CRI3",haiicu_unit_inc$InfectionSite)&(!grepl("^S-.*",haiicu_unit_inc$BSIOrigin))
haiicu_unit_inc$dupl<-duplicated(haiicu_unit_inc$Id)
haiicu_unit_inc$lengthofstay[haiicu_unit_inc$dupl]<-NA
haiicu_unit_inc$InfectionSite<-NA
haiicu_unit_inc$BSIOrigin<-NA
haiicu_unit_inc<-group_by(haiicu_unit_inc,UnitId)%>%summarise(BSI=sum(BSI,na.rm=TRUE),PN=sum(PN,na.rm=TRUE),UTI=sum(UTI,na.rm=TRUE),CRI3=sum(CRI,na.rm=TRUE),
                                                              PRBSI=sum(PRBSI,na.rm=TRUE),lengthofstay=sum(as.numeric(lengthofstay),na.rm=TRUE))

#haiicu_unit_inc<-aggregate(haiicu_unit_inc,by=list(haiicu_unit_inc$UnitId),FUN=count,na.rm=TRUE)
#haiicu_unit_inc<-rename(haiicu_unit_inc,RecordId=Group.1)
haiicu_level1_inf<-merge(haiicu_level1, haiicu_unit_inc, by.x="RecordId",by.y="UnitId", all.x=TRUE)
save(haiicu_level1_inf, file="haiicu_level1_inf.Rda") #aggregated infection data from standard protocol for incidence estimation

#icu light protocol aggregate infection data ---------
haiicu_unitlight_inc<-read.csv("3.HAIICULIGHT$Deno$Inf.csv") #Load 3.HAIICULIGHT$Deno$Inf
haiicu_unitlight_inc<-select(haiicu_unitlight_inc, ParentId,RecordId,InfectionSite,BSIOrigin)

haiicu_unitlight_inc$BSI<-grepl("BSI",haiicu_unitlight_inc$InfectionSite)
haiicu_unitlight_inc$PN<-grepl("PN",haiicu_unitlight_inc$InfectionSite)
haiicu_unitlight_inc$UTI<-grepl("UTI",haiicu_unitlight_inc$InfectionSite)


haiicu_unitlight_inc$CRI3<-grepl("CRI3",haiicu_unitlight_inc$InfectionSite)
haiicu_unitlight_inc$PRBSI<-grepl("BSI|CRI3",haiicu_unitlight_inc$InfectionSite)&(!grepl("^S-.*",haiicu_unitlight_inc$BSIOrigin))

haiicu_unitlight_inc$dupl<-duplicated(haiicu_unitlight_inc$RecordId)
haiicu_unitlight_inc$RecordId<-NA
haiicu_unitlight_inc$InfectionSite<-NA
haiicu_unitlight_inc$BSIOrigin<-NA

haiicu_unitlight_inc<-aggregate(haiicu_unitlight_inc,by=list(haiicu_unitlight_inc$ParentId),FUN=sum,na.rm=TRUE)
haiicu_unitlight_inc$ParentId<-NA

haiicu_unitlight_inc$RecordId<-haiicu_unitlight_inc$Group.1
haiicu_unitlight_inc<-select(haiicu_unitlight_inc,-Group.1)

haiicu_unitlight_inc<-aggregate(haiicu_unitlight_inc,by=list(haiicu_unitlight_inc$RecordId),FUN=sum,na.rm=TRUE)
haiicu_unitlight_inc<-select(haiicu_unitlight_inc, RecordId, BSI, PN, UTI, CRI3,PRBSI)

haiicu_unitlight_deno<-read.csv("2.HAIICULIGHT$Deno.csv") #load 2.HAIICULIGHT$Deno
haiicu_unitlight_inf<-merge(haiicu_unitlight_deno, haiicu_unitlight_inc, by="RecordId", all.x=TRUE)
haiicu_unitlight_inf$RecordId<-haiicu_unitlight_inf$ParentId
haiicu_unitlight_all<-merge(haiicu_level1_light, haiicu_unitlight_inf, by="RecordId", all.x=TRUE)
save(haiicu_unitlight_all, file="haiicu_unitlight_all.Rda") #aggregated file of light protocol for incidence estimation


#get lengthofstay per unit in patient data
#patientdays<-select (haiicu_pt_inf_all, RecordId.x,lengthofstay)
#patientdays$RecordId<-patientdays$RecordId.x


#patientdays_unit<-aggregate(patientdays, by=list(patientdays$RecordId), FUN=sum)
#patientdays_unit$RecordId<-patientdays_unit$Group.1
#patientdays_unit<-select(patientdays_unit,RecordId,lengthofstay)
#haiicu_level1_inf<-merge (haiicu_level1_inf, patientdays_unit,by="RecordId")

#select(haiicu_level1_inf, RecordId,ReportingCountry, HospitalSize, HospitalType,UnitSize,UnitSpecialty,UnitPercentIntub,lengthofstay, BSI,PN,UTI)
haiicustandard1<-select(haiicu_level1_inf, RecordId,ReportingCountry, HospitalSize, HospitalType,UnitSize,UnitSpecialty,UnitPercentIntub,lengthofstay, BSI,PN,UTI,CRI3,PRBSI)

haiiculight1<-select(haiicu_unitlight_all, RecordId, ReportingCountry, HospitalSize, HospitalType,UnitSize,UnitSpecialty,UnitPercentIntub, NumPatDaysUnit2d, BSI,PN,UTI,CRI3,PRBSI)
haiicustandard1$NumPatDaysUnit2d<-haiicustandard1$lengthofstay
haiicustandard1<-select(haiicustandard1, -lengthofstay)
haiicustandard1<-select(haiicustandard1, RecordId,ReportingCountry, HospitalSize, HospitalType,UnitSize,UnitSpecialty,UnitPercentIntub,NumPatDaysUnit2d, BSI,PN,UTI,CRI3,PRBSI)
haiicuall<-rbind(haiiculight1,haiicustandard1)
save(haiicuall,file="haiicuall.Rda") # combined light and standard file (haiiculight missing NumPatDaysUnit2d)

haiiculight2<-select(haiicu_unitlight_all, RecordId, ReportingCountry, HospitalSize, HospitalType,UnitSize,UnitSpecialty,UnitPercentIntub, NumPatDaysUnit, BSI,PN,UTI,CRI3,PRBSI)
haiiculight2$NumPatDaysUnit2d<-haiiculight2$NumPatDaysUnit
haiiculight2<-select(haiiculight2,-NumPatDaysUnit)
haiiculight2$RecordId<-factor(haiiculight2$RecordId)
haiicuall2<-rbind(haiicustandard1,haiiculight2)
haiicuall2$UTI_incdens<-round(haiicuall2$UTI/haiicuall2$NumPatDaysUnit2d*1000,digits=2)
haiicuall2$BSI_incdens<-round((haiicuall2$BSI+haiicuall2$CRI3)/haiicuall2$NumPatDaysUnit2d*1000,digits=2)
haiicuall2$PN_incdens<-round(haiicuall2$PN/haiicuall2$NumPatDaysUnit2d*1000,digits=2)
saveRDS(haiicuall2,"haiicuall2.Rda")




#incidence all ICUs incl light-------
PNinc_country<-haiicuall2%>%group_by(ReportingCountry)%>%summarise(n_PN=sum(PN,na.rm=TRUE),
                                      n_NumPtDays=sum(NumPatDaysUnit2d,na.rm=TRUE),
                                      PNinc=round(1000*sum(PN,na.rm=TRUE)/sum(NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                      meanPNinc=round(mean(1000*PN/NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                      pct25=round(quantile(1000*PN/NumPatDaysUnit2d,probs=c(0.25),na.rm=TRUE),digits=2),
                                      median=round(quantile(1000*PN/NumPatDaysUnit2d,probs=c(0.5),na.rm=TRUE),digits=2),
                                      pct75=round(quantile(1000*PN/NumPatDaysUnit2d,probs=c(0.75),na.rm=TRUE),digits=2)
)
saveRDS(PNinc_country,file="PNinc_country.Rda")
BSIinc_country<-haiicuall2%>%group_by(ReportingCountry)%>%summarise(n_BSI=sum(BSI+CRI3,na.rm=TRUE),
                                                                   n_NumPtDays=sum(NumPatDaysUnit2d,na.rm=TRUE),
                                                                   BSIinc=round(1000*(sum(BSI,na.rm=TRUE)+sum(CRI3,na.rm=TRUE))/sum(NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                                                   meanBSIinc=round(mean(1000*(BSI+CRI3)/NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                                                   pct25=round(quantile(1000*(BSI+CRI3)/NumPatDaysUnit2d,probs=c(0.25),na.rm=TRUE),digits=2),
                                                                   median=round(quantile(1000*(BSI+CRI3)/NumPatDaysUnit2d,probs=c(0.5),na.rm=TRUE),digits=2),
                                                                   pct75=round(quantile(1000*(BSI+CRI3)/NumPatDaysUnit2d,probs=c(0.75),na.rm=TRUE),digits=2)
)
saveRDS(BSIinc_country,file="BSIinc_country.Rda")
UTIinc_country<-haiicuall2%>%group_by(ReportingCountry)%>%summarise(n_UTI=sum(UTI,na.rm=TRUE),
                                                                   n_NumPtDays=sum(NumPatDaysUnit2d,na.rm=TRUE),
                                                                   UTIinc=round(1000*sum(UTI,na.rm=TRUE)/sum(NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                                                   meanUTIinc=round(mean(1000*UTI/NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                                                   pct25=round(quantile(1000*UTI/NumPatDaysUnit2d,probs=c(0.25),na.rm=TRUE),digits=2),
                                                                   median=round(quantile(1000*UTI/NumPatDaysUnit2d,probs=c(0.5),na.rm=TRUE),digits=2),
                                                                   pct75=round(quantile(1000*UTI/NumPatDaysUnit2d,probs=c(0.75),na.rm=TRUE),digits=2)
)
saveRDS(UTIinc_country,file="UTIinc_country.Rda")
PNinc_EU<-haiicuall2%>%filter(!ReportingCountry=="DE")%>%
                      summarise(n_PN=sum(PN,na.rm=TRUE),
                                 n_NumPtDays=sum(NumPatDaysUnit2d,na.rm=TRUE),
                                 PNinc_EU=round(1000*sum(PN,na.rm=TRUE)/sum(NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                 meanPNinc_EU=round(mean(1000*PN/NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                 pct25=round(quantile(1000*PN/NumPatDaysUnit2d,probs=c(0.25),na.rm=TRUE),digits=2),
                                 median=round(quantile(1000*PN/NumPatDaysUnit2d,probs=c(0.5),na.rm=TRUE),digits=2),
                                 pct75=round(quantile(1000*PN/NumPatDaysUnit2d,probs=c(0.75),na.rm=TRUE),digits=2)
                                 )
saveRDS(PNinc_EU,file="PNinc_EU.Rda")
BSIinc_EU<-haiicuall2%>%filter(!ReportingCountry=="DE")%>%summarise(n_BSI=sum(BSI+CRI3,na.rm=TRUE),
                                 n_NumPtDays=sum(NumPatDaysUnit2d,na.rm=TRUE),
                                 BSIinc_EU=round(1000*(sum(BSI,na.rm=TRUE)+sum(CRI3,na.rm=TRUE))/sum(NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                 meanBSIinc_EU=round(mean(1000*(BSI+CRI3)/NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                 pct25=round(quantile(1000*(BSI+CRI3)/NumPatDaysUnit2d,probs=c(0.25),na.rm=TRUE),digits=2),
                                 median=round(quantile(1000*(BSI+CRI3)/NumPatDaysUnit2d,probs=c(0.5),na.rm=TRUE),digits=2),
                                 pct75=round(quantile(1000*(BSI+CRI3)/NumPatDaysUnit2d,probs=c(0.75),na.rm=TRUE),digits=2)
)
saveRDS(BSIinc_EU,file="BSIinc_EU.Rda")
UTIinc_EU<-haiicuall2%>%filter(!ReportingCountry %in% c("DE","FR"))%>%summarise(n_UTI=sum(UTI,na.rm=TRUE),
                                 n_NumPtDays=sum(NumPatDaysUnit2d,na.rm=TRUE),
                                 UTIinc_EU=round(1000*sum(UTI,na.rm=TRUE)/sum(NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                 meanUTIinc_EU=round(mean(1000*UTI/NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                 pct25=round(quantile(1000*UTI/NumPatDaysUnit2d,probs=c(0.25),na.rm=TRUE),digits=2),
                                 median=round(quantile(1000*UTI/NumPatDaysUnit2d,probs=c(0.5),na.rm=TRUE),digits=2),
                                 pct75=round(quantile(1000*UTI/NumPatDaysUnit2d,probs=c(0.75),na.rm=TRUE),digits=2)
)
saveRDS(UTIinc_EU,file="UTIinc_EU.Rda")

#Exposure data -------
haiicuexp<-read.csv("3.HAIICU$PT$EXP.csv") #load 3.HAIICU$Pt$Exp.csv
haiicuexp<-haiicuexp%>%mutate(across(c(DateExpStart, DateExpEnd), parse_mixed_date)) 
#haiicuexp$DateExpStart<-as.Date(haiicuexp$DateExpStart,"%Y-%m-%d") #use this code if date format is yyyy-mm-dd
#haiicuexp$DateExpEnd<-as.Date(haiicuexp$DateExpEnd,"%Y-%m-%d") #use this code if date format is yyyy-mm-dd
#haiicuexp$DateExpStart<-as.Date(haiicuexp$DateExpStart,"%d/%m/%Y") #use this code if date format is dd/mm/yyyy
#haiicuexp$DateExpEnd<-as.Date(haiicuexp$DateExpEnd,"%d/%m/%Y") #use this code if date format is dd/mm/yyyy
#correct if DateExpStart after DateExpEnd--------------------------------------------------

haiicuexp$DateExpEnd<-as.Date(haiicuexp$DateExpEnd)
haiicuexp$DateExpStart<-as.Date(haiicuexp$DateExpStart)
sum(is.na(haiicuexp$DateExpStart)) #data check 2023
sum(is.na(haiicuexp$DateExpEnd)) #data check 2023
haiicuexp<-filter(haiicuexp,!is.na(DateExpStart),!is.na(DateExpEnd))

haiicuexp$errordatexp<-haiicuexp$DateExpStart>haiicuexp$DateExpEnd
haiicuexp$temp<-haiicuexp$DateExpEnd
haiicuexp$DateExpEnd[haiicuexp$errordatexp==TRUE]<-haiicuexp$DateExpStart[haiicuexp$errordatexp==TRUE]
haiicuexp$DateExpStart[haiicuexp$errordatexp==TRUE]<-haiicuexp$temp[haiicuexp$errordatexp==TRUE]
#write.csv(haiicuexp,"haiicuexp_correct.csv")

haiicuexp<-select(haiicuexp,RecordId,ParentId,DateExpStart, DateExpEnd,ExpType)
haiicuexp$Id<-haiicuexp$ParentId
haiicuexp<-merge(haiicuexp,haiicu_pt_unit, by.x="Id",by.y="RecordId.x")
haiicuexp<-haiicuexp%>%rename(ParentId=ParentId.x)
sum(haiicuexp$DateExpEnd<haiicuexp$DateExpStart) #data check exposure end before start 2023
sum(haiicuexp$DateExpEnd>haiicuexp$DateUnitDischarge & haiicuexp$DateExpStart>haiicuexp$DateUnitDischarge)#data check exposure after discharge 2023
sum(haiicuexp$DateExpEnd<haiicuexp$DateUnitAdmission & haiicuexp$DateExpStart<haiicuexp$DateUnitAdmission) #data check exposure before admission 2023
haiicuexp<-filter(haiicuexp,!(DateExpEnd>DateUnitDischarge & DateExpStart>DateUnitDischarge)) #exclude exposures totally out of admission period 2023
haiicuexp<-filter(haiicuexp,!(DateExpEnd<DateUnitAdmission & DateExpStart<DateUnitAdmission)) #exclude exposures totally out of admission period 2023

###update 2023 to address exposure reported after discharge -----

sum(haiicuexp$DateExpEnd>haiicuexp$DateUnitDischarge) #data check exposure period after discharge for exposures started after admission 2023
sum(haiicuexp$DateExpStart<haiicuexp$DateUnitAdmission) #data check exposure period before admission for exposures started before admission 2023

### following code to run if there are exposures before admission or after discharge -------


haiicuexp$expafterdischarge<-as.Date(haiicuexp$DateUnitDischarge)-as.Date(haiicuexp$DateExpEnd)
haiicuexp<-haiicuexp%>%filter(!is.na(haiicuexp$DateExpEnd),!is.na(haiicuexp$DateUnitDischarge),
                              !is.na(haiicuexp$expafterdischarge))
haiicuexp$DateExpEnd[as.numeric(haiicuexp$expafterdischarge)<0]<-haiicuexp$DateUnitDischarge[as.numeric(haiicuexp$expafterdischarge)<0]

haiicuexp$expbeforeadmission<-as.Date(haiicuexp$DateUnitAdmission)-as.Date(haiicuexp$DateExpStart)
haiicuexp<-haiicuexp%>%filter(!is.na(haiicuexp$DateExpStart),!is.na(haiicuexp$DateUnitAdmission),
                              !is.na(haiicuexp$expbeforeadmission))
haiicuexp$DateExpStart[as.numeric(haiicuexp$expbeforeadmission)>0]<-haiicuexp$DateUnitAdmission[as.numeric(haiicuexp$expbeforeadmission)>0]



#haiicuexp<-haiicuexp[,-c(1)]
haiicuexp$expdays<-as.Date(haiicuexp$DateExpEnd)-as.Date(haiicuexp$DateExpStart)+1
icuexp<-haiicuexp%>%select(DateUnitAdmission,DateUnitDischarge,DateExpStart,DateExpEnd,expdays,expbeforeadmission,expafterdischarge, ReportingCountry,los,RecordId.y,UnitId, RecordId,Id) #data check table
write.csv(haiicuexp,"haiicuexp_correct.csv") #update 2023

#intubation exposure -----
haiicuintubdays<-filter(haiicuexp,ExpType=="INT")
#haiicuintub<-select(haiicuexp,Id,expdays[ExpType=="INT"])

haiicuintubdays<-select(haiicuintubdays,UnitId,expdays)
haiicuintubdays<-haiicuintubdays%>%group_by(UnitId)%>%summarise(expdays=sum(expdays,na.rm=TRUE))



haiicuintubdays$RecordId<-haiicuintubdays$UnitId
haiicuall3<-merge(haiicuall2,haiicuintubdays,by=("RecordId"),x.all=TRUE)

#haiicuintubdays<-select(haiicuintubdays,Id,ReportingCountry,expdays)

#clean data by HU and re-merge in total
#icuhung<-filter(haiicuall2,ReportingCountry=="HU")

#Hungarian ICU combine surveillance periods-------------------------------------------------------
#icuhung2<-select(icuhung,RecordId,NumPatDaysUnit2d,BSI,PN,UTI,CRI)
#icuhung<-select(icuhung,-BSI,-PN,-UTI,-CRI,-NumPatDaysUnit2d)
#icuhung2$NumPatDaysUnit2d<-as.numeric(icuhung2$NumPatDaysUnit2d)
#icuhung2$PN[is.na(icuhung2$PN)==TRUE]<-0
#icuhung2$BSI[is.na(icuhung2$BSI)==TRUE]<-0
#icuhung2$UTI[is.na(icuhung2$UTI)==TRUE]<-0
#icuhung2$CRI[is.na(icuhung2$CRI)==TRUE]<-0
#icuhung2<-aggregate(icuhung2,by=list(icuhung2$RecordId),FUN=sum)
#icuhung2$RecordId<-icuhung2$Group.1
#icuhung2<-select(icuhung2,-Group.1)
#icuhung<-icuhung[!duplicated(icuhung),]
#icuhung3<-merge (icuhung2,icuhung,by="RecordId")
#haiicuall<-filter(haiicuall,ReportingCountry!="HU")
#haiicuall<-rbind(haiicuall,icuhung3)
#end of hungarian ICU combine surveillance periods

#get percent intubated for standard protocol
# OBSOLETE
#haiicuintubdays$RecordId<-haiicuintubdays$Id
#haiicuintubdays$RecordId<-haiicuintubdays$UnitId

haiicuallintub<-merge(haiicuall,haiicuintubdays,by="RecordId",all.x=TRUE)

haiicuallintub$percintub<-(as.numeric(haiicuallintub$expdays)/as.numeric(haiicuallintub$NumPatDaysUnit2d))*100


#IAP cases based on date of onset and dates of exposure start and end - to be uncommented-----------------------------------------------------------
haiicu_iap<-merge(haiicu_pt_inf_all[,c("Id","UnitId","hasHai","InfectionSite","dupl_pat","InvasiveDevice","DateOfOnset")],haiicuexp[,c("RecordId","ParentId","ExpType","DateExpStart","DateExpEnd")],by.x="Id",by.y="ParentId")
haiicu_iap<-filter(haiicu_iap,grepl("PN",InfectionSite))
haiicu_iap<-filter(haiicu_iap,ExpType=="INT")
haiicu_iap<-filter(haiicu_iap,(as.Date(DateOfOnset,"%Y-%m-%d")>(as.Date(DateExpStart)) & as.Date(DateOfOnset,"%Y-%m-%d")<(as.Date(DateExpEnd)+3))| (UnitId>45591703 & UnitId<45591914))#data from France analysed differently due to inconsistent dates of exposure - to be corrected in later years
haiicu_iap<-distinct(haiicu_iap,Id,DateOfOnset,.keep_all=TRUE)

haiicu_iap<-haiicu_iap%>%group_by(Id)%>%arrange(DateOfOnset)%>%mutate(dateofonsetprev=lag(DateOfOnset))#added on 13/11/2017 to address cases with multiple reporting of the same infection
haiicu_iap<-haiicu_iap%>%group_by(Id)%>%mutate(diffdateofonset=as.numeric(as.Date(DateOfOnset)-as.numeric(as.Date(dateofonsetprev))))
haiicu_iap<-haiicu_iap%>%group_by(UnitId)%>%filter(is.na(diffdateofonset) | diffdateofonset>7)


haiicu_iapaggr<-haiicu_iap
haiicu_iapaggr<-select(haiicu_iap,UnitId)
haiicu_iapaggr<-count(haiicu_iap,UnitId)
names(haiicu_iapaggr)[names(haiicu_iapaggr)=="n"]<-"IAP"
haiicu_unit_inc<-merge(haiicu_unit_inc,haiicu_iapaggr,by="UnitId",all=TRUE)
haiicu_unit_inc[c("IAP")][is.na(haiicu_unit_inc[c("IAP")])]<-0

#percent intubated in standard protocol by use of variable Intubation
percintubstand<-select(haiicu_pt,ParentId,Intubation)
percintubstand$Intub<-ifelse((percintubstand$Intubation=="Y"),1,ifelse((percintubstand$Intubation=="N"),0,NA))
percintubstand<-percintubstand%>%group_by(ParentId)%>%mutate(percintub=sum(Intub,na.rm=TRUE)/n()*100)
percintubstand<-aggregate(percintubstand,by=list(percintubstand$ParentId),FUN=mean)
percintubstand$RecordId<-percintubstand$Group.1
percintubstand$percintub<-round(percintubstand$percintub,digits=0)
percintubstand<-select(percintubstand,RecordId,percintub)
haiicuallintub$UnitPercentIntub[haiicuallintub$UnitPercentIntub=="N/A"]<-NA
haiicuallintub$UnitPercentIntub<-as.numeric(as.character(haiicuallintub$UnitPercentIntub))
haiicuall_percintub<-merge(haiicuallintub,percintubstand,by="RecordId",all=TRUE)
haiicuall_percintub$UnitPercentIntub<-ifelse(is.na(haiicuall_percintub$UnitPercentIntub)|haiicuall_percintub$UnitPercentIntub=="N/A",haiicuall_percintub$percintub.y,haiicuall_percintub$UnitPercentIntub)


haiicuall_intub<-select(haiicuall_percintub,RecordId:PRBSI)
save(haiicuall_intub,file="haiicuall_percintub.Rda")

#ICU length of stay for light protocol, missing values as "" ---------
haiiculightdeno<-read.csv("2.HAIICULIGHT$Deno.csv") #load 2.HAIICULIGHT$Deno
haiiculightdeno<-filter(haiiculightdeno,is.na(NumPatDaysUnit2d))
haiiculightdeno$RecordId<-haiiculightdeno$ParentId
haiiculightdeno<-select(haiiculightdeno,RecordId,NumPatDaysUnit)

haiicu_percintub<-merge(haiicuall_intub,haiiculightdeno,by="RecordId",all=TRUE)
haiicu_percintub<-haiicu_percintub%>%filter(!ReportingCountry=="DE")
haiicu_percintub$NumPatDaysUnit2d<-ifelse((is.na(haiicu_percintub$NumPatDaysUnit2d)),as.numeric(as.character(haiicu_percintub$NumPatDaysUnit)),haiicu_percintub$NumPatDaysUnit2d)

haiicu_percintub$intubgroup<-cut(haiicu_percintub$UnitPercentIntub,breaks=c(0,30,60,100),labels=c("0-29","30-59","60-100"))
haiicu_percintub <- haiicu_percintub[, !duplicated(colnames(haiicu_percintub))]



#incidence of pneumonia per intubation percentage group ---------
haiicu_percintub$incdens<-round(haiicu_percintub$PN/as.numeric(haiicu_percintub$NumPatDaysUnit2d)*1000,digits=2)
saveRDS(haiicu_percintub,file="haiicu_percintub.Rda")
per_intubgroup<-group_by(haiicu_percintub,intubgroup)
#PNcases_byintubgroup<-summarise(per_intubgroup,PNcases=sum(PN,na.rm=TRUE),patdays=sum(as.numeric(NumPatDaysUnit2d),na.rm=TRUE),PNinc=round(PNcases/patdays*1000,digits=2))
PNinc_byintubgroup<-summarise(per_intubgroup,meanPNinc=mean(incdens[incdens!=Inf],na.rm=TRUE))

#device adjusted pneumonia rates

haiicuintubdays<-select(haiicuintubdays,RecordId,expdays)
haiicustandardexp<-merge(haiicustandard1,haiicuintubdays,by="RecordId")

iapincdens<-merge(haiicu_unit_inc,haiicuintubdays,by.x="UnitId",by.y="RecordId")
iapincdens<-filter(iapincdens,expdays>19)#ICUs with less than 20 days of exposure were excluded due to skewed data
iapincdens$iapincintubdays<-round(1000*iapincdens$IAP/as.numeric(iapincdens$expdays),digits=2)
#iapincdens$iapinc<-round(1000*iapincdens$IAP/as.numeric(iapincdens$expdays),digits=2)
haiicu_level1<-select(haiicu_level1,-UnitId)
haiicudenscountr<-merge(iapincdens,haiicu_level1,by.x="UnitId",by.y="RecordId")
haiicudenscountr<-select(haiicudenscountr,UnitId,ReportingCountry,DataSource, PN,IAP,expdays,iapincintubdays,lengthofstay)
haiicudenscountr<-filter(haiicudenscountr,lengthofstay!=0)

#add number of patients per unit for IAP table per country
patientno_unit<-select(haiicu_pt,ParentId)
patientno_unit$adm<-TRUE
patientno_unit<-patientno_unit%>%group_by(ParentId)%>%summarise(adm=sum(adm,na.rm=TRUE))

patientno_unit<-select(patientno_unit,ParentId,adm)
haiicudenscountr<-merge(haiicudenscountr,patientno_unit,by.x="UnitId",by.y="ParentId")
haiicudenscountr<-filter(haiicudenscountr,adm>9)#ICUs with less than 10 admissions were excluded due to skewed data
haiicudenscountr$icuno<-TRUE
haiicudenscountr$avglos<-round(haiicudenscountr$lengthofstay/haiicudenscountr$adm, digits=2)
haiicudenscountr$intubuse<-round(100*haiicudenscountr$expdays/as.numeric(haiicudenscountr$lengthofstay),digits=1)
haiicudenscountr<-filter(haiicudenscountr,adm>9)#ICUs with less than 10 admissions were excluded due to skewed data

saveRDS(haiicudenscountr,file="haiicuiapdens.Rda")

#IAP table -------

eu_iap<-haiicudenscountr%>%summarise(n_expdays=sum(as.numeric(expdays),na.rm=TRUE),
                                     intubuse=round(mean(intubuse,na.rm=TRUE),digits=2),
                                     n_IAP=sum(IAP,na.rm=TRUE),
                                     aggr_inc=round(1000*sum(IAP,na.rm=TRUE)/sum(as.numeric(expdays,na.rm=TRUE)),digits=2),
                                     avgiaprate=round(mean(iapincintubdays,na.rm=TRUE),digits=2),
                                     iaprate25pct=round(quantile(iapincintubdays,probs=c(0.25),na.rm=TRUE),digits=2),
                                     iapratemedian=round(median(iapincintubdays,na.rm=TRUE),digits=2),
                                     iaprate75pct=round(quantile(iapincintubdays,probs=c(0.75),na.rm=TRUE),digits=2))

by_country<-group_by(haiicudenscountr,ReportingCountry)
IAPtable<-summarise(by_country,
                    avglos=round(mean(as.numeric(avglos),na.rm=TRUE), digits=2),
                    intubuse=round(mean(as.numeric(intubuse),na.rm=TRUE),digits=2),
                    n_expdays=sum(as.numeric(expdays),na.rm=TRUE),
                    n_IAP=sum(IAP,na.rm=TRUE),
                    aggr_inc=round(1000*sum(IAP,na.rm=TRUE)/sum(as.numeric(expdays,na.rm=TRUE)),digits=2),
                    avgiaprate=round(mean(iapincintubdays,na.rm=TRUE),digits=2),
                    iaprate25pct=round(quantile(iapincintubdays,probs=c(0.25),na.rm=TRUE),digits=2),
                    iapratemedian=round(median(iapincintubdays,na.rm=TRUE),digits=2),
                    iaprate75pct=round(quantile(iapincintubdays,probs=c(0.75),na.rm=TRUE),digits=2))
#IAPtable<-bind_rows(IAPtable,eu_iap) #under development

saveRDS(IAPtable,file="IAPtable.Rda")


#pneumonia pathogens -------
micro<-read.csv("4.HAIICU$PT$INF$RES.csv") #load 4.HAIICU$Pt$Inf$Res.csv
microlight<-read.csv("4.HAIICULIGHT$Deno$Inf$Res.csv") #load 4.HAIICULIGHT$Deno$Inf$Res.csv
inf<-read.csv("3.HAIICU$PT$INF.csv") #load 3.HAIICU$Pt$Inf.csv
inflight<-read.csv("3.HAIICULIGHT$Deno$Inf.csv") #load 3.HAIICULIGHT$Deno$Inf
ptlight<-read.csv("2.HAIICULIGHT$Deno.csv")
PN<-select(inf,RecordId,ParentId,InfectionSite)
PN<-filter(PN,grepl("PN",PN$InfectionSite))
PNlight<-select(inflight,RecordId,ParentId,InfectionSite)
PNlight<-filter(PNlight,grepl("PN",PNlight$InfectionSite))



PNmicro<-merge(PN[,c("RecordId","InfectionSite","ParentId")],micro[,c("Antibiotic","ResultIsolate","SIR","ParentId")],by.x="RecordId",by.y="ParentId",all=FALSE)
PNmicro<-select(PNmicro,RecordId,ParentId,InfectionSite,ResultIsolate)
PNmicro<-distinct(PNmicro)
PNmicrolight<-merge(PNlight[,c("RecordId","InfectionSite","ParentId")],microlight[,c("Antibiotic","ResultIsolate","SIR","ParentId")],by.x="RecordId",by.y="ParentId",all=FALSE)
PNmicrolight<-select(PNmicrolight,RecordId,ParentId,InfectionSite,ResultIsolate)
PNmicrolight<-distinct(PNmicrolight)
ptPNmicro<-merge(PNmicro[,c("RecordId","InfectionSite","ResultIsolate", "ParentId")],pat[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)
ptPNmicro<-ptPNmicro[,-c(1)]

ptPNmicrolight<-merge(PNmicrolight[,c("RecordId","InfectionSite","ResultIsolate", "ParentId")],ptlight[,c("RecordId","ParentId")],by.x="Id",by.y="RecordId",all=FALSE)

pt<-read.csv("2.HAIICU$PT.csv")
standard<-haiicu_level1
lightdeno<-read.csv("2.HAIICULIGHT$Deno.csv")
light<-read.csv("1.HAIICULIGHT.csv")


ptPNmicro<-merge(PNmicro[,c("RecordId","InfectionSite","ResultIsolate", "ParentId")],pt[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)
ptPNmicro<-ptPNmicro%>%rename(InfectionId=ParentId)%>%rename(ParentId=ParentId.y)
denoPNmicrolight<-merge(PNmicrolight[,c("ParentId","InfectionSite","ResultIsolate", "RecordId")],lightdeno[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)
denoPNmicrolight$ParentId<-denoPNmicrolight$ParentId.y
denoPNmicrolight<-denoPNmicrolight%>%select(-ParentId.y)
unitPNmicrolight<-merge(denoPNmicrolight[,c("ResultIsolate","ParentId")],light[,c("RecordId","ReportingCountry")],by.x="ParentId",by.y="RecordId",all=FALSE)
unitPNmicro<-merge(ptPNmicro[,c("ResultIsolate","ParentId")],standard[,c("RecordId","ReportingCountry")],by.x="ParentId",by.y="RecordId",all=FALSE)
PNmicroall<-rbind(unitPNmicro,unitPNmicrolight)


PNmicroall$Isolate<-ifelse(!grepl("_",PNmicroall$ResultIsolate),substr(PNmicroall$ResultIsolate,1,3),NA)
#PNmicroall$Isolate<-NA
PNmicroall$Isolate[grepl("PSEAER",PNmicroall$ResultIsolate)]<-"Pseudomonas aeruginosa"
PNmicroall$Isolate[grepl("STAAUR",PNmicroall$ResultIsolate)]<-"Staphylococcus aureus"
PNmicroall$Isolate[grepl("KLE...",PNmicroall$ResultIsolate)]<-"Klebsiella spp."
PNmicroall$Isolate[grepl("ESCCOL",PNmicroall$ResultIsolate)]<-"Escherichia coli"
PNmicroall$Isolate[grepl("CAN...",PNmicroall$ResultIsolate)]<-"Candida spp."
PNmicroall$Isolate[grepl("STEMAL",PNmicroall$ResultIsolate)]<-"Stenotrophomonas maltofphilia"
PNmicroall$Isolate[grepl("ENB...",PNmicroall$ResultIsolate)]<-"Enterobacter spp."
PNmicroall$Isolate[grepl("ACI...",PNmicroall$ResultIsolate)]<-"Acinetobacter spp."
PNmicroall$Isolate[grepl("ENC...",PNmicroall$ResultIsolate)]<-"Enterococcus spp."
PNmicroall$Isolate[grepl("SER...",PNmicroall$ResultIsolate)]<-"Serratia spp."
PNmicroall$Isolate[grepl("PRT...",PNmicroall$ResultIsolate)]<-"Proteus spp."


PNmicroall<-filter(PNmicroall,ResultIsolate!="_NOEXA" & ResultIsolate!="_STERI"&
                              ResultIsolate!="_NA" & ResultIsolate!="_NONI")


#PNmicrotable<-table(PNmicroall$Isolate,PNmicroall$ReportingCountry)
#propPNmicrotable<-addmargins(100*round(prop.table(PNmicrotable,2),digits=2))

PNtable_group<-group_by(PNmicroall,Isolate,ReportingCountry)%>%summarise(n_isol=n())
PNtable_group<-spread(PNtable_group,ReportingCountry,n_isol)
PNtable_group<-PNtable_group%>%replace(is.integer(.)&is.na(.),0)
PNtable_group_sum<-PNtable_group
PNtable_group_sum$total=rowSums(PNtable_group[,c(2:ncol(PNtable_group))],na.rm=TRUE)
PNtable_group_sum<-ungroup(PNtable_group_sum)

PNtable_group_top10<-top_n(PNtable_group_sum,n=10,total)

PNtable_group_top10<-PNtable_group_top10[order(-PNtable_group_top10$total),]

PNtable_group_top10<-mutate(PNtable_group_top10,ATpc=100*round(AT/sum(AT,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,BEpc=100*round(BE/sum(BE,na.rm=TRUE),digits=3))

PNtable_group_top10<-mutate(PNtable_group_top10,EEpc=100*round(EE/sum(EE,na.rm=TRUE),digits=3))

PNtable_group_top10<-mutate(PNtable_group_top10,FRpc=100*round(FR/sum(FR,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,DEpc=100*round(DE/sum(DE,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,HUpc=100*round(HU/sum(HU,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,ITpc=100*round(IT/sum(IT,na.rm=TRUE),digits=3))

PNtable_group_top10<-mutate(PNtable_group_top10,ITGiViTIpc=100*round(eval(as.symbol("IT-GiViTI"))/sum(eval(as.symbol("IT-GiViTI")),na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,ITSPINUTIpc=100*round(eval(as.symbol("IT-SPIN-UTI"))/sum(eval(as.symbol("IT-SPIN-UTI")),na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,LTpc=100*round(LT/sum(LT,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,LUpc=100*round(LU/sum(LU,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,MTpc=100*round(MT/sum(LU,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,PTpc=100*round(PT/sum(PT,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,ROpc=100*round(RO/sum(RO,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,ESpc=100*round(ES/sum(ES,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,SKpc=100*round(SK/sum(SK,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,UKpc=100*round(UK/sum(UK,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,PLpc=100*round(PL/sum(PL,na.rm=TRUE),digits=3))
PNtable_group_top10<-mutate(PNtable_group_top10,totalpc=100*round(total/sum(total,na.rm=TRUE),digits=3))


pn_out <- build_top10_country_tables(PNtable_group_top10)

summaryPNtable_top10 <- pn_out$summary
PNtable_group_totals <- pn_out$totals
PNtable_group_top10_pc <- pn_out$pc
saveRDS(PNtable_group_top10_pc,file="PNtable_group_top10_pc.Rda")



#BSI----------------------------------------------
haiicu_aggr<-haiicu_percintub
saveRDS(haiicu_aggr,file="haiicu_aggr.Rda")
bsi_incid_aggr<-(sum(haiicu_aggr$BSI,na.rm=TRUE)+sum(haiicu_aggr$CRI,na.rm=TRUE))/(sum(as.numeric(haiicu_aggr$NumPatDaysUnit2d),na.rm=TRUE))*1000
haiicu_aggr$bsi_incdens<-round(haiicu_aggr$BSI/as.numeric(haiicu_aggr$NumPatDaysUnit2d)*1000,digits=2)
haiicu_aggr$bsicri_incdens<-round((haiicu_aggr$BSI+haiicu_aggr$CRI)/as.numeric(haiicu_aggr$NumPatDaysUnit2d)*1000,digits=2)
haiicu_aggr$prbsi_incdens<-round((haiicu_aggr$PRBSI)/as.numeric(haiicu_aggr$NumPatDaysUnit2d)*1000,digits=2)

saveRDS(haiicu_aggr,file="haiicu_aggr.Rda")
#average BSI incidence per ICU -------------------
avg_bsi_incdens<-mean(haiicu_aggr$bsi_incdens,na.rm=TRUE)
#proportion of patients in standard protocol with BSI -------------
bsi_pat_prop<-round(sum(grepl("BSI",haiicu_pt_inf_all$InfectionSite))/sum(!haiicu_pt_inf_all$dupl_pat)*100,digits=2)
haiicu_unitstand_bsi<-select(haiicu_pt_inf,RecordId,Id,InfectionSite,InvasiveDevice,BSIOrigin)
haiicu_unitstand_bsi<-filter(haiicu_unitstand_bsi,haiicu_unitstand_bsi$InfectionSite=="BSI"| grepl('CRI3',haiicu_unitstand_bsi$InfectionSite))
haiicu_unitstand_bsi<-rename(haiicu_unitstand_bsi,ParentId=Id)

#bsi in light protocol --------
haiicu_unitlight_bsi<-read.csv("3.HAIICULIGHT$Deno$Inf.csv") #load 3.HAIICULIGHT$Deno$Inf.csv
haiicu_unitlight_bsi<-select(haiicu_unitlight_bsi,RecordId,ParentId,InfectionSite,InvasiveDevice,BSIOrigin)
haiicu_unitlight_bsi<-filter(haiicu_unitlight_bsi,haiicu_unitlight_bsi$InfectionSite=="BSI"| grepl('CRI3',haiicu_unitlight_bsi$InfectionSite))

haiicu_unitall_bsi<-rbind(haiicu_unitstand_bsi,haiicu_unitlight_bsi)#merge standard and light bsi
saveRDS(haiicu_unitall_bsi,file="haiicu_unitall_bsi.Rda")

round(sum(grepl("^C",haiicu_unitall_bsi$BSIOrigin))/summarise(haiicu_unitall_bsi,n=n())*100,digits=2)# % catheter related BSI
round(sum(grepl("^S",haiicu_unitall_bsi$BSIOrigin))/summarise(haiicu_unitall_bsi,n=n())*100,digits=2)# secondary BSI
round(sum(grepl("^$|UO|UNK",haiicu_unitall_bsi$BSIOrigin))/summarise(haiicu_unitall_bsi,n=n())*100,digits=2)# unknown origin BSI plus missing origin

round(sum(grepl("S-PUL",haiicu_unitall_bsi$BSIOrigin))/sum(grepl("S",haiicu_unitall_bsi$BSIOrigin))*100,digits=2)# %secondary to PUL among secondary BSI
round(sum(grepl("S-DIG",haiicu_unitall_bsi$BSIOrigin))/sum(grepl("S",haiicu_unitall_bsi$BSIOrigin))*100,digits=2)
round(sum(grepl("S-UTI",haiicu_unitall_bsi$BSIOrigin))/sum(grepl("S",haiicu_unitall_bsi$BSIOrigin))*100,digits=2)
round(sum(grepl("S-SSI",haiicu_unitall_bsi$BSIOrigin))/sum(grepl("S",haiicu_unitall_bsi$BSIOrigin))*100,digits=2)
round(sum(grepl("S-SST",haiicu_unitall_bsi$BSIOrigin))/sum(grepl("S",haiicu_unitall_bsi$BSIOrigin))*100,digits=2)
round(sum(grepl("S-OTH",haiicu_unitall_bsi$BSIOrigin))/sum(grepl("S",haiicu_unitall_bsi$BSIOrigin))*100,digits=2)



#catheter days estimation --------------
haiicu_expcvc<-select(haiicuexp,Id,RecordId,UnitId,ExpType,DateExpStart,DateExpEnd,DateUnitAdmission,DateUnitDischarge,expdays)
haiicu_expcvc$DateUnitAdmission<-as.Date(haiicu_expcvc$DateUnitAdmission)
haiicu_expcvc$DateUnitDischarge<-as.Date(haiicu_expcvc$DateUnitDischarge)
###update 2022 to address exposure reported after discharge -----
#upgraded 2023 to address all exposures
#haiicu_expcvc$expafterdischarge<-haiicu_expcvc$DateUnitDischarge-haiicu_expcvc$DateExpEnd
#haiicu_expcvc<-haiicu_expcvc%>%filter(!is.na(haiicu_expcvc$DateExpEnd),!is.na(haiicu_expcvc$DateUnitDischarge))
#haiicu_expcvc$DateExpEnd[as.numeric(haiicu_expcvc$expafterdischarge)<0]<-haiicu_expcvc$DateUnitDischarge[as.numeric(haiicu_expcvc$expafterdischarge)<0]
#haiicu_expcvc$expdays<-as.Date(haiicu_expcvc$DateExpEnd)-as.Date(haiicu_expcvc$DateExpStart)+1
haiicu_expcvc<-filter(haiicu_expcvc,ExpType=="CVC",expdays>0)
cvcexp_byicu<-haiicu_expcvc%>%select(UnitId,expdays)%>%group_by(UnitId)%>%summarise(unitexpdays=sum(as.numeric(expdays),na.rm=TRUE))

haiicu_unit_expcvc<-merge(haiicu_aggr,cvcexp_byicu,by.x="RecordId",by.y ="UnitId")
#haiicu_unit_expcvc<-filter(haiicu_unit_expcvc,unitexpdays>49)
saveRDS(haiicu_unit_expcvc,"haiicu_unit_expcvc.Rda")

#catheter utilisation rate ------------
cvc_rate<-round(mean(haiicu_unit_expcvc$unitexpdays/as.numeric(haiicu_unit_expcvc$NumPatDaysUnit2d),na.rm=TRUE)*1000,digits=2)
haiicu_country_expcvc<-haiicu_unit_expcvc%>%select(ReportingCountry,unitexpdays,NumPatDaysUnit2d)%>%group_by(ReportingCountry)%>%summarise(countryexpdays=sum(as.numeric(unitexpdays,na.rm=TRUE),na.rm=TRUE),patientdays=sum(as.numeric(NumPatDaysUnit2d), na.rm=TRUE))
haiicu_country_expcvc$utilrate<-round(as.numeric(haiicu_country_expcvc$countryexpdays)/as.numeric(haiicu_country_expcvc$patientdays),digits=2)
round(mean(haiicu_unit_expcvc$unitexpdays,na.rm=TRUE)/mean(as.numeric(haiicu_unit_expcvc$NumPatDaysUnit2d),na.rm=TRUE)*1000,digits=0)
saveRDS(haiicu_country_expcvc,"haiicu_country_expcvc.Rda")

#crbsi by exposure days -------
haiicu_crbsi<-merge(haiicu_pt,haiicu_unitstand_bsi,by.x="RecordId",by.y="ParentId",all.x=TRUE)
haiicu_crbsi<-haiicu_crbsi[, !duplicated(colnames(haiicu_crbsi))]
haiicu_crbsi<-select(haiicu_crbsi,ParentId,InfectionSite,BSIOrigin)
haiicu_crbsi<-filter(haiicu_crbsi,grepl("CRI3|BSI",haiicu_crbsi$InfectionSite))#|grepl("^C",haiicu_crbsi$BSIOrigin))

haiicu_crbsi$ParentId<-as.factor(haiicu_crbsi$ParentId)

#CVC-associated BSI cases based on date of onset and dates of exposure start and end ---------------------------
haiicu_cvcasbsi<-merge(haiicu_pt_inf_all[,c("Id","RecordId","UnitId","hasHai","InfectionSite","BSIOrigin", "dupl_pat","InvasiveDevice","DateOfOnset", "InfectionOutcome")],haiicuexp[,c("ParentId","ExpType","DateExpStart","DateExpEnd")],by.x="Id",by.y="ParentId")
haiicu_cvcasbsi<-filter(haiicu_cvcasbsi,grepl("CRI3|BSI",InfectionSite),grepl("C-CVC|^C$|UNK|UO|N/A",BSIOrigin))
haiicu_cvcasbsi<-filter(haiicu_cvcasbsi,ExpType=="CVC")
haiicu_cvcasbsi<-filter(haiicu_cvcasbsi,as.Date(DateOfOnset,format="%Y-%m-%d")>(as.Date(DateExpStart)+1),as.Date(DateOfOnset)<(as.Date(DateExpEnd)+2))
haiicu_cvcasbsi<-filter(haiicu_cvcasbsi,as.Date(DateExpEnd)-(as.Date(DateExpStart))>1)
haiicu_cvcasbsi<-distinct(haiicu_cvcasbsi,Id,DateOfOnset,.keep_all=TRUE)
haiicu_cvcasbsi$clabsi<-TRUE

haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,haiicu_cvcasbsi[,c("RecordId","clabsi")],by="RecordId",all=TRUE)
haiicu_pt_inf_all<-haiicu_pt_inf_all%>%unique()
#saveRDS(haiicu_pt_inf_all,file="haiicu_pt_inf_all_clabsi.Rda")
haiicu_cvcasbsiaggr<-haiicu_cvcasbsi
haiicu_cvcasbsiaggr<-select(haiicu_cvcasbsi,UnitId)
haiicu_cvcasbsiaggr<-count(haiicu_cvcasbsi,UnitId)
names(haiicu_cvcasbsiaggr)[names(haiicu_cvcasbsiaggr)=="n"]<-"CVCASBSI"
haiicu_unit_inccvcasbsi<-merge(haiicu_unit_inc,haiicu_cvcasbsiaggr,by="UnitId",all=TRUE)
haiicu_unit_inccvcasbsi[c("CVCASBSI")][is.na(haiicu_unit_inccvcasbsi[c("CVCASBSI")])]<-0
#end of CVC-associated BSI cases based on date of onset and dates of exposure start and end ----------------------------

#crbsin: all catheter related BSIs excluding cri3 (c-cvc, c-art, c-pvc, c), crin: all cri3 --------
haiicu_crbsi_byicu<-haiicu_crbsi%>%group_by(ParentId)%>%summarise(crbsin=sum(grepl("^C.*",BSIOrigin)),prbsi=sum(grepl("BSI|CRI3",InfectionSite)&(!grepl("^S-.*",BSIOrigin))),crin=sum(InfectionSite!="BSI"),cvcbsi=sum(InfectionSite=="CRI3-CVC"|BSIOrigin=="C-CVC"|BSIOrigin=="C"),cri3=sum(InfectionSite=="CRI3-CVC"))
haiicu_crbsi_byicu$totcrbsi<-haiicu_crbsi_byicu$crbsin+haiicu_crbsi_byicu$crin #OBS. totcrbsi incorrect


haiicu_unit_bsidevadj<-merge(haiicu_unit_expcvc,haiicu_crbsi_byicu,by.x="RecordId",by.y="ParentId",all.x=TRUE)
haiicu_unit_bsidevadj$totcrbsi[is.na(haiicu_unit_bsidevadj$totcrbsi)]<-0
haiicu_unit_bsidevadj$cvcbsi[is.na(haiicu_unit_bsidevadj$cvcbsi)]<-0
haiicu_unit_bsidevadj$cri3[is.na(haiicu_unit_bsidevadj$cri3)]<-0
haiicu_unit_bsidevadj$prbsi[is.na(haiicu_unit_bsidevadj$prbsi)]<-0

haiicu_unit_bsidevadj$devadjinc<-round(haiicu_unit_bsidevadj$cvcbsi/as.numeric(haiicu_unit_bsidevadj$unitexpdays)*1000,digits=2)
haiicu_unit_bsidevadj$devadjcri3<-round(haiicu_unit_bsidevadj$cri3/as.numeric(haiicu_unit_bsidevadj$unitexpdays)*1000,digits=2)
haiicu_unit_bsidevadj$prbsiinc<-round(haiicu_unit_bsidevadj$prbsi/as.numeric(haiicu_unit_bsidevadj$NumPatDaysUnit2d)*1000,digits=2)


#haiicu_unit_bsidevadj<-filter(haiicu_unit_bsidevadj,ReportingCountry!="LU") #FILTER OUT LUXEMBOURG*******
saveRDS(haiicu_unit_bsidevadj,"haiicu_unit_bsidevadj.Rda") #device adjusted crbsi rate by icu

#CLABSI table ------------

haiicu_unit_bsidevadj_cvcasbsitable<-merge(haiicu_unit_inccvcasbsi, haiicudenscountr[,c("UnitId","adm","avglos","expdays")], by="UnitId")
haiicu_unit_bsidevadj_cvcasbsitable<-rename(haiicu_unit_bsidevadj_cvcasbsitable,unitexpdays=expdays)
haiicu_unit_bsidevadj_cvcasbsitable$clabsiinc<-round(haiicu_unit_bsidevadj_cvcasbsitable$CVCASBSI/as.numeric(haiicu_unit_bsidevadj_cvcasbsitable$unitexpdays)*1000,digits=2)

haiicu_unit_bsidevadj_crbsitable<-merge(haiicu_unit_bsidevadj, haiicudenscountr[,c("UnitId","lengthofstay","adm","avglos")], by.x="RecordId",by.y="UnitId")
haiicu_unit_bsidevadj_totcritable<-merge(haiicu_unit_bsidevadj, haiicudenscountr[,c("UnitId","lengthofstay","adm","avglos")], by.x="RecordId", by.y="UnitId", all.x=TRUE)
haiicu_unit_bsidevadj_crbsitable<-filter(haiicu_unit_bsidevadj_crbsitable,adm>9)#,unitexpdays>49)
haiicu_unit_bsidevadj_cri3table<-merge(haiicu_unit_bsidevadj, haiicudenscountr[,c("UnitId","lengthofstay","adm","avglos")], by.x="RecordId",by.y="UnitId")
haiicu_unit_bsidevadj_prbsitable<-merge(haiicu_unit_bsidevadj, haiicudenscountr[,c("UnitId","lengthofstay","adm","avglos")], by.x="RecordId",by.y="UnitId", all.x=TRUE)
haiicu_unit_bsidevadj_prbsitable<-filter(haiicu_unit_bsidevadj_prbsitable,adm>9)#,unitexpdays>49)

haiicu_unit_bsidevadj_totcritable<-filter(haiicu_unit_bsidevadj_totcritable,adm>9)#,unitexpdays>49)

haiicu_unit_bsidevadj_clabsitable<-merge(haiicu_unit_bsidevadj,haiicu_unit_bsidevadj_cvcasbsitable[,c("UnitId","CVCASBSI","lengthofstay")],by.x="RecordId",by.y="UnitId")
haiicu_unit_bsidevadj_clabsitable$clabsiinc<-round(haiicu_unit_bsidevadj_clabsitable$CVCASBSI/as.numeric(haiicu_unit_bsidevadj_clabsitable$unitexpdays)*1000,digits=2)
clabsi_bycountry<-haiicu_unit_bsidevadj_clabsitable%>%group_by(ReportingCountry)%>%summarise(countrclabsiinc=round(mean(clabsiinc,na.rm=TRUE),digits=2)) # device adjusted crbsi rate by country
saveRDS(clabsi_bycountry,"clabsi_bycountry.Rda")
saveRDS(haiicu_unit_bsidevadj_cvcasbsitable,"haiicu_unit_bsidevadj_cvcasbsitable.Rda")

saveRDS(haiicu_unit_bsidevadj_clabsitable,"unit_clabsi.Rda")


eu_bsidevadj<-haiicu_unit_bsidevadj_totcritable%>%summarise(nricu=n(),nrpat=sum(adm,na.rm=TRUE),
                                                            avglos=round(mean(as.numeric(avglos),na.rm=TRUE), digits=2),
                                                            n_cvcdays=sum(unitexpdays,na.rm=TRUE),
                                                            cvcuse=round(1000*mean(unitexpdays,na.rm=TRUE)/mean(as.numeric(NumPatDaysUnit2d),na.rm=TRUE),digits=0),
                                                            n_totcrbsi=sum(totcrbsi,na.rm=TRUE),
                                                            aggrinc=round(1000*sum(totcrbsi,na.rm=TRUE)/sum(unitexpdays,na.rm=TRUE),digits=2),
                                                            avgcrbsirate=round(mean(devadjinc,na.rm=TRUE),digits=2),
                                                            crbsirate25pct=round(quantile(devadjinc,probs=c(0.25),na.rm=TRUE),digits=2),
                                                            crbsiratemedian=round(median(devadjinc,na.rm=TRUE),digits=2),
                                                            crbisrate75pct=round(quantile(devadjinc,probs=c(0.75),na.rm=TRUE),digits=2))

bsidevadj_bycountry<-haiicu_unit_bsidevadj_totcritable%>%group_by(ReportingCountry)%>%summarise(countrbsidai=round(mean(devadjinc,na.rm=TRUE),digits=2)) # device adjusted crbsi rate by country
cri3_bycountry<-haiicu_unit_bsidevadj_cri3table%>%group_by(ReportingCountry)%>%summarise(countrbsidai=round(mean(devadjcri3,na.rm=TRUE),digits=2)) # device adjusted cri3 rate by country
saveRDS(bsidevadj_bycountry,file="bsidevadj_bycountry.Rda")

bsidevadj_totcritable_bycountry<-haiicu_unit_bsidevadj_totcritable%>%group_by(ReportingCountry)%>%summarise(
          n_cvcdays=sum(unitexpdays,na.rm=TRUE),
          cvcuse=round(1000*mean(unitexpdays,na.rm=TRUE)/mean(as.numeric(NumPatDaysUnit2d),na.rm=TRUE),digits=0),
          n_totcrbsi=sum(totcrbsi,na.rm=TRUE),
          aggrinc=round(1000*sum(totcrbsi,na.rm=TRUE)/sum(unitexpdays,na.rm=TRUE),digits=2),
          avgcrbsirate=round(mean(devadjinc,na.rm=TRUE),digits=2),
          crbsirate25pct=round(quantile(devadjinc,probs=c(0.25),na.rm=TRUE),digits=2),
          crbsiratemedian=round(median(devadjinc,na.rm=TRUE),digits=2),
          crbisrate75pct=round(quantile(devadjinc,probs=c(0.75),na.rm=TRUE),digits=2))
#bsidevadj_totcritable_bycountry<-bind_rows(bsidevadj_totcritable_bycountry,eu_bsidevadj) #under development
saveRDS(bsidevadj_totcritable_bycountry,file="totcrbsitable.Rda")
saveRDS(haiicu_unit_bsidevadj_totcritable,file="unit_crbsi_table.Rda")


eu_prbsi<-haiicu_unit_bsidevadj_prbsitable%>%summarise( cvcdays=sum(unitexpdays,na.rm=TRUE),
                                                        cvcuse=round(1000*mean(unitexpdays,na.rm=TRUE)/mean(as.numeric(NumPatDaysUnit2d),na.rm=TRUE),digits=0),
                                                        n_prbsi=sum(PRBSI,na.rm=TRUE),
                                                        aggrinc=round(1000*sum(PRBSI,na.rm=TRUE)/sum(NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                                        avgprbsirate=round(mean(prbsiinc,na.rm=TRUE),digits=2),
                                                        prbsirate25pct=round(quantile(prbsiinc,probs=c(0.25),na.rm=TRUE),digits=2),
                                                        prbsiratemedian=round(median(prbsiinc,na.rm=TRUE),digits=2),
                                                        prbisrate75pct=round(quantile(prbsiinc,probs=c(0.75),na.rm=TRUE),digits=2))


prbsiinc_table_bycountry<-haiicu_unit_bsidevadj_prbsitable%>%group_by(ReportingCountry)%>%summarise(cvcdays=sum(unitexpdays,na.rm=TRUE),
                                                                                                    cvcuse=round(1000*mean(unitexpdays,na.rm=TRUE)/mean(as.numeric(NumPatDaysUnit2d),na.rm=TRUE),digits=0),
                                                                                                    pt_days=sum(NumPatDaysUnit2d,na.rm=TRUE),
                                                                                                    n_prbsi=sum(PRBSI,na.rm=TRUE),
                                                                                                    aggrinc=round(1000*sum(PRBSI,na.rm=TRUE)/sum(NumPatDaysUnit2d,na.rm=TRUE),digits=2),
                                                                                                    avgprbsirate=round(mean(prbsiinc,na.rm=TRUE),digits=2),
                                                                                                    prbsirate25pct=round(quantile(prbsiinc,probs=c(0.25),na.rm=TRUE),digits=2),
                                                                                                    prbsiratemedian=round(median(prbsiinc,na.rm=TRUE),digits=2),
                                                                                                    prbisrate75pct=round(quantile(prbsiinc,probs=c(0.75),na.rm=TRUE),digits=2))
#prbsiinc_table_bycountry<-bind_rows(prbsiinc_table_bycountry,eu_prbsi) #under development
saveRDS(prbsiinc_table_bycountry,file="prbsi_table.Rda")

bsidevadj_cri3table_bycountry<-haiicu_unit_bsidevadj_cri3table%>%group_by(ReportingCountry)%>%summarise(cvcdays=sum(unitexpdays,na.rm=TRUE),
                                                                                                        cvcuse=round(1000*mean(unitexpdays,na.rm=TRUE)/mean(as.numeric(NumPatDaysUnit2d),na.rm=TRUE),digits=0),
                                                                                                        n_CRI3=sum(CRI3,na.rm=TRUE),
                                                                                                        aggrinc=round(1000*sum(CRI3,na.rm=TRUE)/sum(unitexpdays,na.rm=TRUE),digits=2),
                                                                                                        avgcrbsirate=round(mean(devadjcri3,na.rm=TRUE),digits=2),
                                                                                                        crbsirate25pct=round(quantile(devadjcri3,probs=c(0.25),na.rm=TRUE),digits=2),
                                                                                                        crbsiratemedian=round(median(devadjcri3,na.rm=TRUE),digits=2),
                                                                                                        crbisrate75pct=round(quantile(devadjcri3,probs=c(0.75),na.rm=TRUE),digits=2))

saveRDS(bsidevadj_cri3table_bycountry,"cri3table.Rda")

#CLABSI incidence------------------

eu_cvcasbsi<-haiicu_unit_bsidevadj_clabsitable%>%summarise(nricu=n(),
                                                            n_cvcdays=sum(unitexpdays,na.rm=TRUE),
                                                            cvcuse=round(1000*mean(unitexpdays,na.rm=TRUE)/mean(as.numeric(NumPatDaysUnit2d),na.rm=TRUE),digits=0),
                                                            n_clabsi=sum(CVCASBSI,na.rm=TRUE),
                                                            aggrinc=round(1000*sum(CVCASBSI,na.rm=TRUE)/sum(unitexpdays,na.rm=TRUE),digits=2),
                                                            avgclabsirate=round(mean(clabsiinc,na.rm=TRUE),digits=2),
                                                            clabsirate25pct=round(quantile(clabsiinc,probs=c(0.25),na.rm=TRUE),digits=2),
                                                            clabbsiratemedian=round(median(clabsiinc,na.rm=TRUE),digits=2),
                                                            clabisrate75pct=round(quantile(clabsiinc,probs=c(0.75),na.rm=TRUE),digits=2))


bsidevadj_cvcasbsitable_bycountry<-haiicu_unit_bsidevadj_clabsitable%>%group_by(ReportingCountry)%>%summarise(nricu=n(),
                                                                                                            cvcexpdays=sum(unitexpdays,na.rm=TRUE),
                                                                                                            cvcuse=round(1000*mean(unitexpdays,na.rm=TRUE)/mean(as.numeric(lengthofstay),na.rm=TRUE),digits=0),
                                                                                                            n_clabsi=sum(CVCASBSI,na.rm=TRUE),
                                                                                                            aggrinc=round(1000*sum(CVCASBSI,na.rm=TRUE)/sum(unitexpdays,na.rm=TRUE),digits=2),
                                                                                                            avgclabsirate=round(mean(clabsiinc,na.rm=TRUE),digits=2),
                                                                                                            clabsirate25pct=round(quantile(clabsiinc,probs=c(0.25),na.rm=TRUE),digits=2),
                                                                                                            clabsratemedian=round(median(clabsiinc,na.rm=TRUE),digits=2),
                                                                                                            clabsisrate75pct=round(quantile(clabsiinc,probs=c(0.75),na.rm=TRUE),digits=2))

saveRDS(bsidevadj_cvcasbsitable_bycountry,"cvcasbsitable.Rda")

haiicu_unit_bsidevadj_crbsitable$cvcuse<-round(100*haiicu_unit_bsidevadj_crbsitable$unitexpdays/as.numeric(haiicu_unit_bsidevadj_crbsitable$NumPatDaysUnit2d),digits=2)

by_country<-group_by(haiicu_unit_bsidevadj_crbsitable,ReportingCountry)
CRBSItable<-summarise(by_country,nricu=n(),nrpat=sum(adm,na.rm=TRUE),
                    avglos=round(mean(avglos,na.rm=TRUE), digits=2),
                    cvcuse=round(mean(cvcuse,na.rm=TRUE),digits=2),
                    avgcrbsirate=round(mean(bsi_incdens,na.rm=TRUE),digits=2),
                    crbsirate25pct=round(quantile(bsi_incdens,probs=c(0.25),na.rm=TRUE),digits=2),
                    crbsiratemedian=round(median(bsi_incdens,na.rm=TRUE),digits=2),
                    crbisrate75pct=round(quantile(bsi_incdens,probs=c(0.75),na.rm=TRUE),digits=2))
saveRDS(CRBSItable,file="CRBSItable.Rda")

#BSI microbiology-----------------------------------

micro<-read.csv("4.HAIICU$PT$INF$RES.csv") #load 4.HAIICU$Pt$Inf$Res.csv
microlight<-read.csv("4.HAIICULIGHT$Deno$Inf$Res.csv") #load 4.HAIICULIGHT$Deno$Inf$Res.csv
inf<-read.csv("3.HAIICU$PT$INF.csv") #load 3.HAIICU$Pt$Inf.csv
inflight<-read.csv("3.HAIICULIGHT$Deno$Inf.csv") #load 3.HAIICULIGHT$Deno$Inf
BSI<-select(inf,RecordId,ParentId,InfectionSite)
BSI<-filter(BSI,grepl("BSI|CRI3",BSI$InfectionSite))
BSIlight<-select(inflight,RecordId,ParentId,InfectionSite)
BSIlight<-filter(BSIlight,grepl("BSI|CRI3",BSIlight$InfectionSite))



BSImicro<-merge(BSI[,c("RecordId","InfectionSite","ParentId")],micro[,c("Antibiotic","ResultIsolate","SIR","ParentId")],by.x="RecordId",by.y="ParentId",all=FALSE)
BSImicro<-select(BSImicro,RecordId,ParentId,InfectionSite,ResultIsolate)
BSImicro<-distinct(BSImicro)
BSImicrolight<-merge(BSIlight[,c("RecordId","InfectionSite","ParentId")],microlight[,c("Antibiotic","ResultIsolate","SIR","ParentId")],by.x="RecordId",by.y="ParentId",all=FALSE)
BSImicrolight<-select(BSImicrolight,RecordId,ParentId,InfectionSite,ResultIsolate)
BSImicrolight<-distinct(BSImicrolight)
#ptBSImicro<-merge(BSImicro[,c("RecordId","InfectionSite","Antibiotic","ResultIsolate","SIR", "ParentId")],pt[,c("RecordId","ParentId")],by.x="Id",by.y="RecordId",all=FALSE)
#ptBSImicrolight<-merge(BSImicrolight[,c("RecordId","InfectionSite","Antibiotic","ResultIsolate","SIR", "ParentId")],ptlight[,c("RecordId","ParentId")],by.x="Id",by.y="RecordId",all=FALSE)

pt<-read.csv("2.HAIICU$PT.csv")
standard<-haiicu_level1
lightdeno<-read.csv("2.HAIICULIGHT$Deno.csv")
light<-read.csv("1.HAIICULIGHT.csv")

ptBSImicro<-merge(BSImicro[,c("RecordId","InfectionSite","ResultIsolate", "ParentId")],pt[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)

ptBSImicro<-ptBSImicro[,-c(1)]
ptBSImicro<-ptBSImicro%>%rename(ParentId=ParentId.y)

denoBSImicrolight<-merge(BSImicrolight[,c("ParentId","InfectionSite","ResultIsolate", "RecordId")],lightdeno[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)
denoBSImicrolight<-denoBSImicrolight[,-c(1)]
denoBSImicrolight<-denoBSImicrolight%>%rename(ParentId=ParentId.y)
unitBSImicrolight<-merge(denoBSImicrolight[,c("ResultIsolate","ParentId")],light[,c("RecordId","ReportingCountry")],by.x="ParentId",by.y="RecordId",all=FALSE)
unitBSImicro<-merge(ptBSImicro[,c("ResultIsolate","ParentId")],standard[,c("RecordId","ReportingCountry")],by.x="ParentId",by.y="RecordId",all=FALSE)
unitBSImicrolight[,'ParentId'] <- as.factor(as.character(unitBSImicrolight[,'ParentId']))
BSImicroall<-rbind(unitBSImicro,unitBSImicrolight)



BSImicroall$Isolate<-ifelse(!grepl("_",BSImicroall$ResultIsolate),substr(BSImicroall$ResultIsolate,1,3),NA)
#BSImicroall$Isolate<-NA
BSImicroall$Isolate[grepl("PSEAER",BSImicroall$ResultIsolate)]<-"Pseudomonas aeruginosa"
BSImicroall$Isolate[grepl("STA...",BSImicroall$ResultIsolate)]<-"Coagulase-negative staphylococci"
BSImicroall$Isolate[grepl("STAAUR",BSImicroall$ResultIsolate)]<-"Staphylococcus aureus"
BSImicroall$Isolate[grepl("KLE...",BSImicroall$ResultIsolate)]<-"Klebsiella spp."
BSImicroall$Isolate[grepl("ESCCOL",BSImicroall$ResultIsolate)]<-"Escherichia coli"
BSImicroall$Isolate[grepl("CAN...",BSImicroall$ResultIsolate)]<-"Candida spp."
BSImicroall$Isolate[grepl("STEMAL",BSImicroall$ResultIsolate)]<-"Stenotrophomonas maltofphilia"
BSImicroall$Isolate[grepl("ENB...",BSImicroall$ResultIsolate)]<-"Enterobacter spp."
BSImicroall$Isolate[grepl("ACI...",BSImicroall$ResultIsolate)]<-"Acinetobacter spp."
BSImicroall$Isolate[grepl("ENC...",BSImicroall$ResultIsolate)]<-"Enterococcus spp."
BSImicroall$Isolate[grepl("SER...",BSImicroall$ResultIsolate)]<-"Serratia spp."
BSImicroall$Isolate[grepl("PRT...",BSImicroall$ResultIsolate)]<-"Proteus spp."



BSImicroall<-filter(BSImicroall,BSImicroall$ResultIsolate!="_NOEXA" & BSImicroall$ResultIsolate!="_STERI" & BSImicroall$Isolate!="BCT")


#BSImicrotable<-table(BSImicroall$Isolate,BSImicroall$ReportingCountry)
#propBSImicrotable<-addmargins(100*round(prop.table(BSImicrotable,2),digits=2))

BSItable_group<-group_by(BSImicroall,Isolate,ReportingCountry)%>%summarise(n_isol=n())
BSItable_group<-spread(BSItable_group,ReportingCountry,n_isol)

BSItable_group_sum<-BSItable_group
BSItable_group_sum$total=rowSums(BSItable_group[,c(2:10)],na.rm=TRUE)
BSItable_group_sum<-ungroup(BSItable_group_sum)

#BSItable_group_sum<-mutate(BSItable_group,total=rowSums(BSItable_group[,c(2:15)],na.rm=TRUE))
BSItable_group_top10<-top_n(BSItable_group_sum,n=10,total)

BSItable_group_top10<-BSItable_group_top10[order(-BSItable_group_top10$total),]

BSItable_group_top10<-mutate(BSItable_group_top10,ATpc=100*round(AT/sum(AT,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,BEpc=100*round(BE/sum(BE,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,CZpc=100*round(CZ/sum(CZ,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,EEpc=100*round(EE/sum(EE,na.rm=TRUE),digits=3))


BSItable_group_top10<-mutate(BSItable_group_top10,FRpc=100*round(FR/sum(FR,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,DEpc=100*round(DE/sum(DE,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,HUpc=100*round(HU/sum(HU,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,ITpc=100*round(IT/sum(IT,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,ITSPINUTIpc=100*round(eval(as.symbol("IT-SPIN-UTI"))/sum(eval(as.symbol("IT-SPIN-UTI")),na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,ITGiViTIpc=100*round(eval(as.symbol("IT-GiViTI"))/sum(eval(as.symbol("IT-GiViTI")),na.rm=TRUE),digits=3))

BSItable_group_top10<-mutate(BSItable_group_top10,LTpc=100*round(LT/sum(LT,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,LUpc=100*round(LU/sum(LU,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,MTpc=100*round(MT/sum(MT,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,PTpc=100*round(PT/sum(PT,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,ROpc=100*round(RO/sum(RO,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,SKpc=100*round(SK/sum(SK,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,PLpc=100*round(PL/sum(PL,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,ESpc=100*round(ES/sum(ES,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,UKpc=100*round(UK/sum(UK,na.rm=TRUE),digits=3))
BSItable_group_top10<-mutate(BSItable_group_top10,totalpc=100*round(total/sum(total,na.rm=TRUE),digits=3))

bsi_out <- build_top10_country_tables(BSItable_group_top10)

summaryBSItable_top10 <- bsi_out$summary
BSItable_group_totals <- bsi_out$totals
BSItable_group_top10_pc <- bsi_out$pc
saveRDS(BSItable_group_top10_pc,file="BSItable_group_top10_pc.Rda")




#UTI--------------------------
haiicu_aggr_uti<-filter(haiicu_aggr,ReportingCountry!="FR",ReportingCountry!="BE",ReportingCountry!="MT",ReportingCountry!="UK",ReportingCountry!="CZ")
UTI_incid_aggr<-sum(haiicu_aggr_uti$UTI,na.rm=TRUE)/(sum(as.numeric(haiicu_aggr_uti$NumPatDaysUnit2d),na.rm=TRUE))*1000
haiicu_aggr_uti$UTI_incdens<-round(haiicu_aggr_uti$UTI/as.numeric(haiicu_aggr_uti$NumPatDaysUnit2d)*1000,digits=2)
#average UTI incidence per ICU
avg_UTI_incdens<-mean(haiicu_aggr_uti$UTI_incdens,na.rm=TRUE)
#proportion of patients in standard protocol with UTI
haiicu_pt_uti<-filter(haiicu_pt_inf_all,ReportingCountry!="FR",ReportingCountry!="BE",ReportingCountry!="MT",ReportingCountry!="UK")
UTI_pat_prop<-round(sum(grepl("UTI",haiicu_pt_uti$InfectionSite))/sum(!haiicu_pt_uti$dupl_pat)*100,digits=2)
haiicu_unitstand_UTI<-select(haiicu_pt_inf,RecordId,Id,InfectionSite,InvasiveDevice)
haiicu_unitstand_UTI<-filter(haiicu_unitstand_UTI,grepl("^UTI",haiicu_unitstand_UTI$InfectionSite))
haiicu_unitstand_UTI<-rename(haiicu_unitstand_UTI,ParentId=Id)
saveRDS(haiicu_pt_uti,"haiicu_pt_uti.Rda")
saveRDS(haiicu_aggr_uti,"haiicu_aggr_uti.Rda")

#UTI in light protocol
haiicu_unitlight_UTI<-read.csv("3.HAIICULIGHT$Deno$Inf.csv") #load 3.HAIICULIGHT$Deno$Inf.csv
haiicu_unitlight_UTI<-select(haiicu_unitlight_UTI,RecordId,ParentId,InfectionSite,InvasiveDevice)
haiicu_unitlight_UTI<-filter(haiicu_unitlight_UTI,grepl("^UTI",haiicu_unitlight_UTI$InfectionSite))
haiicu_unitlight_UTI[,'RecordId'] <- as.factor(as.character(haiicu_unitlight_UTI[,'RecordId']))
haiicu_unitlight_UTI[,'ParentId'] <- as.factor(as.character(haiicu_unitlight_UTI[,'ParentId']))
haiicu_unitall_UTI<-rbind(haiicu_unitstand_UTI,haiicu_unitlight_UTI)#merge standard and light UTI
saveRDS(haiicu_unitall_UTI,file="haiicu_unitall_UTI.Rda")


#urinary catheter days estimation
haiicu_expuc<-select(haiicuexp,Id,RecordId,UnitId,ExpType,DateExpStart,DateExpEnd)
haiicu_expuc$expdays<-as.Date(haiicu_expuc$DateExpEnd)-as.Date(haiicu_expuc$DateExpStart)+1
haiicu_expuc<-filter(haiicu_expuc,ExpType=="UC")
ucexp_byicu<-haiicu_expuc%>%select(UnitId,expdays)%>%group_by(UnitId)%>%summarise(unitexpdays=sum(as.numeric(expdays)))
haiicu_unit_expuc<-merge(haiicu_aggr,ucexp_byicu,by.x="RecordId",by.y ="UnitId")
haiicu_unit_expuc<-filter(haiicu_unit_expuc,ReportingCountry!="FR",ReportingCountry!="BE",ReportingCountry!="MT", ReportingCountry!="UK")



#cauti device adjusted rate
haiicu_unit_expuc<-haiicu_unit_expuc%>%filter(unitexpdays>9)
haiicu_unit_expuc$utidevadj<-round(1000*haiicu_unit_expuc$UTI/haiicu_unit_expuc$unitexpdays,digits=2)
saveRDS(haiicu_unit_expuc,"haiicu_unit_expuc.Rda")

#catheter utilisation rate
uc_rate<-round(mean(haiicu_unit_expuc$unitexpdays/as.numeric(haiicu_unit_expuc$NumPatDaysUnit2d),na.rm=TRUE)*1000,digits=2)
haiicu_country_expuc<-haiicu_unit_expuc%>%select(ReportingCountry,unitexpdays,NumPatDaysUnit2d,UTI,utidevadj)%>%group_by(ReportingCountry)%>%
            summarise(countryexpdays=sum(as.numeric(unitexpdays),na.rm=TRUE),patientdays=sum(as.numeric(NumPatDaysUnit2d),na.rm=TRUE),
                      CAUTIn=sum(UTI,na.rm=TRUE),aggr=round(1000*sum(UTI,na.rm=TRUE)/sum(unitexpdays,na.rm=TRUE),digits=1),meaninc=mean(utidevadj,na.rm=TRUE),
                      cauti25pct=round(quantile(utidevadj,probs=c(0.25),na.rm=TRUE),digits=2),
                      cautimedian=round(median(utidevadj,na.rm=TRUE),digits=2),
                      cauti75pct=round(quantile(utidevadj,probs=c(0.75),na.rm=TRUE),digits=2))
haiicu_country_expuc$utilrate<-round(as.numeric(haiicu_country_expuc$countryexpdays)/as.numeric(haiicu_country_expuc$patientdays),digits=2)
round(mean(haiicu_unit_expuc$unitexpdays,na.rm=TRUE)/mean(as.numeric(haiicu_unit_expuc$NumPatDaysUnit2d),na.rm=TRUE)*1000,digits=0)
saveRDS(haiicu_country_expuc,"haiicu_country_expuc.Rda")

#UTI microbiology--------------------------------

micro<-read.csv("4.HAIICU$Pt$Inf$Res.csv") #load 4.HAIICU$Pt$Inf$Res.csv
microlight<-read.csv("4.HAIICULIGHT$Deno$Inf$Res.csv") #load 4.HAIICULIGHT$Deno$Inf$Res.csv
inf<-read.csv("3.HAIICU$PT$INF.csv") #load 3.HAIICU$Pt$Inf.csv
inflight<-read.csv("3.HAIICULIGHT$Deno$Inf.csv") #load 3.HAIICULIGHT$Deno$Inf
UTI<-select(inf,RecordId,ParentId,InfectionSite)
UTI<-filter(UTI,grepl("^UTI",UTI$InfectionSite))
UTIlight<-select(inflight,RecordId,ParentId,InfectionSite)
UTIlight<-filter(UTIlight,grepl("^UTI",UTIlight$InfectionSite))



UTImicro<-merge(UTI[,c("RecordId","InfectionSite","ParentId")],micro[,c("Antibiotic","ResultIsolate","SIR","ParentId")],by.x="RecordId",by.y="ParentId",all=FALSE)
UTImicro<-select(UTImicro,RecordId,ParentId,InfectionSite,ResultIsolate)
UTImicro<-distinct(UTImicro)
UTImicrolight<-merge(UTIlight[,c("RecordId","InfectionSite","ParentId")],microlight[,c("Antibiotic","ResultIsolate","SIR","ParentId")],by.x="RecordId",by.y="ParentId",all=FALSE)
UTImicrolight<-select(UTImicrolight,RecordId,ParentId,InfectionSite,ResultIsolate)
UTImicrolight<-distinct(UTImicrolight)
#ptUTImicro<-merge(UTImicro[,c("RecordId","InfectionSite","ResultIsolate", "Id")],pt[,c("RecordId","ParentId")],by.x="Id",by.y="RecordId",all=FALSE)
#ptUTImicrolight<-merge(UTImicrolight[,c("RecordId","InfectionSite","Antibiotic","ResultIsolate","SIR", "ParentId")],ptlight[,c("RecordId","ParentId")],by.x="Id",by.y="RecordId",all=FALSE)

pt<-read.csv("2.HAIICU$PT.csv")
standard<-haiicu_level1
lightdeno<-read.csv("2.HAIICULIGHT$Deno.csv")
light<-read.csv("1.HAIICULIGHT.csv")

ptUTImicro<-merge(UTImicro[,c("RecordId","InfectionSite","ResultIsolate", "ParentId")],pt[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)
ptUTImicro<-ptUTImicro[,-c(1)]
ptUTImicro<-ptUTImicro%>%rename(ParentId=ParentId.y)
denoUTImicrolight<-merge(UTImicrolight[,c("ParentId","InfectionSite","ResultIsolate", "RecordId")],lightdeno[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)
denoUTImicrolight<-denoUTImicrolight[,-c(1)]
denoUTImicrolight<-denoUTImicrolight%>%rename(ParentId=ParentId.y)
unitUTImicrolight<-merge(denoUTImicrolight[,c("ResultIsolate","ParentId")],light[,c("RecordId","ReportingCountry")],by.x="ParentId",by.y="RecordId",all=FALSE)

unitUTImicro<-merge(ptUTImicro[,c("ResultIsolate","ParentId")],standard[,c("RecordId","ReportingCountry")],by.x="ParentId",by.y="RecordId",all=FALSE)
unitUTImicrolight[,'ParentId'] <- as.factor(as.character(unitUTImicrolight[,'ParentId']))
UTImicroall<-rbind(unitUTImicro,unitUTImicrolight)


UTImicroall$Isolate<-ifelse(!grepl("_",UTImicroall$ResultIsolate),substr(UTImicroall$ResultIsolate,1,3),NA)
#UTImicroall$Isolate<-NA
UTImicroall$Isolate[grepl("PSEAER",UTImicroall$ResultIsolate)]<-"Pseudomonas aeruginosa"
UTImicroall$Isolate[grepl("STA...",UTImicroall$ResultIsolate)]<-"Coagulase-negative staphylococci"
UTImicroall$Isolate[grepl("STAAUR",UTImicroall$ResultIsolate)]<-"Staphylococcus aureus"
UTImicroall$Isolate[grepl("KLE...",UTImicroall$ResultIsolate)]<-"Klebsiella spp."
UTImicroall$Isolate[grepl("ESCCOL",UTImicroall$ResultIsolate)]<-"Escherichia coli"
UTImicroall$Isolate[grepl("CAN...",UTImicroall$ResultIsolate)]<-"Candida spp."
UTImicroall$Isolate[grepl("STEMAL",UTImicroall$ResultIsolate)]<-"Stenotrophomonas maltofphilia"
UTImicroall$Isolate[grepl("ENB...",UTImicroall$ResultIsolate)]<-"Enterobacter spp."
UTImicroall$Isolate[grepl("ACI...",UTImicroall$ResultIsolate)]<-"Acinetobacter spp."
UTImicroall$Isolate[grepl("ENC...",UTImicroall$ResultIsolate)]<-"Enterococcus spp."
UTImicroall$Isolate[grepl("SER...",UTImicroall$ResultIsolate)]<-"Serratia spp."
UTImicroall$Isolate[grepl("PRT...",UTImicroall$ResultIsolate)]<-"Proteus spp."
UTImicroall$Isolate[grepl("CIT...",UTImicroall$ResultIsolate)]<-"Citrobacter spp."



UTImicroall<-filter(UTImicroall,UTImicroall$ResultIsolate!="_NOEXA" & UTImicroall$ResultIsolate!="_STERI")


#UTImicrotable<-table(UTImicroall$Isolate,UTImicroall$ReportingCountry)
#propUTImicrotable<-addmargins(100*round(prop.table(UTImicrotable,2),digits=2))

UTItable_group<-group_by(UTImicroall,Isolate,ReportingCountry)%>%summarise(n_isol=n())
UTItable_group<-UTItable_group[!is.na(UTItable_group$Isolate),]
UTItable_group<-spread(UTItable_group,ReportingCountry,n_isol)

UTItable_group_sum<-UTItable_group
#uncomment next line for full report
#UTItable_group_sum$total=rowSums(UTItable_group[,c(2:9)],na.rm=TRUE)
UTItable_group_sum$total=rowSums(UTItable_group[,c(2:9)],na.rm=TRUE)
UTItable_group_sum<-ungroup(UTItable_group_sum)

#UTItable_group_sum<-mutate(UTItable_group,total=rowSums(UTItable_group[,c(2:9)],na.rm=TRUE))
UTItable_group_top10<-top_n(UTItable_group_sum,n=10,total)

UTItable_group_top10<-UTItable_group_top10[order(-UTItable_group_top10$total),]

UTItable_group_top10<-mutate(UTItable_group_top10,ATpc=100*round(AT/sum(AT,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,BEpc=100*round(BE/sum(BE,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,EEpc=100*round(EE/sum(EE,na.rm=TRUE),digits=3))

UTItable_group_top10<-mutate(UTItable_group_top10,FRpc=100*round(FR/sum(FR,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,DEpc=100*round(DE/sum(DE,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,HUpc=100*round(HU/sum(HU,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,ITpc=100*round(IT/sum(IT,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,ITSPINUTIpc=100*round(eval(as.symbol("IT-SPIN-UTI"))/sum(eval(as.symbol("IT-SPIN-UTI")),na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,ITGiViTIpc=100*round(eval(as.symbol("IT-GiViTI"))/sum(eval(as.symbol("IT-GiViTI")),na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,LTpc=100*round(LT/sum(LT,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,LUpc=100*round(LU/sum(LU,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,PTpc=100*round(PT/sum(PT,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,ROpc=100*round(RO/sum(RO,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,SKpc=100*round(SK/sum(SK,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,ESpc=100*round(ES/sum(ES,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,UKpc=100*round(UK/sum(UK,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,MTpc=100*round(MT/sum(MT,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,PLpc=100*round(PL/sum(PL,na.rm=TRUE),digits=3))
UTItable_group_top10<-mutate(UTItable_group_top10,totalpc=100*round(total/sum(total,na.rm=TRUE),digits=3))


uti_out <- build_top10_country_tables(UTItable_group_top10)

summaryUTItable_top10 <- uti_out$summary
UTItable_group_totals <- uti_out$totals
UTItable_group_top10_pc <- uti_out$pc
saveRDS(UTItable_group_top10_pc,file="UTItable_group_top10_pc.Rda")

microlight[,'RecordId'] <- as.factor(as.character(microlight[,'RecordId']))
microlight[,'ParentId'] <- as.factor(as.character(microlight[,'ParentId']))
resist<-rbind(micro,microlight)
resist$Isolate[grepl("PSEAER",resist$ResultIsolate)]<-"Pseudomonas aeruginosa"
resist$Isolate[grepl("STA...",resist$ResultIsolate)]<-"Coagulase-negative staphylococci"
resist$Isolate[grepl("STAAUR",resist$ResultIsolate)]<-"Staphylococcus aureus"
resist$Isolate[grepl("KLE...",resist$ResultIsolate)]<-"Klebsiellas"
resist$Isolate[grepl("ESCCOL",resist$ResultIsolate)]<-"Escherichia coli"
resist$Isolate[grepl("CAN...",resist$ResultIsolate)]<-"Candida spp."
resist$Isolate[grepl("STEMAL",resist$ResultIsolate)]<-"Stenotrophomonas maltofphilia"
resist$Isolate[grepl("ENB...",resist$ResultIsolate)]<-"Enterobacter"
resist$Isolate[grepl("ACI...",resist$ResultIsolate)]<-"Acinetobacter spp."
resist$Isolate[grepl("ENC...",resist$ResultIsolate)]<-"Enterococcus"
resist$Isolate[grepl("SER...",resist$ResultIsolate)]<-"Serratia spp."
resist$Isolate[grepl("PRT...",resist$ResultIsolate)]<-"Proteus spp."
resist$Isolate[grepl("CIT...",resist$ResultIsolate)]<-"Citrobacter spp."
resist$Antibiotic[grepl("CAZ|CTX",resist$Antibiotic)]<-"C3G"
resist$Antibiotic[grepl("VAN",resist$Antibiotic)]<-"GLY"
resist$Antibiotic[grepl("ESBL",resist$Antibiotic)]<-"C3G"
resist$Antibiotic[grepl("IPM|MEM",resist$Antibiotic)]<-"CAR"
resist$Isolate<-as.factor(resist$Isolate)
reportingcountry<-select(haiicu_pt_inf_all,InfectionId,ReportingCountry)
reportingcountry$InfectionId<-as.character(reportingcountry$InfectionId)
resist<-left_join(resist,reportingcountry,by=c("ParentId"="InfectionId"))
resist<-resist%>%filter(!is.na(ReportingCountry),ReportingCountry!="DE", SIR!="UNK")
resist<-resist%>%mutate(uniq=(!duplicated(ParentId))) # to measure number of isolates
#_NOTEST results for STAAUR in DE considered as OXA-S (MSSA)

resist<-resist%>%mutate(ReportingCountry=case_when(is.na(ReportingCountry) & RecordType=="HAIICULIGHT$Deno$Inf$Res"~"DE", TRUE~ReportingCountry),
                        SIR=case_when(ReportingCountry=="DE" & ResultIsolate=="STAAUR" & Antibiotic=="_NOTEST"~ "S", TRUE~SIR),
                                      Antibiotic=case_when(ReportingCountry=="DE" & ResultIsolate=="STAAUR" & Antibiotic=="_NOTEST"~ "OXA", TRUE~Antibiotic))

resist<-resist%>%filter(!is.na(ReportingCountry), ReportingCountry!="DE")
saveRDS(resist,"resist.Rda")

#resistance percentage by country and pathogen
country_res<-resist%>%group_by(ReportingCountry)%>%summarise(
                MRSA=round(100*sum(ResultIsolate=="STAAUR" & SIR %in% c("R","IR") & Antibiotic %in% c("OXA","MET"),na.rm=TRUE)/sum(ResultIsolate=="STAAUR" & Antibiotic %in% c("OXA","MET"),na.rm=TRUE),digits=1),
                VRE=round(100*sum(Isolate=="Enterococcus" & SIR %in% c("R","IR") & Antibiotic %in% c("GLY","_NOTEST"),na.rm=TRUE)/sum(Isolate=="Enterococcus" & Antibiotic%in% c("GLY"),na.rm=TRUE),digits=1),
                CEFRPS=round(100*sum(ResultIsolate=="PSEAER" & SIR=="R" & Antibiotic=="C3G",na.rm=TRUE)/sum(ResultIsolate=="PSEAER" & Antibiotic %in% c("C3G"),na.rm=TRUE),digits=1),
                C3GREC=round(100*sum(ResultIsolate=="ESCCOL" & SIR=="R" & Antibiotic=="C3G",na.rm=TRUE)/sum(ResultIsolate=="ESCCOL" & Antibiotic%in% c("C3G"),na.rm=TRUE),digits=1),
                C3GRKP=round(100*sum(Isolate=="Klebsiellas" & SIR=="R" & Antibiotic=="C3G",na.rm=TRUE)/sum(Isolate=="Klebsiellas" & Antibiotic%in% c("C3G"),na.rm=TRUE),digits=1),
                C3GRENT=round(100*sum(Isolate=="Enterobacter" & SIR=="R" & Antibiotic=="C3G",na.rm=TRUE)/sum(Isolate=="Enterobacter" & Antibiotic%in% c("C3G"),na.rm=TRUE),digits=1),
                CRKP=round(100*sum(Isolate=="Klebsiellas" & SIR=="R" & Antibiotic=="CAR",na.rm=TRUE)/sum(Isolate=="Klebsiellas" & Antibiotic %in% c("CAR"),na.rm=TRUE),digits=1),
                CREC=round(100*sum(ResultIsolate=="ESCCOL" & SIR=="R" & Antibiotic=="CAR",na.rm=TRUE)/sum(ResultIsolate=="ESCCOL" & Antibiotic%in% c("CAR"),na.rm=TRUE),digits=1),
                CRENT=round(100*sum(Isolate=="Enterobacter" & SIR=="R" & Antibiotic=="CAR",na.rm=TRUE)/sum(Isolate=="Enterobacter" & Antibiotic%in% c("CAR"),na.rm=TRUE),digits=1),
                CRPS=round(100*sum(ResultIsolate=="PSEAER" & SIR=="R" & Antibiotic=="CAR",na.rm=TRUE)/sum(ResultIsolate=="PSEAER" & Antibiotic%in% c("CAR"),na.rm=TRUE),digits=1),
                CRAB=round(100*sum(ResultIsolate=="ACIBAU" & SIR=="R" & Antibiotic=="CAR")/sum(ResultIsolate=="ACIBAU" & Antibiotic%in% c("CAR")),digits=1)
)

#resistance percentage EU
EU_res<-resist%>%summarise(
  MRSA=round(100*sum(ResultIsolate=="STAAUR" & SIR %in% c("R","IR") & Antibiotic %in% c("OXA","MET"),na.rm=TRUE)/sum(ResultIsolate=="STAAUR" & Antibiotic %in% c("OXA","MET"),na.rm=TRUE),digits=1),
  VRE=round(100*sum(Isolate=="Enterococcus" & SIR %in% c("R","IR") & Antibiotic %in% c("GLY","_NOTEST"),na.rm=TRUE)/sum(Isolate=="Enterococcus" & Antibiotic%in% c("GLY"),na.rm=TRUE),digits=1),
  CEFRPS=round(100*sum(ResultIsolate=="PSEAER" & SIR=="R" & Antibiotic=="C3G",na.rm=TRUE)/sum(ResultIsolate=="PSEAER" & Antibiotic %in% c("C3G"),na.rm=TRUE),digits=1),
  C3GREC=round(100*sum(ResultIsolate=="ESCCOL" & SIR=="R" & Antibiotic=="C3G",na.rm=TRUE)/sum(ResultIsolate=="ESCCOL" & Antibiotic%in% c("C3G"),na.rm=TRUE),digits=1),
  C3GRKP=round(100*sum(Isolate=="Klebsiellas" & SIR=="R" & Antibiotic=="C3G",na.rm=TRUE)/sum(Isolate=="Klebsiellas" & Antibiotic%in% c("C3G"),na.rm=TRUE),digits=1),
  C3GRENT=round(100*sum(Isolate=="Enterobacter" & SIR=="R" & Antibiotic=="C3G",na.rm=TRUE)/sum(Isolate=="Enterobacter" & Antibiotic%in% c("C3G"),na.rm=TRUE),digits=1),
  CRKP=round(100*sum(Isolate=="Klebsiellas" & SIR=="R" & Antibiotic=="CAR",na.rm=TRUE)/sum(Isolate=="Klebsiellas" & Antibiotic %in% c("CAR"),na.rm=TRUE),digits=1),
  CREC=round(100*sum(ResultIsolate=="ESCCOL" & SIR=="R" & Antibiotic=="CAR",na.rm=TRUE)/sum(ResultIsolate=="ESCCOL" & Antibiotic%in% c("CAR"),na.rm=TRUE),digits=1),
  CRENT=round(100*sum(Isolate=="Enterobacter" & SIR=="R" & Antibiotic=="CAR",na.rm=TRUE)/sum(Isolate=="Enterobacter" & Antibiotic%in% c("CAR"),na.rm=TRUE),digits=1),
  CRPS=round(100*sum(ResultIsolate=="PSEAER" & SIR=="R" & Antibiotic=="CAR",na.rm=TRUE)/sum(ResultIsolate=="PSEAER" & Antibiotic%in% c("CAR"),na.rm=TRUE),digits=1),
  CRAB=round(100*sum(ResultIsolate=="ACIBAU" & SIR=="R" & Antibiotic=="CAR")/sum(ResultIsolate=="ACIBAU" & Antibiotic%in% c("CAR")),digits=1)
)

#Infection Outcome
InfOutc_EU<-haiicu_pt_inf_all%>%group_by(InfectionOutcome)%>%filter(!is.na(InfectionOutcome),InfectionOutcome!="N/A",InfectionOutcome!="UNK",InfectionOutcome!="")%>%summarise(n = n()) %>%
  mutate(freq = n / sum(n))
InfOutc<-haiicu_pt_inf_all%>%group_by(ReportingCountry,InfectionOutcome)%>%filter(!is.na(InfectionOutcome),InfectionOutcome!="N/A",InfectionOutcome!="UNK",InfectionOutcome!="")%>%summarise(n = n()) %>%
  mutate(freq = n / sum(n))%>%select(-n)%>%spread(InfectionOutcome,freq)
saveRDS(InfOutc,"InfOutc.Rda")

#unit data
haiicu_level1<-read.csv("1.HAIICU.csv")
levels(haiicu_level1$ReportingCountry)<-c(levels(haiicu_level1$ReportingCountry),"IT-GiViTI","IT-SPIN-UTI")
haiicu_level1$ReportingCountry[haiicu_level1$ReportingCountry=="IT"]<-haiicu_level1$DataSource[haiicu_level1$ReportingCountry=="IT"]#Replace IT with network name

#exposure data####
haiicuexp<-read.csv("haiicuexp_correct.csv")
haiicuexp<-select(haiicuexp,RecordId,ParentId,DateExpStart, DateExpEnd,ExpType)
haiicuexp <- haiicuexp %>% mutate(across(c(DateExpStart, DateExpEnd), parse_mixed_date))
haiicuexp$expdays<-as.Date(haiicuexp$DateExpEnd,format="%Y-%m-%d")-as.Date(haiicuexp$DateExpStart,format="%Y-%m-%d")+1
expint<-filter(haiicuexp, ExpType=="INT")
expcvc<-filter(haiicuexp,ExpType=="CVC")
expuc<-filter(haiicuexp,ExpType=="UC")

ptexpint<-expint%>%select(ParentId,expdays)%>%group_by(ParentId)%>%summarise(expintdays=sum(expdays,na.rm=TRUE))
ptexpcvc<-expcvc%>%select(ParentId,expdays)%>%group_by(ParentId)%>%summarise(expcvcdays=sum(expdays,na.rm=TRUE))
ptexpuc<-expuc%>%select(ParentId,expdays)%>%group_by(ParentId)%>%summarise(expucdays=sum(expdays,na.rm=TRUE))



#merge patient infection and exposure data
haiicu_pt_inf_all<-readRDS("haiicu_pt_inf_all.Rda")
haiicu_pt_inf_all$BSI<-grepl("BSI|CRI3-CVC",haiicu_pt_inf_all$InfectionSite)
haiicu_pt_inf_all$PN<-grepl("PN",haiicu_pt_inf_all$InfectionSite)
haiicu_pt_inf_all$UTI<-grepl("UTI",haiicu_pt_inf_all$InfectionSite)
haiicu_pt_inf_all$CRI<-grepl("CRI3",haiicu_pt_inf_all$InfectionSite)|grepl("C-CVC",haiicu_pt_inf_all$BSIOrigin)
haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,ptexpint,by.x="Id",by.y="ParentId",all=TRUE)
haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,ptexpcvc,by.x="Id",by.y="ParentId",all=TRUE)
haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,ptexpuc,by.x="Id",by.y="ParentId",all=TRUE)

haiicu_iap<-merge(haiicu_pt_inf_all[,c("Id","RecordId","UnitId","hasHai","InfectionSite","dupl_pat","InvasiveDevice","DateOfOnset", "InfectionOutcome")],
                  haiicuexp[,c("ParentId","ExpType","DateExpStart","DateExpEnd")],by.x="Id",by.y="ParentId")
haiicu_iap<-filter(haiicu_iap,grepl("PN",InfectionSite))
haiicu_iap<-filter(haiicu_iap,ExpType=="INT")
haiicu_iap<-filter(haiicu_iap,as.Date(DateOfOnset,format="%d/%m/%Y")>(as.Date(DateExpStart,format="%Y-%m-%d")-1),as.Date(DateOfOnset)<(as.Date(DateExpEnd,format="%Y-%m-%d")+3))
haiicu_iap<-distinct(haiicu_iap,Id,DateOfOnset, .keep_all=TRUE)
haiicu_iap$IAP<-"TRUE"
haiicu_iap<-select(haiicu_iap,RecordId,IAP)
haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,haiicu_iap,by="RecordId",all.x=TRUE)
haiicu_pt_inf_all<-haiicu_pt_inf_all%>%unique()

by_unit<-group_by(haiicu_pt_inf_all,UnitId)
by_country<-group_by(haiicu_pt_inf_all,ReportingCountry)

#patient demographics and clinical characteristics output by unit------------------------------------------
unit_output_pat<-by_unit%>%filter(!dupl_pat,lengthofstay>0)%>%
  summarise(N_pat=n(),patdays=sum(as.numeric(lengthofstay),na.rm=TRUE),
            Gender_F_pc=pct(Gender=="F",N_pat),
            Age_mean=mean(Age,na.rm=TRUE),
            SapsII_mean=mean(SapsII,na.rm=TRUE),
            Origin_HOSP_pc=pct(PatientOrigin=="HOSP",N_pat),
            ImpImmun_pc=(prc(sum(ImpairedImmunity=="Y",na.rm=TRUE)/n())),
            Intub_pc=pct(Intubation=="Y",N_pat),
            AntimicrUnit_pc=pct(AntimicrobialInUnit=="Y",N_pat),
            Trauma_pc=pct(Trauma=="Y",N_pat),
            TypeAdm_med=pct(TypeOfAdmission=="MED",N_pat),
            TypeAdm_ssur=pct(TypeOfAdmission=="SSUR",N_pat),
            TypeAdm_usur=pct(TypeOfAdmission=="USUR",N_pat),
            UrinCath_pc=pct(UrinaryCatheter=="Y",N_pat),
            CVC_pc=pct(CVC=="Y",N_pat),
            IntUse_per100patdays=pct(expintdays,patdays),
            CvcUse_per100patdays=pct(expcvcdays,patdays),
            UcUse_per100patdays=pct(expucdays,patdays),
            HAI_perc=pct(hasHai=="TRUE",N_pat),
            Outcome_D_pc=(prc(sum(OutcomeUnit=="D",na.rm=TRUE)/n())),
            Age_p50=median(as.numeric(Age),na.rm=TRUE),
            Age_p25=quantile(as.numeric(Age),c(0.25),na.rm=TRUE),
            Age_p75=quantile(as.numeric(Age),c(0.75),na.rm=TRUE),
            SapsII_p50=median(SapsII,na.rm=TRUE),
            SapsII_p25=quantile(SapsII,c(0.25),na.rm=TRUE),
            SapsII_p75=quantile(SapsII,c(0.75),na.rm=TRUE)
  )

apache<-by_unit%>%filter(!dupl_pat)%>%summarise(Apache_mean=mean(OtherScoreValue,na.rm=TRUE),
                                                Apache_p50=median(OtherScoreValue,na.rm=TRUE),
                                                Apache_p25=quantile(OtherScoreValue,c(0.25),na.rm=TRUE),
                                                Apache_p75=quantile(OtherScoreValue,c(0.75),na.rm=TRUE)
)

#infection data by unit-----------------------------------------------
unit_output_inf<-by_unit%>%summarise(N_pat=n(),intdays=sum(as.numeric(expintdays)*(!dupl_pat),na.rm=TRUE),cvcdays=sum(as.numeric(expcvcdays)*(!dupl_pat),na.rm=TRUE),
                                     inc_PN=pct(PN=="TRUE",N_pat),
                                     inc_IAP=pct(IAP=="TRUE",intdays),
                                     inc_BSI=pct(BSI=="TRUE",N_pat),
                                     inc_CRI=pct(CRI=="TRUE",cvcdays),
                                     ucdays=sum(expucdays*(!dupl_pat),na.rm=TRUE),
                                     inc_UTI=pct(UTI=="TRUE",N_pat),
                                     bsi=sum(InfectionSite=="BSI"|InfectionSite=="CRI3-CVC",na.rm=TRUE),
                                     PrBSI=pct(BSIOrigin=="UO"|BSIOrigin=="UNK",bsi),
                                     CRI=pct(BSIOrigin=="C-CVC"|InfectionSite=="CRI3-CVC",bsi),
                                     SSSI=pct(BSIOrigin=="S-SSI",bsi),
                                     SPUL=pct(BSIOrigin=="S-PUL",bsi),
                                     SUTI=pct(BSIOrigin=="S-UTI",bsi),
                                     SDIG=pct(BSIOrigin=="S-DIG",bsi),
                                     SSST=pct(BSIOrigin=="S-SST",bsi)
)

unit_output_inf_ncases<-by_unit%>%summarise(PN_cases=sum(PN=="TRUE",na.rm=TRUE),
                                            IAP_cases=sum(IAP=="TRUE",na.rm=TRUE),
                                            BSI_cases=sum(BSI=="TRUE",na.rm=TRUE),
                                            CRI_cases=sum(CRI=="TRUE",na.rm=TRUE),
                                            UTI_cases=sum(UTI=="TRUE",na.rm=TRUE)

)



#generate unique ICU identifier with Country and Id-------------------------------------
unit_output_pat<-merge(unit_output_pat,haiicu_pt_inf_all[,c("ReportingCountry","UnitId")],by="UnitId")
unit_output_pat<-unique(unit_output_pat)
unit_output_pat<-mutate(unit_output_pat,unitcode=paste(ReportingCountry,UnitId,sep=""))
unit_output_inf<-merge(unit_output_inf,unit_output_pat[,c("UnitId","unitcode","ReportingCountry")],by="UnitId")
unit_output_inf_ncases<-merge(unit_output_inf_ncases,unit_output_pat[,c("UnitId","unitcode","ReportingCountry")],by="UnitId")
apache<-merge(apache,haiicu_pt_inf_all[,c("ReportingCountry","UnitId")],by="UnitId",all.x=FALSE,all.y=FALSE)
apache<-unique(apache)
apache<-merge(apache,unit_output_pat[,c("UnitId","unitcode")],by="UnitId")


#patient data by country
by_country<-group_by(unit_output_pat,ReportingCountry)
#country_output_unit<-by_country%>%select(-UnitId,-unitcode)%>%summarise_at(vars(-ReportingCountry),funs(sum(as.numeric(.),na.rm=TRUE),
#                                                      mean(as.numeric(.),na.rm=TRUE),
#                                                      median(as.numeric(.),na.rm=TRUE),
#                                                      p25(as.numeric(.)),
#                                                      p75(as.numeric(.)))
#                                                      )%>%
#                                    select(-(Gender_F_pc_sum:Outcome_D_pc_sum)
#)

country_output_unit<-by_country%>%select(-UnitId,-unitcode)%>%summarise(across(where(is.numeric),list(sum=sum,mean= mean, median=median, p25=p25,p75=p75)))


#apache data by country
apache_by_country<-group_by(apache,ReportingCountry)
country_apache_unit<-apache_by_country%>%select(-UnitId,-unitcode)%>%summarise(across(where(is.numeric),list(mean=mean, median=median, p25=p25,p75=p75)))

#patient data EU
EU_ref_pat<-unit_output_pat%>%select(-UnitId,-unitcode)%>%summarise(across(where(is.numeric),list(sum=sum,mean=mean,median=median, p25=p25,p75=p75)))




#infection data by country
by_country_inf<-group_by(unit_output_inf,ReportingCountry)
country_output_unit_inf<-by_country_inf%>%select(-UnitId,-unitcode)%>%summarise(across(where(is.numeric),list(sum=sum,mean=mean,median=median,p25=p25,p75=p75)))

by_country_inf_ncases<-group_by(unit_output_inf_ncases,ReportingCountry)
country_output_unit_inf_ncases<-by_country_inf_ncases%>%select(-UnitId,-unitcode)%>%
  summarise(across(where(is.numeric),list(sum=sum)))


#infection data EU
EU_ref_inf<-unit_output_inf%>%select(-UnitId,-unitcode)%>%summarise(across(where(is.numeric),list(sum=sum,mean=mean,median=median,p25=p25,p75=p75)))

#Indicators-------------------------------------
# icu_deno<-read.csv("haiicu_deno.csv")
# icu_deno_ind<-read.csv("haiicu_deno_ind.csv")
# icu_deno_ind_spread<-select(icu_deno_ind,-RecordId)
# icu_deno_ind_spread<-icu_deno_ind_spread%>%
#               mutate(IndPcCompl=as.numeric(as.character(IndNumCompliant))/as.numeric(as.character(IndNumObservations)))%>%
#               select(-IndNumCompliant,-IndNumObservations)%>%spread(IndicatorCode,IndPcCompl)
# icu_deno_ind_all<-merge(icu_deno,icu_deno_ind_spread,by.x="RecordId",by.y="ParentId")
# icu_deno_ind_all<-select(icu_deno_ind_all,-RecordType.x,-RecordType.y,-PeriodStart,-PeriodEnd,-AuditStart,-AuditEnd)
# icu_deno_ind_all[icu_deno_ind_all=="UNK"]<-NA
#
# icu_deno_ind_all[,3:9]<-lapply(icu_deno_ind_all[,3:9],unfactor)
# icu_deno_ind_all<-merge(icu_deno_ind_all,haiicu_level1[,c("ReportingCountry","RecordId")],by.x="ParentId",by.y="RecordId")
# icu_deno_ind_all<-rename(icu_deno_ind_all, UnitId=ParentId)
# icu_deno_ind_all<-mutate(icu_deno_ind_all,unitcode=paste(ReportingCountry,UnitId,sep=""))
# icu_deno_ind_all[,10:14]<-lapply(icu_deno_ind_all[,10:14],prc)
#
# #remaining indicators and surveillance period
#icu_unit<-merge(haiicu_level1[,c("RecordId","NumAlcoholHandRubLiters","NumPatientDaysPrevYear","UnitSize","UnitSpecialty")], icu_deno[,c("ParentId","PeriodStart","PeriodEnd")],by.x="RecordId",by.y="ParentId")
haiicu_level1$LocalId<-paste(haiicu_level1$HospitalId,haiicu_level1$UnitId,sep="-")
icu_unit<-merge(haiicu_level1[,c("RecordId","NumAlcoholHandRubLiters","NumPatientDaysPrevYear","UnitSize","UnitSpecialty","LocalId","DateUsedForStatistics")],unit_output_pat[,c("UnitId","unitcode","ReportingCountry")],by.x="RecordId",by.y="UnitId")
#icu_unit<-mutate(icu_unit,alc_cons=NumAlcoholHandRubLiters/NumPatientDaysPrevYear*100)
#
# alcohol_by_country<-group_by(icu_unit,ReportingCountry)%>%summarise(alc_mean=mean(alc_cons,na.rm=TRUE),
#                                                                     alc_median=median(alc_cons,na.rm=TRUE),
#                                                                     alc_p25=p25(alc_cons),
#                                                                     alc_p75=p75(alc_cons))
#
# alcohol_EU<-icu_unit%>%summarise(alc_mean=mean(alc_cons,na.rm=TRUE),
#                                  alc_median=median(alc_cons,na.rm=TRUE),
#                                  alc_p25=p25(alc_cons),
#                                  alc_p75=p75(alc_cons))
#
#
# #EU reference indicators
# EU_ref_ind<-icu_deno_ind_all%>%summarise_each(funs(sum(as.numeric(.),na.rm=TRUE),
#                                                    mean(as.numeric(.),na.rm=TRUE),
#                                                    median(as.numeric(.),na.rm=TRUE),
#                                                    p25(as.numeric(.)),
#                                                    p75(as.numeric(.))),
#                                                    -unitcode)
#
# #indicators by country
# by_country_ind<-group_by(icu_deno_ind_all,ReportingCountry)
# country_output_ind<-by_country_ind%>%summarise_each(funs(sum(as.numeric(.),na.rm=TRUE),
#                                                          mean(as.numeric(.),na.rm=TRUE),
#                                                          median(as.numeric(.),na.rm=TRUE),
#                                                          p25(as.numeric(.)),
#                                                          p75(as.numeric(.))),
#                                                          -ReportingCountry,-RecordId,-unitcode)
#

#Antibiotic use####
ab<-read.csv("3.HAIICU$PT$AM.csv")
pt<-read.csv("2.HAIICU$PT.csv")
unit<-read.csv("1.HAIICU.csv")
ab<-select(ab,-RecordId)
ab<-rename(ab,pt_id=ParentId)
pt<-select(pt,RecordId,ParentId,DateUnitAdmission,DateUnitDischarge)
pt<-rename(pt,pt_id=RecordId,unit_id=ParentId)
unit<-select(unit,RecordId,ReportingCountry)
unit<-rename(unit,UnitId=RecordId)
ab_pt<-merge(pt,ab,by.x="pt_id",by.y="pt_id",all=TRUE)

ab_pt$DateUnitAdmission<-as.Date(ab_pt$DateUnitAdmission)
ab_pt$DateUnitDischarge<-as.Date(ab_pt$DateUnitDischarge)
ab_pt$DateAntimicrobialEnd<-as.Date(ab_pt$DateAntimicrobialEnd)
ab_pt$DateAntimicrobialStart<-as.Date(ab_pt$DateAntimicrobialStart)

ab_pt<-filter(ab_pt,!is.na(DateAntimicrobialEnd))
#write.csv(ab_pt,"ab_pt.csv") #save as csv for manual cleaning of false dates
#ab_pt<-read.csv("ab_pt.csv") #load corrected file but needs to run as.Date functions again
#ab_pt$DateAntimicrobialStart[ab_pt$DateAntimicrobialStart<ab_pt$DateUnitAdmission]<-ab_pt$DateUnitAdmission #intend to clean wrong dates but produces errors
#ab_pt$DateAntimicrobialEnd[ab_pt$DateAntimicrobialEnd>ab_pt$DateUnitDischarge]<-ab_pt$DateUnitDischarge
ab_pt$treatmdays<-ab_pt$DateAntimicrobialEnd-ab_pt$DateAntimicrobialStart+1
ab_pt<-merge(ab_pt,haiicu_level1[,c("RecordId","ReportingCountry")],by.x="unit_id",by.y="RecordId") #get ReportingCountry variable


by_unit_ab<-group_by(ab_pt,unit_id)
by_country_ab<-group_by(ab_pt,ReportingCountry)

unit_ab<-by_unit_ab%>%summarise(N_ab=n(),Carb=sum(grepl("J01DH..",ATCCode)),carb_pc=pct(grepl("J01DH..",ATCCode),N_ab),Carb_d=sum(treatmdays[grepl("J01DH..",ATCCode)]),
                                piptaz=sum(grepl("J01CR05",ATCCode)),piptaz_pc=pct(grepl("J01CR05",ATCCode),N_ab),piptaz_d=sum(treatmdays[grepl("J01CR05",ATCCode)]),
                                Ceph12=sum(grepl("J01DB..|J01DC..",ATCCode)),Ceph12_pc=pct(grepl("J01DB..|J01DC..",ATCCode),N_ab),Ceph12_d=sum(treatmdays[grepl("J01DB..|J01DC..",ATCCode)]),
                                Ceph34=sum(grepl("J01DD..|J01DG..",ATCCode)),Ceph34_pc=pct(grepl("J01DD..|J01DG..",ATCCode),N_ab),Ceph34_d=sum(treatmdays[grepl("J01DD..|J01DG..",ATCCode)]),
                                FQ=sum(grepl("J01MA..",ATCCode)),FQ_pc=pct(grepl("J01MA..",ATCCode),N_ab),FQ_d=sum(treatmdays[grepl("J01MA",ATCCode)]),
                                Glycop=sum(grepl("J01XA..",ATCCode)),Glycop_pc=pct(grepl("J01XA..",ATCCode),N_ab),Glycop_d=sum(treatmdays[grepl("J01XA",ATCCode)]),
                                Polymyx=sum(grepl("J01XB..",ATCCode)),Polymyx_pc=pct(grepl("J01XB..",ATCCode),N_ab),Polymyx_d=sum(treatmdays[grepl("J01XB",ATCCode)]),
                                treatmdays_tot=sum(treatmdays,na.rm=TRUE),empiric=sum(treatmdays[AntimicrobialIndication=="E"],na.rm=TRUE),directed=sum(treatmdays[AntimicrobialIndication=="M"],na.rm=TRUE),
                                prophylactic=sum(treatmdays[AntimicrobialIndication=="P"],na.rm=TRUE)
)


unit_ab<-merge(unit_ab,unit,by.x="unit_id",by.y="UnitId")
unit_ab<-merge(unit_ab,unit_output_pat[,c("UnitId","patdays")],by.x="unit_id",by.y="UnitId")
unit_ab$patdays<-as.numeric(unit_ab$patdays)
unit_indication=select(unit_ab,unit_id, ReportingCountry, N_ab,patdays,treatmdays_tot, empiric,directed, prophylactic)
unit_indication=mutate(unit_indication,unitcode=paste(ReportingCountry,unit_id,sep=""))
unit_ab<-select(unit_ab, -treatmdays_tot, -empiric, -directed, -prophylactic)

#calculate treatment courses per 1000 patient days (pd) and treatment days per 1000 patient days (td)
unit_ab<-mutate(unit_ab, carb_pd=round(Carb/patdays*1000,digits=2),piptaz_pd=round(piptaz/patdays*1000,digits=2), ceph12_pd=round(Ceph12/patdays*1000,digits=2),
                ceph34_pd=round(Ceph34/patdays*1000,digits=2), FQ_pd=round(FQ/patdays*1000,digits=2),Glycop_pd=round(Glycop/patdays*1000,digits=2),
                Polymyx_pd=round(Polymyx/patdays*1000,digits=2),carb_td=round(Carb_d/patdays*1000,digits=2),piptaz_td=round(piptaz_d/patdays*1000,digits=2), ceph12_td=round(Ceph12_d/patdays*1000,digits=2),
                ceph34_td=round(Ceph34_d/patdays*1000,digits=2), FQ_td=round(FQ_d/patdays*1000,digits=2),Glycop_td=round(Glycop_d/patdays*1000,digits=2),
                Polymyx_td=round(Polymyx_d/patdays*1000,digits=2),unitcode=paste(ReportingCountry,unit_id,sep=""))


country_ab<-group_by(unit_ab,ReportingCountry)
country_output_ab<-country_ab%>%select(-unit_id,-unitcode)%>%summarise(across(where(is.numeric),list(mean=mean,median=median,p25=p25,p75=p75)))

unit_indication<-mutate(unit_indication, empiric_td=round(empiric/as.numeric(treatmdays_tot)*100,digits=2),
                        directed_td=round(directed/as.numeric(treatmdays_tot)*100,digits=2),
                        prophylactic_td=round(prophylactic/as.numeric(treatmdays_tot)*100,digits=2)
)
country_ab_indication<-group_by(unit_indication,ReportingCountry)
country_output_ab_indication<-country_ab_indication%>%select(-unit_id,-unitcode, -N_ab,patdays, -treatmdays_tot, -empiric, -directed, -prophylactic)%>%
  summarise(across(where(is.numeric),list(mean=mean,median=median,p25=p25,p75=p75 )))



#Microbiology
micro<-read.csv("4.HAIICU$Pt$Inf$Res.csv")
inf<-read.csv("3.HAIICU$Pt$inf.csv")
inf<-select(inf,RecordId,ParentId,InfectionSite)
PN<-filter(inf,grepl("PN",inf$InfectionSite))
BSI<-filter(inf,grepl("BSI|CRI3-CVC",inf$InfectionSite))
PNmicro<-merge(PN[,c("RecordId","InfectionSite","ParentId")],micro[,c("Antibiotic","ResultIsolate","SIR","ParentId")],by.x="RecordId",by.y="ParentId",all=FALSE)
PNmicro<-select(PNmicro,RecordId,ParentId,InfectionSite,ResultIsolate)
PNmicro<-distinct(PNmicro)
BSImicro<-merge(BSI[,c("RecordId","InfectionSite","ParentId")],micro[,c("Antibiotic","ResultIsolate","SIR","ParentId")],by.x="RecordId",by.y="ParentId",all=FALSE)
BSImicro<-select(BSImicro,RecordId,ParentId,InfectionSite,ResultIsolate)
pt<-read.csv("2.HAIICU$Pt.csv")

ptPNmicro<-merge(PNmicro[,c("RecordId","InfectionSite","ResultIsolate", "ParentId")],pt[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)
ptPNmicro<-ptPNmicro[,-c(1)]
ptPNmicro<-ptPNmicro%>%rename(ParentId=ParentId.y)
ptPNmicro<-distinct(ptPNmicro)
unitPNmicro<-merge(ptPNmicro[,c("ResultIsolate","ParentId")],haiicu_level1[,c("RecordId","ReportingCountry")],by.x="ParentId",by.y="RecordId",all=FALSE)
unitPNmicro<-merge(unitPNmicro,unit_output_pat[,c("UnitId","unitcode")],by.x="ParentId",by.y="UnitId")


ptBSImicro<-merge(BSImicro[,c("RecordId","InfectionSite","ResultIsolate", "ParentId")],pt[,c("RecordId","ParentId")],by.x="ParentId",by.y="RecordId",all=FALSE)
ptBSImicro<-ptBSImicro[,-c(1)]
ptBSImicro<-ptBSImicro%>%rename(ParentId=ParentId.y)
ptBSImicro<-distinct(ptBSImicro)
unitBSImicro<-merge(ptBSImicro[,c("ResultIsolate","ParentId")],haiicu_level1[,c("RecordId","ReportingCountry")],by.x="ParentId",by.y="RecordId",all=FALSE)
unitBSImicro<-merge(unitBSImicro,unit_output_pat[,c("UnitId","unitcode")],by.x="ParentId",by.y="UnitId")

#bsi_micro<-group_by(BSImicro,ResultIsolate)
PNmicro_by_unit<-group_by(unitPNmicro,unitcode)
unit_micro_pn<-PNmicro_by_unit%>%summarise(N_isol=n(),
                                           PSEAER=pct(ResultIsolate=="PSEAER",N_isol),
                                           STAAUR=pct(ResultIsolate=="STAAUR",N_isol),
                                           KLEPNE=pct(ResultIsolate=="KLEPNE",N_isol),
                                           ACISPP=pct(grepl("ACI",ResultIsolate),N_isol),
                                           ESCCOL=pct(ResultIsolate=="ESCCOL",N_isol))

BSImicro_by_unit<-group_by(unitBSImicro,unitcode)
unit_micro_BSI<-BSImicro_by_unit%>%summarise(N_isol=n(),
                                             PSEAER=pct(ResultIsolate=="PSEAER",N_isol),
                                             STAAUR=pct(ResultIsolate=="STAAUR",N_isol),
                                             KLEPNE=pct(ResultIsolate=="KLEPNE",N_isol),
                                             ACISPP=pct(grepl("ACI",ResultIsolate),N_isol),
                                             ESCCOL=pct(ResultIsolate=="ESCCOL",N_isol),
                                             STAEPI=pct(ResultIsolate=="STAEPI",N_isol),
                                             ENCFAI=pct(ResultIsolate=="ENCFAI",N_isol)
)


unit_micro_BSI<-merge(unit_micro_BSI, unit_output_pat[,c("UnitId","unitcode","ReportingCountry")],by="unitcode")
by_country_BSImicro<-group_by(unit_micro_BSI,ReportingCountry)
country_unit_microBSI<-by_country_BSImicro%>%select(-unitcode)%>%summarise(across(where(is.numeric),list(sum=sum,mean=mean,median=median,p25=p25,p75=p75)))


unit_micro_PN<-merge(unit_micro_pn, unit_output_pat[,c("UnitId","unitcode","ReportingCountry")],by="unitcode")
by_country_PNmicro<-group_by(unit_micro_PN,ReportingCountry)
country_unit_microPN<-by_country_PNmicro%>%select(-unitcode)%>%summarise(across(where(is.numeric),list(sum=sum,mean=mean,median=median,p25=p25,p75=p75)))


#resistance####
microres<-merge(inf[,c("RecordId","InfectionSite","ParentId")],micro[,c("Antibiotic","ResultIsolate","SIR","ParentId")],
                by.x="RecordId",by.y="ParentId",all=FALSE)
ptmicrores<-merge(microres[,c("RecordId","ResultIsolate","InfectionSite","Antibiotic","SIR","ParentId")],pt[,c("RecordId","ParentId")],
                  by.x="ParentId",by.y="RecordId",all=FALSE)
ptmicrores<-ptmicrores[,-c(1)]
ptmicrores<-ptmicrores%>%rename(ParentId=ParentId.y)

unitmicrores<-merge(ptmicrores[,c("ResultIsolate","InfectionSite","Antibiotic","SIR","ParentId")],haiicu_level1[,c("RecordId","ReportingCountry")],
                    by.x="ParentId",by.y="RecordId",all=FALSE)
unitmicrores<-merge(unitmicrores,unit_output_pat[,c("UnitId","unitcode")],by.x="ParentId",by.y="UnitId")

microres_by_unit<-group_by(unitmicrores,unitcode)
unit_micro_res<-microres_by_unit%>%
  summarise(pseaercar=sum(ResultIsolate=="PSEAER" & Antibiotic=="CAR",na.rm=TRUE),
            pseaercarR=prc(sum(ResultIsolate=="PSEAER" & Antibiotic=="CAR" & SIR=="R",na.rm=TRUE)/pseaercar),
            acibaucar=sum(ResultIsolate=="ACIBAU" & Antibiotic=="CAR",na.rm=TRUE),
            acibaucarR=prc(sum(ResultIsolate=="ACIBAU" & Antibiotic=="CAR" & SIR=="R",na.rm=TRUE)/acibaucar),
            staaurmet=sum(ResultIsolate=="STAAUR" & Antibiotic=="OXA",na.rm=TRUE),
            staaurmetR=prc(sum(ResultIsolate=="STAAUR" & Antibiotic=="OXA" & SIR=="R",na.rm=TRUE)/staaurmet),
            klepnecar=sum(ResultIsolate=="KLEPNE" & Antibiotic=="CAR",na.rm=TRUE),
            klepnecarR=prc(sum(ResultIsolate=="KLEPNE" & Antibiotic=="CAR" & SIR=="R",na.rm=TRUE)/klepnecar),
            esccolc3g=sum(ResultIsolate=="ESCCOL" & Antibiotic=="C3G",na.rm=TRUE),
            esccolc3gR=prc(sum(ResultIsolate=="ESCCOL" & Antibiotic=="C3G" & SIR=="R",na.rm=TRUE)/esccolc3g))

unit_micro_res<-merge(unit_micro_res, unit_output_pat[,c("UnitId","unitcode","ReportingCountry")],by="unitcode")
by_country_microres<-group_by(unit_micro_res,ReportingCountry)
country_unit_microres<-by_country_microres%>%select(-unitcode)%>%summarise(across(where(is.numeric),list(sum=sum,mean=mean,median=median,p25=p25,p75=p75)))




#reference data
ref<-haiicu_level1%>%group_by(ReportingCountry,DateUsedForStatistics)%>%summarise(n=n())
ref$refdata<-refdata

#demographics 28/08/2017 ----------------------------------------------------------------------

#ICU data----
country_unit<-group_by(haiicu_level1_all,ReportingCountry)
country_unit_table<-country_unit%>%summarise(N=n(), unit_size_median=median(as.numeric(as.character(UnitSize)),na.rm=TRUE),
                                             Spec_med=pct(UnitSpecialty=="MED",N),
                                             Spec_sur=pct(UnitSpecialty=="SURG",N),
                                             Spec_mix=pct(UnitSpecialty=="MIX",N),
                                             Spec_coro=pct(UnitSpecialty=="CORO",N),
                                             Spec_med=pct(UnitSpecialty=="MED",N),
                                             Spec_ounk=pct(UnitSpecialty=="O"|UnitSpecialty=="Unk"|UnitSpecialty=="NEU"|
                                                             UnitSpecialty=="BURN"|UnitSpecialty=="PED"|UnitSpecialty=="NEON",N)
                                             )
saveRDS(country_unit_table,"country_unit_table.Rda")

#exposure data-----
haiicuexp<-read.csv("haiicuexp_correct.csv")
haiicuexp<-select(haiicuexp,RecordId,ParentId,DateExpStart, DateExpEnd,ExpType)
haiicuexp$expdays<-as.numeric(as.Date(haiicuexp$DateExpEnd,format="%Y-%m-%d")-as.Date(haiicuexp$DateExpStart,format="%Y-%m-%d")+1)
expint<-filter(haiicuexp, ExpType=="INT")
expcvc<-filter(haiicuexp,ExpType=="CVC")
expuc<-filter(haiicuexp,ExpType=="UC")

ptexpint<-expint%>%select(ParentId,expdays)%>%group_by(ParentId)%>%summarise(expintdays=sum(expdays,na.rm=TRUE))
ptexpcvc<-expcvc%>%select(ParentId,expdays)%>%group_by(ParentId)%>%summarise(expcvcdays=sum(expdays,na.rm=TRUE))
ptexpuc<-expuc%>%select(ParentId,expdays)%>%group_by(ParentId)%>%summarise(expucdays=sum(expdays,na.rm=TRUE))

haiicu_pt_inf_all<-readRDS("haiicu_pt_inf_all.Rda")
haiicu_pt_inf_all$BSI<-grepl("BSI|CRI3-CVC",haiicu_pt_inf_all$InfectionSite)
haiicu_pt_inf_all$PN<-grepl("PN",haiicu_pt_inf_all$InfectionSite)
haiicu_pt_inf_all$UTI<-grepl("UTI",haiicu_pt_inf_all$InfectionSite)
haiicu_pt_inf_all$CRI<-grepl("CRI3",haiicu_pt_inf_all$InfectionSite)|grepl("C-CVC|^C$",haiicu_pt_inf_all$BSIOrigin)
haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,ptexpint,by.x="Id",by.y="ParentId",all=TRUE)
haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,ptexpcvc,by.x="Id",by.y="ParentId",all=TRUE)
haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,ptexpuc,by.x="Id",by.y="ParentId",all=TRUE)
haiicu_pt_inf_all$Age<-as.numeric(as.character(haiicu_pt_inf_all$Age))
haiicu_pt_inf_all$lengthofstay<-as.numeric(haiicu_pt_inf_all$lengthofstay)
haiicu_pt_inf_all$dupl_pt2<-duplicated(haiicu_pt_inf_all$Id)

haiicu_iap<-merge(haiicu_pt_inf_all[,c("Id","RecordId","UnitId","hasHai","InfectionSite","dupl_pat","InvasiveDevice","DateOfOnset", "InfectionOutcome")],
                  haiicuexp[,c("ParentId","ExpType","DateExpStart","DateExpEnd")],by.x="Id",by.y="ParentId")
haiicu_iap<-filter(haiicu_iap,grepl("PN",InfectionSite))
haiicu_iap<-filter(haiicu_iap,ExpType=="INT")

#haiicu_iap<-haiicu_iap%>%group_by(Id)%>%mutate(dateofonsetprev=lag(DateOfOnset))#added on 13/11/2017 to address cases with multiple reporting of the same infection
#haiicu_iap<-haiicu_iap%>%group_by(Id)%>%mutate(diffdateofonset=as.numeric(as.Date(DateOfOnset)-as.numeric(as.Date(dateofonsetprev))))
#haiicu_iap<-haiicu_iap%>%group_by(UnitId)%>%filter(is.na(diffdateofonset) | diffdateofonset>7)

haiicu_iap<-filter(haiicu_iap,as.Date(DateOfOnset)>(as.Date(DateExpStart)-1),as.Date(DateOfOnset)<(as.Date(DateExpEnd)+3))
haiicu_iap<-distinct(haiicu_iap,Id,DateOfOnset, .keep_all=TRUE)
haiicu_iap$IAP<-"TRUE"
haiicu_iap<-select(haiicu_iap,RecordId,IAP)

haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,haiicu_iap,by="RecordId",all.x=TRUE)

haiicu_pt_inf_all<-merge(haiicu_pt_inf_all,haiicu_cvcasbsi[,c("RecordId","clabsi")],by="RecordId",all=TRUE)
haiicu_pt_inf_all<-haiicu_pt_inf_all%>%unique()
#iap exclude if dates of onset too close (<8 days)
#haiicu_pt_inf_all<-haiicu_pt_inf_all%>%group_by(Id)%>%mutate(dateofonsetprev=lag(DateOfOnset))
#haiicu_pt_inf_all<-haiicu_pt_inf_all%>%group_by(Id)%>%mutate(diffdateofonset=as.numeric(as.Date(DateOfOnset)-as.numeric(as.Date(dateofonsetprev))))
#haiicu_pt_inf_all<-haiicu_pt_inf_all%>%group_by(UnitId)%>%filter(is.na(diffdateofonset) | (diffdateofonset>7 & InfectionSite==lag(InfectionSite))


by_country_demogr<-group_by(haiicu_pt_unit,ReportingCountry)
country_demogr<-by_country_demogr%>%summarise(N_pat=n(),patdays=sum(as.numeric(los),na.rm=TRUE),
                                               avg_los=round(sum(as.numeric(los),na.rm=TRUE)/n(),digits=1),
                                               Gender_F_pc=pct(Gender=="F",N_pat),
                                               Age_median=round(median(as.numeric(Age),na.rm=TRUE),digits=1),
                                               SapsII_median=round(median(as.numeric(SapsII),na.rm=TRUE),digits=1),
                                               Origin_HOSP_pc=pct(PatientOrigin=="HOSP",N_pat),
                                               Trauma_pc=pct(Trauma=="Y",N_pat),
                                               TypeAdm_med=pct(TypeOfAdmission=="MED",N_pat),
                                               TypeAdm_ssur=pct(TypeOfAdmission=="SSUR",N_pat),
                                               TypeAdm_usur=pct(TypeOfAdmission=="USUR",N_pat),
                                               Intub_pc=pct(Intubation=="Y",N_pat),
                                               UrinCath_pc=pct(UrinaryCatheter=="Y",N_pat),
                                               CVC_pc=pct(CVC=="Y",N_pat),
                                               ImpImmun_pc=(prc(sum(ImpairedImmunity=="Y",na.rm=TRUE)/n())),
                                               AntimicrUnit_pc=pct(AntimicrobialInUnit=="Y",N_pat),
                                               Outcome_D_pc=(prc(sum(OutcomeUnit=="D",na.rm=TRUE)/n()))
)
saveRDS(country_demogr,"country_demogr.Rda")


#end of demographics 28/08/2017 ---------------------------------------------------------------

#antibiotic use 28/08/2017---------------------------------------------------------------------

ab<-read.csv("3.HAIICU$PT$AM.csv")
pt<-read.csv("2.HAIICU$PT.csv")
unit<-read.csv("1.HAIICU.csv")
levels(unit$ReportingCountry)<-c(levels(unit$ReportingCountry),"IT-GiViTI","IT-SPIN-UTI")
unit$ReportingCountry[unit$ReportingCountry=="IT"]<-unit$DataSource[unit$ReportingCountry=="IT"]#Replace IT with network name

ab<-select(ab,-RecordId)
ab<-rename(ab,pt_id=ParentId)
pt<-select(pt,RecordId,ParentId,DateUnitAdmission,DateUnitDischarge)
pt<-rename(pt,pt_id=RecordId,unit_id=ParentId)
unit<-select(unit,RecordId,ReportingCountry)
unit<-rename(unit,UnitId=RecordId)
ab_pt<-merge(pt,ab,by.x="pt_id",by.y="pt_id",all=TRUE)

ab_pt$DateUnitAdmission<-as.Date(ab_pt$DateUnitAdmission, format="%d/%m/%Y")
ab_pt$DateUnitDischarge<-as.Date(ab_pt$DateUnitDischarge, format="%d/%m/%Y")
ab_pt<-ab_pt%>%filter(DateUnitDischarge>DateUnitAdmission+1,!is.na(DateUnitDischarge))#exclude patients staying less than two days
ab_pt$DateAntimicrobialEnd<-as.Date(ab_pt$DateAntimicrobialEnd, format="%d/%m/%Y")
ab_pt$DateAntimicrobialStart<-as.Date(ab_pt$DateAntimicrobialStart, format="%d/%m/%Y")

ab_pt<-filter(ab_pt,!is.na(DateAntimicrobialEnd),!is.na(DateUnitDischarge))
ab_pt<-filter(ab_pt,!is.na(DateAntimicrobialStart),!is.na(DateUnitAdmission))
ab_pt$errordatestop<-ab_pt$DateAntimicrobialEnd>ab_pt$DateUnitDischarge
ab_pt$DateAntimicrobialEnd[ab_pt$errordatestop==TRUE]<-ab_pt$DateUnitDischarge[ab_pt$errordatestop==TRUE]
ab_pt$errordatestart<-ab_pt$DateAntimicrobialStart<ab_pt$DateUnitAdmission
ab_pt$DateAntimicrobialStart[ab_pt$errordatestart==TRUE]<-ab_pt$DateUnitAdmission[ab_pt$errordatestart==TRUE]
ab_pt$treatmdays<-ab_pt$DateAntimicrobialEnd-ab_pt$DateAntimicrobialStart+1
ab_pt$treatmdays<-as.numeric(ab_pt$treatmdays)
ab_pt<-ab_pt%>%filter(treatmdays>0)
ab_pt<-merge(ab_pt,haiicu_level1[,c("RecordId","ReportingCountry")],by.x="unit_id",by.y="RecordId") #get ReportingCountry variable


by_unit_ab<-group_by(ab_pt,unit_id)
by_country_ab<-group_by(ab_pt,ReportingCountry)

unit_ab<-by_unit_ab%>%summarise(N_ab=n(),Carb=sum(grepl("J01DH..",ATCCode)),carb_pc=pct(grepl("J01DH..",ATCCode),N_ab),Carb_d=sum(treatmdays[grepl("J01DH..",ATCCode)]),
                                piptaz=sum(grepl("J01CR05",ATCCode)),piptaz_pc=pct(grepl("J01CR05",ATCCode),N_ab),piptaz_d=sum(treatmdays[grepl("J01CR05",ATCCode)]),
                                Ceph12=sum(grepl("J01DB..|J01DC..",ATCCode)),Ceph12_pc=pct(grepl("J01DB..|J01DC..",ATCCode),N_ab),Ceph12_d=sum(treatmdays[grepl("J01DB..|J01DC..",ATCCode)]),
                                Ceph34=sum(grepl("J01DD..|J01DG..",ATCCode)),Ceph34_pc=pct(grepl("J01DD..|J01DG..",ATCCode),N_ab),Ceph34_d=sum(treatmdays[grepl("J01DD..|J01DG..",ATCCode)]),
                                FQ=sum(grepl("J01MA..",ATCCode)),FQ_pc=pct(grepl("J01MA..",ATCCode),N_ab),FQ_d=sum(treatmdays[grepl("J01MA",ATCCode)]),
                                Glycop=sum(grepl("J01XA..",ATCCode)),Glycop_pc=pct(grepl("J01XA..",ATCCode),N_ab),Glycop_d=sum(treatmdays[grepl("J01XA",ATCCode)]),
                                Polymyx=sum(grepl("J01XB..",ATCCode)),Polymyx_pc=pct(grepl("J01XB..",ATCCode),N_ab),Polymyx_d=sum(treatmdays[grepl("J01XB",ATCCode)]),
                                treatmdays_tot=sum(treatmdays,na.rm=TRUE),treatmdays_tot_ind=sum(treatmdays[AntimicrobialIndication!="UNK"],na.rm=TRUE),empiric=sum(treatmdays[AntimicrobialIndication=="E"],na.rm=TRUE),directed=sum(treatmdays[AntimicrobialIndication=="M"],na.rm=TRUE),
                                prophylactic=sum(treatmdays[AntimicrobialIndication=="P"],na.rm=TRUE),selective=sum(treatmdays[AntimicrobialIndication=="S"],na.rm=TRUE),
                                other=sum(treatmdays[AntimicrobialIndication=="O"],na.rm=TRUE)
)



unit_ab<-merge(unit_ab,unit,by.x="unit_id",by.y="UnitId")

unit_ab<-merge(unit_ab,unit_output_pat[,c("UnitId","patdays")],by.x="unit_id",by.y="UnitId")
unit_ab$patdays<-as.numeric(unit_ab$patdays)

unit_ab<-unit_ab%>%mutate(carb_td=round(Carb_d/patdays*100,digits=2),
                          piptaz_td=round(piptaz_d/patdays*100,digits=2),
                          ceph34_td=round(Ceph34_d/patdays*100,digits=2),
                          fq_td=round(FQ_d/patdays*100,digits=2),
                          glycop_td=round(Glycop_d/patdays*100,digits=2),
                          polymyx_td=round(Polymyx_d/patdays*100,digits=2)
)

#DOTs per 100 patient days per country and in Europe
dotpatdays_country<-unit_ab%>%group_by(ReportingCountry)%>%summarise(dotpatdays=round(100*sum(treatmdays_tot)/sum(patdays),digits=1))
dotpatdays_EU<-unit_ab%>%summarise(dotpatdays=round(100*sum(treatmdays_tot)/sum(patdays),digits=1))

unit_indication=select(unit_ab,unit_id, ReportingCountry, N_ab,patdays,treatmdays_tot, treatmdays_tot_ind, empiric,directed, prophylactic,selective,other)
unit_indication=mutate(unit_indication,unitcode=paste(ReportingCountry,unit_id,sep=""))
#unit_ab<-select(unit_ab, -treatmdays_tot, -empiric, -directed, -prophylactic)
country_ab_ind<-unit_ab%>%group_by(ReportingCountry)%>%summarise(emp=sum(empiric,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE),
                                                                 dir=sum(directed,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE),
                                                                 proph=sum(prophylactic,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE),
                                                                 selec=sum(selective,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE),
                                                                 other=sum(other,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE)
)
eu_ab_ind<-unit_ab%>%summarise(emp=sum(empiric,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE),
                               dir=sum(directed,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE),
                               proph=sum(prophylactic,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE),
                               selec=sum(selective,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE),
                               other=sum(other,na.rm=TRUE)/sum(as.numeric(treatmdays_tot_ind),na.rm=TRUE))
unit_ab_groups<-unit_ab%>%select(-empiric, -directed, -prophylactic,-selective,-other,-(Carb:Polymyx_d))
country_ab_groups<-unit_ab_groups%>%group_by(ReportingCountry)%>%summarise_at(vars(N_ab:treatmdays_tot,carb_td:polymyx_td),funs(mean(as.numeric(.),na.rm=TRUE),
                                                                                                           median(as.numeric(.),na.rm=TRUE),
                                                                                                           p25(as.numeric(.)),
                                                                                                           p75(as.numeric(.)))
)
eu_ab_groups<-unit_ab_groups%>%summarise_at(vars(N_ab:treatmdays_tot,carb_td:polymyx_td),list(mean=mean,sum=sum))

country_ab_table<-country_ab_groups%>%select(ReportingCountry:polymyx_td_mean)
eu_ab_table<-eu_ab_groups%>%select(treatmdays_tot_sum,carb_td_mean:polymyx_td_mean)
saveRDS(country_ab_table,"country_ab_table.Rda")
saveRDS(country_ab_ind,"country_ab_ind.Rda")

#end of antibiotic use 28/08/2017---------------------------------------------------------------

#structure and process indicators of antimicrobial stewardship and infection control-----------------------------

deno_standard<-read.csv("2.HAIICU$DENOM.csv")
deno_light<-read.csv("2.HAIICULIGHT$DENO.csv")
deno<-rbind(deno_standard,deno_light)
deno_units<-full_join(deno,select(haiicu_level1_all,
                                  RecordId, ReportingCountry,NumAlcoholHandRubLiters,
                                  NumPatientDaysPrevYear,UnitSize,UnitSpecialty),
                      by=c("ParentId"="RecordId"))
deno_units<-deno_units%>%filter(!is.na(AuditStart),AuditStart!="N/A")
deno_units$AuditStart<-as.Date(deno_units$AuditStart)
deno_units$AuditEnd<-as.Date(deno_units$AuditEnd)
deno_units<-deno_units%>%mutate(audit_days=AuditEnd-AuditStart)
deno_units$audit_days<-as.numeric(deno_units$audit_days)

deno_units$PeriodStart<-as.Date(deno_units$PeriodStart)
deno_units$PeriodEnd<-as.Date(deno_units$PeriodEnd)
deno_units<-deno_units%>%mutate(period_days=PeriodEnd-PeriodStart)
deno_units$period_days<-as.numeric(deno_units$period_days)
deno_units<-deno_units%>%mutate_at(c('NumAlcoholHandRubLiters','NumPatientDaysPrevYear'),as.numeric)
deno_units<-deno_units%>%mutate_at(c(6:12),as.numeric)
deno_units<-deno_units%>%mutate(UnitSize=as.numeric(UnitSize))
deno_units<-deno_units%>%mutate(UnitSpecialty=as.factor(UnitSpecialty))
deno_by_unit<-deno_units%>%group_by(ParentId)%>%
  select(ParentId,ReportingCountry,audit_days,period_days,NumNursingAssistHours7Days:NumUnitAdmission2d,
         NumAlcoholHandRubLiters:UnitSpecialty)%>%
  mutate(across(NumNursingAssistHours7Days:NumUnitAdmission2d,as.numeric)%>%
           summarise(across(where(is.numeric),list(mean=mean),na.rm=TRUE)))
deno_by_unit<-deno_by_unit%>%select(-(NumNursingAssistHours7Days:NumUnitAdmission2d))
deno_by_unit$dupl<-duplicated(deno_by_unit)
deno_by_unit<-deno_by_unit%>%filter(dupl==FALSE)

country_deno<-deno_by_unit%>%select(-ParentId)%>%group_by(ReportingCountry)%>%
  summarise(across(where(is.numeric),list(mean=mean),na.rm=TRUE))

#indicators 7 days --------------------------
deno_7d<-deno_by_unit%>%select(ParentId,ReportingCountry,UnitSize,UnitSpecialty, NumPatDays7Days_mean,NumRegNurseHours7Days_mean,NumNursingAssistHours7Days_mean,
                               NumAlcoholHandRubLiters,NumPatientDaysPrevYear)
deno_7d$dupl<-duplicated(deno_7d)
deno_7d<-deno_7d%>%filter(dupl==FALSE)
country_deno_7d<-deno_7d%>%select(-ParentId)%>%group_by(ReportingCountry)%>%
  summarise(across(where(is.numeric),list(median=median),na.rm=TRUE))
saveRDS(country_deno_7d,"country_deno_7d.Rda")

#indicators assessed by chart review or direct observation---------

deno_ind_standard<-read.csv("3.HAIICU$Denom$Ind.csv")
deno_ind_light<-read.csv("3.HAIICULIGHT$DENO$IND.csv")
deno_ind<-deno_ind_standard
deno_ind<-rbind(deno_ind_standard,deno_ind_light)
deno_ind<-full_join(deno_ind,select(deno, RecordId,ParentId),by=c("ParentId"="RecordId"))
deno_ind_units<-full_join(deno_ind,select(haiicu_level1_all,
                                  RecordId, ReportingCountry,NumAlcoholHandRubLiters,
                                  NumPatientDaysPrevYear,UnitSize,UnitSpecialty),
                      by=c("ParentId.y"="RecordId"))
deno_ind<-deno_ind%>%filter(IndNumObservations!="UNK",IndNumObservations!=0,IndNumCompliant!="UNK")
deno_ind$IndNumCompliant<-as.numeric(deno_ind$IndNumCompliant)
deno_ind$IndNumObservations<-as.numeric(deno_ind$IndNumObservations)
deno_ind<-deno_ind%>%mutate(IndNumCompliant=case_when(IndNumObservations<IndNumCompliant~IndNumObservations, TRUE~IndNumCompliant))
deno_ind<-deno_ind%>%mutate(IndPerc=round(deno_ind$IndNumCompliant/deno_ind$IndNumObservations*100,digits=1))
deno_ind<-deno_ind%>%select(-RecordId)
deno_ind<-deno_ind%>%pivot_wider(names_from=IndicatorCode,values_from=c(IndNumCompliant,IndNumObservations,IndPerc))

deno_ind<-left_join(deno_ind,select(haiicu_level1_all,
                                    RecordId, ReportingCountry),by=c("ParentId.y"="RecordId"))

deno_ind<-deno_ind%>%select(ParentId,ReportingCountry,IndNumCompliant_ASTREV72H,IndNumObservations_ASTREV72H,IndPerc_ASTREV72H,
                            IndNumCompliant_CVCSITDRES,IndNumObservations_CVCSITDRES,IndPerc_CVCSITDRES,
                            IndNumCompliant_INTCUFPRES,IndNumObservations_INTCUFPRES,IndPerc_INTCUFPRES,
                            IndNumCompliant_INTORDECON,IndNumObservations_INTORDECON,IndPerc_INTORDECON,
                            IndNumCompliant_INTPOSNSUP,IndNumObservations_INTPOSNSUP,IndPerc_INTPOSNSUP)


country_ind<-deno_ind%>%select(-ParentId)%>%group_by(ReportingCountry)%>%
  summarise(across(where(is.numeric),list(mean=mean),na.rm=TRUE),n_units=n())

saveRDS(country_ind,"country_ind.Rda")


#master dataframes

#standard
standard$UnitId<-standard$RecordId
standard$RecordId<-NULL
standard_pt<-merge(standard,pt,by.x="UnitId",by.y="ParentId",all=TRUE)
standard_pt$PatientId<-standard_pt$RecordId
standard_pt$RecordId<-NULL
standard_pt_inf<-merge(standard_pt,inf,by.x="PatientId",by.y="Id",all=TRUE)
standard_pt_inf$InfectionId<-standard_pt_inf$RecordId
standard_pt_inf$RecordId<-NULL
standard_pt_inf_micro<-merge(standard_pt_inf,micro,by.x="InfectionId",by.y="ParentId",all=TRUE)


#light
light$UnitId<-light$RecordId
light$RecordId<-NULL
light_deno<-merge(light,lightdeno,by.x="UnitId",by.y="ParentId",all=TRUE)
light_deno$deno<-light_deno$RecordId
light_deno$RecordId<-NULL
inf_light<-inflight
inf_light$InfId<-inf_light$RecordId
inf_light$RecordId<-NULL
light_deno_inf<-merge(light_deno,inf_light,by.x="deno",by.y="ParentId",all=TRUE)
micro_light<-microlight
micro_light_deno_inf<-merge(light_deno_inf,micro_light,by.x="InfId",by.y="ParentId",all=TRUE)

micro_light_deno_inf$RecordType.x<-NULL
micro_light_deno_inf$RecordType.y<-NULL
standard_pt_inf_micro$RecordType.x<-NULL
standard_pt_inf_micro$RecordType.y<-NULL

micro_light_small<-select(micro_light_deno_inf, InfId,UnitId,RecordId,ReportingCountry,HospitalType,Age,InfectionSite, BSIOrigin,Gender,Antibiotic,ResultIsolate,SIR)
micro_standard_small<-select(standard_pt_inf_micro, InfectionId,UnitId,RecordId,ReportingCountry,HospitalType,Age,InfectionSite, BSIOrigin,Gender,Antibiotic,ResultIsolate,SIR)
micro_light_small$InfectionId<-micro_light_small$InfId
micro_light_small$InfId<-NULL

micro_all_small<-rbind(micro_standard_small,micro_light_small)

resistENC%>%filter(Antibiotic %in% c("GLY","VAN","TEC","_NOTEST"))%>%summarise(res=sum(SIR=="R",na.rm=TRUE)/n())


#trend analysis
haiicu_unit_bsidevadj_clabsitable%>%
  filter(ReportingCountry %in% c("FR","ES","LT","PT", "IT-SPIN-UTI"))%>%
  summarise(medianinc=median(clabsiinc,na.rm=TRUE))
haiicudenscountr%>%
  filter(ReportingCountry %in% c("FR","ES","LT","PT","IT-SPIN-UTI"))%>%
  summarise(medianinc=median(iapincintubdays,na.rm=TRUE))

#logreg for the effect of antimicrobial on admission on HAI
haiicu_pt_inf_all<-readRDS("haiicu_pt_inf_all.Rda")
logregdata<-haiicu_pt_inf_all%>%filter(!ReportingCountry%in%c("FR","ES","IT-GiViTI"))%>%select(RecordId,ReportingCountry,Age, AntimicrobialAdmission, Gender,HasHAI, Intubation,OtherScoreValue,ImpairedImmunity,SapsII,TypeOfAdmission,los,OutcomeUnit,DateUnitAdmission,DateOfOnset)

logregdata$lateHAI<-(logregdata$DateOfOnset>(logregdata$DateUnitAdmission+7))
logregdata$lateHAI[is.na(logregdata$lateHAI)]<-FALSE
logregdata$HasHAIYES <- ifelse(logregdata$HasHAI == 'Y', 1, 0)
logregdata$AntimicrobialAdmissionYES <- ifelse(logregdata$AntimicrobialAdmission == 'Y', 1, 0)
logregdata$GenderMale <- ifelse(logregdata$Gender == 'M', 1, 0)
logregdata$IntubationYES <- ifelse(logregdata$Intubation == 'Y', 1, 0)
logregdata$TypeOfAdmissionSUR <- ifelse(logregdata$TypeOfAdmission == 'SSUR', 1, 0)
logregdata$ImpairedImmunityYES <- ifelse(logregdata$ImpairedImmunity == 'Y', 1, 0)
logregdata$Death <- ifelse(logregdata$OutcomeUnit == 'D', 1, 0)

model <- glm(HasHAIYES~AntimicrobialAdmissionYES+GenderMale+IntubationYES+TypeOfAdmissionSUR+ImpairedImmunityYES,family=binomial,data=logregdata)
summary(model)

model <- glm(HasHAIYES~AntimicrobialAdmissionYES+GenderMale+IntubationYES+TypeOfAdmissionSUR+ImpairedImmunityYES+los,family=binomial,data=logregdata)
summary(model)


#analysis including APACHE score
logregdata_apach<-logregdata%>%filter(!is.na(OtherScoreValue))
model_apach <- glm(HasHAIYES~AntimicrobialAdmissionYES+GenderMale+IntubationYES+ImpairedImmunityYES+TypeOfAdmissionSUR+OtherScoreValue+los,family=binomial,data=logregdata_apach)
summary(model_apach)

#analysis including only late-onset HAIs >7d post-admission
model_apach_late <- glm(lateHAI~AntimicrobialAdmissionYES+GenderMale+IntubationYES+TypeOfAdmissionSUR+OtherScoreValue+los,family=binomial,data=logregdata_apach)
summary(model_apach_late)

#XGboost--------
logregdata<-logregdata_apach%>%
  select(HasHAIYES,AntimicrobialAdmissionYES,GenderMale,IntubationYES,ImpairedImmunityYES,TypeOfAdmissionSUR,los,OtherScoreValue)
library(xgboost)
library(caret)
library(SHAPforxgboost)
data<-logregdata_apach
parts = createDataPartition(data$HasHAIYES, p = .7, list = F)
train = data[parts, ]
test = data[-parts, ]
train_x = data.matrix(train[, -1])
train_y = train[,1]
test_x = data.matrix(test[, -1])
test_y = test[, 1]
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)
watchlist = list(train=xgb_train, test=xgb_test)
params <- list(
  objective = "binary:logistic",  # For binary classification
  eval_metric = "logloss",        # Common for binary problems
  max.depth = 3
)

model = xgb.train(params = params,
                  data = xgb_train, 
                  watchlist = watchlist, 
                  nrounds = 200)

final = xgboost(params = params,
                data = xgb_train, 
                nrounds = 100, 
                verbose = 0)

#model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 200)
#final = xgboost(data = xgb_train, max.depth = 3, nrounds = 100, verbose = 0)
pred_y = predict(final, xgb_test)
mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse
importance_matrix <- xgb.importance(model = final)
ggplot(importance_matrix, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Horizontal bars for better readability
  labs(
    title = "XGBoost Feature Importance",
    x = "Features",
    y = "Gain (Contribution to Model)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  )

predictions <- predict(final, xgb_test)

#performance metrics
pred_class <- as.numeric(predictions > 0.03)  # For binary classification
accuracy <- mean(pred_class == test_y)
confusion <- table(Actual = test_y, Predicted = pred_class)
precision <- confusion[2,2] / sum(confusion[2,])
recall <- confusion[1,1] / sum(confusion[1,])
f1 <- 2 * precision * recall / (precision + recall)
library(pROC)
roc_obj <- roc(test_y, predictions)  # Raw probabilities, not classes
auc_value <- auc(roc_obj)
plot(roc_obj, main=paste("AUC =", round(auc_value, 3)))
#variable contributions
avg_contributions <- colMeans(predict(final, xgb_test, predcontrib = TRUE))
print(avg_contributions)

shap_result <- shap.values(X_model = train_x, model = final)
shap_importance <- shap_result$mean_abs_shap
feature_names <- colnames(train_x)
names(shap_importance) <- feature_names

# Visualize contributions for this observation

contributions <- predict(final, xgb_test[1:100,], predcontrib = TRUE)

sample_contrib <- as.data.frame(contributions[1,])
sample_contrib$Feature <- rownames(sample_contrib)
colnames(sample_contrib)[1] <- "Contribution"

sample_contrib <- sample_contrib[order(abs(sample_contrib$Contribution), decreasing = TRUE),]
print(sample_contrib)

ggplot(sample_contrib[sample_contrib$Feature != "BIAS",], 
       aes(x = reorder(Feature, Contribution), y = Contribution, 
           fill = Contribution > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Contributions for Observation #1",
       x = "Feature", y = "Contribution to Log-Odds") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green"), 
                    name = "Direction",
                    labels = c("Toward Class 0", "Toward Class 1"))

# Create dataframe of actuals vs predictions
results <- data.frame(
  Actual = test_y,
  Predicted = predictions
)

# Plot
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values", x = "Actual", y = "Predicted") +
  theme_minimal()

#SHAP-------
shap_values <- shap.values(xgb_model = final, X_train = train_x)
shap_values$mean_shap_score
shap_long <- shap.prep(xgb_model = final, X_train = train_x)
shap.plot.summary(shap_long)