library(dplyr)
library(tidyr)
#library("XLConnect", lib.loc="~/R/R-3.2.2/library")

library(XLConnect)

prc<-function(x){return(100*round(x,digits=4))}
pct<-function(x,tot){prc(sum(x,na.rm=TRUE)/tot)}
unfactor<-function(x){as.numeric(as.character(x))}
p25<-function(x){quantile(x,c(0.25),na.rm=TRUE)}
p75<-function(x){quantile(x,c(0.75),na.rm=TRUE)}

country<-"LT"
refdata<-"National Reference Data"

mainDir<-getwd()
subDir<-"output_folder"

if (!file.exists(subDir)) {
  dir.create(file.path(mainDir, subDir))

}

#unit data
haiicu_level1<-read.csv("1.HAIICU.csv")
levels(haiicu_level1$ReportingCountry)<-c(levels(haiicu_level1$ReportingCountry),"IT-GiViTI","IT-SPIN-UTI")
haiicu_level1$ReportingCountry[haiicu_level1$ReportingCountry=="IT"]<-haiicu_level1$DataSource[haiicu_level1$ReportingCountry=="IT"]#Replace IT with network name

#exposure data
haiicuexp<-read.csv("haiicuexp_correct.csv")
haiicuexp<-select(haiicuexp,RecordId,ParentId,DateExpStart, DateExpEnd,ExpType)
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

saveRDS(haiicu_pt_inf_all,"haiicu_pt_inf_all_full_2023.Rda")

haiicu_iap<-merge(haiicu_pt_inf_all[,c("Id","RecordId","UnitId","hasHai","InfectionSite","dupl_pat","InvasiveDevice","DateOfOnset", "InfectionOutcome")],
                  haiicuexp[,c("ParentId","ExpType","DateExpStart","DateExpEnd")],by.x="Id",by.y="ParentId")
haiicu_iap<-filter(haiicu_iap,grepl("PN",InfectionSite))
haiicu_iap<-filter(haiicu_iap,ExpType=="INT")
haiicu_iap<-filter(haiicu_iap,as.Date(DateOfOnset,format="%Y-%m-%d")>(as.Date(DateExpStart,format="%Y-%m-%d")-1),as.Date(DateOfOnset,format="%Y-%m-%d")<(as.Date(DateExpEnd,format="%Y-%m-%d")+3))
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

#Antibiotic use
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


#resistance
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



#transfer data to excel file
#wb<-loadWorkbook("unit_output_test.xlsx")
#createName(wb,name="thisHosp",formula="TestSheet!$A$4",overwrite=TRUE)
#writeNamedRegion(wb,unit_output_pat,name="thisHosp")

TempFile<-("unit_output_test_2020.xlsx")
setwd(file.path(mainDir, subDir))


for (h in unit_output_pat$unitcode[unit_output_pat$ReportingCountry==country]){
  template<-loadWorkbook(TempFile)
  #template<-paste("unit_output_",h,".xlsx",sep="")
  #file.copy(TempFile,template)
  #wb<-loadWorkbook(template)

  createName(template,name="EU_output_pat_this",formula="TestSheet!$A$1",overwrite=TRUE)
  writeNamedRegion(template,EU_ref_pat,name="EU_output_pat_this")

  createName(template,name="unit_output_pat_this",formula="TestSheet!$A$3",overwrite=TRUE)
  writeNamedRegion(template,unit_output_pat[c(unit_output_pat$unitcode==h),],name="unit_output_pat_this")

  #writeWorksheet(template,data=EU_ref_pat,sheet="TestSheet",startRow=6,startCol=2)
  createName(template,name="unit_output_inf_this",formula="TestSheet!$A$6",overwrite=TRUE)
  writeNamedRegion(template,unit_output_inf[c(unit_output_inf$unitcode==h),],name="unit_output_inf_this")

  createName(template,name="country_output_unit_this",formula="TestSheet!$A$9",overwrite=TRUE)
  writeNamedRegion(template,country_output_unit[c(country_output_unit$ReportingCountry==substr(h,1,2)),],name="country_output_unit_this")

  createName(template,name="country_output_unit_inf_this",formula="TestSheet!$A$12",overwrite=TRUE)
  writeNamedRegion(template,country_output_unit_inf[c(country_output_unit_inf$ReportingCountry==substr(h,1,2)),],name="country_output_unit_inf_this")

  createName(template,name="EU_output_inf",formula="TestSheet!$A$15",overwrite=TRUE)
  writeNamedRegion(template,EU_ref_inf,name="EU_output_inf")

  # createName(template,name="EU_output_ind",formula="TestSheet!$A$18",overwrite=TRUE)
  # writeNamedRegion(template,EU_ref_ind,name="EU_output_ind")
  #
  # createName(template,name="country_output_ind_this",formula="TestSheet!$A$21",overwrite=TRUE)
  # writeNamedRegion(template,country_output_ind[c(country_output_ind$ReportingCountry==substr(h,1,2)),],name="country_output_ind_this")
  #
  # createName(template,name="unit_output_ind_this",formula="TestSheet!$A$24",overwrite=TRUE)
  # writeNamedRegion(template,icu_deno_ind_all[c(icu_deno_ind_all$unitcode==h),],name="unit_output_ind_this")

  createName(template,name="unit_surveillance_period",formula="TestSheet!$A$27",overwrite=TRUE)
  writeNamedRegion(template,icu_unit[c(icu_unit$unitcode==h),],name="unit_surveillance_period")
  #
  # createName(template,name="country_alcohol",formula="TestSheet!$A$30",overwrite=TRUE)
  # writeNamedRegion(template,alcohol_by_country[c(alcohol_by_country$ReportingCountry==substr(h,1,2)),],name="country_alcohol")
  #
  # createName(template,name="EU_alcohol",formula="TestSheet!$A$33",overwrite=TRUE)
  # writeNamedRegion(template,alcohol_EU,name="EU_alcohol")

  createName(template,name="unit_micro_BSI",formula="TestSheet!$A$36",overwrite=TRUE)
  writeNamedRegion(template,unit_micro_BSI[c(unit_micro_BSI$unitcode==h),],name="unit_micro_BSI")

  createName(template,name="country_unit_microBSI",formula="TestSheet!$A$39",overwrite=TRUE)
  writeNamedRegion(template,country_unit_microBSI[c(country_unit_microBSI$ReportingCountry==substr(h,1,2)),],name="country_unit_microBSI")


  createName(template,name="unit_micro_PN",formula="TestSheet!$A$42",overwrite=TRUE)
  writeNamedRegion(template,unit_micro_PN[c(unit_micro_PN$unitcode==h),],name="unit_micro_PN")

  createName(template,name="country_unit_microPN",formula="TestSheet!$A$45",overwrite=TRUE)
  writeNamedRegion(template,country_unit_microPN[c(country_unit_microPN$ReportingCountry==substr(h,1,2)),],name="country_unit_microPN")

  createName(template,name="unit_output_inf_ncases",formula="TestSheet!$A$48",overwrite=TRUE)
  writeNamedRegion(template,unit_output_inf_ncases[c(unit_output_inf_ncases$unitcode==h),],name="unit_output_inf_ncases")

  createName(template,name="country_output_unit_inf_ncases",formula="TestSheet!$A$51",overwrite=TRUE)
  writeNamedRegion(template,country_output_unit_inf_ncases[c(country_output_unit_inf_ncases$ReportingCountry==substr(h,1,2)),],name="country_output_unit_inf_ncases")

  createName(template,name="unit_micro_res",formula="TestSheet!$A$54",overwrite=TRUE)
  writeNamedRegion(template,unit_micro_res[c(unit_micro_res$unitcode==h),],name="unit_micro_res")

  createName(template,name="country_unit_microres",formula="TestSheet!$A$57",overwrite=TRUE)
  writeNamedRegion(template,country_unit_microres[c(country_unit_microres$ReportingCountry==substr(h,1,2)),],name="country_unit_microres")

  createName(template,name="apache",formula="TestSheet!$A$60",overwrite=TRUE)
  writeNamedRegion(template,apache[c(apache$unitcode==h),],name="apache")

  createName(template,name="country_apache_unit",formula="TestSheet!$A$63",overwrite=TRUE)
  writeNamedRegion(template,country_apache_unit[c(country_apache_unit$ReportingCountry==substr(h,1,2)),],name="country_apache_unit")

  createName(template,name="unit_ab",formula="TestSheet!$A$66",overwrite=TRUE)
  writeNamedRegion(template,unit_ab[c(unit_ab$unitcode==h),],name="unit_ab")

  createName(template,name="country_output_ab",formula="TestSheet!$A$69",overwrite=TRUE)
  writeNamedRegion(template,country_output_ab[c(country_output_ab$ReportingCountry==substr(h,1,2)),],name="country_output_ab")

  createName(template,name="unit_indication",formula="TestSheet!$A$72",overwrite=TRUE)
  writeNamedRegion(template,unit_indication[c(unit_indication$unitcode==h),],name="unit_indication")

  createName(template,name="country_output_ab_indication",formula="TestSheet!$A$75",overwrite=TRUE)
  writeNamedRegion(template,country_output_ab_indication[c(country_output_ab_indication$ReportingCountry==substr(h,1,2)),],name="country_output_ab_indication")

  createName(template,name="reference_data",formula="TestSheet!$A$78",overwrite=TRUE)
  writeNamedRegion(template,ref[c(ref$ReportingCountry==substr(h,1,2)),],name="reference_data")


  report_file<-paste("unit_output_",country,"-",icu_unit$LocalId[icu_unit$unitcode==h],".xlsx",sep="")
  setForceFormulaRecalculation(template,"*",TRUE)
  hideSheet(template,sheet="TestSheet")
  saveWorkbook(template,report_file)
 }

setwd(mainDir)
xlcFreeMemory()

