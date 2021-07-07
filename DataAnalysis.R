### 資料匯入與處理
library(jsonlite)
library(dplyr)
Year103<-fromJSON("https://apiservice.mol.gov.tw/OdService/download/A17000000J-020066-wtF")
Year104<-fromJSON("https://apiservice.mol.gov.tw/OdService/download/A17000000J-020066-5aV")
Year105<-fromJSON("https://apiservice.mol.gov.tw/OdService/download/A17000000J-020066-45f")

#第一題
Year103105<-inner_join(Year103,Year105,by="大職業別")
#欄位名稱處理
ColName<-colnames(Year103105)
ColName<-gsub("-","",ColName)
ColName<-gsub(".x","103",ColName)
ColName<-gsub(".y","105",ColName)
colnames(Year103105)<-ColName
#清洗資料和字串轉數字
Year103105$大學薪資103<-gsub("—","NA",Year103105$大學薪資103)
Year103105$大學薪資103<-as.numeric(Year103105$大學薪資103)
Year103105$大學薪資105<-gsub("—","NA",Year103105$大學薪資105)
Year103105$大學薪資105<-as.numeric(Year103105$大學薪資105)
###105年度薪資較103年度薪資高的職業有哪些?
compare103105<-Year103105%>%
  mutate(compare103105=大學薪資105/大學薪資103)%>%
  arrange(desc(compare103105))%>%
  filter(compare103105>1)%>%
  select("大職業別",compare103105)
head(compare103105,10)
### 提高超過5%的的職業有哪些?
over5<-compare103105%>%
  filter(compare103105>1.05)
head(over5)
### 主要的職業種別是哪些種類呢?
over5Split<-strsplit(over5$大職業別,"-")
mainType<-NULL
for(i in 1:length(over5$大職業別)){
  mainType<-c(mainType,over5Split[[i]][1])
}
table(mainType)

#第二題
Year103104105<-rbind(Year103,Year104,Year105)
#欄位名稱處理
ColName<-colnames(Year103104105)
ColName<-gsub("-","",ColName)
colnames(Year103104105)<-ColName
#資料清洗和字串轉數字
Year103104105$`大學女/男`<-gsub("—","NA",Year103104105$`大學女/男`)
Year103104105$`大學女/男`<-gsub("…","NA",Year103104105$`大學女/男`)
Year103104105$`大學女/男`<-as.numeric(Year103104105$`大學女/男`)
### 103到105年度的大學畢業薪資資料，哪些行業男生薪資比女生薪資多?
male<-Year103104105%>%
  filter(`大學女/男`<100)%>%
  arrange(`大學女/男`)%>%
  select("年度","大職業別","大學女/男")
head(male,10)
#不重複職業
male%>%
  summarise(professionNumber=n_distinct(大職業別))
### 哪些行業女生薪資比男生薪資多? 
female<-Year103104105%>%
  filter(`大學女/男`>100)%>%
  arrange(desc(`大學女/男`))%>%
  select("年度","大職業別","大學女/男")
head(female,10)

#第三題
#欄位名稱處理
ColName<-colnames(Year105)
ColName<-gsub("-","",ColName)
colnames(Year105)<-ColName
#資料清洗和字串轉數字
Year105$大學薪資<-gsub("—","NA",Year105$大學薪資)
Year105$研究所及以上薪資<-gsub("—","NA",Year105$研究所及以上薪資)
Year105$大學薪資<-as.numeric(Year105$大學薪資)
Year105$研究所及以上薪資<-as.numeric(Year105$研究所及以上薪資)
###研究所薪資差異
grad<-Year105%>%
  mutate(compareGrad=研究所及以上薪資/大學薪資)%>%
  arrange(desc(compareGrad))%>%
  select("大職業別",compareGrad)
head(grad,10)

#第四題
like<-Year105%>%
  mutate(compareGrad=研究所及以上薪資/大學薪資)%>%
  arrange(desc(compareGrad))%>%
  select("大職業別","大學薪資","研究所及以上薪資",compareGrad)%>%
  filter(大職業別=="資訊及通訊傳播業")
like

