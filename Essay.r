#Loadinam paketus.
if(!require(tidyverse)) install.packages("tidyverse")
require(tidyverse)
if(!require(readxl)) install.packages("readxl")
require(readxl)
if(!require(httr)) install.packages("httr")
require(httr)
if(!require(eurostat)) install.packages("eurostat")
require(eurostat)
if(!require(knitr)) install.packages("knitr");
require(knitr)
if(!require(kableExtra)) install.packages("kableExtra");
require(kableExtra)
if(!require(ggrepel)) install.packages("ggrepel")
require(ggrepel)
if(!require(scales)) install.packages("scales")
require(scales)
if(!require(reshape2)) install.packages("reshape2")
require(reshape2)
if(!require(gridExtra)) install.packages("gridExtra")
require(gridExtra)

#Lietuviu kalba.
Sys.setlocale("LC_ALL","Lithuanian")


##Pirmas grafikas. Pasaulinė pajamų nelygybė 1935-1960m.
#png(filename = "historicalinequality3560.png",width = 9,height = 4,units = "in",res=200)
url <- "https://www.wider.unu.edu/sites/default/files/WIID/WIID_19Dec2018.xlsx"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
y<- read_excel(tf)
y<- select(y, c(5,6))
y<- aggregate(y$gini_reported, list(y$year), mean, na.rm=TRUE)
y<- y[c(9:33),]
ggplot(y, aes(Group.1,x))+
        geom_line(aes(col="Gini indekso reikšmė"))+
        scale_x_continuous(breaks = seq(1930,1960,5))+  
        labs(x="Metai", 
             y="GINI",
             title="Pasaulinė pajamų nelygybė 1935-1960m.",
             subtitle="Šaltinis:UNU-WIDER, World Income Inequality Database (WIID4)")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.title=element_blank())+
        theme(legend.position="bottom")
        
#dev.off()

##Antras Grafikas. Bendruju salies pajamu dalis neatskaicius mokesciu 77-89m.
#png(filename = "top10bot507789.png",width = 9,height = 4,units = "in",res=200)
url <- "https://drive.google.com/uc?export=download&id=1HwkWLpv9bl7DQHiuIwuTZi1bMLInFGnS"
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
y<- read.csv(tf, stringsAsFactors = FALSE, sep = ";", header=FALSE)
df<- y[c(3:28),]
top10<-filter(df, V1=="p90p100")
bot50<-filter(df, V1=="p0p50")
ggplot()+
        geom_line(data=top10, aes(V2,V3, color="Bottom 50%", group=1))+
        geom_line(data=bot50, aes(V2,V3, color="Top 10%", group=1))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(x="Metai", 
             y="Pajamų dalis", 
             title="Bendrųjų šalies pajamų dalis neatskaičius mokesčių 1977-1989m. JAV",
             subtitle="Duomenų šaltinis: WID (World Inequality Database)")+
        theme(legend.title=element_blank())+
        theme(legend.position="bottom")
#dev.off()

##Trecias grafikas. GINI ir Marginal tax rate santykis.
png(filename = "ginivsmarginaltax.png",width = 9,height = 4,units = "in",res=200)
url <- "https://www.taxpolicycenter.org/file/180473/download?token=qXOB5ObN"
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
tax<- read_excel(tf)
tax<-tax[c(5:39),]
tax<-data.frame(Year = unlist(tax[,c(1,3,5)]), marginaltaxrate = unlist(tax[,c(2,4,6)]))
tax$Year <- as.numeric(sub(",", ".", tax$Year, fixed=T))
tax$marginaltaxrate <- as.numeric(sub(",", ".", tax$marginaltaxrate, fixed=T))
tax<-tax %>% filter(Year>=1962)
URL<- "https://drive.google.com/uc?export=download&id=14kyiUXlsrQRV_wUxvyXx8Lf2w-s66ulv" 
temp<- tempfile()
download.file(URL,temp, mode="wb")
table1<- unz(temp, "WID_Data_Metadata/WID_Data_16062019-220346.csv")
gini<- read.csv(table1, header=FALSE, stringsAsFactors = FALSE, sep=";")
unlink(temp)
gini<-gini[(-2),]
gini[gini==""]<-NA
gini$V2 <- as.numeric(sub(",", ".", gini$V2, fixed=T))
gini$V3 <- as.numeric(sub(",", ".", gini$V3, fixed=T))
gini<-gini[complete.cases(gini),]
ggplot()+
        geom_line(data=tax,aes(Year, marginaltaxrate,group=1, color="Ribinis individualių pajamų mokestis %"))+
        geom_line(data=gini, aes(V2, V3, group=1, color="GINI"))+
        labs(x="Metai", 
             y="Ribinis individualių pajamų mokestis",
             title="Ribinio individualių pajamų mokesčio ir GINI indekso santykis",
             subtitle="Šaltinis:UNU-WIDER, World Income Inequality Database (WIID4) & Tax Policy Center")+
        scale_x_continuous(breaks = seq(1913,2017,3))+
        scale_y_continuous(labels = function(x) paste0(x*100, "%"))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        theme(legend.title=element_blank())+
        theme(legend.position="bottom")
##dev.off()


##Ketvirtas Grafikas. Darbuotojai pagal issilavinima.
png(filename = "workerseducation.png",width = 9,height = 4,units = "in",res=200)
educationalemployment<- get_eurostat("lfsi_educ_a", stringsAsFactors=FALSE)
educationalemployment<- filter(educationalemployment,
                               unit=="PC_EMP",
                               sex=="T", 
                               isced11%in%c("ED0-2","ED5-8", "ED3_4"),
                               geo=="EU28",
                               age=="Y20-64")
primaryeducation<-subset(educationalemployment,educationalemployment$isced11=="ED0-2" )
primaryeducation<-select(primaryeducation, 6,7)
secondaryeducation<-subset(educationalemployment,educationalemployment$isced11=="ED3_4" )
secondaryeducation<-select(secondaryeducation, 6,7)
tertiaryeducation<-subset(educationalemployment,educationalemployment$isced11=="ED5-8" )
tertiaryeducation<-select(tertiaryeducation, 6,7)
ggplot()+
        geom_line(data=primaryeducation,aes(x=time, y=values, group=1, color="Mažesnis nei pagrindinis ir pagrindinis išsilavinimas"))+
        geom_line(data=tertiaryeducation,aes(x=time, y=values,group=1, color="Aukštasis išsilavinimas"))+
        geom_line(data=secondaryeducation, aes(x=time, y=values, group=1, color="Vidurinis ir aukštesnysis išsilavinimas"))+
        scale_x_date(date_breaks="2 year", date_labels="%Y")+
        scale_y_continuous(labels = function(x) paste0(x, "%"), breaks=seq(10,55,5))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(x="Metai",
             y="Dirbančiųjų skaičius",
             title="Dirbančiųjų lygio (procentais nuo visų dirbančiųjų) skirtumas pagal išsilavinimą",
             subtitle="Šaltinis:Eurostat [lfsi_educ_a]")+
        theme(legend.title=element_blank())+
        theme(legend.position="bottom")
##dev.off()

##Penktas grafikas. Amziaus projekcija.
##png(filename = "amziausprojekcija.png",width = 9,height = 4,units = "in",res=200)
Metai<-c(2000,2005,2010,2020,2030,2040,2050,2075,2100 )
Vidurkis<-c(29.7,30.4,31.3,33.1,35.2,37.1,38.8,42.3,45.5)
visuomenesamzius<-data.frame(Metai,Vidurkis)
ggplot(visuomenesamzius, aes(Metai, Vidurkis, group=1))+
        geom_line(aes(col=""))+
        geom_point()+
        labs(x="Metai",
        y="Vidutinis amžius", 
        title="Vidutinio amžiaus kitimo projekcija", subtitle="Duomenų šaltinis:The coming acceleration of global population ageing,\nW.Sanderson & S.Scherbov, 2008, Nature.com")+
        theme(legend.title=element_blank())+
        theme(legend.position="none")
##dev.off()

##Sestas grafikas. Gender Pay gapas.
##png(filename = "genderpaygap.png",width = 9,height = 4,units = "in",res=200)
genderpaygap<-get_eurostat("earn_gr_gpgr2", stringsAsFactors=FALSE)
genderpaygap<- filter(genderpaygap, 
                      geo=="EU28" )
ggplot(genderpaygap, aes(x=time, y=values))+
        geom_line(aes(color="Lyčių algų skirtumas (Gender pay gap)"))+
        labs(x="Metai", y="Algų skirtumas",
             title="Vyrų ir moterų uždarbio skirtumo kitimo dinamika (2010-2017)",
             subtitle="Šaltinis:Eurostat [earn_gr_gpgr2]")+
        theme(legend.title=element_blank())+
        theme(legend.position="bottom")+
        scale_y_continuous(breaks=c(seq(10,20,2)), limits = c(10, 20))+
        geom_hline(aes(yintercept = mean(genderpaygap$values), color="vidurkis"),linetype=2, size=0.5)
##dev.off()

##Septintas Grafikas. Koreliacija.
##png(filename = "koreliacija.png",width = 9,height = 4,units = "in",res=200)
url <- "https://drive.google.com/uc?export=download&id=1jn5Ac87ifSzpCGgcb8DVYodeLDB3RpXh"
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
wages<- read.csv(tf, header = TRUE, stringsAsFactors = FALSE, sep = ",")
url2 <- "https://drive.google.com/uc?export=download&id=11MiTc9RpyRR43pbGYgEzGXppoQDiqEmu"
GET(url2, write_disk(tf2 <- tempfile(fileext = ".csv")))
ginikof<- read.csv(tf2, header = TRUE, stringsAsFactors = FALSE, sep = ",")
wages <- wages %>% filter(SERIES=="EXR",
                          PERIOD=="H" ) %>%
        select(1,2,15)
wages <- wages %>% mutate_if(is.numeric, round, 2)
ginikof <- ginikof %>% filter(AGE=="TOT") %>%
        select (1,2,19)
koreliacija <- left_join(wages, ginikof,by="Country")
koreliacija <- na.omit(koreliacija)
koreliacija <- koreliacija[,-c(1,4)]
rownames(koreliacija) <- koreliacija[,1]
ggplot(koreliacija, aes(x=Value.x,y=Value.y))+
        geom_point()+
        geom_smooth(method=lm, se=TRUE)+
        geom_label_repel(aes(label=rownames(koreliacija)),position = "identity", label.padding = 0.2)+
        scale_x_continuous(labels=dollar_format(prefix="$"), breaks=c(seq(0,14,2)), limits = c(0, 14))+
        labs(x="Minimalus valandinis atlyginimas, $", 
             y="GINI",
             title="Koreliacija tarp Minimalaus valandinio atlyginimo \n(Vietinės valiutos santykiu su 2016m. JAV Dolerio kursu) ir GINI indekso",
             subtitle="Šaltinis:OECD(2016),(IDD),(RMW)")
##dev.off()

##Astuntas Grafikas. Gini before/after taxation palyginimas.
##png(filename = "ginibeforetaxation.png",width = 9,height = 4,units = "in",res=200)
url <- "https://drive.google.com/uc?export=download&id=1E54lTO0kkOS_HZUqm2Yr5vk2j4t8rE_V"
GET(url, write_disk(tf <- tempfile(fileext = ".csv")))
ginibeforetaxes<- read.csv(tf, header = TRUE, stringsAsFactors = FALSE, sep = ",")
ginibeforetaxes <- ginibeforetaxes %>% filter(AGE=="TOT") %>%
        select (2,19)
url2 <- "https://drive.google.com/uc?export=download&id=11MiTc9RpyRR43pbGYgEzGXppoQDiqEmu"
GET(url2, write_disk(tf2 <- tempfile(fileext = ".csv")))
ginikof<- read.csv(tf2, header = TRUE, stringsAsFactors = FALSE, sep = ",")
ginikof <- ginikof %>% filter(AGE=="TOT") %>%
        select (1,2,19)
ggplot()+
        geom_bar(data=ginikof,
                 aes(x=reorder(Country,Value), y=Value,group=1),  stat="identity", col="white", fill="steelblue")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(x="Valstybės", y="GINI",
             title="GINI indeksas po mokesčių",
             subtitle="Šaltinis:OECD(2016),(IDD)")+
        theme(legend.position="bottom")+
        theme(legend.title=element_blank())
ggplot()+
        geom_bar(data=ginibeforetaxes,
                 aes(x=reorder(Country,Value), y=Value, group=1), stat="identity", col="white", fill="steelblue")+
        theme(legend.position="bottom")+
        theme(legend.title=element_blank())+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(x="Valstybės", y="GINI",
             title="GINI indeksas prieš mokesčius",
             subtitle="Šaltinis:OECD(2016),(IDD)")+
        theme(legend.position="bottom")+
        theme(legend.title=element_blank())

##dev.off()

## Devintas desimtas grafikai. Svietimas
trng_aes_101 <- get_eurostat("trng_aes_101", stringsAsFactors = FALSE)
trng_aes_182 <- get_eurostat("trng_aes_182", stringsAsFactors = FALSE)
trng_aes_101 <- trng_aes_101 %>% filter(training=="FE_NFE",
                                        age=="Y25-64" ,
                                        geo=="EU28") %>%
                                 select(5,6)
trng_aes_182 <- trng_aes_182 %>% filter (geo=="EU28",
                                         sex=="T",
                                         training=="TOTAL", ) %>%
                                select(5,6)

ggplot(trng_aes_101, aes(x=time, y=values))+
        geom_line(aes(col=""))+
        geom_point()+
        coord_cartesian(ylim=c(0,100))+
        scale_y_continuous(labels = function(x) paste0(x, "%"))+
        labs(x="Metai",
             y="Dalyvavusių mokymuose procentas",
             title="Darbuotojų, dalyvavusių kvalifikacijos kėlimo mokymuose  \nprocentas ES šalyse",
             subtitle="Šaltinis:Eurostat [trng_aes_101]")+
        theme(legend.position = "none")

ggplot(trng_aes_182, aes(x=time, y=values))+
        geom_line(aes(col=""))+
        geom_point()+
        coord_cartesian(ylim=c(0,100))+
        scale_y_continuous(labels = function(x) paste0(x, "%"))+
        labs(x="Metai", 
             y="Suinteresuotųjų mokymais procentas",
             title="Darbuotojų, ieškojusių informacijos \napie galimybes kelti kvalifikaciją, procentas ES šalyse",
             subtitle="Šaltinis:Eurostat [trng_aes_182]")+
        theme(legend.position = "none")
##dev.off()

