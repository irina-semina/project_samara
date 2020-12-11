library("swirl")
library("ggplot2")
library("sandwich")
library("stargazer")
library("corrplot")
library("AER")  
library("sandwich")
library("lmtest")
library("car")
library("stargazer") 
library("ggplot2")
library("openintro")
library("OIdata")
library("gdata")
library("doBy")
library("plm") 
library("ivpack") 
library("dplyr")
library("psych")
library("corrplot")
library("gap")
library('sqldf')

library("mfx") # подсчет предельных эффектов
library("ggplot2") # графики
library("lmtest") # линейные регрессии 
library("foreign") # чтение файлов в некоторых форматах
library("dplyr") # манипуляции с таблицами
library("broom") # описание модели в виде таблички

#Creates 'Table 1', i.e., description of baseline patient
#characteristics, which is essential in every medical research
library('tableone')

library('caret')
library('randomForest')
library('grf') #Generalized Random Forests
library('mvtnorm')

cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

data <- read.csv("CIAN_data_for_lin.csv", sep=";", dec=".", header=TRUE)

ggplot(data1, aes(x=Full_square, y=Price)) + 
  geom_point(alpha=0.6) +
  geom_smooth() +  xlab("Площадь в квадратных метрах")+  ylab("Цена в рублях") 

ggplot(data1, aes(x=log(Full_square), y=log(Price)) + 
  geom_point(alpha=0.6) +
  geom_smooth() +  xlab("Площадь в квадратных метрах")+  ylab("Цена в рублях") 


names(data)

data$firs_floor[data$Etazh==1]=1
data$firs_floor[data$Etazh!=1]=0

count(data$firs_floor[data$firs_floor==1])

data$last_floor[data$share_etazh==1]=1
data$last_floor[data$share_etazh!=1]=0


data$big_kitch[(data$Kitchen >15.0)==1] =1
data$big_kitch[(data$Kitchen <=15.0)!=1] =0

mean(data$big_kitch)

shapiro.test(modlin3$residuals)

hist(modlin$residuals)

data<-subset(data, Price<quantile(Price, 0.995))






modlin <- lm(data = data, Price~ Year+	Full_square+	Musor_dummy+	lift_dummy+	
               avaria+	share_etazh+	Owner+	raion_zheleznodor+
               raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
               raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+
               gaz_autonomic+	gaz_central+	warm_autonomic_cot+	warm_individual+	
               warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
               room3+	room4+	room5+	room_mn+	park_naz+	
               park_podzem+	kit_sh)

modlin2 <- lm(data = data, Price~ Year+	Full_square+	Musor_dummy+	lift_dummy+	
               avaria+	share_etazh+	share_etazh^2+ Owner+	raion_autozavod+	raion_zheleznodor+
               raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
               raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+
               gaz_autonomic+	gaz_central+	warm_autonomic_cot+	warm_individual+	
               warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
               room3+	room4+	room5+		room_mn+	park_naz+	
               park_podzem+	big_kitch)

modlin3 <- lm(data = data, Price~ Year+	Full_square+	Musor_dummy+	lift_dummy+	
                avaria+	Owner+	raion_zheleznodor+
                raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
                raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+	
                gaz_autonomic+	gaz_central+	warm_autonomic_cot+	warm_individual+	
                warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
                room3+	room4+	room5+	room_mn+	park_naz+	
                park_podzem)

modlin3_1 <- lm(data = data, Price~ Year+	Full_square+	Musor_dummy+	lift_dummy+	
                avaria+ firs_floor+last_floor+	raion_zheleznodor+
                raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
                raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+	
                	warm_autonomic_cot+	warm_individual+	
                warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
                room3+	room4+	room5+	room_mn+	park_naz+	
                park_podzem)

modlin3_1 <- lm(data = data, log(Price)~ Year+log(Full_square)+	Musor_dummy+	lift_dummy+	
                  avaria+ firs_floor+last_floor+	raion_zheleznodor+
                  raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
                  raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+	
                  warm_autonomic_cot+	warm_individual+	
                  warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
                  room3+	room4+	room5+	room_mn+	park_naz+	
                  park_podzem)

modlin3_2 <- lm(data = data, Price~ Year+Full_square+	Musor_dummy+	lift_dummy+	
                  avaria+ firs_floor+last_floor+	raion_zheleznodor+
                  raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
                  raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+	
                  warm_autonomic_cot+	warm_individual+	
                  warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
                  room3+	room4+	room5+	room_mn+	park_naz+	
                  park_podzem)

stargazer(modlin,modlin3_2,modlin3_1,
          se=list(cse(modlin),cse(modlin3_2),cse(modlin3_1)),
          title="Оценка моделей", type="text", font.size='tiny',
          column.labels= NULL,
          df=FALSE, digits=1)

stargazer(modlin,modlin3_1,
          se=list(cse(modlin),cse(modlin3_1)),
          title="Оценка моделей", type="text", font.size='tiny',
          column.labels= NULL,
          df=FALSE, digits=1)


modlin_1 <- lm(data = data, Price~ Year+	Full_square+	Musor_dummy+	lift_dummy+	
               avaria+	share_etazh+	Owner+	raion_autozavod+	raion_zheleznodor+
               raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
               raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+	mix+
               gaz_autonomic+	gaz_central+	warm_autonomic_cot+	warm_individual+	
               warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
               room3+	room4+	room5+	room_kv+	room_mn+	room_st+	park_naz+	
               park_podzem+	kit_sh)

modlin_2 <- lm(data = data, price_m2~ Year+	Musor_dummy+	lift_dummy+	
                avaria+	share_etazh+	share_etazh^2+ Owner+	raion_autozavod+	raion_zheleznodor+
                raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
                raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+	mix+
                gaz_autonomic+	gaz_central+	warm_autonomic_cot+	warm_individual+	
                warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
                room3+	room4+	room5+	room_kv+	room_mn+	room_st+	park_naz+	
                park_podzem+	big_kitch)

modlin_3 <- lm(data = data, price_m2~ Year+	Musor_dummy+	lift_dummy+	
                 avaria+	firs_floor+last_floor+ Owner+	raion_zheleznodor+
                 raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
                 raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+	
                 gaz_autonomic+	gaz_central+	warm_autonomic_cot+	warm_individual+	
                 warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
                 room3+	room4+	room5+	room_mn+	park_naz+	
                 park_podzem)


modlin_4 <- lm(data = data, price_m2~ Year+	Musor_dummy+	lift_dummy+	
                 avaria+	firs_floor+ Owner+	raion_zheleznodor+
                 raion_kirovskii+	raion_kuibish+	raion_leninskii+	raion_okt+	
                 raion_prom+	raion_samar+	raion_ussr+	Old+	Wood+	zhelezobet+	
                 gaz_autonomic+	gaz_central+	warm_autonomic_cot+	warm_individual+	
                 warm_kotel+	warm_pech+	warm_central+	tr_walk+	tr_tr+	room2+
                 room3+	room4+	room5+	room_mn+	park_naz+	
                 park_podzem+	big_kitch)


waldtest(modlin3_1, modlin, vcov = vcovHC)


vif(modlin3_1)

ggplot(data, aes(x = Price)) + geom_density(alpha = .4) +
  xlab("Цена за квадратный метр, рублей")+  ylab("") 

ggplot(data, aes(x = price_m2)) + geom_density(alpha = .4) +
  xlab("Цена за квадратный метр, рублей")+  ylab("") 


