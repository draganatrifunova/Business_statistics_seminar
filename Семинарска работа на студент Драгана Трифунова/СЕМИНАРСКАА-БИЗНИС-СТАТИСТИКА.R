#tabela so raspredelba na cestoti za age
n_age=length(Mall_Spending$Age)
sort(Mall_Spending$Age)
range(Mall_Spending$Age)
max(Mall_Spending$Age, na.rm = TRUE)-min(Mall_Spending$Age, na.rm=TRUE)
breaks_age=seq(18, 70.05, by=3.47)
breaks_age
age.cut=cut(Mall_Spending$Age, breaks_age, right=FALSE)
age_freq=table(age.cut)
age_rel.freq=age_freq/n
age_cum.freq=cumsum(age_freq)
age_rel.cum.freq=cumsum(age_freq)/n
age_rel.cum.freq2=cumsum(age_rel.freq)
age_procent.freq=age_rel.freq*100
age_procent.cum.freq=age_rel.cum.freq*100
midpoints_age<-(breaks_age[-1]+breaks_age[-length(breaks_age)])/2
#final_table <- data.frame(freq, midpoints, rel.freq, cum.freq, rel.cum.freq, rel.cum.freq2, procent.freq, procent.cum.freq)

final_table_age <-data.frame(age_freq, midpoints_age, age_rel.freq, age_cum.freq, age_rel.cum.freq, age_rel.cum.freq2, age_procent.freq, age_procent.cum.freq)

#histogram age
hist(Mall_Spending$Age, right = FALSE, col=colors, main="Возраст на купувачите", xlab="интервали", ylab="честоти")

#poligon
d_age=density(Mall_Spending$Age)
plot(d_age, main="Полигон за возраста на купувачите", xlab="интервали", ylab="густина")
polygon(d_age, col="skyblue")





sd(age)
#tabela so raspredelba na cestoti za kupuvacite
n=length(Mall_Spending$Spending.Score..1.100.)
sort(Mall_Spending$Spending.Score..1.100.)
range(Mall_Spending$Age)
range(Mall_Spending$Spending.Score..1.100.)
max(Mall_Spending$Spending.Score..1.100., na.rm = TRUE)-min(Mall_Spending$Spending.Score..1.100., na.rm=TRUE)
#ja odreduvam sirinata na intervalot
#w >= R/k  -->   w >= 98/15
sirina_interval_poeni<-98/15
sirina_interval_poeni <- 98/15
sirina_interval_poeni
breaks=seq(1, 99, by=6.54)
breaks
score<-Mall_Spending$Spending.Score..1.100.
score.cut=cut(Mall_Spending$Spending.Score..1.100., breaks, right=FALSE)
freq=table(score.cut)
rel.freq=freq/n
cum.freq=cumsum(freq)
rel.cum.freq=cumsum(freq)/n
rel.cum.freq2=cumsum(rel.freq)
procent.freq=rel.freq*100
procent.cum.freq=rel.cum.freq*100
midpoints<-(breaks[-1]+breaks[-length(breaks)])/2
final_table <- data.frame(freq, midpoints, rel.freq, cum.freq, rel.cum.freq, rel.cum.freq2, procent.freq, procent.cum.freq)
final_table
breaks
#2 obid za podobar interval
breaks=seq(1, 99.1, by=6.54)
breaks
score<-Mall_Spending$Spending.Score..1.100.
score.cut=cut(Mall_Spending$Spending.Score..1.100., breaks, right=FALSE)
freq=table(score.cut)
rel.freq=freq/n
cum.freq=cumsum(freq)
rel.cum.freq=cumsum(freq)/n
rel.cum.freq2=cumsum(rel.freq)
procent.freq=rel.freq*100
procent.cum.freq=rel.cum.freq*100
midpoints<-(breaks[-1]+breaks[-length(breaks)])/2
final_table <- data.frame(freq, midpoints, rel.freq, cum.freq, rel.cum.freq, rel.cum.freq2, procent.freq, procent.cum.freq)
#kreiranje na histogram za score
colors<-col(c("red", "white", "yellow", "blue", "purple", "lightblue", "pink", "green", "black", "orange"))
colors=c("red", "blue", "orange", "purple", "pink", "lightblue", "black", "white","green")
hist(Mall_Spending$Spending.Score..1.100., right=FALSE, col=colors, main="Поени на купувачите", xlab="поени", ylab="кумулативна честота")
#kreiranje poligon za poeni na kupuvacite
d=density(Mall_Spending$Spending.Score..1.100.)
plot(d, main="Полигон за поените на купувачите", xlab="поени", ylab="густина")
polygon(d, col="pink")
#steblo list za godini
steblo_list_godini <-stem(Mall_Spending$Age)
steblo_list_godini
#steblo list za poeni
stem(Mall_Spending$Spending.Score..1.100.)
#grafik na rasejuvanjne 
library(tidyverse)
plot(x=Mall_Spending$Age, y=Mall_Spending$Spending.Score..1.100., main="Години на потрошувачите насптроти поени на потрошувачите", xlab="Години на потрошувачите", ylab="Поени на потрошувачите")
#prosek na age, score
mean(Mall_Spending$Age)
mean(Mall_Spending$Spending.Score..1.100.)
#medijana na age, score
medijana_age <- median(Mall_Spending$Age)
medijana_age
medijana_score <- median(Mall_Spending$Spending.Score..1.100.)
medijana_score
#posebna funkcija za naoganje na modata
mode<-function(x){
  unique_val <- unique(x)
  counts<-vector()
  for(i in 1 :length(unique_val))
  {
    counts[i]<-length(which(x==unique_val[i]))
  }
  position<-c(which(counts==max(counts)))
  if(length(unique_val)==length(x))
    mode_x <- 'Mode does not exist'
  else
    mode_x <-unique_val[position]
  return (mode_x)
}
mode(Mall_Spending$Age)
mode(Mall_Spending$Spending.Score..1.100.)
#kvartali, opseg i interkvartalen raspon godini
prv_kvartal_age<-quantile(Mall_Spending$Age, 0.25)
vtor_kvartal_age<-quantile(Mall_Spending$Age, 0.50)
tret_kvartal_age<-quantile(Mall_Spending$Age, 0.75)
prv_kvartal_age
vtor_kvartal_age
tret_kvartal_age
#kvartali score
kvartali_score <- quantile(Mall_Spending$Spending.Score..1.100.)
kvartali_score
#opseg na score
max(Mall_Spending$Spending.Score..1.100., na.rm=TRUE)-min(Mall_Spending$Spending.Score..1.100., na.rm=TRUE)
opseg_score<-max(Mall_Spending$Spending.Score..1.100., na.rm=TRUE)-min(Mall_Spending$Spending.Score..1.100., na.rm=TRUE)
opseg_score
#opseg godini
opseg_godini<-max(Mall_Spending$Age, na.rm=TRUE)-min(Mall_Spending$Age, na.rm=TRUE)
opseg_godini
#IQR godini
interkvartalen_godini=IQR(Mall_Spending$Age)
interkvartalen_godini
#IQR score
interkvartalen_score=IQR(Mall_Spending$Spending.Score..1.100.)
interkvartalen_score
#disperzija i standardna devijacija
#age
standardna_devijacija_age=sd(Mall_Spending$Age)
standardna_devijacija_age
#disperzija age preku sd
disperzija_age=sd(Mall_Spending$Age)^2
disperzija_age
#standardna devijacija score
standardna_devijacija_score=sd(Mall_Spending$Spending.Score..1.100.)
standardna_devijacija_score
#disperzija score preku standardna devijacija
disperzija_score=sd(Mall_Spending$Spending.Score..1.100.)^2
disperzija_score

#koeficient na korelacija za godinite i poenite na kupuvacite
cor(Mall_Spending$Age, Mall_Spending$Spending.Score..1.100.)
koeficient_korelacija=cor(Mall_Spending$Age, Mall_Spending$Spending.Score..1.100.)
koeficient_korelacija

# 95% intervali na doverba za poenite na kupuvacot
error<-qnorm(0.975)*sd(Mall_Spending$Spending.Score..1.100.)/sqrt(length(Mall_Spending$Spending.Score..1.100.))
error
#zet alfa pola
qnorm(0.975)
#prosek-eror
pocetok_interval_doverba=mean(Mall_Spending$Spending.Score..1.100.)-error
#prosek+error
kraj_interval_dobverba=mean(Mall_Spending$Spending.Score..1.100.)+error
pocetok_interval_doverba
kraj_interval_dobverba

#testiranje na hipotezi
slucaen_primerok_score<-c(81, 44, 46, 77, 88, 97, 13, 17, 83, 49)
zet_statistika<-(mean(slucaen_primerok_score)-mean(Mall_Spending$Spending.Score..1.100.))/sd(Mall_Spending$Spending.Score..1.100.)
zet_statistika
zet_statistika<-0.3601368*sqrt(length(Mall_Spending$Spending.Score..1.100.))
zet_statistika
#ja otkrivam zet alfa
#so nivo na znacajnost 0.05
qnorm(1-0.05)

#test za raspredelba na poenite na kupuvacot
shapiro.test(Mall_Spending$Spending.Score..1.100.)


#testiranje hipotezi za nezavisnost
#testiranje hipotezi za nezavisnost
summary(table(Mall_Spending$Age, Mall_Spending$Spending.Score..1.100.))
x=Mall_Spending$Age
y=Mall_Spending$Spending.Score..1.100.
chisq.test(x, y)


#regresiona analiza
regresija=lm(Mall_Spending$Age ~ Mall_Spending$Spending.Score..1.100.)
newdata=data.frame(m=20)
regresija.res=resid(regresija)
plot(Mall_Spending$Spending.Score..1.100., regresija.res, main="Права на регресија на годините во зависност од поените", xlab="возраст", ylab="поени", col="blue")
abline(regresija)
lm(formula=Mall_Spending$Age ~ Mall_Spending$Spending.Score..1.100.)


#naoganje koeficient na determinantnost
(cor(Mall_Spending$Age, Mall_Spending$Spending.Score..1.100.))**2
