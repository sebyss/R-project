library(UsingR)
mtcars
dataset=mtcars
attach(dataset)
names(dataset)


#====== Indicatori numerici=======
# Quartile
summary(hp)
summary(mpg)
summary(cyl)
summary(wt)

#Dispersia de selectie
library(moments)
var(hp)
var(mpg)
var(cyl)
var(wt)

#Abatere standad
skewness(hp)
skewness(mpg)
skewness(cyl)
skewness(wt)

#Tabele
table(hp) # cele mai multe masini cu acelasi nr de cp sun cele cu 110,175,180
table(mpg) # msinile care au acelasi consum in comun sunt de cel mult 2
table(cyl) # mjoritatea masinilor din acest set de date au motor cu 8 cilindri
table(wt) # cele mai multe masini care au aceeasi greutate sunt in numar de 3 (3.44)

#barplot table
barplot(table(hp))
barplot(table(mpg))
barplot(table(cyl))
barplot(table(wt))


#tabele cut - impartind variabila in 5
table(cut(hp,5)) # majoritatea masinilor au intre 51 si 109cp
table(cut(mpg,5)) #majoritatea masinilor merg intre 15.1 - 19.8 mile cu 1 galon
table(cut(wt,5)) #majoritatea masinilor cantaresc intre 3.08 si 3.86

barplot(table(cut(hp,5)))
barplot(table(cut(mpg,5)))
barplot(table(cut(wt,5)))

#Curbe de densitate si boxplot-uri
plot(density(hp))
plot(density(wt))
plot(density(mpg))
plot(density(cyl))

boxplot(hp)
boxplot(wt)
boxplot(mpg)


###============================= Ipoteze

### Ipoteza 1 : Consumul masinilor cu 4 cilindri este mai redus decat a celor cu 6. => Fals

v4Set = subset(dataset,cyl==4,select=c(mpg))
v6Set = subset(dataset,cyl==6,select=c(mpg))

t.test(v4Set,v6Set,alternative = "greater")

###Ipoteza 2 : Consumul masinilor este mai ridicat daca masina are transmisie manuala => Adevarat

manual = subset(dataset,am==1,select=c(mpg))
automat = subset(dataset,am==0,select=c(mpg))

t.test(manual,automat,alternative="greater")
mt
### Ipoteza 3 : Daca motorul este in forma de v consumul creste => Fals

v = subset(dataset,vs==0,select=c(mpg))
l = subset(dataset,vs==1,select=c(mpg))

t.test(v,l,alternative="greater")

### Ipoteza 4 : Masinile cu mai multi cilindri au mai multi cp => Adevarat

v4Set = subset(dataset,cyl==4,select=c(hp))
v8Set = subset(dataset,cyl==8,select=c(hp))

t.test(v8Set,v4Set,alternative = "greater")

### Ipoteza 5 : Masinile cu 3 trepte consuma mai mult decat cele cu 5 => Fals

cincivit = subset(dataset,gear==5,select=c(mpg))
treivit = subset(dataset,gear==3,select=c(mpg))

t.test(treivit,cincivit,alternative="greater")

### Ipoteza 6 : masinile care au motoare cu mai multi cilindi cantaresc mai mult => adevarat

v8 = subset(dataset,cyl==8,select=c(wt))
v4 = subset(dataset,cyl==4,select=c(wt))

t.test(v8,v4,alternative="greater")

### Ipoteza 7 : Masinile cu cutie automata au motoare mai mari( cc) => Adevarat

automata = subset(dataset,am==0,select=c(disp))
manuala = subset(dataset,am==1,select=c(disp))

t.test(automata,manuala,alternative="greater")

###===================Teste

summary(aov(mpg~cyl+wt)) #=> consumul este afectat in functie de nr de cilindii si greutate 
boxplot(mpg~cyl)
kruskal.test(mpg~cyl) 

summary(aov(hp~cyl+gear)) #=> caii putere sunt afectati de nr de cilindrii si viteze
boxplot(hp~cyl)
kruskal.test(hp~cyl)

summary(aov(disp~cyl+wt)) #=> marimea motorului este afectata de nr de cilindii si greutate
boxplot(disp~cyl)
kruskal.test(disp~cyl)

####=====================Regresii

model=lm(mpg~hp+am) # consumul mai este afectat si de tipul cutiei de viteze
summary(model)

model11=lm(mpg~hp)
summary(model11)
plot(mpg~hp)
abline(model11) # => consumul creste impreuna cu cp 

model22=lm(disp~hp+gear+wt) # cc este afectata si de nr de viteze dar si de greutatea masinii
summary(model22)

model2=lm(disp~hp) 
summary(model2)
plot(disp~hp)
abline(model2) # => ne putem da seama ca motoarele mai mari au mai multi cp

model33=lm(hp~qsec+disp) # cp sunt influentati de timpul qsec dar si de capacitatea motorulu
summary(model33)

model3=lm(hp~qsec)
summary(model3)
plot(hp~qsec)
abline(model3) # => cu cat masina are mai multi cp, aceasta parcurge mai repede 1/4mile





