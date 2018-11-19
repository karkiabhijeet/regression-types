#Load the data and view the dataset 
carprice<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = F)
View(carprice)

#Check the structure
str(carprice)


#############Data Preparation Start#############

#Check duplicates
nrow(unique(carprice)) #unique rows equal to total rows so no duplicates

#Check NAs
sum(is.na(carprice)) #no NA values

#check summary for  
summary(carprice) #Need to change the Categorical Variables to factors 

################################################################################
#Outlier treatment will be done here because the company requires variables that
#effect the prices in majority so that the norms can be taken into consideration 
#during the manufacturing. Outliers might correspond to specific models and 
#therefore will distort the generic variable weights
################################################################################
#We will first check the numerical variables for outliers

# Variable: Wheelbase
quantile(carprice$wheelbase,seq(0,1,0.01)) #Jump from 99% to 100%
#fix
carprice$wheelbase[which(carprice$wheelbase>115.544)]<-115.544

# Variable: carlength
quantile(carprice$carlength,seq(0,1,0.01)) #Variance below 3% and above 99%
#fix
carprice$carlength[which(carprice$carlength>202.480 )]<-202.480 
carprice$carlength[which(carprice$carlength<155.900 )]<-155.900

# Variable: carwidth
quantile(carprice$carwidth,seq(0,1,0.01)) #Minor outlier below 1%
#fix
carprice$carwidth[which(carprice$carwidth<62.536 )]<-62.536

# Variable: carheight
quantile(carprice$carheight,seq(0,1,0.01)) #No Outlier

# Variable: curbweight
quantile(carprice$curbweight,seq(0,1,0.01)) #Below 1% above 98%
#fix
carprice$curbweight[which(carprice$curbweight<1819.72 )]<-1819.72
carprice$curbweight[which(carprice$curbweight>3768.40 )]<-3768.40  

# Variable: enginesize
quantile(carprice$enginesize,seq(0,1,0.01)) #Below 3% above 98%
#fix
carprice$enginesize[which(carprice$enginesize<90.00 )]<-90.00
carprice$enginesize[which(carprice$enginesize>256.08 )]<-256.08  

# Variable: boreratio
quantile(carprice$boreratio,seq(0,1,0.01)) #Below 1% above 99%
#fix
carprice$boreratio[which(carprice$boreratio<2.9100 )]<-2.9100
carprice$boreratio[which(carprice$boreratio>3.8000 )]<-3.8000  

# Variable: stroke
quantile(carprice$stroke,seq(0,1,0.01)) #Below 2% above 95%
#fix
carprice$stroke[which(carprice$stroke<2.6400 )]<-2.6400
carprice$stroke[which(carprice$stroke>3.6400 )]<-3.6400  

# Variable: compressionratio
quantile(carprice$compressionratio,seq(0,1,0.01)) #No Outlier

# Variable: horsepower
quantile(carprice$horsepower,seq(0,1,0.01)) #Below 4% above 97%
#fix
carprice$horsepower[which(carprice$horsepower<62.00 )]<-62.00
carprice$horsepower[which(carprice$horsepower>184.00)]<-184.00

# Variable: peakrpm
quantile(carprice$peakrpm,seq(0,1,0.01)) #above 99%
#fix
carprice$peakrpm[which(carprice$peakrpm>6000)]<-6000

# Variable: citympg
quantile(carprice$citympg,seq(0,1,0.01)) #above 98%
#fix
carprice$citympg[which(carprice$citympg>38.00)]<-38.00

# Variable: highwaympg
quantile(carprice$highwaympg,seq(0,1,0.01)) #above 99%
#fix
carprice$highwaympg[which(carprice$highwaympg>49.88)]<-49.88

# Variable: horsepower
quantile(carprice$horsepower,seq(0,1,0.01)) #Below 4% above 97%
#fix
carprice$horsepower[which(carprice$horsepower<62.00 )]<-62.00
carprice$horsepower[which(carprice$horsepower>184.00)]<-184.00

# Variable: price
quantile(carprice$price,seq(0,1,0.01)) #Below 5% above 98%
#fix
carprice$price[which(carprice$price<6197.00 )]<-6197.00
carprice$price[which(carprice$price>36809.60)]<-36809.60

#We begin preparing categorical variables

#variable: symboling
carprice$symboling<-as.factor(carprice$symboling)
summary(carprice$symboling)
levels(carprice$symboling)[1] <- "prettysafe"
levels(carprice$symboling)[2:3] <- "safe"
levels(carprice$symboling)[3:4] <- "risky"
levels(carprice$symboling)[4] <- "prettyrisky"
str(carprice)
dummy<-model.matrix(~symboling - 1,data=carprice)
dummy<-dummy[,-1]
#View(dummy)
carprice<-cbind(carprice[,-2], dummy)
#View(carprice)

#variable:CarName
library(tidyr)
carprice<-separate(carprice,CarName,c("CarCompany"),extra="drop", sep=" ")
summary(carprice$CarCompany)
str(carprice)
#correct the typos
carprice$CarCompany[which(carprice$CarCompany=='maxda')] <-'mazda'
carprice$CarCompany[which(carprice$CarCompany=='Nissan')] <-'nissan'
carprice$CarCompany[which(carprice$CarCompany=='vokswagen')] <-'volkswagen'
carprice$CarCompany[which(carprice$CarCompany=='vw')] <-'volkswagen'
carprice$CarCompany[which(carprice$CarCompany=='toyouta')] <-'toyota'
carprice$CarCompany[which(carprice$CarCompany=='porcshce')] <-'porsche'
carprice$CarCompany<-as.factor(carprice$CarCompany)
str(carprice)
dummy<-model.matrix(~CarCompany - 1,data=carprice)
dummy<-dummy[,-1]
#View(dummy)
carprice<-cbind(carprice[,-2], dummy)
#View(carprice)


#variable: fueltype
carprice$fueltype<-as.factor(carprice$fueltype)
str(carprice)
dummy<-model.matrix(~fueltype - 1,data=carprice)
#View(dummy)
carprice<-cbind(carprice[,-2], dummy)
carprice<-carprice[1:48]
#View(carprice)

#variable: aspiration
carprice$aspiration<-as.factor(carprice$aspiration)
str(carprice)
dummy<-model.matrix(~aspiration - 1,data=carprice)
#View(dummy)
carprice<-cbind(carprice[,-2], dummy)
carprice<-carprice[1:48]
#View(carprice)

#variable: doornumber
carprice$doornumber<-as.factor(carprice$doornumber)
str(carprice)
dummy<-model.matrix(~doornumber - 1,data=carprice)
#View(dummy)
carprice<-cbind(carprice[,-2], dummy)
carprice<-carprice[1:48]
#View(carprice)


#variable: carbody
carprice$carbody<-as.factor(carprice$carbody)
str(carprice)
dummy<-model.matrix(~carbody - 1,data=carprice)
dummy<-dummy[,-1]
#View(dummy)
carprice<-cbind(carprice[,-2], dummy)
#View(carprice)

#variable: drivewheel
carprice$drivewheel<-as.factor(carprice$drivewheel)
str(carprice)
dummy<-model.matrix(~drivewheel - 1,data=carprice)
dummy<-dummy[,-1]
#View(dummy)
carprice<-cbind(carprice[,-2], dummy)
#View(carprice)

#variable: enginelocation
carprice$enginelocation<-as.factor(carprice$enginelocation)
str(carprice)
dummy<-model.matrix(~enginelocation - 1,data=carprice)
#View(dummy)
carprice<-cbind(carprice[,-2], dummy)
carprice<-carprice[1:52]
#View(carprice)

#variable: enginetype
carprice$enginetype<-as.factor(carprice$enginetype)
str(carprice)
dummy<-model.matrix(~enginetype - 1,data=carprice)
dummy<-dummy[,-1]
#View(dummy)
carprice<-cbind(carprice[,-7], dummy)
#View(carprice)

#variable: cylindernumber
carprice$cylindernumber<-as.factor(carprice$cylindernumber)
str(carprice)
dummy<-model.matrix(~cylindernumber - 1,data=carprice)
dummy<-dummy[,-1]
#View(dummy)
carprice<-cbind(carprice[,-7], dummy)
#View(carprice)

#variable: fuelsystem
carprice$fuelsystem<-as.factor(carprice$fuelsystem)
str(carprice)
dummy<-model.matrix(~fuelsystem - 1,data=carprice)
dummy<-dummy[,-1]
#View(dummy)
carprice<-cbind(carprice[,-8], dummy)
#View(carprice)

#Remove Car_ID
carprice<-carprice[,-1]

#Derived Metrics
carprice$mpgratio<-carprice$highwaympg/carprice$citympg
carprice$WeightDistribution<-carprice$curbweight/carprice$wheelbase
carprice$frameratio<-carprice$carlength/carprice$carheight

#Outlier Check
# Variable: mpgratio
quantile(carprice$mpgratio,seq(0,1,0.01)) #above 99%
#fix
carprice$mpgratio[which(carprice$mpgratio>1.470588 )]<-1.470588 

#Outlier Check
# Variable: WeightDistribution
quantile(carprice$WeightDistribution,seq(0,1,0.01)) #above 99%
#fix
carprice$WeightDistribution[which(carprice$WeightDistribution>34.37421 )]<-34.37421  

#Outlier Check
# Variable: frameratio
quantile(carprice$frameratio,seq(0,1,0.01)) #above 99%
#fix
carprice$frameratio[which(carprice$frameratio>3.773448 )]<-3.773448

#############Data Preparation Complete#############
#data<-carprice
#set the seed to 100 
set.seed(100)
# Create row indices for train dataset
trainindices= sample(1:nrow(carprice), 0.7*nrow(carprice))
# Create the train data set
traindata = carprice[trainindices,]
# Create test dataset
testdata = carprice[-trainindices,]

#Model_1 
model1<-lm(price~., data = traindata)
summary(model1)

#Remove NA columns and re-run
model2<-lm(price~wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+
             stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg+
             symbolingsafe+symbolingrisky+symbolingprettyrisky+CarCompanyaudi+
             CarCompanybmw+CarCompanybuick+CarCompanychevrolet+CarCompanydodge+
             CarCompanyhonda+CarCompanyisuzu+CarCompanyjaguar+CarCompanymazda+
             CarCompanymercury+CarCompanymitsubishi+CarCompanynissan+
             CarCompanypeugeot+CarCompanyplymouth+CarCompanyporsche+
             CarCompanyrenault+CarCompanysaab+CarCompanysubaru+CarCompanytoyota+
             CarCompanyvolkswagen+CarCompanyvolvo+fueltypediesel+aspirationstd+
             doornumberfour+carbodyhardtop+carbodyhatchback+carbodysedan+
             carbodywagon+drivewheelfwd+drivewheelrwd+enginelocationfront+
             enginetypeohc+enginetypeohcv+enginetyperotor+cylindernumberfive+
             cylindernumberfour+cylindernumbersix+fuelsystem2bbl+fuelsystemmfi+
             fuelsystemspdi+fuelsystemmpfi+mpgratio+WeightDistribution+frameratio, data=traindata)
summary(model2)

#Check StepAIC
library(MASS)
step<-stepAIC(model2, direction="both")
step
#Model3 from StepAIC output
model3<-lm(formula = price ~ carlength + carwidth + carheight + curbweight + 
             enginesize + boreratio + stroke + peakrpm + citympg + highwaympg + 
             symbolingsafe + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
             CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
             CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
             CarCompanyplymouth + CarCompanyporsche + CarCompanyrenault + 
             CarCompanysaab + CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
             fueltypediesel + aspirationstd + carbodyhardtop + carbodyhatchback + 
             carbodysedan + carbodywagon + drivewheelrwd + enginelocationfront + 
             enginetypeohc + enginetyperotor + cylindernumberfour + fuelsystem2bbl + 
             fuelsystemmpfi + mpgratio + frameratio, data = traindata)
summary(model3)

#Check VIF
library(car)
vif(model3)

#Model4 - frameratio
model4<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
             curbweight + enginesize + boreratio + stroke + peakrpm + 
             symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
             CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
             CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
             CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
             CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
             aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginelocationfront + enginetypeohc + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             fuelsystem2bbl + WeightDistribution , data = traindata)
summary(model4)

#Check VIF
vif(model4)

#Model5 - curbweight
model5<-lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
             enginesize + boreratio + stroke + peakrpm + 
             symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
             CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
             CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
             CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
             CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
             aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginelocationfront + enginetypeohc + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             fuelsystem2bbl + WeightDistribution , data = traindata)
summary(model5)

#Check VIF
vif(model5)

#Model6 - wheelbase
model6<-lm(formula = price ~ carlength + carwidth + carheight + 
             enginesize + boreratio + stroke + peakrpm + 
             symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
             CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
             CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
             CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
             CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
             aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginelocationfront + enginetypeohc + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             fuelsystem2bbl + WeightDistribution , data = traindata)
summary(model6)

#Check VIF
vif(model6)

#Model7 - enginesize
model7<-lm(formula = price ~ carlength + carwidth + carheight + 
             boreratio + stroke + peakrpm + 
             symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
             CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
             CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
             CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
             CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
             aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginelocationfront + enginetypeohc + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             fuelsystem2bbl + WeightDistribution , data = traindata)
summary(model7)

#Check VIF
vif(model7)
  
#Model8 - carlength
model8<-lm(formula = price ~  carwidth + carheight + 
             boreratio + stroke + peakrpm + 
             symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
             CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
             CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
             CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
             CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
             aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginelocationfront + enginetypeohc + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             fuelsystem2bbl + WeightDistribution , data = traindata)
summary(model8)

#Check VIF
vif(model8)


#model9 - enginetypeohc
model9<-lm(formula = price ~  carwidth + carheight + 
             boreratio + stroke + peakrpm + 
             symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
             CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
             CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
             CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
             CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
             aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginelocationfront  + 
             cylindernumberfive + cylindernumberfour + cylindernumbersix + 
             fuelsystem2bbl + WeightDistribution , data = traindata)
summary(model9)

#Check VIF
vif(model9)

#model10 - cylindernumbersix
model10<-lm(formula = price ~  carwidth + carheight + 
             boreratio + stroke + peakrpm + 
             symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
             CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
             CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
             CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
             CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
             aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginelocationfront  + 
             cylindernumberfive + cylindernumberfour + 
             fuelsystem2bbl + WeightDistribution , data = traindata)
summary(model10)

#Check VIF
vif(model10)

#model11 - fuelsystem2bbl
model11<-lm(formula = price ~  carwidth + carheight + 
              boreratio + stroke + peakrpm + 
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model11)

#Check VIF
vif(model11)

#model11 - fuelsystem2bbl
model11<-lm(formula = price ~  carwidth + carheight + 
              boreratio + stroke + peakrpm + 
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model11)

#Check VIF
vif(model11)

#model12 - CarCompanysaab
model12<-lm(formula = price ~  carwidth + carheight + 
              boreratio + stroke + peakrpm + 
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanydodge + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model12)

#Check VIF
vif(model12)

#model13 - CarCompanymercury
model13<-lm(formula = price ~  carwidth + carheight + 
              boreratio + stroke + peakrpm + 
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model13)

#Check VIF
vif(model13)

#model14 - peakrpm
model14<-lm(formula = price ~  carwidth + carheight + 
              boreratio + stroke +  + 
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model14)

#Check VIF
vif(model14)

#model15 - carheight
model15<-lm(formula = price ~  carwidth  + 
              boreratio + stroke +  + 
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanydodge + CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model15)

#Check VIF
vif(model15)

#model16 - CarCompanydodge
model16<-lm(formula = price ~  carwidth  + 
              boreratio + stroke +  + 
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model16)

#Check VIF
vif(model16)

#model17 - drivewheelrwd
model17<-lm(formula = price ~  carwidth  + 
              boreratio + stroke +
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model17)

#Check VIF
vif(model17)

#model18 - boreratio
model18<-lm(formula = price ~  carwidth  + 
              stroke +
              symbolingrisky + CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model18)

#Check VIF
vif(model18)

#model19 - symbolingrisky
model19<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick + CarCompanychevrolet + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model19)

#Check VIF
vif(model19)

#model20 - CarCompanychevrolet
model20<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyplymouth + CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model20)

#Check VIF
vif(model20)

#model21 - CarCompanyplymouth
model21<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanynissan + CarCompanypeugeot + 
              CarCompanyrenault  + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model21)

#Check VIF
vif(model21)

#model23 - CarCompanyrenault
model23<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model23)

#Check VIF
vif(model23)

#model24 - carbodyhardtop
model24<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd  + carbodyhatchback + carbodysedan + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model24)

#Check VIF
vif(model24)

#model25 - carbodysedan
model25<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd  + carbodyhatchback + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model25)

#Check VIF
vif(model25)

#model26 - carbodyhatchback
model26<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              aspirationstd  + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model26)

#Check VIF
vif(model26)

#model27 - CarCompanyvolkswagen
model27<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + 
              aspirationstd  + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model27)

#Check VIF
vif(model27)

#model28 - CarCompanyvolkswagen
model28<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive + cylindernumberfour + 
              WeightDistribution , data = traindata)
summary(model28)

#Check VIF
vif(model28)

#model29 - cylindernumberfour
model29<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + 
              carbodywagon + enginelocationfront  + 
              cylindernumberfive  + 
              WeightDistribution , data = traindata)
summary(model29)

#Check VIF
vif(model29)

#model30 - cylindernumberfive
model30<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + CarCompanymazda + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + 
              carbodywagon + enginelocationfront  + 
              WeightDistribution , data = traindata)
summary(model30)

#Check VIF
vif(model30)


#model31 - CarCompanymazda
model31<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + 
              CarCompanymitsubishi + CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + 
              carbodywagon + enginelocationfront  + 
              WeightDistribution , data = traindata)
summary(model31)

#Check VIF
vif(model31)


#model32 - CarCompanymitsubishi
model32<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + 
              CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + 
              carbodywagon + enginelocationfront  + 
              WeightDistribution , data = traindata)
summary(model32)

#Check VIF
vif(model32)


#model33 - carbodywagon
model33<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + 
              CarCompanypeugeot + 
              CarCompanysubaru + CarCompanytoyota + 
              enginelocationfront  + 
              WeightDistribution , data = traindata)
summary(model33)

#Check VIF
vif(model33)

#model34 - CarCompanytoyota
model34<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + 
              CarCompanypeugeot + 
              CarCompanysubaru + 
              enginelocationfront  + 
              WeightDistribution , data = traindata)
summary(model34)

#Check VIF
vif(model34)

#model 34 looks promising with 2 high value VIFs let's try to predict and check first


#Predict Values
Predict <- predict(model34,testdata)
testdata$error<-testdata$price-testdata$test_price
testdata$test_price <- Predict
cor(testdata$price,testdata$test_price)^2
#predicted R-squared: 0.883962
#Model R-squared: 0.9429


#Let us smake one try to correct the VIF

#model35 - WeightDistribution
model35<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              CarCompanyjaguar + 
              CarCompanypeugeot + 
              CarCompanysubaru + 
              enginelocationfront, data = traindata)
summary(model35)

#Check VIF
vif(model35)

#Predict Values
Predict <- predict(model35,testdata)
testdata$error<-testdata$price-testdata$test_price
testdata$test_price <- Predict
cor(testdata$price,testdata$test_price)^2
#predicted R-squared: 0.8274276
#Model R-squared:  0.9129 

#Now VIF is under 2 and all p-values less than 5% but Predicted r-squared have dropped. 
#let's try 1 more iteration



#model36 
model36<-lm(formula = price ~  carwidth  + 
              stroke +
              CarCompanybmw + CarCompanybuick  + 
              enginelocationfront, data = traindata)
summary(model36)

#Stroke has become insignificant let's remove it
model37<-lm(formula = price ~  carwidth  + 
              CarCompanybmw + CarCompanybuick  + 
              enginelocationfront, data = traindata)
summary(model37)
#Predict Values
Predict <- predict(model37,testdata)
testdata$error<-testdata$price-testdata$test_price
testdata$test_price <- Predict
cor(testdata$price,testdata$test_price)^2
#predicted R-squared: 0.7831929
#Model R-squared:  0.8584 

#Conclusion: Model34 works the best, below are the plots given:
#predicted R-squared: 0.883962
#Model R-squared: 0.9429

#plot Actual vs Predicted
testdata$ID <- seq.int(nrow(testdata))
ggplot(testdata, aes(ID,price)) + geom_line(aes(colour = "blue" )) +
  geom_line(aes(x=ID, y=test_price, colour="red"))

#plot Errors
ggplot(testdata, aes(ID,error)) + geom_point() +
  geom_hline(yintercept = 0)

