### packages
library(dplyr)
library(rgdal) #readOGR
library(spdep) #make weights matrix
library(corrplot)

### spatialdataframe
spat.data = readOGR(dsn = ".", layer = "gn_250m_wo_null")

spat_data = spat.data #copy the original data to keep it
names(spat.data) #show variable names
summary(spat.data)

##### 1. checkout variables

df <- as.data.frame(spat.data@data)
head(df)

### outliers

boxplot(df$numpo)
which (df$numpo == max(df$numpo))

spat.data@data[(df$numpo == max(df$numpo)),] <- NA

# btw variables
df
df0 <- df[,-c(1:4,6:8)]
M <- cor(df0)
corrplot(M, method='color')

##############2. preset

# standardized
stdz <- function(data){
  (data - mean(data)) / sd(data)
}

#spat.data@data[,5] <- stdz(spat.data@data[,5])

for (i in 5:(dim(spat.data@data)[2]-4)){
  spat.data@data[,i] <- stdz(spat.data@data[,i])
}

# spplot #make map

spplot(spat.data,"numpo", main=list(label="민원 단속건수",cex=2,font=2))
spplot(spat.data,"parking")
spplot(spat.data,"cctv")
spplot(spat.data,"pub")
spplot(spat.data,"indu")
spplot(spat.data,"edu")
spplot(spat.data,"agri")
spplot(spat.data,"cul")
spplot(spat.data,"wel")
spplot(spat.data,"comm")
spplot(spat.data,"accom")
spplot(spat.data,"office")
spplot(spat.data,"medi")
spplot(spat.data,"one")
spplot(spat.data,"two")
spplot(spat.data,"reli")
spplot(spat.data,"stor")
spplot(spat.data,"ath")
spplot(spat.data,"dwell")

###################################
##### 3. analysis

### make weight matrix (nb type)
queen.nb=poly2nb(spat.data) 
rook.nb=poly2nb(spat.data,queen=FALSE) 

queen.listw=nb2listw(queen.nb) #convert nb to listw type
rook.listw=nb2listw(rook.nb) #convert nb to listw type
listw1= queen.listw
#define the regression equation
reg.eq1=numpo~parking+cctv+ind+cul+wel+accom+office+medi+one+two+reli+dwell+stor+ath+pub+edu+comm+food


##### run models: OLS, Lag Error

### OLS
reg1=lm(reg.eq1,data=spat.data)
reg1b=lm(numpo~parking+cctv+ind+cul+wel+accom+office+medi+one+two+reli+dwell+stor+ath+pub+edu+comm+food-1,data=spat.data)
summary(reg1)
summary(reg1b)

### model selection
lm.morantest(reg1,listw1)
lm.LMtests(reg1,listw1,test=c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))


### SEM Spatial Error Model  y=XB+u,   u=LWu+e
reg4=errorsarlm(reg.eq1,data=spat.data, listw1)
summary(reg4)

result <- summary(reg4)
rslt <- result$Coef[result$Coef[,4] < 0.05,]


reg4$coefficients

plot(reg4$residuals)
result <- summary(reg4)
rslt <- result$Coef[result$Coef[,4] < 0.05,]

### save weight values
weight <- (rslt[(2:dim(rslt)[1]),1])
