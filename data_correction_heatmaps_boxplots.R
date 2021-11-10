

## change rows that only have 0 for all three species and all stages to NAs
# reorder data that all stages an all species are single columns -> into WIDE format 

library(plotly)
library(dplyr)
library(readr)
library(tidyverse)
complete_dataset_2000_2020 <- read_csv("Schreibtisch/Seabirds/complete_dataset_2000_2020.csv")
data <- complete_dataset_2000_2020


data$Species_Stage <- paste(data$Species, data$Stage)
data$Species<- NULL
data$Stage <- NULL
data$Age <- NULL

data$Species_Stage<- gsub(' ', '_', data$Species_Stage)

spread_data <- data %>%
  pivot_wider(names_from = "Species_Stage", 
              values_from = "Abundance")


# filter rows that have all 0 for and island for all species and all stages

NA_data<- spread_data %>% filter(Cormorant_breeder == 0,Cormorant_nonbreeder==0, Cormorant_total==0, Booby_nonbreeder ==0,Booby_breeder==0, Booby_total==0, Pelican_nonbreeder ==0,Pelican_breeder==0, Pelican_total==0)

NA_data<- NA_data %>% arrange(Name_place)
spread_data<- spread_data %>% arrange(Name_place)

NA_data[,9:17]<- NA

# get only rows from the spread_data df that are not in NA_data 

new_data<- anti_join(spread_data, NA_data, by=c("Name_place","Type" ,  "Longitude","Latitude", "Area" ,  "Year" , "Month","Day" ))

# check that newdata and NA_data sum up to spread data 
7137+423 #7560 -> MATCH

# combine those to dataframe

new_data<- rbind.data.frame(new_data, NA_data)

# correct the NA rows with 0 in them for some islands: 

new_data<- new_data %>% arrange(Name_place)

# save data as version2, in case the following step is wrong

#write.csv(data,"Schreibtisch/Seabirds/outlier_corrected_data_V2.csv", row.names = FALSE)

# continue working with data
## replace 0s in rows with many NAs -> not sure if correct , ask Giannina 
new_data[209:252,9:17]<- NA
new_data[468:504,9:17]<- NA
new_data[699:756,9:17]<- NA
new_data[968:1008,9:17]<- NA
new_data[1225:1260,9:17]<- NA
new_data[1477:1512,9:17]<- NA
new_data[1727:1764,9:17]<- NA
new_data[1965:2016,9:17]<- NA
new_data[2226:2268,9:17]<- NA
new_data[2476:2520,9:17]<- NA
new_data[2725:2772,9:17]<- NA
new_data[2981:3024,9:17]<- NA
new_data[3236:3276,9:17]<- NA
new_data[3487:3528,9:17]<- NA
new_data[3732:3780,9:17]<- NA
new_data[3985:4032,9:17]<- NA
new_data[4250:4260,9:17]<- NA
new_data[4494:4536,9:17]<- NA
new_data[4560:4571,9:17]<- NA
new_data[4719:4788,9:17]<- NA
new_data[5001:5040,9:17]<- NA
new_data[5239:5292,9:17]<- NA
new_data[5738:5796,9:17]<- NA
new_data[6000:6048,9:17]<- NA
new_data[6241:6300,9:17]<- NA
new_data[6508:6552,9:17]<- NA
new_data[6700:6804,9:17]<- NA
new_data[7006:7056,9:17]<- NA
new_data[7229:7308,9:17]<- NA
new_data[7515:7560,9:17]<- NA


## remove the ouliers from the boxplots

# merge columns again to create LONG format: 

data<- new_data %>% gather(Stage, Abundance, c(9:11, 12:14, 15:17))

# separate the TYPE column into the columns Species and Type
data<- separate(data, Stage, sep = "_", into = c("Species", "Stage"))




# create boxplots with the new dataset 

###
######  BREEDER   #########
#########################


data1<- data %>% filter(Stage =="breeder", Species =="Cormorant")
data2<- data %>% filter(Stage =="breeder", Species =="Booby")
data3<- data %>% filter(Stage =="breeder", Species =="Pelican")


fig1 <- plot_ly(data1, x = ~Abundance, color = ~Name_place, type = "box") 
fig2 <- plot_ly(data2, x = ~Abundance, color = ~Name_place, type = "box") 
fig3 <- plot_ly(data3, x = ~Abundance, color = ~Name_place, type = "box") 

fig <- subplot(fig1, fig2, fig3, nrows = 1)%>% 
  layout(title = 'Breeders')

annotations = list( 
  list( 
    x = 0.01,  
    y = 1.0,  
    text = "<b> Cormorant </b>",  
    xref = "paper", 
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.33,  
    y = 1,  
    text = "<b> Booby </b>",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.65,  
    y = 1,  
    text = "<b> Pelican </b>", 
    size = 16,
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ))

fig <- fig %>%layout(annotations = annotations) 
fig



###########################################################
##################### check for outliers:######################
##############################################################


str(data)
data<- as.data.frame(data)

########### Cormorant 

# I. Pescadores
data %>% filter(Name_place == "I. Pescadores", Species =="Cormorant", Stage=="breeder", Abundance >= 600000) ## 12/2007
pesc<- data %>% filter(Name_place == "I. Pescadores", Species =="Cormorant", Stage=="breeder") # look at other months

data[which(c(data$Name_place=="I. Pescadores" & data$Species =="Cormorant"& data$Stage=="breeder"& data$Abundance>= 600000)),] # index is 12399
data[12399,11]<- 60000 # replace the 600.000 with 60.000

# I, Macabi
data %>% filter(Name_place == "I. Macabí", Species =="Cormorant", Stage=="breeder", Abundance >= 870000) ## 09/2003 and 11/2005
pesc<- data %>% filter(Name_place == "I. Macabí", Species =="Cormorant", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Macabí" & data$Species =="Cormorant"& data$Stage=="breeder"& data$Abundance>= 870000)),] 
data[11602,11]<- 87500 
data[11616,11]<- 104000

# I. Chincha Centro
data %>% filter(Name_place == "I. Chincha Centro", Species =="Cormorant", Stage=="breeder", Abundance >= 340000) ## 10/2013
pesc<- data %>% filter(Name_place == "I. Chincha Centro", Species =="Cormorant", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Chincha Centro" & data$Species =="Cormorant"& data$Stage=="breeder"& data$Abundance>= 340000)),] 
data[8690,11]<- 34754 

# I. Ballestas
data %>% filter(Name_place == "I. Ballestas", Species =="Cormorant", Stage=="breeder", Abundance >= 705000) ## 10/2006
pesc<- data %>% filter(Name_place == "I. Ballestas", Species =="Cormorant", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Ballestas" & data$Species =="Cormorant"& data$Stage=="breeder"& data$Abundance>= 705000)),] 
data[7850,11]<- 75000 


## Booby

# I. Macabí
data %>% filter(Name_place == "I. Macabí", Species =="Booby", Stage=="breeder", Abundance >= 462000) 
pesc<- data %>% filter(Name_place == "I. Macabí", Species =="Booby", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Macabí" & data$Species =="Booby"& data$Stage=="breeder"& data$Abundance>= 462000)),] 
data[34306,11]<- 46200 

# "I. Guañape Norte"

data %>% filter(Name_place == "I. Guañape Norte", Species =="Booby", Stage=="breeder", Abundance >= 600000) ## 07/2010
pesc<- data %>% filter(Name_place == "I. Guañape Norte", Species =="Booby", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Guañape Norte" & data$Species =="Booby"& data$Stage=="breeder"& data$Abundance>= 462000)),] ## unsicher
data[34306,11]<- 46200 



# "I. Chincha Sur

data %>% filter(Name_place == "I. Chincha Sur", Species =="Booby", Stage=="breeder", Abundance >= 180000) ## 10 & 11 2011
pesc<- data %>% filter(Name_place == "I. Chincha Sur", Species =="Booby", Stage=="breeder") 

data[which(c(data$Name_place=="I. Chincha Sur" & data$Species =="Booby"& data$Stage=="breeder"& data$Abundance>= 462000)),] ## unsicher -> would not nessecarily consider outliers



# "I. Chincha   ## unischer 

data %>% filter(Name_place == "I. Chincha Norte", Species =="Booby", Stage=="breeder", Abundance >= 427000) ## 10/2014
pesc<- data %>% filter(Name_place == "I. Chincha Norte", Species =="Booby", Stage=="breeder") 
data[which(c(data$Name_place=="I. Chincha Norte" & data$Species =="Booby"& data$Stage=="breeder"& data$Abundance>= 427000)),] ## unsicher -> would not nessecarily consider outliers


#############################
#################################
## Pelican
####################################

# P. San Juan

data %>% filter(Name_place == "P. San Juan", Species =="Pelican", Stage=="breeder", Abundance >= 29000) ## 01/2011
pesc<- data %>% filter(Name_place == "P. San Juan", Species =="Pelican", Stage=="breeder") 
data[which(c(data$Name_place=="P. San Juan" & data$Species =="Pelican"& data$Stage=="breeder"& data$Abundance>= 29000)),] ## unsicher -> would not nessecarily consider outliers
data[60315,11]<- 2970 


# I. Santa  -> clear outlier, but no indication for mistakes

data %>% filter(Name_place == "I. Santa", Species =="Pelican", Stage=="breeder", Abundance >= 97875) ## 10/2010
pesc<- data %>% filter(Name_place == "I. Santa", Species =="Pelican", Stage=="breeder") 
#data[which(c(data$Name_place=="I. Santa" & data$Species =="Pelican"& data$Stage=="breeder"& data$Abundance>= 29000)),] ## unsicher -> would not nessecarily consider outliers
#data[60315,11]<- 2970 


# I. Macabi
data %>% filter(Name_place == "I. Macabí", Species =="Pelican", Stage=="breeder", Abundance >= 70000)  # 10 & 11 2003, 01/2004, 01/2005
pesc<- data %>% filter(Name_place == "I. Macabí", Species =="Pelican", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Macabí" & data$Species =="Pelican"& data$Stage=="breeder"& data$Abundance>= 70000)),] # there are 0 to much in all rows
data[56954,11]<- 8750 
data[56955,11]<- 15000 
data[56963,11]<- 15000
data[56974,11]<- 7500 

# I. Lobos de Afuera

data %>% filter(Name_place == "I. Lobos de Afuera", Species =="Pelican", Stage=="breeder", Abundance >= 75000)  # 10 & 11 2011, 01/2012
pesc<- data %>% filter(Name_place == "I. Lobos de Afuera", Species =="Pelican", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Macabí" & data$Species =="Pelican"& data$Stage=="breeder"& data$Abundance>= 70000)),] # I do not consider them mistakes -> for 3 months it is that high


# "I. Guañape Sur"

data %>% filter(Name_place == "I. Guañape Sur", Species =="Pelican", Stage=="breeder", Abundance >= 70000) ## 12/2009
pesc<- data %>% filter(Name_place == "I. Guañape Sur", Species =="Pelican", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Guañape Sur" & data$Species =="Pelican"& data$Stage=="breeder"& data$Abundance>= 70000)),] 
data[55760,11]<- 7097 


# "I. Guañape Norte"

data %>% filter(Name_place == "I. Guañape Norte", Species =="Pelican", Stage=="breeder", Abundance >= 60000) ## 10/2008
pesc<- data %>% filter(Name_place == "I. Guañape Norte", Species =="Pelican", Stage=="breeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Guañape Norte" & data$Species =="Pelican"& data$Stage=="breeder"& data$Abundance>= 60000)),] 
data[55496,11]<- 6000 


######
##### NONBREEDER   ##########
############################


data1<- data %>% filter(Stage =="nonbreeder", Species =="Cormorant")
data2<- data %>% filter(Stage =="nonbreeder", Species =="Booby")
data3<- data %>% filter(Stage =="nonbreeder", Species =="Pelican")


#ggplot(data1,aes(Name_place,Abundance))+geom_boxplot(aes(fill=Species)) + facet_wrap(~Species , ncol = 1, nrow=3 ,  scales="free_y")  +labs(title = "breeder")


fig1 <- plot_ly(data1, x = ~Abundance, color = ~Name_place, type = "box") 
fig2 <- plot_ly(data2, x = ~Abundance, color = ~Name_place, type = "box") 
fig3 <- plot_ly(data3, x = ~Abundance, color = ~Name_place, type = "box") 

fig <- subplot(fig1, fig2, fig3, nrows = 1)%>% 
  layout(title = 'Nonbreeders')

annotations = list( 
  list( 
    x = 0.01,  
    y = 1.0,  
    text = "<b> Cormorant </b>",  
    xref = "paper", 
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.33,  
    y = 1,  
    text = "<b> Booby </b>",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.65,  
    y = 1,  
    text = "<b> Pelican </b>", 
    size = 16,
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ))

fig <- fig %>%layout(annotations = annotations) 
fig


##########################################
############# check for outliers #####################
##########################################

## Cormorant

# P. Coles

data %>% filter(Name_place == "P. Coles", Species =="Cormorant", Stage=="nonbreeder", Abundance >= 480000)  # 07/2015
pesc<- data %>% filter(Name_place == "P. Coles", Species =="Cormorant", Stage=="nonbreeder") #looks like a 0 too much

data[which(c(data$Name_place=="P. Coles" & data$Species =="Cormorant"& data$Stage=="nonbreeder"& data$Abundance>= 480000)),] # there are 0 to much in all rows
data[5937,11]<- 48434 


## Booby

# Guanape Norte

data %>% filter(Name_place == "I. Guañape Norte", Species =="Booby", Stage=="nonbreeder", Abundance >= 5900000) ## 05/2010
pesc<- data %>% filter(Name_place == "I. Guañape Norte", Species =="Booby", Stage=="nonbreeder") # look at other months -> remove a 0 from the abundance number

data[which(c(data$Name_place=="I. Guañape Norte" & data$Species =="Booby"& data$Stage=="nonbreeder"& data$Abundance>= 5900000)),] 
data[25280,11]<- 593204 

## Pelican

# I. Chao
data %>% filter(Name_place == "I. Chao", Species =="Pelican", Stage=="nonbreeder", Abundance >= 371000) ## 07/2010
pesc<- data %>% filter(Name_place == "I. Chao", Species =="Pelican", Stage=="nonbreeder") # looks like a 0 too much
data[which(c(data$Name_place=="I. Chao" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 371000)),] 
data[46205,11]<- 37117 


# Guanape Norte # not sure

data %>% filter(Name_place == "I. Guañape Norte", Species =="Pelican", Stage=="nonbreeder", Abundance >= 235500) ## 02/2012
pesc<- data %>% filter(Name_place == "I. Guañape Norte", Species =="Pelican", Stage=="nonbreeder") # maybe one 5 too much, could be duplicated
data[which(c(data$Name_place=="I. Guañape Norte" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 235500)),] 
data[47981,11]<- 23525 


# I. Lobos de Tierra

data %>% filter(Name_place == "I. Lobos de Tierra", Species =="Pelican", Stage=="nonbreeder", Abundance >= 160000) ## 10/2006 & 04/2012
pesc<- data %>% filter(Name_place == "I. Lobos de Tierra", Species =="Pelican", Stage=="nonbreeder") # the 0s could be too much
data[which(c(data$Name_place=="I. Lobos de Tierra" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 160000)),] 
data[49176,11]<- 16000 
data[49243,11]<- 22935 


# P. Lomitas
data %>% filter(Name_place == "P. Lomitas", Species =="Pelican", Stage=="nonbreeder", Abundance >= 50000) ## 03/2020
pesc<- data %>% filter(Name_place == "P. Lomitas", Species =="Pelican", Stage=="nonbreeder") # the 0 could be too much
data[which(c(data$Name_place=="P. Lomitas" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 50000)),] 
data[52356,11]<- 5425 


# P. Coles

data %>% filter(Name_place == "P. Coles", Species =="Pelican", Stage=="nonbreeder", Abundance >= 80000) ## 06 & 07 2007, 05&06 2011
pesc<- data %>% filter(Name_place == "P. Coles", Species =="Pelican", Stage=="nonbreeder") # the 0s could be too much
data[which(c(data$Name_place=="P. Coles" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 80000)),] 
data[51202,11]<- 9500 
data[51203,11]<- 9080 
data[51245,11]<- 8805 
data[51247,11]<- 12875
data[51248,11]<- 8000 

# P. San Juan # not sure

data %>% filter(Name_place == "P. San Juan", Species =="Pelican", Stage=="nonbreeder", Abundance >= 25500) ##
pesc<- data %>% filter(Name_place == "P. San Juan", Species =="Pelican", Stage=="nonbreeder") # the 5 could be duplicated
data[which(c(data$Name_place=="P. San Juan" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 25000)),] 
data[52762,11]<- 25596 ## also still an outlier after correction



# I. Macabi 

data %>% filter(Name_place == "I. Macabí", Species =="Pelican", Stage=="nonbreeder", Abundance >= 112000) ## 09/2003
pesc<- data %>% filter(Name_place == "I. Macabí", Species =="Pelican", Stage=="nonbreeder") # the 0 could be too much
data[which(c(data$Name_place=="I. Macabí" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 112000)),] 
data[49402,11]<- 11200


# I. Lobos de Afuera

data %>% filter(Name_place == "I. Lobos de Afuera", Species =="Pelican", Stage=="nonbreeder", Abundance >= 120000) ## 09/2010 & 02/2012 
pesc<- data %>% filter(Name_place == "I. Lobos de Afuera", Species =="Pelican", Stage=="nonbreeder") # the 0s could be too much
data[which(c(data$Name_place=="I. Lobos de Afuera" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 120000)),] 
data[48972,11]<- 12000 
data[48988,11]<- 12600 


# I. Asia

data %>% filter(Name_place == "I. Asia", Species =="Pelican", Stage=="nonbreeder", Abundance >= 30000) ## 10/2010 & 01/2012 
pesc<- data %>% filter(Name_place == "I. Asia", Species =="Pelican", Stage=="nonbreeder") # the 0s could be too much
data[which(c(data$Name_place=="I. Asia" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 30000)),] 
data[45438,11]<- 3024 
data[45461,11]<- 6000 


# I. Don Martin

data %>% filter(Name_place == "I. Don Martín", Species =="Pelican", Stage=="nonbreeder", Abundance >= 30000) ## 12/2010 & 11/2015
pesc<- data %>% filter(Name_place == "I. Don Martín", Species =="Pelican", Stage=="nonbreeder") # the 0s could be too much, 11/2015 is fine
data[which(c(data$Name_place=="I. Don Martín" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 30000)),] 
data[47457,11]<- 7503 

# I. Huampano  # unischer

data %>% filter(Name_place == "I. Huampanú", Species =="Pelican", Stage=="nonbreeder", Abundance >= 50000) ## 06/2010 
pesc<- data %>% filter(Name_place == "I. Huampanú", Species =="Pelican", Stage=="nonbreeder") # clearly an outlier but not sure how to correct it
#data[which(c(data$Name_place=="I. Huampanú" & data$Species =="Pelican"& data$Stage=="nonbreeder"& data$Abundance>= 50000)),] 
#data[47457,11]<- 7503 



### COULD RUN PLOT CODE AGAIN TO SEE CORRECTED BOXPLOTS

### TO DO:  NEED TO SUM CORRECTED BREEDERS AND NONBREEDERS UP TO MAKE CORRECTED TOTAL


# store data without total adjusted in new file
write.csv(data,"Schreibtisch/Seabirds/outlier_corrected_data.csv", row.names = FALSE)








######################################
###############################################
##################################################

# USE OUTLIER CORRECTED DATA FOR THE HEATMAPS


library(readr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(hrbrthemes)  # DOES NOT WORK ON MY R-VERISON
library(viridis)
library(d3heatmap) # DOES NOT WORK ON MY R-VERSION
library(dplyr)
library(gplots)

# LOAD THE DATA
outlier_corrected_data <- read_csv("Schreibtisch/Seabirds/outlier_corrected_data.csv")
data<- outlier_corrected_data

data<- data %>% arrange(Name_place, Year, Month)


# CREATE DATE COLUMN
data$Date <- paste(data$Month, data$Year)
data$Date<- gsub(' ', '_', data$Date)


# SELECT SUBSETS OF THE DATA
CB <- data %>% filter(Species=="Cormorant", Stage == "breeder") # Cormorant breeder
CN <- data %>% filter(Species=="Cormorant", Stage == "nonbreeder") # Cormorant Nonbreeder
BB <- data %>% filter(Species=="Booby", Stage == "breeder")
BN <- data %>% filter(Species=="Booby", Stage == "nonbreeder")
PB <- data %>% filter(Species=="Pelican", Stage == "breeder")
PN <- data %>% filter(Species=="Pelican", Stage == "nonbreeder")

CB[,2:10] <- NULL   # remove location information etc. 
CN[,2:10] <- NULL
BB[,2:10] <- NULL
BN[,2:10] <- NULL
PB[,2:10] <- NULL
PN[,2:10] <- NULL


# create matrix format

CB<- pivot_wider(CB, names_from = Date, values_from = Abundance)
CB<- as.data.frame(CB)
rownames(CB) <- CB[,1]
CB<- CB[,-1]


CN<- pivot_wider(CN, names_from = Date, values_from = Abundance)
CN<- as.data.frame(CN)
rownames(CN) <- CN[,1]
CN<- CN[,-1]


BB<- pivot_wider(BB, names_from = Date, values_from = Abundance)
BB<- as.data.frame(BB)
rownames(BB) <- BB[,1]
BB<- BB[,-1]

BN<- pivot_wider(BN, names_from = Date, values_from = Abundance)
BN<- as.data.frame(BN)
rownames(BN) <- BN[,1]
BN<- BN[,-1]


PB<- pivot_wider(PB, names_from = Date, values_from = Abundance)
PB<- as.data.frame(PB)
rownames(PB) <- PB[,1]
PB<- PB[,-1]


PN<- pivot_wider(PN, names_from = Date, values_from = Abundance)
PN<- as.data.frame(PN)
rownames(PN) <- PN[,1]
PN<- PN[,-1]


# matrix format
CB<- as.matrix(CB)
CN<- as.matrix(CN)
BB<- as.matrix(BB)
BN<- as.matrix(BN)
PB<- as.matrix(PB)
PN<- as.matrix(PN)


p1 <- heatmaply(CB, 
               dendrogram = "none",
               xlab = "", ylab = "", 
               na.value =  "black",             # confused why nas are not black
               main = "Cormorant Breeder",
               scale = "none",
               # margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = F,
               branches_lwd = 0.1,
               label_names = c("Island", "Date:", "Abundance"),
               fontsize_row = 5, fontsize_col = 5,
               labCol = colnames(CB),
               labRow = rownames(CB),
               heatmap_layers = theme(axis.line=element_blank())
)

p1



p2 <- heatmaply(CN, 
                dendrogram = "none",
                xlab = "", ylab = "", 
                na.value =  "black",             # confused why nas are not black
                main = "Cormorant Nonbreeder",
                scale = "none",
                # margins = c(60,100,40,20),
                grid_color = "white",
                grid_width = 0.00001,
                titleX = FALSE,
                hide_colorbar = F,
                branches_lwd = 0.1,
                label_names = c("Island", "Date:", "Abundance"),
                fontsize_row = 5, fontsize_col = 5,
                labCol = colnames(CN),
                labRow = rownames(CN),
                heatmap_layers = theme(axis.line=element_blank())
)

p2



p3 <- heatmaply(BB, 
                dendrogram = "none",
                xlab = "", ylab = "", 
                na.value =  "black",             # confused why nas are not black
                main = "Booby Breeder",
                scale = "none",
                # margins = c(60,100,40,20),
                grid_color = "white",
                grid_width = 0.00001,
                titleX = FALSE,
                hide_colorbar = F,
                branches_lwd = 0.1,
                label_names = c("Island", "Date:", "Abundance"),
                fontsize_row = 5, fontsize_col = 5,
                labCol = colnames(BB),
                labRow = rownames(BB),
                heatmap_layers = theme(axis.line=element_blank())
)

p3



p4 <- heatmaply(BN, 
                dendrogram = "none",
                xlab = "", ylab = "", 
                na.value =  "black",             # confused why nas are not black
                main = "Booby Nonbreeder",
                scale = "none",
                # margins = c(60,100,40,20),
                grid_color = "white",
                grid_width = 0.00001,
                titleX = FALSE,
                hide_colorbar = F,
                branches_lwd = 0.1,
                label_names = c("Island", "Date:", "Abundance"),
                fontsize_row = 5, fontsize_col = 5,
                labCol = colnames(BN),
                labRow = rownames(BN),
                heatmap_layers = theme(axis.line=element_blank())
)

p4



p5 <- heatmaply(PB, 
                dendrogram = "none",
                xlab = "", ylab = "", 
                na.value =  "black",             # confused why nas are not black
                main = "Pelican Breeder",
                scale = "none",
                # margins = c(60,100,40,20),
                grid_color = "white",
                grid_width = 0.00001,
                titleX = FALSE,
                hide_colorbar = F,
                branches_lwd = 0.1,
                label_names = c("Island", "Date:", "Abundance"),
                fontsize_row = 5, fontsize_col = 5,
                labCol = colnames(PB),
                labRow = rownames(PB),
                heatmap_layers = theme(axis.line=element_blank())
)

p5



p6 <- heatmaply(PN, 
                dendrogram = "none",
                xlab = "", ylab = "", 
                na.value =  "black",             # confused why nas are not black
                main = "Pelican Nonbreeder",
                scale = "none",
                # margins = c(60,100,40,20),
                grid_color = "white",
                grid_width = 0.00001,
                titleX = FALSE,
                hide_colorbar = F,
                branches_lwd = 0.1,
                label_names = c("Island", "Date:", "Abundance"),
                fontsize_row = 5, fontsize_col = 5,
                labCol = colnames(PN),
                labRow = rownames(PN),
                heatmap_layers = theme(axis.line=element_blank())
)

p6




## save the heatmaps

library(htmlwidgets)
saveWidget(p1, file= "Schreibtisch/Seabirds/heatmaps/CB.html")
saveWidget(p2, file= "Schreibtisch/Seabirds/heatmaps/CN.html")
saveWidget(p3, file= "Schreibtisch/Seabirds/heatmaps/BB.html")
saveWidget(p4, file= "Schreibtisch/Seabirds/heatmaps/BN.html")
saveWidget(p5, file= "Schreibtisch/Seabirds/heatmaps/PB.html")
saveWidget(p6, file= "Schreibtisch/Seabirds/heatmaps/PN.html")














