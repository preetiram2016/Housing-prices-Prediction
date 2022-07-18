setwd("C:/Users/Admin/Desktop/Preeti IIT Kanpur/R language/Projects")

housing_train = read.csv("housing_train.csv")
housing_test  = read.csv("housing_test.csv")

View(housing_train)

housing_test$Price=NA

housing_train$data = "train"
housing_test$data = "test"

housing_all = rbind(housing_train,housing_test)
View(housing_all)

library(dplyr)
glimpse(housing_all)

table(housing_all$Suburb)

table(housing_all$Address)

housing_all$Postcode = NULL

table(housing_all$Type)

table(housing_all$Method)

table(housing_all$SellerG)

table(housing_all$CouncilArea)

names(housing_all)[sapply(housing_all,function(x) is.character(x))]

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

cat_col = c("Suburb","Type","Method","SellerG","CouncilArea")

for (cat in cat_col)
{
  housing_all = CreateDummies(housing_all,cat,50)
}

## find missing values

lapply(housing_all,function(x) sum(is.na(x)))

## Replacing NA's with mean

for (col in names(housing_all))
{
  if (sum(is.na(housing_all[,col])) > 0 & !(col %in% c("data","Price")))
  {
    housing_all[is.na(housing_all[,col]),col] = mean(housing_all[housing_all$data=="train",col],na.rm = T)
  }
}

housing_all = housing_all %>% 
              mutate(YearBuilt = round(YearBuilt,1),
                     Bedroom2 = round(Bedroom2,0),
                     Bathroom = round(Bathroom,0),
                     Car = round(Car,0))


max_seller = housing_train$Price[which.max(housing_train$SellerG)]

## splitting Address column:

tidyr::extract(housing_all, Address, c("num", "city1","city2"), 
               regex = "(.+) (\\w+) (\\w+)")

library(tidyr)

housing_all = housing_all %>% 
              extract(Address, c("num", "city1","city2"), 
                      regex = "(.+) (\\w+) (\\w+)") %>% 
              mutate(New_Address = paste0(city1," ",city2)) %>% 
              select(-num,-city1,-city2)

View(housing_train)

table(housing_all$New_Address)

## separate train and test data from housing_all

housing_train = housing_all %>% 
                filter(housing_all$data == "train") %>% 
                select(-data)

housing_test = housing_all %>% 
  filter(housing_all$data == "test") %>% 
  select(-data,-Price)

### separate train data into train set and validation set

set.seed(2)
s = sample(1:nrow(housing_train),0.7*nrow(housing_train))
housing_train1 = housing_train[s,]
housing_train2 = housing_train[-s,]

names(housing_train1)

housing_train1$New_Address = NULL

## Build linear model

fit = lm(Price~.-CouncilArea_-Distance,data = housing_train1)

summary(fit)

## Drop variables based on VIF

library(car)

vif(fit)

sort(vif(fit),decreasing = T)

## Drop variables based on p values

fit = step(fit)

summary(fit)

fit = lm(Price~.-CouncilArea_
         -Distance
         -Landsize
         -Suburb_Abbotsford
         -Suburb_SouthMelbourne
         -Suburb_NorthMelbourne
         -Suburb_Murrumbeena
         -Suburb_Ashburton 
         -Suburb_BrunswickEast
         -Suburb_Niddrie
         -Suburb_FitzroyNorth
         -Suburb_Ormond
         -Suburb_Strathmore 
         -Suburb_Burwood
         -Suburb_BrunswickWest 
         -Suburb_Sunshine 
         -Suburb_SurreyHills
         -Suburb_HawthornEast
         -Suburb_Elwood
         -Suburb_Newport 
         -Suburb_Doncaster
         -Suburb_AscotVale
         -Suburb_Footscray 
         -Suburb_Thornbury 
         -Suburb_Yarraville 
         -Suburb_Carnegie 
         -Suburb_PortMelbourne 
         -Suburb_Bentleigh           
         -Suburb_Hawthorn  
         -Suburb_Brunswick           
         -Suburb_StKilda             
         -Suburb_Richmond            
         -Method_SP
         -SellerG_Rendina           
         -SellerG_Raine            
         -SellerG_Love  
         -SellerG_Williams           
         -SellerG_Village             
         -SellerG_Stockdale         
         -SellerG_Hodges             
         -SellerG_McGrath            
         -SellerG_Noel              
         -SellerG_Gary              
         -SellerG_Jas               
         -SellerG_Miles 
         -SellerG_Sweeney 
         -SellerG_Fletchers          
         -SellerG_Woodards
         -SellerG_Brad              
         -SellerG_Biggin            
         -SellerG_Ray               
         -SellerG_Buxton
         -SellerG_hockingstuart
         -SellerG_Nelson            
         -CouncilArea_Whitehorse     
         -CouncilArea_Bayside  
         -CouncilArea_GlenEira  
         -CouncilArea_Darebin
         -Suburb_Rosanna
         -Suburb_Bulleen 
         -Suburb_Melbourne
         -SellerG_Barry             
         ,data = housing_train1)

## lets predict the Prices

val.pred = predict(fit,newdata = housing_train2)
errors = housing_train2$Price-val.pred

## mean absolute error(MAE)

mae = abs(errors)/housing_train2$Price

hist(mae)

## finding RMSE(root mean square error)
library(dplyr)
e = errors**2 %>% mean() %>% sqrt()

sqrt(var(val.pred)) 

1-var(errors)/var(housing_train2$Price)  ### R^2 value
score = 212467/e

library(ggplot2)
ggplot(data=housing_train2, aes(x=housing_train2$Price, y=val.pred)) + 
  geom_point() +
  geom_point(data=housing_train2, aes(x=housing_train2$Price, y=val.pred), color='green')


ggplot(housing_train2,aes(x = Rooms,y = errors))+geom_point()
## Build linear model on Original data

real_fit = lm(Price~.,data = housing_train)

summary(real_fit)

housing_train$New_Address = NULL

names(housing_train)
## Drop variables based on VIF

library(car)

vif(real_fit)

sort(vif(real_fit),decreasing = T)

real_fit = lm(Price~.-CouncilArea_-Distance,data = housing_train)

## Drop variables based on p values

real_fit = step(real_fit)

summary(real_fit)

real_fit = lm(Price~.-CouncilArea_
         -Distance
         -Landsize
         -Suburb_Abbotsford
         -Suburb_SouthMelbourne
         -Suburb_NorthMelbourne
         -Suburb_Murrumbeena
         -Suburb_Ashburton 
         -Suburb_BrunswickEast
         -Suburb_Niddrie
         -Suburb_FitzroyNorth
         -Suburb_Ormond
         -Suburb_Strathmore 
         -Suburb_Burwood
         -Suburb_BrunswickWest 
         -Suburb_Sunshine 
         -Suburb_SurreyHills
         -Suburb_HawthornEast
         -Suburb_Elwood
         -Suburb_Newport 
         -Suburb_Doncaster
         -Suburb_AscotVale
         -Suburb_Footscray 
         -Suburb_Thornbury 
         -Suburb_Yarraville 
         -Suburb_Carnegie 
         -Suburb_PortMelbourne 
         -Suburb_Bentleigh           
         -Suburb_Hawthorn  
         -Suburb_Brunswick           
         -Suburb_StKilda             
         -Suburb_Richmond            
         -Method_SP
         -SellerG_Rendina           
         -SellerG_Raine            
         -SellerG_Love  
         -SellerG_Williams           
         -SellerG_Village             
         -SellerG_Stockdale         
         -SellerG_Hodges             
         -SellerG_McGrath            
         -SellerG_Noel              
         -SellerG_Gary              
         -SellerG_Jas               
         -SellerG_Miles 
         -SellerG_Sweeney 
         -SellerG_Fletchers          
         -SellerG_Woodards
         -SellerG_Brad              
         -SellerG_Biggin            
         -SellerG_Ray               
         -SellerG_Buxton
         -SellerG_hockingstuart
         -SellerG_Nelson            
         -CouncilArea_Whitehorse     
         -CouncilArea_Bayside  
         -CouncilArea_GlenEira  
         -CouncilArea_Darebin
         -Suburb_Rosanna
         -Suburb_Bulleen 
         -Suburb_Melbourne
         -SellerG_Barry
         -Suburb_MooneePonds
         ,data = housing_train)


## lets predict the Prices

real.pred = predict(real_fit,newdata = housing_test)


## save project into folder
write.csv(real.pred,"U_Preeti_P1_Part2",row.names = F)

getwd() ## to check current working directory

plot(real_fit,1) # residual vs fitted values => non-linearity in the data exists or not

plot(real_fit,2) # errors are normal or not

plot(real_fit,3) # variance is constant or not

plot(real_fit,4) # outliers in the data if cook's distance >1


##### project 1 part(1)


table(housing_train$SellerG)

glimpse(houseing_train)
length(table(housing_train$Postcode))

sqrt(var(housing_train$Price))

###
hist(housing_train$Distance,main = "Normal DIstribution")

###
sum(is.na(housing_train$YearBuilt))

##
table(housing_train$Type)
sum(is.na(housing_train$Type))

type_h = housing_train$Price[housing_train$Type == "h"]
mean(type_h)
type_t = housing_train$Price[housing_train$Type == "t"]
mean(type_t)
mean(type_h)-mean(type_t)

##
max(tapply(housing_train$Price,housing_train$CouncilArea,mean))

###
max(tapply(housing_train$Price,housing_train$CouncilArea,var))

sum(is.na(housing_train$CouncilArea))

###
sum(is.na(housing_train$SellerG))

table(housing_train$SellerG)

seller_prices = tapply(housing_train$Price,housing_train$SellerG,sum)    
max(seller_prices)   

##
var(housing_train$Price)  ## standard deviation(sd)=sqrt(variance)
