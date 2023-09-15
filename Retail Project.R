setwd("D:\\IITK DATA ANALYTICS\\Final Projects\\Retail Project")
getwd()


retail_train = read.csv("store_train.csv", stringsAsFactors = F)
retail_test = read.csv("store_test.csv", stringsAsFactors = F)

table(retail_train$store)

library(dplyr)
glimpse(retail_train)

retail_test$store = NA

retail_test$data = 'test'
retail_train$data = 'train'

retail_all = rbind(retail_train,retail_test) # Combing the datasets

glimpse(retail_all)

# excluding this column as it doesn't have a affect on the decision
retail_all$Id = NULL 
retail_all$state_alpha = NULL
retail_all$countyname = NULL
retail_all$Areaname = NULL
retail_all$storecode = NULL
retail_all$countytownname = NULL


# in this step we have assigned dummy variables to the column "store_type"
table(retail_all$store_Type)

retail_all = retail_all %>%    
  mutate(
    gs = as.numeric(store_Type == "Grocery Store"),
    sm1 = as.numeric(store_Type == "Supermarket Type1"),
    sm2 = as.numeric(store_Type == "Supermarket Type2"),
    sm3 = as.numeric(store_Type == "Supermarket Type3"),
  ) %>%
  select(-store_Type)


glimpse(retail_all)

lapply(retail_all,function(retail_all) sum(is.na(retail_all)))

# Omitting NA value rows from population and country

retail_all = retail_all[!is.na(retail_all$population), ]
retail_all = retail_all[!is.na(retail_all$country), ]


# separate the data set

retail_train = retail_all %>% filter(data == 'train') %>% select(-data)
retail_test = retail_all %>% filter(data == 'test') %>% select(-data,-store)

# splitting the data into 80-20 for testing

set.seed(2)
s = sample(1:nrow(retail_train),0.8*nrow(retail_train))
retail_train_data1 = retail_train[s,]
retail_train_data2 = retail_train[-s,]

# drop variable based on vif

library(car)

for_vif = lm(store~.-sm3-sales0-sales2-sales3,data = retail_train_data1)
sort(vif(for_vif), decreasing = T)

formula(for_vif)


log_fit = glm(store ~ (sales0 + sales1 + sales2 + sales3 + sales4 + country + 
                         State + CouSub + population + gs + sm1 + sm2 + sm3) - sm3 - 
                sales0 - sales2 - sales3,
              data = retail_train_data1, family = "binomial")


summary(log_fit)

log_fit = step(log_fit)

formula(log_fit)

log_fit = glm(store ~ sales4, 
              data = retail_train_data1, family = "binomial")
summary(log_fit)


#performance validation
library(pROC)

train_score = predict(log_fit,newdata = retail_train_data2, type = 'response')

auc(roc(retail_train_data2$store,train_score))


# auc for train data in 0.6855
# auc for entire data

for_vif = lm(store~.-sm3-sales0-sales2-sales3, data = retail_train)
sort(vif(for_vif), decreasing = T)

formula(for_vif)
summary(for_vif)

log_fit_final = glm(store ~ (sales0 + sales1 + sales2 + sales3 + sales4 + country + 
                               State + CouSub + population + gs + sm1 + sm2 + sm3) - sm3 - 
                      sales0 - sales2 - sales3,
                    data = retail_train, family = 'binomial')

summary(log_fit_final)

log_fit_final = step(log_fit_final)

summary(log_fit_final)

log_fit_final = glm(store ~ sales4, 
              data = retail_train, family = "binomial")
summary(log_fit_final)


#probability scores

prob_score = predict(log_fit_final,newdata = retail_test, type = 'response')

auc(roc(retail_test$store,prob_score))

write.csv(prob_score,"Punit Alkunte - Retail Project - Attemp 1.csv", row.names = F)

















