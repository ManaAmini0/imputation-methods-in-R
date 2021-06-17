# this code is suitable for ILPD

my_data <- read.csv(file.choose(),header= FALSE)

s=dim(my_data)
################## change sex to num  

typeof(my_data[,2])
# my_data[,2]<- as.numeric(my_data[,2])

for (i in 1:s[1]) {
  t="Female"

  if(t==my_data[i,2]){
    my_data[i,2] <- '1'
  }
  else{
    my_data[i,2] <- '-1'
  }
}
my_data[,2]<- as.numeric(my_data[,2])

########################################## ADD MISSING VALUE
my_data<-as.matrix(my_data)
nan_data<-my_data

N=s[1]
tmp<-matrix(1 ,nrow = s[1] , ncol = 1)
for (i in 1:s[2]) {
  tmp[,1]=nan_data[,i]
  tmp[rbinom(N, 1, 0.1) == 1] <- NA 
  nan_data[,i]=tmp[,1]
}

################################################################ median & mean
library(mice)
library(Hmisc)
library(impute)

nan_data=as.matrix(nan_data)

impmedian<-matrix(1, nrow = s[1], ncol = s[2])
impmean<-matrix(1, nrow = s[1], ncol = s[2])


for (i in 1:s[2]) {
  impmedian[,i]=impute(nan_data[,i], median)   # this is TRUE
  
  impmean[,i]=impute(nan_data[,i], mean) #this is TRUE
}

#################  RMSE

nrmse_median=0
nrmse_mean=0

for (i in 1:s[2]) {
  rmse_median=0
  rmse_mean=0
  
  max_=max(my_data[,i])
  min_=min(my_data[,i])
  
  t_median=sqrt( mean((my_data[,i]-impmedian[,i])^2) / (max_ - min_) )
  nrmse_median<-nrmse_median+t_median
  
  t_mean=sqrt( mean((my_data[,i]-impmean[,i])^2) / (max_ - min_) )
  nrmse_mean<-nrmse_mean+t_mean
  
  
}
nrmse_mean<-nrmse_mean/s[2]
nrmse_median<-nrmse_median/s[2]

################################################################### pmm  

nrmse_pmm=0

for (i in 1:s[2]) {
  
  ### create data to impute with PMM
  if(i>1 && i<s[2]){
    tmpdata=data.frame(nan_data[,i] , my_data[,1:(i-1)] ,if(i<s[2]){my_data[,(i+1):s[2]]})
  }
  if(i==1){
    tmpdata=data.frame(nan_data[,i] , my_data[,(i+1):s[2]])
  }
  if(i==s[2]){
    tmpdata=data.frame(nan_data[,i] , my_data[,1:(i-1)])
  }
  
  ### apply pmm algorithm
  m=s[2]
  imp_multi <- mice(tmpdata, m , method = "pmm")  # Impute missing values multiple times
  data_imp_multi_all <- complete(imp_multi,       # Store multiply imputed data
                                 "repeated",
                                 include = TRUE)
  
  data_imp_multi <- data.frame(data_imp_multi_all[ , 1:m+1], tmpdata[, 2:s[2]]) # Combine imputed Y and X1-X4 (for convenience)
  
  ##########################  RMSE
  rmse=0  # it's for each column of dataset
  
  max_=max(my_data[,i])
  min_=min(my_data[,i])  
  
  for (j in 1:m-1) {
    t=0
    
    t=sqrt( mean((my_data[,i]-data_imp_multi[,j])^2) / (max_ - min_) )
    if(is.na(t)){
      t=0
    }
    rmse=t+rmse
  }
  rmse<-rmse/m
  nrmse_pmm<-nrmse_pmm+rmse
  
  # print(rmse)
  ##########################
  remove(imp_multi)
  remove(data_imp_multi)
  remove(data_imp_multi_all)

}
nrmse_pmm<-nrmse_pmm/s[2]

print("NRMSE for pmm:  ")
print(nrmse_pmm)

###################################################################  norm
nrmse_norm=0

for (i in 1:s[2]) {
  
  ### create data to impute with norm
  if(i>1 && i<s[2]){
    tmpdata=data.frame(nan_data[,i] , my_data[,1:(i-1)] ,if(i<s[2]){my_data[,(i+1):s[2]]})
  }
  if(i==1){
    tmpdata=data.frame(nan_data[,i] , my_data[,(i+1):s[2]])
  }
  if(i==s[2]){
    tmpdata=data.frame(nan_data[,i] , my_data[,1:(i-1)])
  }
  
  ### apply pmm algorithm
  m=s[2]
  imp_multi <- mice(tmpdata, m , method = "norm")  # Impute missing values multiple times
  data_imp_multi_all <- complete(imp_multi,       # Store multiply imputed data
                                 "repeated",
                                 include = TRUE)
  
  data_imp_multi <- data.frame(data_imp_multi_all[ , 1:m+1], tmpdata[, 2:s[2]]) # Combine imputed 
  ##########################  RMSE
  rmse=0  # it's for each column of dataset
  
  max_=max(my_data[,i])
  min_=min(my_data[,i])  
  
  for (j in 1:m-1) {
    t=0
    
    t=sqrt( mean((my_data[,i]-data_imp_multi[,j])^2) / (max_ - min_) )
    if(is.na(t)){
      t=0
    }
    rmse=t+rmse
    
  }
  rmse<-rmse/m
  nrmse_norm<-nrmse_norm+rmse

  ##########################
  remove(imp_multi)
  remove(data_imp_multi)
  remove(data_imp_multi_all)
  
}
nrmse_norm<-nrmse_norm/s[2]

print("NRMSE for norm:  ")
print(nrmse_norm)

############################################################################  norm.nob
nrmse_nob=0
for (i in 1:s[2]) {
  
  ### create data to impute with norm.nob
  if(i>1 && i<s[2]){
    tmpdata=data.frame(nan_data[,i] , my_data[,1:(i-1)] ,if(i<s[2]){my_data[,(i+1):s[2]]})
  }
  if(i==1){
    tmpdata=data.frame(nan_data[,i] , my_data[,(i+1):s[2]])
  }
  if(i==s[2]){
    tmpdata=data.frame(nan_data[,i] , my_data[,1:(i-1)])
  }
  
  ### apply pmm algorithm
  m=s[2]
  imp_multi <- mice(tmpdata, m , method = "norm.nob")  # Impute missing values multiple times
  data_imp_multi_all <- complete(imp_multi,       # Store multiply imputed data
                                 "repeated",
                                 include = TRUE)
  
  data_imp_multi <- data.frame(data_imp_multi_all[ , 1:m+1], tmpdata[, 2:s[2]]) # Combine imputed
  
  ##########################  RMSE
  rmse=0  # it's for each column of dataset
  
  max_=max(my_data[,i])
  min_=min(my_data[,i])  
  
  for (j in 1:m-1) {
    t=0
    t=sqrt( mean((my_data[,i]-data_imp_multi[,j])^2) / (max_ - min_) )
    if(is.na(t)){
      t=0
    }
    rmse=t+rmse
  }
  rmse<-rmse/m
  nrmse_nob<-nrmse_nob+rmse
  
  print(rmse)
  ##########################
  remove(imp_multi)
  remove(data_imp_multi)
  remove(data_imp_multi_all)
  
}
nrmse_nob<-nrmse_nob/s[2]
print("NRMSE for norm.nob:  ")
print(nrmse_nob)

############################################################################  sample
nrmse_sample=0
for (i in 1:s[2]) {
  
  ### create data to impute with sample
  if(i>1 && i<s[2]){
    tmpdata=data.frame(nan_data[,i] , my_data[,1:(i-1)] ,if(i<s[2]){my_data[,(i+1):s[2]]})
  }
  if(i==1){
    tmpdata=data.frame(nan_data[,i] , my_data[,(i+1):s[2]])
  }
  if(i==s[2]){
    tmpdata=data.frame(nan_data[,i] , my_data[,1:(i-1)])
  }
  
  ### apply pmm algorithm
  m=s[2]
  imp_multi <- mice(tmpdata, m , method = "sample")  # Impute missing values multiple times
  data_imp_multi_all <- complete(imp_multi,       # Store multiply imputed data
                                 "repeated",
                                 include = TRUE)
  
  data_imp_multi <- data.frame(data_imp_multi_all[ , 1:m+1], tmpdata[, 2:s[2]]) # Combine imputed Y and X1-X4 (for convenience)
  
  ##########################  RMSE
  rmse=0  # it's for each column of dataset
  
  max_=max(my_data[,i])
  min_=min(my_data[,i])  
  
  for (j in 1:m-1) {
    t=0
    t=sqrt( mean((my_data[,i]-data_imp_multi[,j])^2) / (max_ - min_) )
    if(is.na(t)){
      t=0
    }
    rmse=t+rmse
  }
  rmse<-rmse/m
  nrmse_sample<-nrmse_sample+rmse
  
  #print(rmse)
  
  ##########################
  remove(imp_multi)
  remove(data_imp_multi)
  remove(data_imp_multi_all)
  
}
nrmse_sample<-nrmse_sample/s[2]
print("NRMSE for sample:  ") 
print(nrmse_sample)


###############################################################################  KNN

library(VIM)
library(laeken)


# impKNN=kNN(nan_data[,i], numFun = weightedMean, weightDist=TRUE)


tmp=kNN(
  nan_data,
  variable = colnames(nan_data),
  metric = NULL,
  k = 5,
  dist_var = colnames(nan_data),
  weights = NULL,
  numFun = median,
  catFun = maxCat,
  makeNA = NULL,
  NAcond = NULL,
  impNA = TRUE,
  donorcond = NULL,
  mixed = vector(),
  mixed.constant = NULL,
  trace = FALSE,
  imp_var = TRUE,
  imp_suffix = "imp",
  addRF = FALSE,
  onlyRF = FALSE,
  addRandom = FALSE,
  useImputedDist = TRUE,
  weightDist = FALSE
)

impKNN<-tmp[,1:s[2]]
###########

nrmse_knn=0

for (i in 1:s[2]) {
  rmse_knn=0
  
  max_=max(my_data[,i])
  min_=min(my_data[,i])
  
  t=sqrt( mean((my_data[,i]-impKNN[,i])^2) / (max_ - min_) )
  nrmse_knn<-nrmse_knn+t
  
  
}
nrmse_knn<-nrmse_knn/s[2]

print("NRMSE for KNN:  ") 
print(nrmse_knn)

##################################################################  represent 

df <- data.frame(mean=nrmse_mean,
                 median=nrmse_median,
                 KNN=nrmse_knn,
                 pmm=nrmse_pmm,
                 norm=nrmse_norm,
                 norm_nob=nrmse_nob,
                 sample=nrmse_sample
                 )
print(df)

