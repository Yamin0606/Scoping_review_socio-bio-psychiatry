# These are homemade functions 
#replace "" with NA, x is data frame
rpls <- function(x){
  for (i in 2:ncol(x)){
    x[,i] <- ifelse(x[,i]=="",NA,x[,i])
  }
  return(x)
}

#for mat a data frame; x is data frame
format <- function(x){
  x1 <- cbind(x$Var1,as.data.frame(lapply(x[,2:3],as.numeric)));names(x1)[1] <- "Var1"
  return(x1)
}

#
merge_dup <- function(dd,d1,d2){
  freq=cbind(table(d1$ID),table(d2$ID))
  for (i in unique(dd$ID)) {
    num1=freq[i,1]
    num2=freq[i,2]
    key10=d1[d1$ID==i,"value1"]
    key20=d2[d2$ID==i,"value2"]
    key1_n <- c()
    for (j in 1:num1){
      key1_n <- c(key1_n,rep(key10[j],num2))
    }
    df_n <- data.frame(ID=rep(i,num1*num2),key1=key1_n,key2=rep(key20,num1))
    df <- rbind(df,df_n)
  }
  return(df)
}


