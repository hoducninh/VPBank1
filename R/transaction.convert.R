#' @title Quickly convert data frame to transaction
#' @description This function helps users to convert data frame to
#' transaction data quickly
#' @author Anh Hoang Duc (anhhd3@vpbank.com.vn)
#' @param Data set - This functions supports 2 kinds of data wide & long
#' @param Type - Two option: "wide" and "long"
#' @keywords Data converter
#' @return Return a transaction data
#' @import arulesViz dplyr arules reshape2
#' @export
#' @examples
#' df1 <- data.frame(Transaction_ID = c(1,2,1,3,1,2), Products = c(2,3,2,4,4,5))
#' transaction.convert(df1, type = "long")
#' df2 <- data.frame(ID = c(1,2,3), Product_1 = c(1,1,0), Product_2 = c(0,1,1), Product_3 = c(0,0,1))
#' transaction.convert(df2, type = "wide")

transaction.convert <- function(data, type = c("wide","long")){
  type <- match.arg(type)
  library(dplyr)
  library(reshape2)
  library(arules)
  library(arulesViz)
  if (type == "wide"){
    names(data)[1]="ID"
    data[data==0] <- NA
    data.product.melt <- melt(data, id.vars = "ID") %>% na.omit %>% select(-3) %>% distinct()
    write.table(data.product.melt, file = "transaction.txt", sep = ",", col.names = FALSE,
                row.names = FALSE)
    transaction <- read.transactions("transaction.txt", format = "single", cols=c(1,2), sep = ",", encoding = "Unicode")
    fn <- "transaction.txt"
    if(file.exists(fn)) file.remove(fn)  
  }
  else{
    data.product.melt <- data %>% select(c(1,2)) %>% distinct()
    write.table(data.product.melt, file = "transaction.txt", sep = ",", col.names = FALSE,
                row.names = FALSE)
    transaction <- read.transactions("transaction.txt", format = "single", cols=c(1,2), sep = ",", encoding = "Unicode")
    fn <- "transaction.txt"
    if(file.exists(fn)) file.remove(fn)    
  }
  return(transaction);
}
