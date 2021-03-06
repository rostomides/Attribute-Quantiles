#Created by : Larbi Bedrani
#Date : April 18, 2018
#Version: 1.2

#For a given column COL of numeric values in a dataset, this function append a new column containing the quantile number in which falls the corresponding numerical value. The appended column's name is "Quantiles + number of quantiles + COL".
#i.e. If a column Costs is splitted into 50 quantiles using this function, it will return the original dataset to which a column "Quantiles_50_Costs" is appended.

Attribute_quantiles = function(dataset, column_name, n_quantiles){
  dataset <- as.matrix(dataset)
  if(n_quantiles > nrow(dataset)){
    message("Number of quantiles cannot be higher than dataset's row number")
    return(NaN)
  }
  #Calculate the number of elements per quantile
  q_length <- floor(nrow(dataset)/n_quantiles)
  #If number of elements per quantile is not int get the remainder
  q_rem <- nrow(dataset)%%n_quantiles
  
  #Calculate the number of element per quantile
  n_elements <- rep(q_length, n_quantiles)
  #If q_rem != 0 then add one to the qrem first quantiles length
  if(q_rem>0){
    n_elements[1:q_rem] <- n_elements[1:q_rem] + 1
  }
  
  #Create the quantile labels vector (Append the quantile number to the letter Q)
  quantile_number <- 0
  quantile_labels <-c()
  qq <- sapply(n_elements, function(x){
    quantile_number <<- quantile_number + 1
    quantile_labels <<-c(quantile_labels, rep(paste0("Q", quantile_number), x))
    return()
  })
  #Create a colomn allowing to get the initial ordering of the rows
  dataset <- cbind(dataset, "OrDeRiNg" = seq(1:nrow(dataset)))
  #Order the dataset by the column
  dataset <- dataset[order(as.numeric(dataset[,column_name])),]
  #append the quantile's label vector 
  dataset <- cbind(dataset, quantile_labels)
  colnames(dataset)[ncol(dataset)] <- paste("Quantiles", n_quantiles, column_name, sep="_")
  
  #Get back the initial ordering
  dataset <- dataset[order(dataset[,"OrDeRiNg"]),]
  dataset <- dataset[,!colnames(dataset)%in%c("OrDeRiNg")]
  
  #Return the dataset
  return(dataset)
}


#######################
#Test it!
######################

dataset = data.frame("labels"=sample(paste0("lab", seq(1000))), "Values"=rnorm(1000, 120, 6))


dataset_15 = Attribute_quantiles(dataset=dataset, column_name="Values", n_quantiles=15)
dataset_49 = Attribute_quantiles(dataset=dataset, column_name="Values", n_quantiles=49)
dataset_1000 = Attribute_quantiles(dataset=dataset, column_name="Values", n_quantiles=1000)




