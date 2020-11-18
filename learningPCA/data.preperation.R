#===============================================================================
# PREPARE THE FEA State DATA FOR A PCA
#===============================================================================
#load the data
my.data <- read.csv (paste (path.data.raw, 'StateAndCountyData.csv',                             sep = ""))
str(my.data)
head(my.data)

#load variable names and descriptions
variables <- read.csv(paste(path.data.raw, 'VariableList.csv',sep = ""))

#===============================================================================
#Data cleaning
#===============================================================================

#get rid of the variable that has only NA entries 
variables <- rbind.data.frame(variables[c(1:19 , 21: nrow(variables)), ])

#set up data for a singular value decomposition
#all collumns have to be numeric
#so,let's create a matrix with just values of different variables of states
#we will call this table var.tb which we will leave empty for now:

#*  State\dimensions       var1  var2  ...
#*  State1
#*  State2
#*  .
#*  .
#*  .

States <- c(sort(unique(my.data$State)))
var.tb <- data.frame(States = States)

#create a first column for variable 1
#iterate over states with a particular variable
#selectthe vriable I want to calculate means for
for (v in 1:length(variables$Variable_Code)){
  var.cd = variables$Variable_Code[v]
  #now we have the vectorswe need to create the data frame var.tb which later will 
  #contain all mean values for all variables for each state:
  
  #---------create vector of mean  variable for each state--------------
  # create output vector list that can be filled with means of explanatory
  # variable for each state
  St.var.cd <- c(rep(NA, length(States)))
  
  #create vector of mean variable for each state
  #retrieve explanatory variable data
  var.cd.data <- my.data[my.data$Variable_Code == var.cd, ]
  
  #calulate the mean of that variable per county for each state 
  for (s in 1: length(States) ){
    St.var.cd[s] <- mean( na.omit(c(var.cd.data$Value 
                                    [var.cd.data$State ==  States[s]])))
  }
  
  #bind the new column to the table of all variables
  var.tb <- cbind.data.frame(var.tb, St.var.cd)
}

#let'sremove the first column States, which was just there to provide the frame
#work and move it into the row names
#remove first column

row.names(var.tb) <- States #assign States vector to rownames
colnames (var.tb) <- c('States',variables$Variable_Code)
var.tb <- var.tb[,2:ncol(var.tb)]

#replace the missing data entries with the mean of the collumn 
for (i in 1:nrow(var.tb)){
  for (j in 1:ncol(var.tb)){
    if (is.na (var.tb[i,j])){
      var.tb[i,j] <- mean(na.omit(var.tb[,j]))
    }
  }
}

#subtract column means from every entry in the dataframe var.tb to centre the
#data
for (i in 1:ncol(var.tb)){
  var.tb[,i] <- var.tb[,i] - c(rep( mean(var.tb[,i]), nrow(var.tb)))
}

#ultimately we will save the new table as a csv file in the data.clean
#folder
write.csv(var.tb, 
          paste(path.data.clean,"StateDataClean.csv", sep = ""), 
          row.names = FALSE)

# also save the variable list
write.csv(variables, 
          paste(path.data.clean,"VariableListClean.csv", sep = ""), 
          row.names = FALSE)

#===============================================================================
# PREPARE THE FEA COUNTY DATA FOR A PCA
#===============================================================================
#load the data
my.data <- read.csv (paste (path.data.raw, 'StateAndCountyData.csv',                             sep = ""))
str(my.data)
head(my.data)

#load variable names and descriptions
variables <- read.csv(paste(path.data.raw, 'VariableList.csv',sep = ""))

#===============================================================================
#Data cleaning
#===============================================================================

#get rid of the variable that has only NA entries 
variables <- rbind.data.frame(variables[c(1:19 , 21: nrow(variables)), ])

#set up data for a singular value decomposition
#all collumns have to be numeric
#so,let's create a matrix with just values of different variables of County
#we will call this table var.tb which we will leave empty for now:

#*  State\dimensions       var1  var2  ...
#*  State1
#*  State2
#*  .
#*  .
#*  .

County <- c(sort(unique(my.data$County)))
var.tb <- data.frame(County = County)



#create a first column for variable 1
#iterate over County with a particular variable
#selectthe vriable I want to calculate means for
for (v in 1:length(variables$Variable_Code)){
  var.cd = variables$Variable_Code[v]
  #now we have the vectorswe need to create the data frame var.tb which later will 
  #contain all mean values for all variables for each state:
  
  #---------create vector of mean  variable for each state--------------
  # create output vector list that can be filled with means of explanatory
  # variable for each state
  St.var.cd <- c(rep(NA, length(County)))
  
  #create vector of mean variable for each state
  #retrieve explanatory variable data
  var.cd.data <- my.data[my.data$Variable_Code == var.cd, ]
  
  #calulate the mean of that variable per county for each state 
  for (s in 1: length(County) ){
    St.var.cd[s] <- mean( na.omit(c(var.cd.data$Value 
                                    [var.cd.data$County ==  County[s]])))
  }
  
  #bind the new column to the table of all variables
  var.tb <- cbind.data.frame(var.tb, St.var.cd)
}
#capture progress by saving
var.tb1 <- var.tb


#-------------------------------------------------------------------------------
#let'sremove the first column County, which was just there to provide the frame
#work and move it into the row names
#remove first column

row.names(var.tb) <- County #assign County vector to rownames
colnames (var.tb) <- c('County',variables$Variable_Code)
var.tb <- var.tb[,2:ncol(var.tb)]

#replace the missing data entries with the mean of the collumn 
for (i in 1:nrow(var.tb)){
  for (j in 1:ncol(var.tb)){
    if (is.na (var.tb[i,j])){
      var.tb[i,j] <- mean(na.omit(var.tb[,j]))
    }
  }
}
head(var.tb)
#subtract column means from every entry in the dataframe var.tb to centre the
#data
for (i in 1:ncol(var.tb)){
  var.tb[,i] <- var.tb[,i] - c(rep( mean(var.tb[,i]), nrow(var.tb)))
}

#ultimately we will save the new table as a csv file in the data.clean
#folder
write.csv(var.tb, 
          paste(path.data.clean,"CountyDataClean.csv", sep = ""), 
          row.names = FALSE)




