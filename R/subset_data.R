#subset.data
#----------------------------------
# Objective: Extracting/ Separating data based on class.

# Description: This method that enable to separate the data based on class. This method
#              takes in data frame. The last column assumed to be the class data. This
#              function only takes in data frame

# Usage: subset.data(data)

# Arguments: data: A data frame. The last column must be the categorical variable

# Value: The output is a list of the C (number of class) data frame

# Example:

#     data = PlantGrowth
#     a = subset.data(data)
#     a[1]
#     a[2]
#     a[3]


#     data = iris
#     b = subset.data(data)
#     b[1]
#     b[2]
#     b[3]

# Source code

subset.data = function(data) {

  data = data

  col.n = ncol(data)

  class.name = as.character(unique(data[,col.n]))

  class.name = noquote(class.name)

  class.n = length(unique(data[,col.n]))

  class.data = list()

  for (i in 1:class.n) {

    class.data[[i]] = data[data[col.n] ==  class.name[i], ]

  }

  return(class.data)
}
