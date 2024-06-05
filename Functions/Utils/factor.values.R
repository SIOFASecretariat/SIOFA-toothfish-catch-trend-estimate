# Convert factor to numeric 

factor.values = function(x) as.numeric(levels(x))[x]  # get the actual values of a factor with numeric labels
