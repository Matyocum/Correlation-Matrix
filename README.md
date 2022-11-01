# Correlation-Matrix
Correlation Matrix in R


empty_m <- matrix(ncol = length(Telco_Customers),
                  nrow = length(Telco_Customers),
                  dimnames = list(names(Telco_Customers),
                                  names(Telco_Customers)))

calculate_cramer <- function(m, Telco_Customers) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r,c]] <- assocstats(table(Telco_Customers[[r]], Telco_Customers[[c]]))$cramer
    }
  }
  return(m)
}

cor_matrix <- calculate_cramer(empty_m,Telco_Customers)

cor_matrix

install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix, method = 'number')

![image](https://user-images.githubusercontent.com/114650133/199357424-9c275f59-4c2a-4f48-9dd1-cae2168d75eb.png)
