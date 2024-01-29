# ANOVA_4

# Create a data frame with sample data
data <- data.frame(
  'c1' = c(7, 7, 15, 11, 9),
  'c2' = c(12, 17, 12, 18, 18),
  'c3' = c(14, 18, 18, 19, 19),
  'c4' = c(19, 25, 22, 19, 23),
  'c5' = c(7, 10, 11, 15, 11)
)

# Calculate the sum, count, mean, and variance for each column
sum_row <- colSums(data, na.rm = TRUE)
count_row <- colSums(!is.na(data))
division_row <- sum_row / count_row
variance_row <- apply(data, 2, function(x) var(x, na.rm = TRUE))

# Add these calculations as new rows in the data frame
data2 <- rbind(Sumatorio = sum_row, Conteo = count_row, Media = division_row, Varianza = variance_row)

# Print the updated data frame
print(data2)

# Calculate the pooled variance
sum_weighted_variances <- sum(sapply(data, function(x) (length(x) - 1) * var(x)))
sum_degrees_of_freedom <- sum(sapply(data, function(x) length(x) - 1))
pooled_variance <- sum_weighted_variances / sum_degrees_of_freedom

# Print the pooled variance
print(pooled_variance)

# Bartlett's Test Calculations
log_var_individuals <- sapply(data, function(x) (length(x) - 1) * log(var(x)))
sum_log_var_individuals <- sum(log_var_individuals)
log_pooled_variance <- sum_degrees_of_freedom * log(pooled_variance)
q <- log_pooled_variance - sum_log_var_individuals
k <- ncol(data)
sum_inverse_degrees_of_freedom <- sum(sapply(data, function(x) 1 / (length(x) - 1)))
inverse_sum_degrees_of_freedom <- 1 / sum_degrees_of_freedom
c <- 1 + (1 / (3 * (k - 1))) * (sum_inverse_degrees_of_freedom - inverse_sum_degrees_of_freedom)

# Print q and c
print(q)
print(c)

# Calculate x_0
x_0 = q/c
print(x_0)

# Significance level and degrees of freedom for chi-square
alpha <- 0.05
df <- 4  # Replace with your degrees of freedom

# Calculate the critical value for chi-square
chi_square_critical <- qchisq(1 - alpha, df)

# Print the critical chi-square value
print(chi_square_critical)

# Perform Bartlett's test
bartlett_result <- bartlett.test(data)

# Print Bartlett's test result
print(bartlett_result)

# ANOVA Calculations
sum_squares <- sum(sapply(data, function(x) sum(x^2)))
total_sum <- sum(sapply(data, sum))^2
N <- sum(sapply(data, length))
SSTotal <- sum_squares - (total_sum / N)
print(SSTotal)

sum_squares_treatments <- sum(sapply(data, function(x) sum(x)^2 / length(x)))
total_sum_squared <- total_sum^2 / N
SSTratamientos <- sum_squares_treatments - total_sum_squared
print(SSTratamientos)

SSError <- SSTotal - SSTratamientos
print(SSError)

# ANOVA Table Creation
anova_table <- data.frame(
  'Fuente de variación' = c('Tratamientos (Entre)', 'Errores (Dentro)', 'Total'),
  'S.S.' = c(475.76, 161.2, 636.96),
  'g.l.' = c('k-1=4', 20, 24),
  'C.M.' = c(118.94, 8.06, NA),  # NA for total as CM is not calculated for total
  'F' = c(14.7568, NA, NA)       # NA for errors and total where F value is not applicable
)

# Print the ANOVA table
print(anova_table)

# Calculate the critical F-value for ANOVA
f_critical_value <- qf(1 - alpha, 4, 20)  # Replace 4 and 20 with your degrees of freedom

# Print the critical F-value
print(f_critical_value)
