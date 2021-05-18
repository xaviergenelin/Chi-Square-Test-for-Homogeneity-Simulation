library(ggplot2)

# Setup
p1 <- c(1/3, 1/3, 1/3)
p2 <- c(1/10, 3/10, 6/10)
p3 <- c(1/10, 1/10, 8/10)
n1 <- c(20, 30, 50, 100)
n2 <- c(20, 30, 50, 100)

# Create a dataframe to store the results for each sample size and multinomial values
results <- data.frame(n1 = rep(rep(c(20,30,50,100), each = 4), 3),
                      n2 = rep(rep(c(20,30,50,100), 4), 3),
                      prob =  rep(c("equal", "mixed1", "mixed2"), each = 16),
                      alpha = 0
                      )
row <- 0 
for (size1 in n1){
  for (size2 in n2){
    row <- row + 1
    comparisons <- replicate(50000, {data1 <- rmultinom(n = 1, size = size1, prob = p1)# Get 1 random multinomial for a size in n1 and for each probability in p1
                      data2 <- rmultinom(n = 1, size = size2, prob = p1) # Get 1 random multinomial for a size in n1 and for each probability in p1
                      combined <- cbind(data1, data2) # Combine the two random multinomials into a dataframe
                      observed <- t(combined) # Transpose the dataframe so each row is the multinomial
                      colSum <- colSums(observed) # Get the total values in each of the 3 columns
                      probs <- colSum / (size1 + size2) # Get the probability of each column based on the observed values 
                      row1Expected <- size1*probs # Calculate the expected value for each column in row 1
                      row2Expected <- size2*probs # Calculate the expected value for each column in row 2
                      # Create a dataframe to store the expected counts for each cell
                      # If the expected value is 0, then make it 0.5 to avoid NaN values
                      expected <- rbind(replace(row1Expected, row1Expected == 0, 0.5),
                                        replace(row2Expected, row2Expected == 0, 0.5))
                      # Calculate the Pearson value based on the expected and observed values
                      chiSq <- sum((observed - expected)^2/expected)
                      cutoff <- qchisq(p = 0.95,df = 2) # Calculate the cutoff, df = 2 since (col-1)(row-1) = (3-1)(2-1) = 2
                      chiSq >= cutoff}) # Calculate if the test statistic is greater than the cutoff
    
    alpha <- mean(comparisons) # Get the average value of the 50000 comparisons
    results[row, "alpha"] <- alpha # Store the value in the results data frame based on the current row index
  }
}
# Repeat the above process for the probabilities p2
for (size1 in n1){
  for (size2 in n2){
    row <- row + 1
    comparisons <- replicate(50000, {data1 <- rmultinom(n = 1, size = size1, prob = p2)
                      data2 <- rmultinom(n = 1, size = size2, prob = p2)
                      
                      combined <- cbind(data1, data2)
                      observed <- t(combined)
                      colSum <- colSums(observed)
                      probs <- colSum / (size1 + size2) 
                      row1Expected <- size1*probs
                      row2Expected <- size2*probs
                      expected <- rbind(replace(row1Expected, row1Expected == 0, 0.5),
                                        replace(row2Expected, row2Expected == 0, 0.5))
                      chiSq <- sum((observed - expected)^2/expected)
                      cutoff <- qchisq(p = 0.95,df = 2)
                      chiSq >= cutoff})
    alpha <- mean(comparisons) # Get the average value of the 50000 comparisons
    results[row, "alpha"] <- alpha
  }
}
# Repeat the above process for probabilities p3
for (size1 in n1){
  for (size2 in n2){
    row <- row + 1
    comparisons <- replicate(50000, {data1 <- rmultinom(n = 1, size = size1, prob = p3)
                      data2 <- rmultinom(n = 1, size = size2, prob = p3)
                      
                      combined <- cbind(data1, data2)
                      observed <- t(combined)
                      colSum <- colSums(observed)
                      probs <- colSum / (size1 + size2) 
                      row1Expected <- size1*probs
                      row2Expected <- size2*probs
                      expected <- rbind(replace(row1Expected, row1Expected == 0, 0.5),
                                        replace(row2Expected, row2Expected == 0, 0.5))
                      chiSq <- sum((observed - expected)^2/expected)
                      cutoff <- qchisq(p = 0.95,df = 2)
                      chiSq >= cutoff})
    alpha <- mean(comparisons)
    results[row, "alpha"] <- alpha
  }
}

ggplot() + 
  geom_line(data = results, aes(x = n1, y = alpha, color = prob)) +
  #scale_color_discrete(name = "prob", labels = c("equal", "mixed1", "mixed2")) +
  facet_grid(~n2) + 
  theme(panel.background = element_rect(fill = "lightgray"))