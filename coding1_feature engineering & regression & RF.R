install.packages("car")
library(car)
install.packages("forcats")
library(forcats)
install.packages("leaps")
library(leaps)
install.packages("progress")
library(progress)
install.packages("rmarkdown")
install.packages("dplyr")
library(dplyr) # Monte-Carlo
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

#import and inspect data
vcr <- read.csv('in-vehicle-coupon-recommendation.csv')
head(vcr)

dim(vcr)
names(vcr)
str(vcr)
summary(vcr)

#turn the characters to categorical data
vcr$destination <- as.factor(vcr$destination)
vcr$passanger <- as.factor(vcr$passanger)
vcr$weather <- as.factor(vcr$weather)
vcr$temperature <- as.factor(vcr$temperature)
vcr$time <- as.factor(vcr$time)
vcr$couponType <- as.factor(vcr$couponType)
vcr$expiration <- as.factor(vcr$expiration)
vcr$gender <- as.factor(vcr$gender)
vcr$age <- as.factor(vcr$age)
vcr$maritalStatus <- as.factor(vcr$maritalStatus)
vcr$has_children <- as.factor(vcr$has_children)
vcr$education <- as.factor(vcr$education)
vcr$occupation <- as.factor(vcr$occupation)
vcr$income <- as.factor(vcr$income)
vcr$cartype <- as.factor(vcr$cartype)
vcr$Bar <- as.factor(vcr$Bar)
vcr$CoffeeHouse <- as.factor(vcr$CoffeeHouse)
vcr$CarryAway <- as.factor(vcr$CarryAway)
vcr$RestaurantLessThan20 <- as.factor(vcr$RestaurantLessThan20)
vcr$Restaurant20To50 <- as.factor(vcr$Restaurant20To50)
vcr$toCoupon_GEQ5min <- as.factor(vcr$toCoupon_GEQ5min)
vcr$toCoupon_GEQ15min <- as.factor(vcr$toCoupon_GEQ15min)
vcr$toCoupon_GEQ25min <- as.factor(vcr$toCoupon_GEQ25min)
vcr$direction_same <- as.factor(vcr$direction_same)
vcr$direction_opp <- as.factor(vcr$direction_opp)
vcr$Y <- as.factor(vcr$Y)
summary(vcr)

# test if it is missing value or anything else
colSums(is.na(vcr))
    # then the blank means: the blank ones are assumed as one factor, so cannot be recognized as an NA. 
vcr$cartype <- as.character(vcr$cartype)
vcr$Bar <- as.character(vcr$Bar)
vcr$CoffeeHouse <- as.character(vcr$CoffeeHouse)
vcr$CarryAway <- as.character(vcr$CarryAway)
vcr$RestaurantLessThan20 <- as.character(vcr$RestaurantLessThan20)
vcr$Restaurant20To50 <- as.character(vcr$Restaurant20To50)

vcr[vcr == ''] <- NA
colSums(is.na(vcr))

vcr$cartype <- as.factor(vcr$cartype)
vcr$Bar <- as.factor(vcr$Bar)
vcr$CoffeeHouse <- as.factor(vcr$CoffeeHouse)
vcr$CarryAway <- as.factor(vcr$CarryAway)
vcr$RestaurantLessThan20 <- as.factor(vcr$RestaurantLessThan20)
vcr$Restaurant20To50 <- as.factor(vcr$Restaurant20To50)
summary(vcr)
colSums(is.na(vcr)) #now the missing values just appear
vcr2 <-vcr
vcr2 <- vcr2[-which(is.na(vcr2$Bar)), ]
vcr2 <- vcr2[-which(is.na(vcr2$CoffeeHouse)), ]
vcr2 <- vcr2[-which(is.na(vcr2$CarryAway)), ]
vcr2 <- vcr2[-which(is.na(vcr2$RestaurantLessThan20)), ]
vcr2 <- vcr2[-which(is.na(vcr2$Restaurant20To50)), ]

c <- match("cartype",names(vcr2))
vcr2 <- vcr2[,-c]

colSums(is.na(vcr2))
summary(vcr2)

#vcr3: after data processing - delete missing values and redundant variables, integrate toCoupon distance. 
vcr3 <- vcr2[,-24]
vcr3$toCoupon <- "5min more"
vcr3$toCoupon <- ifelse(vcr3$toCoupon_GEQ15min == 0, "5min - 15min",
                        ifelse(vcr3$toCoupon_GEQ15min == 1 & vcr3$toCoupon_GEQ25min == 0, "15min - 25min",
                               ifelse(vcr3$toCoupon_GEQ25min == 1, "more than 25min", vcr3$toCoupon)))
vcr3$toCoupon <- as.factor(vcr3$toCoupon)
levels(vcr3$toCoupon)
summary(vcr3)
vcr3 <- vcr3[, c(1:19, which(names(vcr3) == "toCoupon"), setdiff(1:ncol(vcr3), c(1:19, which(names(vcr3) == "toCoupon"))))]
vcr3 <- vcr3[,-c(21,22,23)]

#based on vcr3: for categorical data, use histogram to show their relation with Y
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
for (k in 1:11){
  no <- 2*k
  noo <- no-1
  filename <- paste0("figure", k,"-",k+1, ".png")
  filepath <- file.path(getwd(), filename) 
  png(filepath, width = 1600, height = 1200, res = 300)

  plots <- lapply(names(vcr3)[noo:no], function(var) {
    ggplot(vcr3, aes_string(x = var, fill = "factor(Y)")) +
      geom_bar(position = "dodge") +
      labs(
        title = var,
        x = var,
        y = "Frequency",
        fill = "Y"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  grid.arrange(grobs = plots, nrow = 1, ncol = 2)
  dev.off()
}


#correlation analysis? not suitable for categorical variable

#scatter plot: x
library(ggplot2)
par(mfrow = c(1,1)) 
pairs(vcr[,c(1:5,26)]) #cannot see any pattern

#frequency
for (i in c(1:25)) {
  plot(vcr[,i],vcr$Y,xlab='',
       main = paste('if Accept vs.',names(vcr)[i]))
}
plot(vcr[,1],vcr$Y)

match("Y",names(vcr2))
#crosstable: cramer's V test------------------------------
install.packages("vcd")
install.packages("grid")
library(vcd)
library(grid)
summary(vcr3)
cramer_v_matrix <-function(df) {
  result_matrix <- matrix(NA,ncol = 22, nrow = 22)
  colnames(result_matrix) <- colnames(vcr3)
  rownames(result_matrix) <- colnames(vcr3)
  for (i in 1:22){
    for (j in 1:22){
      if (i==j){
        result_matrix[i,j] <- 1
      } else {
        tb1 <-table(vcr3[,i],vcr3[,j])
        result_matrix[i,j] <- assocstats(tb1)$cramer
        result_matrix[j,i] <- result_matrix[i,j]
      }
    }
  }
  return(result_matrix)
}
result <- cramer_v_matrix(df)
print(result)

result1 <- round(result,4)
write.csv(result1,file="data3010_variable correlation.csv",row.names = TRUE)

#visualize the crosstab
library(corrplot)
corrplot(result1,method = "color",tl.col="black",tl.cex = 0.5)

#try to do Monte-Carlo Simulation for chi-square-vcr6--------------------
library(forcats)
summary(vcr2)
vcr6 <- vcr3 
vcr6$income <- fct_collapse(vcr6$income, 
                            Low = c("Less than $12500", "$12500 - $24999","$25000 - $37499"),
                            Medium = c("$37500 - $49999", "$50000 - $62499","$62500 - $74999"),
                            High = c("$75000 - $87499","$87500 - $99999","$100000 or More"))
levels(vcr6$age)
vcr6$age <- fct_collapse(vcr6$age, 
                         "below21" = "below21",
                         "20-30" = c("21", "26"),
                         "30-40" = c("31","36"),
                         "40-50" = c("41","46"),
                         "50plus" = "50plus")
c(levels(vcr6$occupation))
vcr6$occupation <- fct_collapse(vcr6$occupation, 
                                "unemployed" = "Unemployed",
                                "student" = "Student",
                                "retired" = "Retired",
                                "employed" = c("Architecture & Engineering","Arts Design Entertainment Sports & Media",
                                               "Building & Grounds Cleaning & Maintenance","Business & Financial",
                                               "Community & Social Services","Computer & Mathematical", 
                                               "Construction & Extraction","Education&Training&Library",
                                               "Farming Fishing & Forestry","Food Preparation & Serving Related",
                                               "Healthcare Practitioners & Technical","Healthcare Support",
                                               "Installation Maintenance & Repair","Legal","Life Physical Social Science",
                                               "Management","Office & Administrative Support","Personal Care & Service",
                                               "Production Occupations","Protective Service",
                                               "Sales & Related","Transportation & Material Moving"))

summary(vcr6)
vcr7 <- list()
# define metrics
num_simulations <- 5000 
sample_size <- 100       
variables <- c()
for (i in 1:21){
  every_var <- colnames(vcr6)[i]
  variables <- c(variables, colnames(vcr6)[i]) 
}
# store sig result
significance_counts <- numeric(length(variables))  # record the number of sig for every IV

# Monte-Carlo
install.packages('dplyr')
library(dplyr)
for (sim in 1:num_simulations) {
  # random 50 lines
  sample_chi <- vcr6 %>% sample_n(sample_size)
  # chi-square for each var
  for (i in seq_along(variables)) {
    var <- variables[i]
    # create cross table
    contingency_table <- table(sample_chi[[var]], sample_chi$Y)
    # make sure cross table valid (column & row > 1)
    if (nrow(contingency_table) > 1 && ncol(contingency_table) > 1) {
      # chi square
      test_result <- chisq.test(contingency_table, simulate.p.value = TRUE)
      # detect if p is valid
      if (!is.na(test_result$p.value) && test_result$p.value < 0.05) {
        significance_counts[i] <- significance_counts[i] + 1
      }
    }
  }
}
significance_frequencies <- significance_counts / num_simulations
names(significance_frequencies) <- variables
print(significance_frequencies)

# plot as their suggestion: order by height & classes as colors
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
test_color <- brewer.pal(6,"Pastel2")
test_color

sf <- data.frame(significance_frequencies)
sf$class <- list("Environment","Environment","Environment","Environment","Environment"
                 ,"Coupon Information","Coupon Information","Demographic"
                 ,"Demographic","Demographic","Demographic","Demographic","Demographic"
                 ,"Demographic","Customer's Habit","Customer's Habit", "Customer's Habit"
                 ,"Customer's Habit", "Customer's Habit", "Accessibility","Accessibility")
sf <- sf[order(-sf$significance_frequencies), ]
class_colors <- c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#FFF2AE")
unlist(sf$class)
sf$class <- as.factor(unlist(sf$class))
names(class_colors) <- levels(sf$class)
rownames(sf)

ylim_max <- max(sf$significance_frequencies) * 1.2
par(mgp = c(3, 0.3, 0))
par(col.axis = "gray1")
par(col.lab = "gray1")
bar_positions <- barplot(
  height = sf$significance_frequencies,
  names.arg = NULL,
  col = class_colors[sf$class], 
  las = 2,                              # spin x label for room
  main = "Monte-Carlo Simulation of Chi-Square (5000 times)",
  ylab = "Significance Frequencies",
  ylim = c(0, ylim_max),
  border = "blue3"                      # border of the bar
)

# add values on each bar
text(x = bar_positions,            
     y = -0.02,                   
     labels = c("couponType","expiration","destination","CoffeeHouse",         
                "passanger","toCoupon","weather","time",                
                "Bar","temperature","age","occupation",          
                "Restaurant20To50","income","gender","has_children",        
                "maritalStatus","CarryAway","direction_same","RestaurantLess20",
                "education"), 
     srt = 45,                   
     adj = 1,                    
     xpd = TRUE,               
     cex = 0.8,              
     col = "black") 

text(
  x = bar_positions,                      
  y = sf$significance_frequencies,                
  labels = sf$significance_frequencies,            
  pos = 3,                                   
  cex = 0.8,                                 
  col = "black"                              
)
# add legend
legend(
  "topright", 
  legend = paste(levels(sf$class)), 
  fill = class_colors, 
  cex = 0.8, 
  inset = c(-0.3,0), 
  bty = "n" 
)

# turn into plots:heatmap
library(ggplot2)
chi_plot <- data.frame(
  Variable = variables,
  Frequency = significance_frequencies
)
# heatmap
ggplot(chi_plot, aes(x = Variable, y = Frequency, fill = Frequency)) +
  geom_bar(stat = "identity", color = "black") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Frequency") +
  geom_text(aes(label = round(Frequency, 3)), vjust = -0.5, size = 4) +  
  labs(
    title = "Monte-Carlo Simulation of Chi-Square (5000 times)",
    x = "Variables",
    y = "Significance Frequencies"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text = element_text(size = 12),               
    axis.title = element_text(size = 12)            
  )

#vcr6.1: based on vcr6 and delete MonteCarlo less than .05
#carryaway, direction_opp, direction_same, education, maritalStatus, RestaurantLessThan20
names(vcr6)
vcr6.1 <- vcr6[,-c(17,21,22,12,10,18)]
write.csv(vcr6.1,file = "v4_Vehicle Coupon Recommendation_after MonteCarlo.csv",row.names = TRUE)
vcr2.1 <- vcr2[,-c(20,24)]
write.csv(vcr2.1,file = "v1_VCR_just delete meaningless 3 variables.csv",row.names = TRUE)

#delete high correlated variables---------------------------
#destination with time, destination with passenger, destination with direction
#direction_same with direction_opposite -> delete direction_opp
#weather with temperature ->delete temperature
#marital status with has children
vcr4 <- vcr2
vcr4 <- vcr4[,-match("direction_opp",names(vcr4))]
vcr4 <- vcr4[,-match("temperature",names(vcr4))]
vcr4 <- vcr4[,-match("destination",names(vcr4))]
summary(vcr4)
colnames(vcr4)


#delete 5 less relevant variables with if accept coupon ------------------------
test_lowest <- result2[24,]
low_to_delete <- names(test_lowest[order(test_lowest)][1:5])

match(low_to_delete[5],names(vcr3))
vcr4 <- vcr3

for (i in 1:4){
  a <- match(low_to_delete[i],names(vcr4))
  if (is.na(a) == TRUE){
    
    
    print(i)
    vcr4 <- vcr4
  } else {
  print(low_to_delete[i])
  vcr4 <- vcr4[,-a]
  }
}
summary(vcr4)
colnames(vcr4)

# delete meaningless variable: all equals to 1
match("toCoupon_GEQ5min",names(vcr4))
vcr4 <- vcr4[,-16]

write.csv(vcr4,file = "v2_Vehicle Coupon Recommendation_after cleaning.csv",row.names = TRUE)

# merge some variables with a lot of levels--------------------
vcr4$income <- fct_collapse(vcr4$income, 
                          Low = c("Less than $12500", "$12500 - $24999","$25000 - $37499"),
                          Medium = c("$37500 - $49999", "$50000 - $62499","$62500 - $74999"),
                          High = c("$75000 - $87499","$87500 - $99999","$100000 or More"))
levels(vcr4$age)
vcr4$age <- fct_collapse(vcr4$age, 
                         "below21" = "below21",
                         "20-30" = c("21", "26"),
                         "30-40" = c("31","36"),
                         "40-50" = c("41","46"),
                         "50plus" = "50plus")
c(levels(vcr4$occupation))
vcr4$occupation <- fct_collapse(vcr4$occupation, 
                                "unemployed" = "Unemployed",
                                "student" = "Student",
                                "retired" = "Retired",
                                "employed" = c("Architecture & Engineering","Arts Design Entertainment Sports & Media",
                                               "Building & Grounds Cleaning & Maintenance","Business & Financial",
                                               "Community & Social Services","Computer & Mathematical", 
                                               "Construction & Extraction","Education&Training&Library",
                                               "Farming Fishing & Forestry","Food Preparation & Serving Related",
                                               "Healthcare Practitioners & Technical","Healthcare Support",
                                               "Installation Maintenance & Repair","Legal","Life Physical Social Science",
                                               "Management","Office & Administrative Support","Personal Care & Service",
                                               "Production Occupations","Protective Service",
                                               "Sales & Related","Transportation & Material Moving"))
vcr4$toCoupon <- "5min more"
vcr4$toCoupon <- ifelse(vcr4$toCoupon_GEQ15min == 0, "5min - 15min",
                 ifelse(vcr4$toCoupon_GEQ15min == 1 & vcr4$toCoupon_GEQ25min == 0, "15min - 25min",
                 ifelse(vcr4$toCoupon_GEQ25min == 1, "more than 25min", vcr4$toCoupon)))
vcr4$toCoupon <- as.factor(vcr4$toCoupon)
levels(vcr4$toCoupon)
vcr4 <- vcr4[, c(1:15, which(names(vcr4) == "toCoupon"), setdiff(1:ncol(vcr4), c(1:15, which(names(vcr4) == "toCoupon"))))]
vcr4 <- vcr4[,-c(17,18)]
summary(vcr4)

write.csv(vcr4,file = "v3_Vehicle Coupon Recommendation_after cleaning.csv",row.names = TRUE)

# chi-square detect for univariates----------------------------------
for (i in 1: 16){
  X <- table(vcr4[,i],vcr4$Y)
  Xtitle <- paste0(colnames(vcr4)[i], "vs. If accepted")
  print(Xtitle)
  Chi <- chisq.test(X, correct=F)
  print(Chi)
  if (Chi$p.value < .001){
    print("significant result")
  } else {
    print("not significant")
  }
}
# because there are numeric records, the result can be easily statistically significant. 
  
#subset selection:by level------------------------------
library(leaps)
# build a progress bar
install.packages("progress")
library(progress)
nvmax <- 17
pb <- progress_bar$new(
  format = "  Running regsubsets [:bar] :percent in :elapsed ETA: :eta",
  total = 100,
  clear = FALSE,
  width = 60
)
#step by step different number of nvmax
subset_results_17 <- list()
for (i in 1:nvmax) {
  pb$tick()  # update the progress bar
  subset_results_17[[i]] <- regsubsets(Y ~ ., data = vcr4, nvmax = i, really.big=T)
  Sys.sleep(0.5)  # how much time
}

#for the result
sum17 <- summary(subset_results_17[[nvmax]])
which.min(sum17$bic)
which.min(sum17$cp)
which.max(sum17$adjr2)

#subset selection:by variable------------------------------
library(leaps)
# build a progress bar
install.packages("progress")
library(progress)
nvmax <- 17
pb2 <- progress_bar$new(
  format = "  Running regsubsets [:bar] :percent in :elapsed ETA: :eta",
  total = 20,
  clear = FALSE,
  width = 60
)
#convert to value to avoid dumb variable transformation (test level)
subset_results_2 <- list()
vcr5 <- vcr4 
for (i in 1:17) {
  vcr5[,i] <- as.numeric(vcr5[,i])
}
summary(vcr5)

for (i in 1:nvmax) {
  pb$tick()  # update the progress bar
  subset_results_2[[i]] <- regsubsets(Y ~ ., data = vcr5, nvmax = i, really.big=T)
  Sys.sleep(0.5)  # how much time
}

summary(regsubsets(Y ~ ., data = vcr5, nvmax = 10, really.big=T))
#for the result
var_select <- list()
var_select <- summary(subset_results_2[[nvmax]])
var_select$outmat[10,] #passanger + weather + time + couponType + expiration + has_children + education + Bar + CoffeeHouse + Restaurant20To50 + toCoupon
var_select$outmat[5,] #weather + time + expiration + Bar + CoffeeHouse + toCoupon

'''too much time,do not run this
subset.model <- regsubsets(Y ~ ., data=vcr4, nvmax = 10, really.big=T)
sum_subset <- summary(subset.model)'''

class(vcr4[,1])
# stepwise regression---------------
for (i in 1:18){
  type <- class(vcr4[,i])
  print(type)
}

any(is.na(vcr4$Y))
any(is.nan(vcr4$Y))
any(is.infinite(vcr4$Y))

# all the variables (17v)
full_model <- glm(vcr4$Y ~ ., data = vcr4, family = binomial)
step_model <- step(full_model, direction='both') #AIC = 14481
summary(step_model)
coef(step_model)

# 10 variables
full_model_10v <- glm(vcr4$Y ~ passanger + weather + time + couponType + expiration + has_children + education + Bar + CoffeeHouse + Restaurant20To50 + toCoupon, data = vcr4, family = binomial)
step_model_10v <- step(full_model_10v, direction='both') #AIC = 15464 -> 14512 (without to_Coupon: 14528)
summary(step_model_10v)
coef(step_model_10v)

# 5 variables
full_model_5v <- glm(vcr4$Y ~ weather + expiration + time + Bar + CoffeeHouse, data = vcr4, family = binomial)
step_model_5v <- step(full_model_5v, direction='both') #AIC = 15762 ->15681 (without to_Coupon: 15732)
summary(step_model_5v)
coef(step_model_5v)

#all variable before data processing
summary(vcr6)
vcr6 <- vcr2[,-20] #all variables need to be more than 2 levels
full_model_25v <- glm(vcr6$Y ~ ., data = vcr6, family = binomial)
step_model_25v <- step(full_model_25v, direction='both') #AIC = 14294
summary(step_model_25v)
coef(step_model_25v)


#lasso regression----------
)
