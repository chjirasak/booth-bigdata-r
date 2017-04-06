#### Purchases of Ben and Jerry's Ice Cream
setwd("/Users/jirasak/Documents/workspace/r-workspace/bigdataclass/Week 2/hw")
benjer = read.csv("BenAndJerry.csv")

## explore a bit test test
names(benjer)

##################################################
# cleanup
# clean up promotion type
##################################################
promotion_type2 = benjer$promotion_type
promotion_type2[is.na(promotion_type2)] <- 0
benjer$promotion_type_new <- as.factor(promotion_type2)

benjer$race <- factor(benjer$race,
                      levels=1:4,labels=c("white","black","asian","other"))
benjer$hispanic_origin <- benjer$hispanic_origin==1
benjer$benjer_spent <- (benjer$price_paid_deal + benjer$price_paid_non_deal)
benjer$benjer_share <- (benjer$benjer_spent) / benjer$total_spent
benjer$is_black <- benjer$race=="black"

##################################################
## Quick check
##################################################
hist(benjer$benjer_share)

##################################################
# Quantity versus Type of promotion
##################################################
hist(promotion_type2, breaks=4, main="Histogram of Promotion Type (0 is No Promotion)", xlab="Promotion Type")
boxplot(quantity ~ promotion_type_new, data=benjer, xlab="Promotion Type", ylab="Quantity", main="Box Plot Quantity vs. Promotion Type")
summary(glm(quantity ~ promotion_type_new, data=benjer))
# Check promotion interaction with other fields
summary(glm(quantity ~ promotion_type_new*race, data=benjer))
summary(glm(quantity ~ promotion_type_new*hispanic_origin, data=benjer))

##################################################
# Ice Cream Basket Share per Promotion ... or Race
##################################################
boxplot(benjer_share ~ promotion_type_new, data=benjer,
        ylab='Ben and Jerry Basket Share',
        xlab='Promotion Type',
        main='Box Plot of Ben and Jerry Basket Share vs Promotion Type')
summary(glm(benjer_share ~ promotion_type_new, data=benjer))

boxplot(benjer_share ~ race, data=benjer, 
        ylab='Ben and Jerry Basket Share',
        xlab='Race',
        main='Box Plot of Ben and Jerry Basket Share vs Race')
summary(glm(benjer_share ~ race, data=benjer))
boxplot(benjer_share ~ household_income, data=benjer,
        ylab='Ben and Jerry Basket Share',
        xlab='Household Income',
        main='Box Plot of Ben and Jerry Basket Share vs Household Income')
summary(glm(benjer_share ~ is_black, data=benjer))

summary(glm(benjer_share ~ household_income, data=benjer))
boxplot(total_spent ~ household_income, data=benjer)

# Quantity
## difficult because most data is 1.0
benjer.small.quant <- benjer[benjer$quantity < 3,]
boxplot(quantity~age_of_female_head, data=benjer.small.quant)

# Average Price
## create a new variable for price per unit

priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity

y <- log(1+priceper1)

## grab some covariates of interest
## we'll create a properly formatted data.frame

x <- benjer[,c("flavor_descr","size1_descr",
               "household_income","household_size")]

## relevel 'flavor' to have baseline of vanilla

x$flavor_descr <- relevel(x$flavor_descr,"VAN")

## coupon usage

x$usecoup = factor(benjer$coupon_value>0)

x$couponper1 <- benjer$coupon_value/benjer$quantity

## organize some demographics

x$region <- factor(benjer$region, 
                   levels=1:4, labels=c("East","Central","South","West"))

x$married <- factor(benjer$marital_status==1)

x$race <- factor(benjer$race,
                 levels=1:4,labels=c("white","black","asian","other"))

x$hispanic_origin <- benjer$hispanic_origin==1

x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)

x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)

x$sfh <- benjer$type_of_residence==1

x$internet <- benjer$household_internet_connection==1

x$tvcable <- benjer$tv_items>1

## combine x and y, just to follow my recommended `way to use glm'
## cbind is `column bind'.  It takes two dataframes and makes one.

xy <- cbind(x,y)

## fit the regression
y
fit <- glm(y~., data=xy)
summary(fit)

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column

pvals <- summary(fit)$coef[-1,4]
pvals

## source the fdr_cut function

source("fdr.R")

k <- rank(pvals, ties.method="min")
pvals <= (0.01 * k/N)
N <- length(pvals)
alpha <- max(pvals[ pvals <= (0.01*k/N) ])
pvals[order(pvals)]
fdr_cut(pvals, 0.001, TRUE)

boxplot(y ~ quantity, data=xy)
summary(glm(y~size1_descr, data=xy))
