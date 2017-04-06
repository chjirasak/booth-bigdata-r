#### Purchases of Ben and Jerry's Ice Cream
setwd("hw1")
#### Purchases of Ben and Jerry's Ice Cream

benjer = read.csv("BenAndJerry.csv")

## explore a bit

names(benjer)

## create a new variable for price per unit

priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity

y <- log(1+priceper1)

## grab some covariates of interest
## we'll create a properly formatted data.frame

x <- benjer[,c("flavor_descr","size1_descr",
               "household_income","household_size")]
#### Add promotion type
promotion_type2 = benjer$promotion_type
promotion_type2[is.na(promotion_type2)] <- 0
x$promotion_type_new <- as.factor(promotion_type2)

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

fit <- glm(y~., data=xy)
summary(fit)

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column

get_coefs <- function(fit, q) {
  pvals <- summary(fit)$coef[-1,4] 
  coefs = summary(fit)$coef
  coefs = coefs[coefs[,4] < fdr_cut(pvals, q),]
  coefs = coefs[order(coefs[,4]),]
  return(coefs)
}

coefs <- get_coefs(fit, 0.01)
coefs
nrow(coefs)

coefs <- get_coefs(fit, 0.05)
coefs
nrow(coefs)

## source the fdr_cut function

source("fdr.R")

k <- rank(pvals, ties.method="min")
pvals <= (0.01 * k/N)
N <- length(pvals)
alpha <- max(pvals[ pvals <= (0.01 * k/N) ])
weeded_out_pvals = pvals[pvals <= fdr_cut(pvals, 0.05)]
weeded_out_pvals[order(weeded_out_pvals)]

pvals[pvals <= fdr_cut(pvals, 0.05)]
length(pvals[pvals < fdr_cut(pvals, 0.05)])

fdr_cut(pvals, 0.05, TRUE)

