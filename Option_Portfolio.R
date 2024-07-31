## ----------------For Comparison of functions log(1 + x) and x------------------
# Define a sequence of x values from -0.1 to 0.1 
x <- seq(-0.1, 0.1, by=0.001)
# Calculate log(1 + x) for each x value 
log_1_plus_x <- log(1 + x)
# Plot log(1 + x) vs x 
plot(x, log_1_plus_x, type="l", lwd=2,xlab="x", ylab="log(1 + x)",
     main="Comparison of log(1 + x) and x")
# Add the line y = x for comparison 
abline(a=0, b=1, lwd=2, lty=2)
# Add a legend with reduced size 
legend("topleft", legend=c("log(1 + x)", "y = x"), lty=c(1, 2), lwd=2, cex=1.4) 

## ------------------------ Data Pre Processing-----------------------
stock_price<-read.csv("C://Users//samim//Downloads//Stock_Prices.csv") 
stock_price$Date=as.Date(stock_price$Date)
str(stock_price)

## ------------------------Q-Q Plot----------------------------------
# Define a function to generate QQ plots
generate_qq_plot <- function(stock, stock_name, color) 
{
  stock_Pt <- stock_price[[stock]][which(stock_price$Date >= "2023-04-03" &
                                           stock_price$Date <= "2023-08-11")]   
  stock_Pt1 <- stock_price[[stock]][which(stock_price$Date >= "2023-03-31" &
                                            stock_price$Date < "2023-08-11")]
  qqnorm(log(stock_Pt/stock_Pt1), main = stock_name, col = color, pch = 20)
  qqline(log(stock_Pt/stock_Pt1)) 
}
# Plot QQ plots for each stock 
par(mfrow=c(3,3))
generate_qq_plot("Sun.Pharma", "Sun Pharma", "orange")
generate_qq_plot("Cipla", "Cipla", "royalblue4") 
generate_qq_plot("ONGC", "ONGC", "orangered4") 
generate_qq_plot("JSW", "JSW", "blue3")
generate_qq_plot("TCS", "TCS", "slateblue")
generate_qq_plot("Maruti.Suzuki", "Maruti.Suzuki", "red2")
generate_qq_plot("Hindustan.Uniliver", "Hindustan.Uniliver", "blue4")
generate_qq_plot("Tata.Power", "Tata.Power", "midnightblue")
generate_qq_plot("Zomato", "Zomato", "red1")


## ----------------------Kolmogorov Smirnov Test---------------------------------
for (i in 2:10) 
{  
  # Calculate log returns for the current stock column   
  log_return <- function(stock, stock_price)
  {   
    stock_Pt <- stock_price[which(stock_price$Date >= "2023-04-03" & 
                                    stock_price$Date <= "2023-08-11"), stock]     
    stock_Pt1 <- stock_price[which(stock_price$Date >= "2023-03-31" & 
                                     stock_price$Date < "2023-08-11"), stock]     
    log(stock_Pt / stock_Pt1)  
  }  
  log_returns <- log_return(i, stock_price)   
  # Perform the test   
  re <-ks.test(log_returns, "pnorm", mean =mean(log_returns), sd = sd(log_returns))   
  # Print the result of the test   
  if (re$p.value > 0.01) {  
    cat("The log returns of stock", colnames(stock_price)[i], 
        "follow a normal distribution (fail to reject H0).\n") 
  } 
  else 
  {    
    cat("The log returns of stock", colnames(stock_price)[i], 
        "do not follow a normal distribution (reject H0).\n")  
  } 
}

## --------------------------Table----------------------------------
# Define a function to calculate option premiums for different combinations of parameters for all stocks 
calculate_option_premiums_all <- function(stocks, strike_prices_list, maturity_dates, interest_rates) 
{   
  all_premiums <- list()   
  for (i in seq_along(stocks)) {    
    premiums <- matrix(NA, nrow = length(strike_prices_list[[i]]) *
                         length(maturity_dates) * length(interest_rates),ncol = 4,
                       dimnames = list(NULL, c("Strike Price", "Maturity Date", "Interest Rate", "Option Premium")))     
    counter <- 1    
    for (strike in strike_prices_list[[i]]) 
    {      
      for (maturity in maturity_dates) 
      {        
        for (rate in interest_rates)
        {           
          stock <- stocks[i]     
          stock_Pt <- stock_price[[stock]][which(stock_price$Date >= "2023-04-03" &
                                                   stock_price$Date <= "2023-08-11")]     
          stock_Pt1 <- stock_price[[stock]][which(stock_price$Date >= "2023-03-31" &
                                                    stock_price$Date < "2023-08-11")]         
          S0 <- stock_price[[stock]][which(stock_price$Date == "2023-08-14")]  
          sig <- sqrt(var(log(stock_Pt/stock_Pt1)))               
          T <- maturity           
          r <- rate           
          d1 <- (log(S0 / strike) + (r + sig^2/2) * T) / (sig * sqrt(T))          
          d2 <- d1 - sig * sqrt(T)           
          premium <- (pnorm(d1) * S0 - pnorm(d2) * strike * exp(-r * T))          
          # Round premium to two decimal places         
          premium <- round(premium, 2)           
          premiums[counter, ] <- c(strike, maturity, rate, 	premium)           
          counter <- counter + 1         
        }}}     
    all_premiums[[stock]] <- as.data.frame(premiums)   
  }   
  return(all_premiums) 
}
# Set up parameters 
strike_prices_list <- list(   "Sun.Pharma" = c(450, 453, 455),   "Cipla" = c(1230, 1235, 1240),   "ONGC" = c(176, 179, 181),    
                              "JSW" = c(346, 348, 356),   "TCS" = c(3450, 3452, 3455),   "Maruti.Suzuki" = c(9315, 9317, 9320), 
                              "Hindustan.Uniliver" = c(2535, 2537, 2540),   "Tata.Power" = c(230,232,234),   "Zomato" = c(90, 93, 95) ) 
maturity_dates <- c(0.25, 0.5) 
interest_rates <- seq(0.01, 0.02, by = 0.01) 
stocks <- names(strike_prices_list)
# Calculate option premiums for all combinations for all stocks 
all_premiums <- calculate_option_premiums_all(stocks, strike_prices_list, maturity_dates, interest_rates)
# Access premiums for all
all_premiums


## ------------------------------One Risky vs Risk Free Asset------------------------------------
#Savings Account Return 2.70% 
#Assumption we can bear only 10% loss 
log_return_ONGC_1year<-{  
  ONGC_Pt <- stock_price$ONGC[which(stock_price$Date >= "2022-10-12" & stock_price$Date <= "2024-04-01")]     
  ONGC_Pt1 <- stock_price$ONGC[which(stock_price$Date >= "2022-10-11" & stock_price$Date < "2024-04-01")]      
  log(ONGC_Pt/ONGC_Pt1) 
} 
ER<-mean(log_return_ONGC_1year) 
sigONGC<-sqrt(var(log_return_ONGC_1year)) 
SBI<-log(.027) 
VR<-0.1 
w<-(VR-SBI)/(sigONGC*qnorm(0.01)+(ER+SBI)) 
paste("w:" ,exp(w))

## -------------------------Portfolio Optimization---------------------------

#--------------------------To find the efficient Portfolio---------- 
#~R 
log_return <- function(stock, stock_price) {
  stock_Pt <- stock_price[which(stock_price$Date >= "2022-10-12" & stock_price$Date <= "2024-04-01"), stock]   
  stock_Pt1 <- stock_price[which(stock_price$Date >= "2022-10-11" & stock_price$Date < "2024-04-01"), stock]   
  log(stock_Pt / stock_Pt1) 
} 
mean_ret<-colMeans(log_return(2:10,stock_price))#mu~ 
cov_mat<-cov(log_return(2:10,stock_price))*252 #omega_i_j 
one_curl<-as.matrix(rep(1,9),nrow=9) 
#----------------------------------- 
library(MASS) 
A <- t(mean_ret) %*% ginv(cov_mat) %*% one_curl 
B <- t(mean_ret) %*% ginv(cov_mat) %*% mean_ret 
C <- t(one_curl) %*% ginv(cov_mat) %*% one_curl 
D <- B * C - A^2 
g <- as.numeric(B/D) * (ginv(cov_mat) %*% one_curl) - as.numeric(A/D) * (ginv(cov_mat) %*% mean_ret) 
h <- as.numeric(C/D) * (ginv(cov_mat) %*% mean_ret) - as.numeric(A/D) * (ginv(cov_mat) %*% one_curl) 
mu_min=as.numeric((-t(g)%*%cov_mat%*%h)/(t(h)%*%cov_mat%*%h)) 
mu_p_values <- seq(mu_min, max(mean_ret), by = 0.0001)
sig_mup_values <- numeric(length(mu_p_values))
# Calculate minimum value of mean_ret
for (i in seq_along(mu_p_values)) {   
  mu_p <- mu_p_values[i]   
  if (mu_p >= mu_min) {   
    w_p <- g + mu_p * h     
    # Calculate sig_mup    
    sig_mup <- sqrt(t(w_p) %*% cov_mat %*% w_p)    
    # Store sig_mup value     
    sig_mup_values[i] <- sig_mup  
  } 
} 
#-----------------------------Plot mu_p vs sig_mup ----------------------------------
plot(sig_mup_values,mu_p_values, type = "l", ylab = "Expected Reurn", xlab = "Risk ",
     main = "Expected Return vs Risk",col="red") 
legend("topleft", legend = c("Efficient Frontier"), col = "red", lty = 1, bty = "n") 
#-------------------------------------
var_mu_min=as.numeric((t(g)%*%cov_mat%*%g)-((t(g)%*%cov_mat%*%h)^2/(t(h)%*%cov_mat%*%h)))
mu_f=log(0.027) 
mu_p<-log(0.10) 
cp=as.numeric((mu_p-mu_f)/t(mean_ret-as.matrix(mu_f*one_curl,nrow=9))%*%ginv(cov_mat)%*%(mean_ret-mu_f*one_curl)) 
wbar=ginv(cov_mat)%*%(mean_ret-mu_f*one_curl) 
wt<-wbar/as.numeric(t(one_curl)%*%wbar) 
w_mup=as.numeric(cp*(t(one_curl)%*%wbar))*wt 
risky<-(cp*(t(one_curl)%*%wbar)) 
risk_free<-1-risky 
paste("Optical Allocation minimizing the variance with Expected return 10%:") 
print(w_p) 
paste("The expected return of minimum variance portfolio is:",mu_min) 
paste("Smallest possible variance of portfolio is",var_mu_min)
paste("Weights of Efficient Portfolio(w_mup):") 
print(w_mup) 
paste("The tangency portfolio is given by")
print(wt) 
paste("The weight to put on risky asset:",risky) 
paste("The weight to put on risk free asset:",risk_free)


##--------------------------------Summary-------------------------------------

data.frame("Tangency_Portfolio"=wt*36,row.names = colnames(stock_price[2:10]))