#No. 1 ----

data <- c(1500,6000,3500,3800,1800,5500,4800,4200,3900,3000)
n <- length(data)

#a) 
# https://en.wikipedia.org/wiki/Log-normal_distribution#Maximum_likelihood_estimation_of_parameters
# https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?article=2927&context=etd
mu <- sum(log(data))/n
sigma.2 <- sum((log(data)-mu)^2)/n
sigma <- sqrt(sigma.2)

#b) On fait la stop-loss avec les paramêtres estimés plus tôt:


stop_loss <- function(d,mu,sigma) {
    exp(mu + (sigma^2)/2) * (1 - pnorm((log(d)-mu-sigma^2)/sigma)) - d*(1-pnorm((log(d)-mu)/sigma))
}

stop_loss(seq(2000,3000,100),mu,sigma)

#c)
limited_loss <- function(x,mu,sigma) {
    exp(mu+sigma^2/2)*pnorm((log(x)-mu-sigma^2)/sigma) + x*(1-pnorm((log(x)-mu)/sigma))
}

limited_loss(seq(3000,4000,100),mu,sigma)

#d)
set.seed(1)
m <- c(100000,500000,1000000)

stop_exp <- function(m) sum(sapply(rlnorm(m,mu,sigma),function(i) max((i-2000),0)))/m
sapply(m,stop_exp)

# data1 <- rlnorm(m[1],mu,sigma)
# data2 <- rlnorm(m[2],mu,sigma) 
# data3 <- rlnorm(m[3],mu,sigma) 
# # m=100 000
# sum(data1[data1>2000] - 2000)/length(data1)
# #m=500 000
# sum(data2[data2>2000] - 2000)/length(data2)
# #m=1 000 000
# sum(data3[data3>2000] - 2000)/length(data3)



#No.2 ----
# my try
# bin = [a,b] : L'intervalle des valeurs
# densité de la lognormale bucketé
# on veut maximisier sum(count*log(f))
f <- function(a,b,mu,sd){
    pnorm((log(b)-mu)/sd)-pnorm((log(a)-mu)/sd)
}

#gml : grouped maximum likelyhood

gml.data <- data.frame("a" = c(seq(0,1500,250),2000,2500,3000),
                  "b" = c(seq(250,1500,250),2000,2500,3000,5000),
                  "count" = c(3,4,5,5,4,3,3,3,2,2))

gml.fmax <- function(mu,sd){
    sum(apply(gml.data,1,function(x) x[3]*log(f(x[1],x[2],mu,sd))))
}

mu.int <- seq(6.5,7,len = 10000)
sigma.int <- seq(0,1,len = 200)

out <- sapply(mu.int, function(i) sapply(sigma.int,function(j) gml.fmax(i,j)))
coord <- which(out == max(out), arr.ind = TRUE)
values <- c(mu.int[coord[2]],sigma.int[coord[1]])
values
save(values,file = "grouped.Rda")

out[is.infinite(out)] <- -5000
persp(mu.int,sigma.int,out, theta=80,phi=80,ticktype="detailed")
#a) SAS
mu.group <- 6.8418
sigma.group <- 0.8278
    
#b)
stop_loss(seq(2000,3000,100),mu.group,sigma.group)

#c)
limited_loss(seq(3000,4000,100),mu.group,sigma.group)


#No.3 ----

x <- 5
r <- 2

beta <- list()
beta$a <- 2
beta$b <- 2
#b)
val <- seq(0,1,length = 100)
fx <- pbeta(val,beta$a,beta$b)
plot(val,fx,type = "l", lty=1, xlab="x",ylab = expression(f[X](x)))

#c)
beta_post <- list()
beta_post$a <- beta$a + r
beta_post$b <- beta$b + x
bayes_estime <- (beta_post$a)/(beta_post$a + beta_post$b)

#d)
n <- 1e6
simul <- rbeta(n,beta_post$a,beta_post$b)
mean(simul)



# Whatever

library(stargazer)
clean_star <- function(...){
    output <- capture.output(stargazer(...))
    output <- output[4:length(output)]
    cat(paste(output,collapse = "\n"),"\n")
}
d <- seq(3000,4000,100)
sl <- limited_loss(d,mu.group,sigma.group)
mat <- matrix(0,nrow = 2, ncol = 11)
mat[1,] <- d
mat[2,] <- sl

clean_star(t(mat))
>>>>>>> 3e3fdc25c7ecab0d6f27e4d84554a4fc24f25752
