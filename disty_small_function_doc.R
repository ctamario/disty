












# skapa två random vectorer att korrelera mot varandra.

x1 <- rnorm(6, mean=0, sd=0.5)
x2 <- rnorm(6, mean=0, sd=0.5)

cor(x1,x2)
plot(x2,x1)

rhos <- rep(NA, 1000) 

for(i in 1:1000){
  x1 <- rnorm(10, mean=0, sd=0.5)
  x2 <- rnorm(10, mean=0, sd=0.5)
  x1[zeros <- sample(1:5,5)] <- 0
  x2[zeros] <- 0
  rhos[i] <- cor(x1,x2)
}

hist(rhos)





c_mk3 %>% filter(river_id=="32000") %>% filter(n_years>10) %>% ggplot(aes(x=rho_trout)) + geom_histogram()

c_mk3 %>% filter(river_id=="32000") %>% filter(n_years>10) %>% ggplot(aes(x=rho_pike)) + geom_histogram()



rhos1 <- rep(NA, 10000)
rhos2 <- rep(NA, 10000)
for(i in 1:10000){
  x1 <- rnorm(n=10, mean=1.2, sd=0.5)
  x2 <- x1+rnorm(n=10, mean=0, sd=0.3)
  y1 <- rep(0,10)
  y2 <- rep(0,10)
  sub1 <- sample(1:10, 10)
  sub2 <- sample(1:10, 10)
  y1[sub1]<-x1[sub1]
  y2[sub2]<-x2[sub2]
  rhos1[i] <- cor(y1,y2)
  rhos2[i] <- cor(x1,x2)
}
#par(mfrow=c(1,2))
hist(rhos1, xlim=c(-1,1))
#hist(rhos2, xlim=c(-1,1))



rhos1 <- rep(NA, 10000)
rhos2 <- rep(NA, 10000)
for(i in 1:10000){
  x1 <- rnorm(n=10, mean=1.2, sd=0.5)
  x2 <- x1+rnorm(n=10, mean=0, sd=0.3)
  y1 <- x1
  y2 <- x2
  y1[sample(1:10, 9)] <- 0
  y2[sample(1:10, 9)] <- 0
  rhos1[i] <- cor(y1,y2)
  rhos2[i] <- cor(x1,x2)
}
par(mfrow=c(1,2))
hist(rhos1, xlim=c(-1,1))
#hist(rhos2, xlim=c(-1,1))

par(mfrow=c(5,2))

for(j in 0:9){
  rhos1 <- rep(NA, 10000)
  rhos2 <- rep(NA, 10000)
  for(i in 1:10000){
    x1 <- rnorm(n=10, mean=1.2, sd=0.5)
    x2 <- x1+rnorm(n=10, mean=0, sd=0.3)
    y1 <- x1
    y2 <- x2
    y1[sample(1:10, j)] <- 0
    y2[sample(1:10, j)] <- 0
    rhos1[i] <- cor(y1,y2)
    rhos2[i] <- cor(x1,x2)
  }
  
  hist(rhos1, xlim=c(-1,1), main="")
}

#hist(rhos2, xlim=c(-1,1))




plot(rhos1, type="l", ylim=c(-1,1))
plot(rhos2, type="l", ylim=c(-1,1))

rhos1 <- rep(NA, 10)
rhos2 <- rep(NA, 10)

for(j in 1:100){
  x1 <- rnorm(n=10, mean=1.2, sd=0.5)
  x2 <- x1+rnorm(n=10, mean=0, sd=0.3)
  y1 <- x1
  y2 <- x2
  
  for(i in 1:9){
    y1[which.min(y1*NA^(y1 <=0))] <- 0 #Weird way of looking for the lowest value that isn't a zero. 
    y2[which.min(y2*NA^(y2 <=0))] <- 0
    rhos1[i] <- cor(y1,y2)
    rhos2[i] <- cor(x1,x2)
  }
  
  lines(rhos1, type="l", ylim=c(0,1))
}


plot(rhos2, type="l", ylim=c(0,1))

par(mfrow=c(1,2))
hist(rhos1, xlim=c(-1,1))
hist(rhos2, xlim=c(-1,1))








plot(rhos1, type="l", ylim=c(-1,1))

rhos1 <- rep(NA, 10)
rhos2 <- rep(NA, 10)

for(j in 1:100){
  x1 <- rnorm(n=10, mean=1.2, sd=0.5)
  x2 <- x1+rnorm(n=10, mean=0, sd=0.3)
  y1 <- x1
  y2 <- x2
  
  sub1 <- sample(1:10,10)
  sub2 <- sample(1:10,10)
  
  for(i in 1:9){
    y1[sub1[i]] <- 0 #Weird way of looking for the lowest value that isn't a zero. 
    y2[sub1[i]] <- 0
    rhos1[i] <- cor(y1,y2)
    rhos2[i] <- cor(x1,x2)
  }
  
  lines(rhos1, type="l", ylim=c(0,1))
}


r





x1 <- rnorm(n=10, mean=1.2, sd=0.5)
x2 <- x1+rnorm(n=10, mean=0, sd=0.3)

sub1 <- sample(1:10, 2)
sub2 <- sample(1:10, 2)
y1 <- x1
y2 <- x2
y1[sub1]<-0
y2[sub2]<-0

cor_in <- data.frame(y1,y2)

count_pairs <- function(cor_in){
  t <- nrow(cor_in)
  d <- cor_in
  cor_in$two_above <- NA
  for(i in 1:t){
    cor_in$two_above[i] <- sum(c(d[i,] > 0))
  }
  paired_both <- sum(cor_in$two_above == 2)
  paired_one <- sum(cor_in$two_above == 1)
  percentage_zero <- sum(d == 0) / (nrow(d)*2)
  avg_gt0_site1 <- mean(d[which(d[1]>0),1])
  avg_ge0_site1 <- mean(d[,1])
  avg_gt0_site2 <- mean(d[which(d[2]>0),2])
  avg_ge0_site2 <- mean(d[,2])
  return(c(paired_both, paired_one, percentage_zero, 
           avg_gt0_site1, avg_ge0_site1, avg_gt0_site2, avg_ge0_site2))
}

f_n_gt0_one <- function(in_data){
  t <- nrow(in_data)
  d <- in_data
  for(i in 1:t){
    in_data$two_above[i] <- sum(c(d[i,] > 0))
  }
  paired_one <- sum(in_data$two_above == 1)
  return(paired_one)
}

f_n_gt0_two <- function(in_data){
  t <- nrow(in_data)
  d <- in_data
  for(i in 1:t){
    in_data$two_above[i] <- sum(c(d[i,] > 0))
  }
  paired_one <- sum(in_data$two_above == 2)
  return(paired_one)
}

f_n_gt0_one(cor_in)
f_n_gt0_two(cor_in)

f_n_gt0_two

for(i in 1:t){
  cor_in$two_above[1] <- sum(c(d[5,] > 0))
}

count_pairs(cor_in)

sum(cor_in == 0) / (nrow(cor_in)*2)

sum(c(cor_in[3,] > 0))

plot(y1, y2)












