


# What I need to do is get a megakey
# 

setwd("C:/jobb/disty")


library(pacman)
p_load(sp, rgdal, riverdist, tidyr, dplyr, ggplot2, regclass, sjPlot, lme4, gridExtra, nlme, ggExtra, lmerMultiMember, MASS, scales)


### Function
select_river <- function(df, numbah){
  riv_list <- unique(consolidate_megakeys$river_id)
  out_df <- df[which(df$river_id == riv_list[numbah]),]
  return(out_df)
}


### Load main data file
outfiles_megakeys <- Sys.glob("C:/jobb/disty/out_files/megakeys/*")

consolidate_megakeys <- read.csv(outfiles_megakeys[1], sep=" ", dec=".")
for(i in 2:length(outfiles_megakeys)){
  consolidate_megakeys <- rbind(consolidate_megakeys, read.csv(outfiles_megakeys[i], sep=" ", dec="."))
}

### Create a list of all rivers
riv_list <- unique(consolidate_megakeys$river_id)

### Raw data
rawdf <- read.csv("rawdata_20220707.csv", sep=";", dec=",", header = T, encoding = "UTF-8")
rawdf$XKOORLOK1 <- rawdf$XKOORLOK
rawdf$YKOORLOK1 <- rawdf$YKOORLOK
rawdf$XKOORLOK <- factor(rawdf$XKOORLOK)
rawdf$YKOORLOK <- factor(rawdf$YKOORLOK)

rawdf$XYKOORLOK <- paste(rawdf$XKOORLOK, rawdf$YKOORLOK)

head(rawdf)

### Just make a small little subset to make everything work
cd <- consolidate_megakeys[1:100,]


### Slowly start to do some function
lokal_x2 <- c(as.character(cd[1,3]), as.character(cd[1,4]))
df_sub <- rawdf %>% dplyr::filter(XYKOORLOK == lokal_x2[1] | XYKOORLOK == lokal_x2[2])
df_sub2 <- df_sub %>% dplyr::select(c(year_mean, ÖringTOT_mean, XYKOORLOK)) %>% spread(XYKOORLOK, ÖringTOT_mean)
df_sub3 <- df_sub2[complete.cases(df_sub2),]

f1_sp <- function(data_in, row_in, species){
  lokal_x2 <- c(as.character(data_in[row_in,3]), as.character(data_in[row_in,4]))
  df_sub <- rawdf %>% dplyr::filter(XYKOORLOK == lokal_x2[1] | XYKOORLOK == lokal_x2[2])
  df_sub2 <- df_sub %>% dplyr::select(c(year_mean, bquote(.(species)), XYKOORLOK)) %>% spread(XYKOORLOK, bquote(.(species)))
  df_sub3 <- df_sub2[complete.cases(df_sub2),]
  return(df_sub3)
}


f1 <- function(row_in){
  lokal_x2 <- c(as.character(cd[row_in,3]), as.character(cd[row_in,4]))
  df_sub <- rawdf %>% dplyr::filter(XYKOORLOK == lokal_x2[1] | XYKOORLOK == lokal_x2[2])
  df_sub2 <- df_sub %>% dplyr::select(c(year_mean, ÖringTOT_mean, XYKOORLOK)) %>% spread(XYKOORLOK, ÖringTOT_mean)
  df_sub3 <- df_sub2[complete.cases(df_sub2),]
  return(df_sub3)
}


f1_sp(1, "ÖringTOT_mean")


out.data <- f1(10)

f_n_years <- function(in_data){
  n_years <- nrow(in_data)
  return(n_years)
}
f_n_years(out.data)

f_cor <- function(in_data){
  spearmans_r <- cor.test(in_data[,2], in_data[,3], method="spearman")$estimate
  return(spearmans_r)
}
f_cor(out.data)

f_n_gt0_one <- function(in_data){
  t <- nrow(in_data)
  d <- in_data[,2:3]
  for(i in 1:t){
    in_data$two_above[i] <- sum(c(d[i,] > 0))
  }
  paired_one <- sum(in_data$two_above == 1)
  return(paired_one)
}

f_n_gt0_one(out.data)

f_n_gt0_two <- function(in_data){
  t <- nrow(in_data)
  d <- in_data[,2:3]
  for(i in 1:t){
    in_data$two_above[i] <- sum(c(d[i,] > 0))
  }
  paired_one <- sum(in_data$two_above == 2)
  return(paired_one)
}

f_n_gt0_two(out.data)

for(i in 1:10){
  out.data <- f1(i)
  if(nrow(out.data)>3){
    cd$rho[i] <- f_cor(out.data)
    cd$n[i] <- f_n_years(out.data)
    cd$n_gt0_one[i] <- f_n_gt0_one(out.data)
    cd$n_gt0_two[i] <- f_n_gt0_two(out.data)
    cd$avg_gt0_site1[i] <- mean(d[which(d[1]>0),1])
    cd$avg_ge0_site1[i] <- mean(d[,1])
    cd$avg_gt0_site2[i] <- mean(d[which(d[2]>0),2])
    cd$avg_ge0_site2[i] <- mean(d[,2])
  } else {
    cd$rho[i] <- NA
    cd$n[i] <- NA
    cd$n_gt0_one[i] <- NA
    cd$n_gt0_two[i] <- NA
  }
}



run_it1 <- function(dataset_in){
  f_df <- dataset_in
  for(i in 1:nrow(f_df)){
    out.data <- f1_sp(consolidate_megakeys, i, "ÖringTOT_mean")
    if(NROW(out.data)>3){
      f_df$trout_rho[i] <- f_cor(out.data)
      f_df$trout_n[i] <- f_n_years(out.data)
      f_df$trout_n_gt0_one[i] <- f_n_gt0_one(out.data)
      f_df$trout_n_gt0_two[i] <- f_n_gt0_two(out.data)
      f_df$trout_avg_gt0_site1[i] <- mean(out.data[which(out.data[2]>0),2])
      f_df$trout_avg_ge0_site1[i] <- mean(out.data[,2])
      f_df$trout_avg_gt0_site2[i] <- mean(out.data[which(out.data[3]>0),3])
      f_df$trout_avg_ge0_site2[i] <- mean(out.data[,3])
      f_df$trout_avg[i] <- mean(c(out.data[,2], out.data[,2]))
    } else {
      f_df$trout_rho[i] <- NA
      f_df$trout_n[i] <- NA
      f_df$trout_n_gt0_one[i] <- NA
      f_df$trout_n_gt0_two[i] <- NA
      f_df$trout_avg_gt0_site1[i] <- NA
      f_df$trout_avg_ge0_site1[i] <- NA
      f_df$trout_avg_gt0_site2[i] <- NA
      f_df$trout_avg_ge0_site2[i] <- NA
      f_df$trout_avg[i] <- NA
    }
  }
  return(f_df)
}

run_it2 <- function(dataset_in){
  f_df <- dataset_in
  for(i in 1:nrow(f_df)){
    out.data <- f1_sp(consolidate_megakeys, i, "Gädda_mean")
    if(NROW(out.data)>3){
      f_df$pike_rho[i] <- f_cor(out.data)
      f_df$pike_n[i] <- f_n_years(out.data)
      f_df$pike_n_gt0_one[i] <- f_n_gt0_one(out.data)
      f_df$pike_n_gt0_two[i] <- f_n_gt0_two(out.data)
      f_df$pike_avg_gt0_site1[i] <- mean(out.data[which(out.data[2]>0),2])
      f_df$pike_avg_ge0_site1[i] <- mean(out.data[,2])
      f_df$pike_avg_gt0_site2[i] <- mean(out.data[which(out.data[3]>0),3])
      f_df$pike_avg_ge0_site2[i] <- mean(out.data[,3])
      f_df$pike_avg[i] <- mean(c(out.data[,2], out.data[,2]))
    } else {
      f_df$pike_rho[i] <- NA
      f_df$pike_n[i] <- NA
      f_df$pike_n_gt0_one[i] <- NA
      f_df$pike_n_gt0_two[i] <- NA
      f_df$pike_avg_gt0_site1[i] <- NA
      f_df$pike_avg_ge0_site1[i] <- NA
      f_df$pike_avg_gt0_site2[i] <- NA
      f_df$pike_avg_ge0_site2[i] <- NA
      f_df$pike_avg[i] <- NA
    }
  }
  return(f_df)
}

run_it3 <- function(dataset_in){
  f_df <- dataset_in
  for(i in 1:nrow(f_df)){
    out.data <- f1_sp(consolidate_megakeys, i, "Elrit_mean")
    if(NROW(out.data)>3){
      f_df$minnow_rho[i] <- f_cor(out.data)
      f_df$minnow_n[i] <- f_n_years(out.data)
      f_df$minnow_n_gt0_one[i] <- f_n_gt0_one(out.data)
      f_df$minnow_n_gt0_two[i] <- f_n_gt0_two(out.data)
      f_df$minnow_avg_gt0_site1[i] <- mean(out.data[which(out.data[2]>0),2])
      f_df$minnow_avg_ge0_site1[i] <- mean(out.data[,2])
      f_df$minnow_avg_gt0_site2[i] <- mean(out.data[which(out.data[3]>0),3])
      f_df$minnow_avg_ge0_site2[i] <- mean(out.data[,3])
      f_df$minnow_avg[i] <- mean(c(out.data[,2], out.data[,2]))
    } else {
      f_df$minnow_rho[i] <- NA
      f_df$minnow_n[i] <- NA
      f_df$minnow_n_gt0_one[i] <- NA
      f_df$minnow_n_gt0_two[i] <- NA
      f_df$minnow_avg_gt0_site1[i] <- NA
      f_df$minnow_avg_ge0_site1[i] <- NA
      f_df$minnow_avg_gt0_site2[i] <- NA
      f_df$minnow_avg_ge0_site2[i] <- NA
      f_df$minnow_avg[i] <- NA
    }
  }
  return(f_df)
}

run_it4 <- function(dataset_in){
  f_df <- dataset_in
  for(i in 1:nrow(f_df)){
    out.data <- f1_sp(consolidate_megakeys, i, "Abbor_mean")
    if(NROW(out.data)>3){
      f_df$perch_rho[i] <- f_cor(out.data)
      f_df$perch_n[i] <- f_n_years(out.data)
      f_df$perch_n_gt0_one[i] <- f_n_gt0_one(out.data)
      f_df$perch_n_gt0_two[i] <- f_n_gt0_two(out.data)
      f_df$perch_avg_gt0_site1[i] <- mean(out.data[which(out.data[2]>0),2])
      f_df$perch_avg_ge0_site1[i] <- mean(out.data[,2])
      f_df$perch_avg_gt0_site2[i] <- mean(out.data[which(out.data[3]>0),3])
      f_df$perch_avg_ge0_site2[i] <- mean(out.data[,3])
      f_df$perch_avg[i] <- mean(c(out.data[,2], out.data[,2]))
    } else {
      f_df$perch_rho[i] <- NA
      f_df$perch_n[i] <- NA
      f_df$perch_n_gt0_one[i] <- NA
      f_df$perch_n_gt0_two[i] <- NA
      f_df$perch_avg_gt0_site1[i] <- NA
      f_df$perch_avg_ge0_site1[i] <- NA
      f_df$perch_avg_gt0_site2[i] <- NA
      f_df$perch_avg_ge0_site2[i] <- NA
      f_df$perch_avg[i] <- NA
    }
  }
  return(f_df)
}

run_it5 <- function(dataset_in){
  f_df <- dataset_in
  for(i in 1:nrow(f_df)){
    out.data <- f1_sp(consolidate_megakeys, i, "Mört_mean")
    if(NROW(out.data)>3){
      f_df$roach_rho[i] <- f_cor(out.data)
      f_df$roach_n[i] <- f_n_years(out.data)
      f_df$roach_n_gt0_one[i] <- f_n_gt0_one(out.data)
      f_df$roach_n_gt0_two[i] <- f_n_gt0_two(out.data)
      f_df$roach_avg_gt0_site1[i] <- mean(out.data[which(out.data[2]>0),2])
      f_df$roach_avg_ge0_site1[i] <- mean(out.data[,2])
      f_df$roach_avg_gt0_site2[i] <- mean(out.data[which(out.data[3]>0),3])
      f_df$roach_avg_ge0_site2[i] <- mean(out.data[,3])
      f_df$roach_avg[i] <- mean(c(out.data[,2], out.data[,2]))
    } else {
      f_df$roach_rho[i] <- NA
      f_df$roach_n[i] <- NA
      f_df$roach_n_gt0_one[i] <- NA
      f_df$roach_n_gt0_two[i] <- NA
      f_df$roach_avg_gt0_site1[i] <- NA
      f_df$roach_avg_ge0_site1[i] <- NA
      f_df$roach_avg_gt0_site2[i] <- NA
      f_df$roach_avg_ge0_site2[i] <- NA
      f_df$roach_avg[i] <- NA
    }
  }
  return(f_df)
}

cd2 <- run_it1(cd)
cd3 <- run_it2(cd2)

head(cd3)

cd3 %>% ggplot(aes(x=trout_rho, y=log(trout_avg+1)))+
  geom_point()

test <- run_it1(consolidate_megakeys)

rm(test)


start_time <- Sys.time()
test <- run_it1(consolidate_megakeys)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
test2 <- run_it2(test)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
test_add3 <- run_it3(test2)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
test_add4 <- run_it4(test_add3)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
test_add5 <- run_it5(test_add4)
end_time <- Sys.time()
end_time - start_time

head(test2)

table(test2$pike_n_gt0_two)
table(test2$trout_n_gt0_two)

### Testar lite...

test3 <- test_add5 %>% filter(n_years>5)

test4 <- test3 %>% filter(vtyp_same == T)

test5 <- test4 %>% filter(over_frag01 == 0)

test6 <- test4 %>% filter(over_frag01 == 0) %>% filter(flowconn01 == 1)

test5 %>% ggplot(aes(y = trout_rho, x = log(trout_avg+1))) + geom_point() + theme_classic()

test5 %>% ggplot(aes(y = pike_rho, x = log(pike_avg+1))) + geom_point() + theme_classic()

test6 %>% ggplot(aes(y = trout_rho, x = log(trout_avg+1))) + geom_point() + theme_classic()

test6 %>% ggplot(aes(y = pike_rho, x = log(pike_avg+1))) + geom_point() + theme_classic()

test5$trout_avg_lg <- log(test5$trout_avg+1)



### Fortsätter att testa lite ... 

truehist(test5$pike_rho)

test4$trout_prop_unpaired <- test4$trout_n_gt0_one/test4$n_years
test4$minnow_prop_unpaired <- test4$minnow_n_gt0_one/test4$n_years
test4$roach_prop_unpaired <- test4$roach_n_gt0_one/test4$n_years
test4$perch_prop_unpaired <- test4$perch_n_gt0_one/test4$n_years
test4$pike_prop_unpaired <- test4$pike_n_gt0_one/test4$n_years


pike <- test4 %>% filter(pike_n_gt0_two > 0)
trout <- test4 %>% filter(trout_n_gt0_two > 0)
roach <- test4 %>% filter(roach_n_gt0_two > 0)
minnow <- test4 %>% filter(minnow_n_gt0_two > 0)
perch <- test4 %>% filter(perch_n_gt0_two > 0)



truehist(test4$trout_rho, prob=F, ylim=c(0,600))
truehist(trout$trout_rho, prob=F, ylim=c(0,600))

truehist(test4$pike_rho, prob=F, ylim=c(0,700), h=0.1)
truehist(pike$pike_rho, prob=F, ylim=c(0,700), h=0.1)

truehist(test4$roach_rho, prob=F, ylim=c(0,260), h=0.1)
truehist(roach$roach_rho, prob=F, ylim=c(0,260), h=0.1)

truehist(test4$minnow_rho, prob=F, ylim=c(0,700), h=0.1)
truehist(minnow$minnow_rho, prob=F, ylim=c(0,700), h=0.1)

truehist(test4$perch_rho, prob=F, xlim=c(-1,1.1), ylim=c(0,300), h=0.1)
truehist(perch$perch_rho, prob=F, xlim=c(-1,1.1), ylim=c(0,300), h=0.1)

### 

pike %>% ggplot(aes(y=pike_rho, x=pike_avg)) + geom_point() + theme_classic()
trout %>% ggplot(aes(y=trout_rho, x=trout_avg)) + geom_point() + theme_classic()
roach %>% ggplot(aes(y=roach_rho, x=roach_avg)) + geom_point() + theme_classic()
minnow %>% ggplot(aes(y=minnow_rho, x=minnow_avg)) + geom_point() + theme_classic()
perch %>% ggplot(aes(y=perch_rho, x=perch_avg)) + geom_point() + theme_classic()

###

write.table(test_add5, file="C:/jobb/disty/fulldata_2022_10_18.csv", row.names=F, sep=";", dec=".")

###

alldata <- read.csv(file="C:/jobb/disty/fulldata_2022_10_18.csv", sep=";", dec=".", encoding="UTF-8")


###

alldata1 <- alldata %>% filter(n_years>5)

alldata1$trout_prop_unpaired <- alldata1$trout_n_gt0_one/alldata1$n_years
alldata1$minnow_prop_unpaired <- alldata1$minnow_n_gt0_one/alldata1$n_years
alldata1$roach_prop_unpaired <- alldata1$roach_n_gt0_one/alldata1$n_years
alldata1$perch_prop_unpaired <- alldata1$perch_n_gt0_one/alldata1$n_years
alldata1$pike_prop_unpaired <- alldata1$pike_n_gt0_one/alldata1$n_years


alldata1$sqrt_rivdist <- sqrt(alldata1$rivdist)
alldata1$sqrt_pythdist <- sqrt(alldata1$pythdist)

### Make random effects for multimembership 

alldata1$site1 <- alldata1$fromid
alldata1$site2 <- alldata1$toid
alldata1$random_site1 <- paste0(alldata1$river_id,"_",alldata1$site1)
alldata1$random_site2 <- paste0(alldata1$river_id,"_",alldata1$site2)
alldata1$random_site3 <- paste0(alldata1$random_site1,",",alldata1$random_site2)

membership_matrix1 <- weights_from_vector(alldata1$random_site3)

### Make the important filtration

pike <- alldata1 %>% filter(pike_n_gt0_two > 0)
trout <- alldata1 %>% filter(trout_n_gt0_two > 0)
roach <- alldata1 %>% filter(roach_n_gt0_two > 0)
minnow <- alldata1 %>% filter(minnow_n_gt0_two > 0)
perch <- alldata1 %>% filter(perch_n_gt0_two > 0)

minnow <- minnow[-which(minnow$minnow_avg > 1000),]

###


m1 <- lme4::lmer(data=trout, trout_rho ~ log(trout_avg+1) + rivdistlg * flowconn01 * over_frag01 + (1|river_id))
summary(m1)
plot_model(m1, type="pred")

m2 <- lme4::lmer(data=pike, pike_rho ~ log(pike_avg+1) + rivdistlg * flowconn01 * over_frag01 + (1|river_id))
summary(m2)
plot_model(m2, type="pred")

m3 <- lme4::lmer(data=roach, roach_rho ~ log(roach_avg+1) + rivdistlg * flowconn01 * over_frag01 + (1|river_id))
summary(m3)
plot_model(m3, type="pred")

m4 <- lme4::lmer(data=minnow, minnow_rho ~ log(minnow_avg+1) + rivdistlg * flowconn01 * over_frag01 + (1|river_id))
summary(m4)
plot_model(m4, type="pred")

m5 <- lme4::lmer(data=perch, perch_rho ~ log(perch_avg+1) + rivdistlg * flowconn01 * over_frag01 + (1|river_id))
summary(m5)
plot_model(m5, type="pred")

summary(lmer(data=test5, pike_rho ~ log(pike_avg+1)))

###

trout %>% ggplot(aes(x = trout_n_gt0_one, y = trout_rho)) + geom_point(alpha = 0.1) + theme_classic()
trout %>% ggplot(aes(x = trout_n_gt0_two, y = trout_rho)) + geom_point(alpha = 0.1) + theme_classic()

pike %>% ggplot(aes(x = pike_n_gt0_one, y = pike_rho)) + geom_point(alpha = 0.1) + theme_classic()
pike %>% ggplot(aes(x = pike_n_gt0_two, y = pike_rho)) + geom_point(alpha = 0.1) + theme_classic()

roach %>% ggplot(aes(x = roach_n_gt0_one, y = roach_rho)) + geom_point(alpha = 0.1) + theme_classic()
roach %>% ggplot(aes(x = roach_n_gt0_two, y = roach_rho)) + geom_point(alpha = 0.1) + theme_classic()

minnow %>% ggplot(aes(x = minnow_n_gt0_one, y = minnow_rho)) + geom_point(alpha = 0.1) + theme_classic()
minnow %>% ggplot(aes(x = minnow_n_gt0_two, y = minnow_rho)) + geom_point(alpha = 0.1) + theme_classic()

perch %>% ggplot(aes(x = perch_n_gt0_one, y = perch_rho)) + geom_point(alpha = 0.1) + theme_classic()
perch %>% ggplot(aes(x = perch_n_gt0_two, y = perch_rho)) + geom_point(alpha = 0.1) + theme_classic()

### 

trout %>% ggplot(aes(x = trout_prop_unpaired, y = trout_rho)) + geom_point(alpha = 0.1) + theme_classic()

pike %>% ggplot(aes(x = pike_prop_unpaired, y = pike_rho)) + geom_point(alpha = 0.1) + theme_classic()

roach %>% ggplot(aes(x = roach_prop_unpaired, y = roach_rho)) + geom_point(alpha = 0.1) + theme_classic()

minnow %>% ggplot(aes(x = minnow_prop_unpaired, y = minnow_rho)) + geom_point(alpha = 0.1) + theme_classic()

perch %>% ggplot(aes(x = perch_prop_unpaired, y = perch_rho, size=n_years)) + geom_point(alpha = 0.1) + geom_smooth(method="lm") + theme_classic()




### 

trout %>% ggplot(aes(x = trout_prop_unpaired, y = trout_avg)) + geom_point(alpha = 0.1) + theme_classic()

pike %>% ggplot(aes(x = pike_prop_unpaired, y = pike_avg)) + geom_point(alpha = 0.1) + theme_classic()

roach %>% ggplot(aes(x = roach_prop_unpaired, y = roach_avg)) + geom_point(alpha = 0.1) + theme_classic()

minnow %>% ggplot(aes(x = minnow_prop_unpaired, y = minnow_avg)) + geom_point(alpha = 0.1) + theme_classic()

perch %>% ggplot(aes(x = perch_prop_unpaired, y = perch_avg, size=n_years)) + geom_point(alpha = 0.1) + theme_classic()


###

trout2_fc <- trout %>% filter(!is.na(trout_rho)) %>% filter(flowconn01 == 1)
minnow2_fc <- minnow %>% filter(!is.na(minnow_rho)) %>% filter(flowconn01 == 1)
roach2_fc <- roach %>% filter(!is.na(roach_rho)) %>% filter(flowconn01 == 1)
perch2_fc <- perch %>% filter(!is.na(perch_rho)) %>% filter(flowconn01 == 1)
pike2_fc <- pike %>% filter(!is.na(pike_rho)) %>% filter(flowconn01 == 1)

trout2 <- trout %>% filter(!is.na(trout_rho))
minnow2 <- minnow %>% filter(!is.na(minnow_rho))
roach2 <- roach %>% filter(!is.na(roach_rho)) 
perch2 <- perch %>% filter(!is.na(perch_rho))
pike2 <- pike %>% filter(!is.na(pike_rho))

membership_matrix_trout  <- weights_from_vector(trout2$random_site3)
membership_matrix_minnow  <- weights_from_vector(minnow2$random_site3)
membership_matrix_roach <- weights_from_vector(roach2$random_site3)
membership_matrix_perch <- weights_from_vector(perch2$random_site3)
membership_matrix_pike <- weights_from_vector(pike2$random_site3)

membership_matrix_trout_fc  <- weights_from_vector(trout2_fc$random_site3)
membership_matrix_minnow_fc  <- weights_from_vector(minnow2_fc$random_site3)
membership_matrix_roach_fc <- weights_from_vector(roach2_fc$random_site3)
membership_matrix_perch_fc <- weights_from_vector(perch2_fc$random_site3)
membership_matrix_pike_fc <- weights_from_vector(pike2_fc$random_site3)

### Make some models

summary(m_main1 <- lmerMultiMember::lmer(data=trout2_fc, rho_trout~sqrt_rivdist*over_frag01+trout_avg + (1|river_id) + (1|site), memberships = list(site=membership_matrix_trout_fc)))
summary(m_main2 <- lmerMultiMember::lmer(data=minnow2_fc, rho_minnow~sqrt_rivdist*over_frag01+minnow_avg + (1|river_id) + (1|site), memberships = list(site=membership_matrix_minnow_fc)))
summary(m_main3 <- lmerMultiMember::lmer(data=roach2_fc, rho_roach~sqrt_rivdist*over_frag01+roach_avg + (1|river_id) + (1|site), memberships = list(site=membership_matrix_roach_fc)))
summary(m_main4 <- lmerMultiMember::lmer(data=perch2_fc, rho_perch~sqrt_rivdist*over_frag01+perch_avg + (1|river_id) + (1|site), memberships = list(site=membership_matrix_perch_fc)))
summary(m_main5 <- lmerMultiMember::lmer(data=pike2_fc, rho_pike~sqrt_rivdist*over_frag01+pike_avg + (1|river_id) + (1|site), memberships = list(site=membership_matrix_pike_fc)))

plot_model(m_main1, type = "pred", terms=c("sqrt_rivdist", "over_frag01 [0,1]"))+theme_classic()
plot_model(m_main2, type = "pred", terms=c("sqrt_rivdist", "over_frag01 [0,1]"))+theme_classic()
plot_model(m_main3, type = "pred", terms=c("sqrt_rivdist", "over_frag01 [0,1]"))+theme_classic()
plot_model(m_main4, type = "pred", terms=c("sqrt_rivdist", "over_frag01 [0,1]"))+theme_classic()
plot_model(m_main5, type = "pred", terms=c("sqrt_rivdist", "over_frag01 [0,1]"))+theme_classic()

plot_model(m_main1, type = "pred", terms=c("trout_avg"))+theme_classic()
plot_model(m_main2, type = "pred", terms=c("minnow_avg"))+theme_classic()
plot_model(m_main3, type = "pred", terms=c("roach_avg"))+theme_classic()
plot_model(m_main4, type = "pred", terms=c("perch_avg"))+theme_classic()
plot_model(m_main5, type = "pred", terms=c("pike_avg"))+theme_classic()

### Make some plots

m_main1_pred <- ggeffects::ggpredict(m_main1, terms=c("sqrt_rivdist", "over_frag01 [0,1]"), type="fe")
m_main2_pred <- ggeffects::ggpredict(m_main2, terms=c("sqrt_rivdist", "over_frag01 [0,1]"), type="fe")
m_main3_pred <- ggeffects::ggpredict(m_main3, terms=c("sqrt_rivdist", "over_frag01 [0,1]"), type="fe")
m_main4_pred <- ggeffects::ggpredict(m_main4, terms=c("sqrt_rivdist", "over_frag01 [0,1]"), type="fe")
m_main5_pred <- ggeffects::ggpredict(m_main5, terms=c("sqrt_rivdist", "over_frag01 [0,1]"), type="fe")

mp1 <- ggplot(data=m_main1_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=trout2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=trout2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic()


mp2 <- ggplot(data=m_main2_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=minnow2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=minnow2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic()


mp3 <- ggplot(data=m_main3_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=roach2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=roach2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic()


mp4 <- ggplot(data=m_main4_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=perch2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=perch2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic()


mp5 <- ggplot(data=m_main5_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=pike2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=pike2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic()



grid.arrange(mp1, mp2, mp3, mp4, mp5)

##### 



### 3. Fragment table fun



f_table_list <- Sys.glob("C:/jobb/disty/out_files/f_tables/use_these_all/*.csv")

f_table <- read.csv(f_table_list[1], sep=" ")
for(i in 2:length(f_table_list)){
  f_table <- rbind(f_table, read.csv(f_table_list[i], sep=" "))
}

f_table$unique_frag <- paste0(f_table$river_id, "_", f_table$fragmentID)

#Testing to merge 

alldata1_frag <- alldata1 %>% inner_join(f_table, by=c("fromXKOORLOK"="XKOORLOK", "fromYKOORLOK"="YKOORLOK"))

table(alldata1_frag$unique_frag)

portf <- alldata1_frag %>% filter(over_frag01 == 0) %>% filter(vtyp_same == 1) %>%
  group_by(unique_frag) %>% summarise(trout_rho_mean = mean(trout_rho, na.rm = T), n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 5) 

portf %>% ggplot(aes(x=frag_size, y=trout_rho_mean)) + geom_point()

summary(lm(data=portf, trout_rho_mean ~ frag_size))

### 4. portfolio effect


outfiles <- Sys.glob("C:/jobb/disty/out_files/occ/*.csv")

consolidate_portfolio <- read.csv(outfiles[1], sep=";", dec=".")
for(i in 2:length(outfiles)){
  consolidate_portfolio <- rbind(consolidate_portfolio, read.csv(outfiles[i], sep=";", dec="."))
}









