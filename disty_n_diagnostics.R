


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

# We need occurrence data ...

raw_occ <- rawdf %>% group_by(XYKOORLOK, XKOORLOK, YKOORLOK) %>% summarise(trout_occ = mean(ÖRKLASS_mean),
                                                                          minnow_occ = mean(ELKLASS_mean),
                                                                          roach_occ = mean(MÖKLASS_mean),
                                                                          perch_occ = mean(ABKLASS_mean),
                                                                          pike_occ = mean(GÄKLASS_mean),
                                                                          n_occasions = n())

alldata1 <- alldata1 %>% left_join(raw_occ, by=c("fromXY" = "XYKOORLOK"))

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

summary(m_main1 <- lmerMultiMember::lmer(data=trout2_fc, rho_trout~sqrt_rivdist*over_frag01 + (1|river_id) + (1|site), memberships = list(site=membership_matrix_trout_fc)))
summary(m_main2 <- lmerMultiMember::lmer(data=minnow2_fc, rho_minnow~sqrt_rivdist*over_frag01 + (1|river_id) + (1|site), memberships = list(site=membership_matrix_minnow_fc)))
summary(m_main3 <- lmerMultiMember::lmer(data=roach2_fc, rho_roach~sqrt_rivdist*over_frag01 + (1|river_id) + (1|site), memberships = list(site=membership_matrix_roach_fc)))
summary(m_main4 <- lmerMultiMember::lmer(data=perch2_fc, rho_perch~sqrt_rivdist*over_frag01 + (1|river_id) + (1|site), memberships = list(site=membership_matrix_perch_fc)))
summary(m_main5 <- lmerMultiMember::lmer(data=pike2_fc, rho_pike~sqrt_rivdist*over_frag01 + (1|river_id) + (1|site), memberships = list(site=membership_matrix_pike_fc)))

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
  theme_classic() + theme(legend.position = "none")


mp2 <- ggplot(data=m_main2_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=minnow2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=minnow2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic() + theme(legend.position = "none")


mp3 <- ggplot(data=m_main3_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=roach2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=roach2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic() + theme(legend.position = "none")


mp4 <- ggplot(data=m_main4_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=perch2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=perch2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic() + theme(legend.position = "none")


mp5 <- ggplot(data=m_main5_pred, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), linetype = 2, alpha=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  #facet_wrap(facets = vars(facet))+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  geom_rug(data=pike2_fc %>% filter(over_frag01==1), aes(x=sqrt_rivdist), color=hue_pal()(2)[2], sides="t", inherit.aes = F)+
  geom_rug(data=pike2_fc %>% filter(over_frag01==0), aes(x=sqrt_rivdist), color=hue_pal()(2)[1], sides="b", inherit.aes = F)+
  theme_classic() + theme(legend.position = "none")



grid.arrange(mp1, mp2, mp3, mp4, mp5, nrow=1)
#dev.copy2pdf(file="C:/jobb/disty/figs_new/synchrony_distance_between.pdf", height=3, width=15)




##### 



### 3. Fragment table fun



f_table_list <- Sys.glob("C:/jobb/disty/out_files/f_tables/use_these_all/*.csv")

f_table <- read.csv(f_table_list[1], sep=" ")
for(i in 2:length(f_table_list)){
  f_table <- rbind(f_table, read.csv(f_table_list[i], sep=" "))
}

f_table$unique_frag <- paste0(f_table$river_id, "_", f_table$fragmentID)

## Testing to merge 

alldata1_frag <- alldata1 %>% inner_join(dplyr::select(f_table, XKOORLOK, YKOORLOK, fragmentID, unique_frag), by=c("fromXKOORLOK"="XKOORLOK", "fromYKOORLOK"="YKOORLOK"))

table(alldata1_frag$unique_frag)

## 3.5 Synchrony by fragment size PROXY

portf_trout <- alldata1_frag %>% filter(over_frag01 == 0) %>% filter(vtyp_same == 1) %>%
  group_by(unique_frag, river_id.x) %>% filter(trout_n_gt0_two > 0) %>%
  summarise(trout_rho_mean = mean(trout_rho, na.rm = T), 
            trout_avg_mean = mean(trout_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) 

portf_minnow <- alldata1_frag %>% filter(minnow_avg < 1000) %>% filter(over_frag01 == 0) %>% filter(vtyp_same == 1) %>%
  group_by(unique_frag, river_id.x) %>% filter(minnow_n_gt0_two > 0) %>%
  summarise(minnow_rho_mean = mean(minnow_rho, na.rm = T), 
            minnow_avg_mean = mean(minnow_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) 

portf_roach <- alldata1_frag %>% filter(over_frag01 == 0) %>% filter(vtyp_same == 1) %>%
  group_by(unique_frag, river_id.x) %>% filter(roach_n_gt0_two > 0) %>%
  summarise(roach_rho_mean = mean(roach_rho, na.rm = T), 
            roach_avg_mean = mean(roach_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) 

portf_perch <- alldata1_frag %>% filter(over_frag01 == 0) %>% filter(vtyp_same == 1) %>%
  group_by(unique_frag, river_id.x) %>% filter(perch_n_gt0_two > 0) %>%
  summarise(perch_rho_mean = mean(perch_rho, na.rm = T), 
            perch_avg_mean = mean(perch_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) 

portf_pike <- alldata1_frag %>% filter(over_frag01 == 0) %>% filter(vtyp_same == 1) %>%
  group_by(unique_frag, river_id.x) %>% filter(pike_n_gt0_two > 0) %>%
  summarise(pike_rho_mean = mean(pike_rho, na.rm = T), 
            pike_avg_mean = mean(pike_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) 


pl1 <- portf_trout %>% ggplot(aes(x=frag_size, y=trout_rho_mean)) + 
  geom_point()+ geom_smooth(method="lm", color="black") + theme_classic()
pl2 <- portf_minnow %>% ggplot(aes(x=frag_size, y=minnow_rho_mean)) + 
  geom_point() + geom_smooth(method="lm", color="black") + theme_classic()
pl3 <- portf_roach %>% ggplot(aes(x=frag_size, y=roach_rho_mean)) + 
  geom_point() + geom_smooth(method="lm", color="black") + theme_classic()
pl4 <- portf_perch %>% ggplot(aes(x=frag_size, y=perch_rho_mean)) + 
  geom_point() + geom_smooth(method="lm", color="black") + theme_classic()
pl5 <- portf_pike %>% ggplot(aes(x=frag_size, y=pike_rho_mean)) + 
  geom_point() + geom_smooth(method="lm", color="black") + theme_classic()

pl6 <- portf_trout %>% ggplot(aes(x=frag_size, y=trout_avg_mean)) + 
  geom_point()+ geom_smooth(method="lm", color="black") + theme_classic()
pl7 <- portf_minnow %>% ggplot(aes(x=frag_size, y=minnow_avg_mean)) + 
  geom_point() + geom_smooth(method="lm", color="black") + theme_classic()
pl8 <- portf_roach %>% ggplot(aes(x=frag_size, y=roach_avg_mean)) + 
  geom_point() + geom_smooth(method="lm", color="black") + theme_classic()
pl9 <- portf_perch %>% ggplot(aes(x=frag_size, y=perch_avg_mean)) + 
  geom_point() + geom_smooth(method="lm", color="black") + theme_classic()
pl10 <- portf_pike %>% ggplot(aes(x=frag_size, y=pike_avg_mean)) + 
  geom_point() + geom_smooth(method="lm", color="black") + theme_classic()

grid.arrange(pl1, pl2, pl3, pl4, pl5, nrow=1)
#dev.copy2pdf(file="C:/jobb/disty/figs_new/portfolio.pdf", height=3, width=15)
grid.arrange(pl6, pl7, pl8, pl9, pl10, nrow=1)
#dev.copy2pdf(file="C:/jobb/disty/figs_new/density_fragment_size.pdf", height=3, width=15)



summary(lmer(data=portf_trout2, trout_rho_mean ~ frag_size + (1|river_id.x)))
summary(lmer(data=portf_minnow2, minnow_rho_mean ~ frag_size + (1|river_id.x)))
summary(lmer(data=portf_roach, roach_rho_mean ~ frag_size + (1|river_id.x)))
summary(lmer(data=portf_perch, perch_rho_mean ~ frag_size + (1|river_id.x)))
summary(lmer(data=portf_pike, pike_rho_mean ~ frag_size + (1|river_id.x)))

summary(lm(data=portf_trout2, trout_rho_mean ~ frag_size))
summary(lm(data=portf_minnow2, minnow_rho_mean ~ frag_size))
summary(lm(data=portf_roach, roach_rho_mean ~ frag_size))
summary(lm(data=portf_perch, perch_rho_mean ~ frag_size))
summary(lm(data=portf_pike, pike_rho_mean ~ frag_size))

## 

portf_trout2 <- portf_trout[-order(portf_trout$frag_size)[1:2],]

portf_minnow2 <- portf_minnow[-order(portf_minnow$frag_size)[1],]

### 4. portfolio effect


occ_table <- alldata1_frag %>% group_by(fromXY, unique_frag) %>% summarise(trout_occ_mean = mean(trout_occ),
                                                                           minnow_occ_mean = mean(minnow_occ),
                                                                           roach_occ_mean = mean(roach_occ),
                                                                           perch_occ_mean = mean(perch_occ),
                                                                           pike_occ_mean = mean(pike_occ),
                                                                           n_occasions=mean(n_occasions))
#occ_table2 <- occ_table %>% group_by(unique_frag) %>% summarise(trout_occ_mean2 = mean(trout_occ_mean), n_sites = n())

portf_trout2 <- alldata1_frag %>% filter(over_frag01 == 0) %>% 
  group_by(unique_frag, river_id) %>% filter(trout_n_gt0_two > 0) %>%
  summarise(trout_rho_mean = mean(trout_rho, na.rm = T), 
            trout_avg_mean = mean(trout_avg, na.rm = T),
            trout_occ_mean1 = mean(trout_occ, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) %>% left_join(occ_table, by="unique_frag")

portf_trout2 <- portf_trout2 %>% left_join(occ_table, by="unique_frag")

plot(data=portf_trout2, trout_occ_mean2~trout_occ_mean1)

summary(m_pf1 <- lme4::glmer(data=portf_trout2, trout_occ_mean ~ trout_rho_mean*frag_size + (1|river_id) + (1|unique_frag), family="binomial", weights=n_occasions))

plot_model(m_pf1, type="pred", terms=c("frag_size[all]","trout_rho_mean[0.1,0.9]"))+theme_classic()
pdata_pf1 <- ggeffects::ggpredict(m_pf1, type="fe", terms=c("frag_size[all]","trout_rho_mean[0.1,0.9]"))


ggplot(data=pdata_pf1, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.2, linetype="dashed")+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  geom_abline(intercept=c(0,1), slope=0, linetype="dashed")+
  #scale_y_continuous(limits = c(0,1.1))+
  ylab("Persistence (mean occurrence)")+
  xlab("Fragment size proxy (sqrt)")+
  theme_classic()

#dev.copy2pdf(file="C:/jobb/disty/figs_new/portfolio_effect_trout.pdf", height=3, width=4)


portf_minnow2 <- alldata1_frag %>% filter(over_frag01 == 0) %>% 
  group_by(unique_frag, river_id) %>% filter(minnow_n_gt0_two > 0) %>%
  summarise(minnow_rho_mean = mean(minnow_rho, na.rm = T), 
            minnow_avg_mean = mean(minnow_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) %>% left_join(occ_table, by="unique_frag")

summary(m_pf2 <- lme4::glmer(data=portf_minnow2, minnow_occ_mean ~ minnow_rho_mean*frag_size + (1|river_id) + (1|unique_frag), family="binomial", weights=n_occasions))

plot_model(m_pf2, type="pred", terms=c("frag_size[all]","minnow_rho_mean[0.1,0.9]"))+theme_classic()
pdata_pf2 <- ggeffects::ggpredict(m_pf2, type="fe", terms=c("frag_size[all]","minnow_rho_mean[0.1,0.9]"))

ggplot(data=pdata_pf2, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.2, linetype="dashed")+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  geom_abline(intercept=c(0,1), slope=0, linetype="dashed")+
  #scale_y_continuous(limits = c(0,1.1))+
  ylab("Persistence (mean occurrence)")+
  xlab("Fragment size proxy (sqrt)")+
  theme_classic()

#dev.copy2pdf(file="C:/jobb/disty/figs_new/portfolio_effect_minnow.pdf", height=3, width=4)


portf_roach2 <- alldata1_frag %>% filter(over_frag01 == 0) %>%
  group_by(unique_frag, river_id) %>% filter(roach_n_gt0_two > 0) %>%
  summarise(roach_rho_mean = mean(roach_rho, na.rm = T), 
            roach_avg_mean = mean(roach_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) %>% left_join(occ_table, by="unique_frag")

summary(m_pf3 <- lme4::glmer(data=portf_roach2, roach_occ_mean ~ roach_rho_mean*frag_size + (1|river_id)+ (1|unique_frag), family="binomial", weights=n_occasions))

plot_model(m_pf3, type="pred", terms=c("frag_size","roach_rho_mean[0.1,0.9]"))+theme_classic()

portf_perch2 <- alldata1_frag %>% filter(over_frag01 == 0) %>% 
  group_by(unique_frag, river_id) %>% filter(perch_n_gt0_two > 0) %>%
  summarise(perch_rho_mean = mean(perch_rho, na.rm = T), 
            perch_avg_mean = mean(perch_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3) %>% left_join(occ_table, by="unique_frag") 

summary(m_pf4 <- lme4::glmer(data=portf_perch2, perch_occ_mean ~ perch_rho_mean*frag_size + (1|river_id)+ (1|unique_frag), family="binomial", weights=n_occasions))

plot_model(m_pf4, type="pred", terms=c("frag_size","perch_rho_mean[0.1,0.9]"))+theme_classic()

portf_pike2 <- alldata1_frag %>% filter(over_frag01 == 0) %>% 
  group_by(unique_frag, river_id) %>% filter(pike_n_gt0_two > 0) %>%
  summarise(pike_rho_mean = mean(pike_rho, na.rm = T), 
            pike_avg_mean = mean(pike_avg, na.rm = T),
            n = n(), frag_size = mean(rivdistlg)) %>% filter(n > 3)  %>% left_join(occ_table, by="unique_frag")

summary(m_pf5 <- lme4::glmer(data=portf_pike2, pike_occ_mean ~ pike_rho_mean*frag_size + (1|river_id)+ (1|unique_frag), family="binomial"))

plot_model(m_pf5, type="pred", terms=c("frag_size[all]","pike_rho_mean[0.1,0.9]"))+theme_classic()

pdata_pf5 <- ggeffects::ggpredict(m_pf5, type="fe", terms=c("frag_size[all]","pike_rho_mean[0.1,0.9]"))

ggplot(data=pdata_pf5, aes(x=x, y=predicted, colour=group))+
  geom_line()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.2, linetype="dashed")+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  geom_abline(intercept=c(0,1), slope=0, linetype="dashed")+
  #scale_y_continuous(limits = c(0,1.1))+
  ylab("Persistence (mean occurrence)")+
  xlab("Fragment size proxy (sqrt)")+
  theme_classic()

#dev.copy2pdf(file="C:/jobb/disty/figs_new/portfolio_effect_pike.pdf", height=3, width=4)


outfiles <- Sys.glob("C:/jobb/disty/out_files/occ/*.csv")



##### 5. True large vs small fragments

# Load the file with large vs small

l_vs_s <- read.csv("C:/jobb/disty/out_files/f_tables/temp_list_large_small.csv", sep=";", dec=",")

l_vs_s_data <- alldata1_frag %>% left_join(dplyr::select(l_vs_s, "XKOORLOK", "YKOORLOK", "Stor"), by=c("fromXKOORLOK"="XKOORLOK", "fromYKOORLOK"="YKOORLOK"))

l_vs_s_data <- l_vs_s_data %>% filter(!is.na(Stor)) %>% filter(over_frag01 == 0) %>% filter(rivdist<5000)

l_vs_s_data <- l_vs_s_data %>% group_by(river_id, fromXY, unique_frag, Stor) %>% summarise(trout_rho_mean = mean(trout_rho, na.rm = T),
                                                                                trout_avg_lg_mean = mean(trout_avg, na.rm = T),
                                                                                rivdist_mean = mean(rivdist, na.rm = T))

l_vs_s_data %>% ggplot(aes(x=Stor, y=trout_rho_mean, group = Stor)) + geom_point() + theme_classic()
#dev.copy2pdf(file="C:/jobb/disty/figs_new/large_vs_small_rho.pdf", height=3, width=4)
l_vs_s_data %>% ggplot(aes(x=Stor, y=trout_avg_lg_mean, group = Stor)) + geom_point() + theme_classic()
#dev.copy2pdf(file="C:/jobb/disty/figs_new/large_vs_small_density.pdf", height=3, width=4)

summary(lm(data=l_vs_s_data, trout_rho_mean ~ Stor + rivdist_mean))

##### 6. Testing against Larsen et al.

alldata2_frag <- alldata1_frag

alldata2_frag$dedw <- alldata1_frag$pythdist/alldata1_frag$rivdist
alldata2_frag$dedw[alldata2_frag$dedw > 1] <- 1

alldata2_frag <- alldata2_frag %>% group_by(river_id) %>% mutate(median_ed=median(pythdist),
                                                                 median_dedw=median(dedw))

alldata2_frag$cat <- NA
alldata2_frag$cat[which(alldata2_frag$pythdist <= alldata2_frag$median_ed & alldata2_frag$dedw > alldata2_frag$median_dedw)] <- "D1"
alldata2_frag$cat[which(alldata2_frag$pythdist > alldata2_frag$median_ed & alldata2_frag$dedw > alldata2_frag$median_dedw)] <- "D2"
alldata2_frag$cat[which(alldata2_frag$pythdist <= alldata2_frag$median_ed & alldata2_frag$dedw <= alldata2_frag$median_dedw)] <- "D3"
alldata2_frag$cat[which(alldata2_frag$pythdist > alldata2_frag$median_ed & alldata2_frag$dedw <= alldata2_frag$median_dedw)] <- "D4"

alldata3_frag <- alldata2_frag %>% filter(trout_n_gt0_two > 0) 

summary(synch_all <- lme4::lmer(data=alldata3_frag, rho_trout~cat*over_frag01 + (1|river_id)))

synch_all_pred <- ggeffects::ggpredict(synch_all, terms=c("cat", "over_frag01 [0,1]"), type="fe")

ggplot(data=synch_all_pred, aes(x=x, y=predicted, colour=group))+
  geom_point(position=position_dodge(width=0.2))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, colour=group), position=position_dodge(width=0.2), width=0.2)+
  #geom_line(aes(y=conf.low, color=group), linetype=2)+
  #geom_line(aes(y=conf.high, color=group), linetype=2)+
  geom_abline(intercept=0, slope=0, linetype="dashed")+
  theme_classic()

#dev.copy2pdf(file="C:/jobb/disty/figs_new/larsen_preds.pdf", height=3, width=4)





