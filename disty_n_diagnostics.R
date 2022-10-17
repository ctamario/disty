


# What I need to do is get a megakey
# 

setwd("C:/jobb/disty")


library(pacman)
p_load(sp, rgdal, riverdist, tidyr, dplyr, ggplot2, regclass, sjPlot, lme4, gridExtra, nlme, ggExtra, lmerMultiMember)


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

f1_sp <- function(row_in, species){
  lokal_x2 <- c(as.character(cd[row_in,3]), as.character(cd[row_in,4]))
  df_sub <- rawdf %>% dplyr::filter(XYKOORLOK == lokal_x2[1] | XYKOORLOK == lokal_x2[2])
  df_sub2 <- df_sub %>% dplyr::select(c(year_mean, bquote(.(species)), XYKOORLOK)) %>% spread(XYKOORLOK, bquote(.(species)))
  df_sub3 <- df_sub2[complete.cases(df_sub2),]
  return(df_sub3)
}

f1(1,species=ÖringTOT_mean)

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
    out.data <- f1_sp(i, "ÖringTOT_mean")
    if(nrow(out.data)>3){
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
    out.data <- f1_sp(i, "Elrit_mean")
    if(nrow(out.data)>3){
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

cd2 <- run_it1(cd)
cd3 <- run_it2(cd2)


cd3 %>% ggplot(aes(x=trout_rho, y=log(trout_avg+1)))+
  geom_point()


