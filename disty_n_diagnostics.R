


# What I need to do is get a megakey
# 




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
rawdf <- read.csv("../rawdata_20220707.csv", sep=";", dec=",", header = T, encoding = "UTF-8")
rawdf$XKOORLOK1 <- rawdf$XKOORLOK
rawdf$YKOORLOK1 <- rawdf$YKOORLOK
rawdf$XKOORLOK <- factor(rawdf$XKOORLOK)
rawdf$YKOORLOK <- factor(rawdf$YKOORLOK)

rawdf2 <- right_join(rawdf, sites, by = c("XKOORLOK", "YKOORLOK"))

### Just make a small little subset to make everything work
cd <- consolidate_megakeys[1:10,]


### Slowly start to do some function
lokal_x2 <- c(as.character(cd[1,1]), as.character(cd[1,2]))
df_sub <- rawdf2 %>% dplyr::filter(id == lokal_x2[1] | id == lokal_x2[2])
df_sub2 <- df_sub %>% dplyr::select(c(year_mean, ÖringTOT_mean, id)) %>% spread(id, ÖringTOT_mean)
df_sub3 <- df_sub2[complete.cases(df_sub2),]


























