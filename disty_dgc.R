






rm(list=ls())

is_over_frag3<-function(fseg, fvert, tseg, tvert){
  
  from_seg <- sites$seg[fseg]
  from_vert <- sites$vert[fvert]
  to_seg <- sites$seg[tseg]
  to_vert <- sites$vert[tvert]
  
  site_to_site_route <- detectroute(from_seg, to_seg, MyR3)
  dam_seg_vector <- dams$seg
  dam_vert_vector <- dams$vert
  dire <- riverdirection(startseg=from_seg, startvert=from_vert, 
                         endseg=to_seg, endvert=to_vert, rivers=MyR3, flowconnected = T)
  
  if(is.na(dire)){
    dire <- "forking"
  }
  
  over_frag_within <- NA
  over_frag_to <- NA
  over_frag_from <- NA
  over_frag_any <- NA
  
  if(from_seg == to_seg){
    if(TRUE %in% (from_seg %in% dam_seg_vector)){
      over_frag_within <- TRUE %in% (dam_vert_vector[which(dam_seg_vector == from_seg)] %in% from_vert:to_vert)
      over_frag_any <- FALSE
    } else {
      over_frag_within <- FALSE
    }
  } else {
    if(TRUE %in% (dam_seg_vector %in% site_to_site_route)){
      #FÃ¶r frÃ¥npunkten
      if(site_to_site_route[1] %in% dam_seg_vector){
        if(dire != "up"){
          over_frag_from <- TRUE %in% (from_vert > dam_vert_vector[which(dam_seg_vector == site_to_site_route[1])])
        } else if (dire == "up") {
          over_frag_from <- TRUE %in% (from_vert < dam_vert_vector[which(dam_seg_vector == site_to_site_route[1])])
        } else {
          over_frag_from <- FALSE  
        }
      } else {
        over_frag_from <- FALSE
      }
      
      
      #FÃ¶r tillpunkten
      #If denna Ã¤r true
      if(site_to_site_route[length(site_to_site_route)] %in% dam_seg_vector){
        if(dire == "down"){
          over_frag_to <- to_vert < dam_vert_vector[which(dam_seg_vector == site_to_site_route[length(site_to_site_route)])]
        } else {
          over_frag_to <- to_vert > dam_vert_vector[which(dam_seg_vector == site_to_site_route[length(site_to_site_route)])]
        }
      } else {
        over_frag_to <- FALSE
      }
    } else {
      over_frag_to <- FALSE
    }
  }
  
  if(length(site_to_site_route)>2){
    #over_frag_any <- TRUE %in% (dam_seg_vector %in% site_to_site_route)
    over_frag_any <- TRUE %in% (dam_seg_vector %in% site_to_site_route[2:(length(site_to_site_route)-1)])
  }
  
  
  #riverdistance(startseg=from_seg, startvert=from_vert, endseg=to_seg, endvert=to_vert, rivers=MyR3, map=TRUE)
  #riverpoints(seg=Em1_dams$seg, vert=Em1_dams$vert, rivers=MyR3)
  return(TRUE %in% c(over_frag_any, over_frag_within, over_frag_to, over_frag_from))
  
  
}

library(pacman)
p_load(sp, rgdal, riverdist, tidyr, dplyr, ggplot2, regclass, sjPlot, lme4)

#Set working directory for the catchment you will work on.
#it needs to contain three files: river polylines, electrofishing sites, and dam sites.
setwd("C:/jobb/disty/74000")

MyRivernetwork <- line2network(path=".", layer="74000_rivers", tolerance=5)
# 
# plot(MyRivernetwork)
# 
# topologydots(rivers=MyRivernetwork)

MyR2 <- cleanup(MyRivernetwork)

MyR3 <- sequenceverts(MyR2)

sites <- pointshp2segvert(path=".", layer="74000_sites", rivers=MyR3)
#Remove all sites with snapping distance >100 meters as these likely are not in the rivers
hist(sites$snapdist, main="snapping distance (m)")

remove_long_snaps <- function(in_data, dist){
  if(TRUE %in% (in_data$snapdist > dist) == T){
    in_data <- in_data[-which(in_data$snapdist > dist),]
  }
  return(in_data)
}

sites <- remove_long_snaps(sites, dist=100)


hist(sites$snapdist, main="snapping distance (m)")

#topologydots(rivers=MyR3)

plot(MyR3, color=F, segmentnum=F)
riverpoints(seg=sites$seg, vert=sites$vert, rivers=MyR3, col="blue")

#zoomtoseg(seg=c(192), rivers=MyR3)

#detectroute(start=121, end=14, rivers=MyR3)

#riverdistance(startseg=Em1_sites$seg[9], startvert=Em1_sites$vert[9], 
#              endseg=Em1_sites$seg[17], endvert=Em1_sites$vert[17], rivers=MyR3, map=TRUE)

#Try calculating for the first 5 sites...

#dmat <- riverdistancemat(Em1_sites$seg, Em1_sites$vert, MyR3)


##testing
#isflowconnected(81, 46, MyR3)



##adding dams??

dams <- pointshp2segvert(path=".", layer="74000_dams", rivers=MyR3)
hist(dams$snapdist, main="snapping distance (m)")
dams <- remove_long_snaps(dams, dist=100)
hist(dams$snapdist, main="snapping distance (m)")


### Check for overlapping 

check_for_duped_sites <- function(site_layer, dam_layer){
  hej <- rbind(site_layer[,1:2], dam_layer[,1:2])
  return(list(TRUE %in% duplicated(hej),duplicated(hej)))
}

check_for_duped_sites2 <- function(site_layer, dam_layer){
  hej <- rbind(site_layer[,1:2], dam_layer[,1:2])
  return(cbind(hej,duplicated(hej)))
}

check_for_duped_sites(sites, dams) #JUST PROCEED IF FALSE, OTHERWISE YOU NEED TO MOVE THE SERS SITE DOWNSTREAM AUTOMATICALLY 
check_for_duped_sites2(sites, dams) 

#zoomtoseg(seg=c(514), rivers=MyR3)

# KÃ¶r matris

conn_mat <- matrix(NA, nrow=nrow(sites), ncol=nrow(sites))

for(i in 1:nrow(sites)){
  for(j in 1:nrow(sites)){
    conn_mat[i,j]<-is_over_frag3(fseg=i,fvert=i,tseg=j,tvert=j)
  }
}

unique.matrix(conn_mat)

uni_mat <- unique.matrix(conn_mat)

#### Create a fragment table based on the connectedness-matrix above



create_fragments <- function(conn_mat_in, i){
  uniq_conn_mat <- unique.matrix(conn_mat_in)
  a <- which(!uniq_conn_mat[i,] == T)
  b <- rep(i, length(uniq_conn_mat[which(!uniq_conn_mat[i,] == T)]))
  return(data.frame(site=a,fragmentID=b))
}

fragment_table <- create_fragments(conn_mat, i=1)
for(j in 2:nrow(unique.matrix(conn_mat))){
  fragment_table <- rbind(fragment_table, create_fragments(conn_mat, i=j))
}


fragment_table

table(fragment_table$site)

#### Plotting the fragment identity of sites

plot(MyR3, segmentnum=F, color = FALSE, lwd=1)

for(i in seq_along(unique(fragment_table$fragmentID))){
  Ns <- fragment_table$site[which(fragment_table$fragmentID == i)]
  riverpoints(seg=sites$seg[Ns], vert=sites$vert[Ns], rivers=MyR3, col=rainbow(max(fragment_table$fragmentID))[i], pch=16, cex=1.5)
}

riverpoints(seg=dams$seg, vert=dams$vert, rivers=MyR3, col="black", pch=17, cex=1)

#dev.copy2pdf(file="74000_map.pdf", width=10, height=7)

#Trouble with sites belonging to more than one fragment?
cbind(fragment_table, duplicated(fragment_table[,1]))
trouble_list <- fragment_table$site[duplicated(fragment_table[,1])]
riverpoints(seg=sites$seg[trouble_list], vert=sites$vert[trouble_list], rivers=MyR3, col="black", pch=17, cex=1)




##
#sites$id <- rownames(sites)
sites$id <- as.character(1:nrow(sites))
sites$XYKOORLOK <- paste(sites$XKOORLOK, sites$YKOORLOK)

key <- expand.grid(sites$id, sites$id)
key <- key[,c(2,1)]
names(key) <- c("fromid", "toid")

key2 <- expand.grid(sites$XYKOORLOK, sites$XYKOORLOK)
key2 <- key2[,c(2,1)]
names(key2) <- c("fromXY", "toXY")

key3 <- cbind(key,key2)

key3$over_frag <- NA
for(i in 1:nrow(key3)){
  key3$over_frag[i] <- is_over_frag3(fseg=key3$fromid[i],fvert=key3$fromid[i],
                                     tseg=key3$toid[i],tvert=key3$toid[i])
}

key3$pythdist <- NA
for(i in 1:nrow(key3)){
  key3$pythdist[i] <- pdist(c(as.numeric(sites$s99tm_e[key3$fromid[i]]),as.numeric(sites$s99tm_n[key3$fromid[i]])),
                            c(as.numeric(sites$s99tm_e[key3$toid[i]]),as.numeric(sites$s99tm_n[key3$toid[i]])))
}

key3$rivdist <- NA
for(i in 1:nrow(key3)){
  key3$rivdist[i] <- riverdistance(startseg=sites$seg[key3$fromid[i]], 
                                   startvert=sites$vert[key3$fromid[i]], 
                                   endseg=sites$seg[key3$toid[i]], 
                                   endvert=sites$vert[key3$toid[i]], rivers=MyR3, map=F)
}

key3$flowconn <- NA
for(i in 1:nrow(key3)){
  key3$flowconn[i] <- isflowconnected(seg1=sites$seg[key3$fromid[i]], 
                                      seg2=sites$seg[key3$toid[i]], rivers=MyR3)
}



table(key3$over_frag)
table(conn_mat)

ggplot(data=key3, aes(y=rivdist, x=pythdist, color=over_frag))+
  geom_point()+
  geom_abline(slope=1, intercept=0)+
  theme_classic()

#key4<-key3


### Ta bort dupletter pÃ¥ nÃ¥got vÃ¤nster
# https://stackoverflow.com/questions/25297812/pair-wise-duplicate-removal-from-dataframe
sort_key3 <- key3[,1:2]
for (i in 1:nrow(key3)){
  sort_key3[i, ] = sort(key3[i,1:2])
}

#duplicated(sort_key3)

test <- key3[!duplicated(sort_key3),]

test2 <- test[-which(test$pythdist==0),]

#####
### Bryt ut XKOORLOK och YKOORLOK

test2$fromXKOORLOK <- as.integer(substr(test2$fromXY, 0, 6))
test2$fromYKOORLOK <- as.integer(substr(test2$fromXY, 8, 13))
test2$toXKOORLOK  <- as.integer(substr(test2$toXY, 0, 6))
test2$toYKOORLOK  <- as.integer(substr(test2$toXY, 8, 13))


### Plotta lite

ggplot(data=test2, aes(y=rivdist, x=pythdist, color=over_frag))+
  geom_point()+
  geom_abline(slope=1, intercept=0)+
  theme_classic()

ggplot(data=test2, aes(y=rivdist, x=pythdist, color=flowconn))+
  geom_point()+
  geom_abline(slope=1, intercept=0)+
  theme_classic()

ggplot(data=test2, aes(y=rivdist, x=over_frag))+
  geom_boxplot()+
  theme_classic()


### Megakey, nu kÃ¶r vi SERS hÃ¤rnÃ¤st

megakey <- test2

#write.table(sites[,c(17:18,27)], file="Em1_key.csv", sep=";", dec=",", row.names = F)

#rawdf <- read.csv("Em1_rawdata.csv", sep=";", dec=",", header = T, encoding = "UTF-8")
#rawdf$Em1id

rawdf <- read.csv("../rawdata_20220707.csv", sep=";", dec=",", header = T, encoding = "UTF-8")
rawdf$XKOORLOK1 <- rawdf$XKOORLOK
rawdf$YKOORLOK1 <- rawdf$YKOORLOK
rawdf$XKOORLOK <- factor(rawdf$XKOORLOK)
rawdf$YKOORLOK <- factor(rawdf$YKOORLOK)

rawdf2 <- right_join(rawdf, sites, by = c("XKOORLOK", "YKOORLOK"))


### Correlation function ...



#megakey$rho <- NA
#megakey$n_years <- NA

# for(i in 1:nrow(megakey)){
#   lokal_x2 <- c(as.character(megakey[i,1]), as.character(megakey[i,2]))
#   df_sub <- rawdf2 %>% dplyr::filter(id == lokal_x2[1] | id == lokal_x2[2])
#   df_sub2 <- df_sub %>% dplyr::select(c(year_mean, ÖringTOT_mean, id)) %>% spread(id, ÖringTOT_mean)
#   df_sub3 <- df_sub2[complete.cases(df_sub2),]
#   if(nrow(df_sub3)>3){
#     ct <- cor.test(df_sub3[,2], df_sub3[,3], method="spearman")
#     megakey$rho[i] <- ct$estimate
#     megakey$n_years[i] <- nrow(df_sub3)
#   } else {
#     megakey$rho[i] <- NA
#   }
# }


megakey$n_years <- NA

for(i in 1:nrow(megakey)){
  lokal_x2 <- c(as.character(megakey[i,1]), as.character(megakey[i,2]))
  df_sub <- rawdf2 %>% dplyr::filter(id == lokal_x2[1] | id == lokal_x2[2])
  df_sub2 <- df_sub %>% dplyr::select(c(year_mean, ÖringTOT_mean, id)) %>% spread(id, ÖringTOT_mean)
  df_sub3 <- df_sub2[complete.cases(df_sub2),]
  
  df_sub4 <- df_sub %>% dplyr::select(c(year_mean, Elrit_mean, id)) %>% spread(id, Elrit_mean)
  df_sub5 <- df_sub4[complete.cases(df_sub4),]
  
  df_sub6 <- df_sub %>% dplyr::select(c(year_mean, Mört_mean, id)) %>% spread(id, Mört_mean)
  df_sub7 <- df_sub6[complete.cases(df_sub6),]
  
  df_sub8 <- df_sub %>% dplyr::select(c(year_mean, Abbor_mean, id)) %>% spread(id, Abbor_mean)
  df_sub9 <- df_sub8[complete.cases(df_sub8),]
  
  df_sub10 <- df_sub %>% dplyr::select(c(year_mean, Gädda_mean, id)) %>% spread(id, Gädda_mean)
  df_sub11 <- df_sub10[complete.cases(df_sub10),]
  
  df_sub12 <- df_sub %>% dplyr::select(c(year_mean, LaxTOT_mean, id)) %>% spread(id, LaxTOT_mean)
  df_sub13 <- df_sub12[complete.cases(df_sub12),]
  
  if(nrow(df_sub3)>3){
    ct <- cor.test(df_sub3[,2], df_sub3[,3], method="spearman")
    megakey$n_years[i] <- nrow(df_sub3)
    megakey$rho_trout[i] <- ct$estimate
  } else {
    megakey$rho_trout[i] <- NA
  }
  
  if(nrow(df_sub5)>3){
    ct2 <- cor.test(df_sub5[,2], df_sub5[,3], method="spearman")
    megakey$rho_minnow[i] <- ct2$estimate
  } else {
    megakey$rho_minnow[i] <- NA
  }
  
  if(nrow(df_sub7)>3){
    ct3 <- cor.test(df_sub7[,2], df_sub7[,3], method="spearman")
    megakey$rho_roach[i] <- ct3$estimate
  } else {
    megakey$rho_roach[i] <- NA
  }
  
  if(nrow(df_sub9)>3){
    ct4 <- cor.test(df_sub9[,2], df_sub9[,3], method="spearman")
    megakey$rho_perch[i] <- ct4$estimate
  } else {
    megakey$rho_perch[i] <- NA
  }
  
  if(nrow(df_sub11)>3){
    ct5 <- cor.test(df_sub11[,2], df_sub11[,3], method="spearman")
    megakey$rho_pike[i] <- ct5$estimate
  } else {
    megakey$rho_pike[i] <- NA
  }
  
  if(nrow(df_sub13)>3){
    ct6 <- cor.test(df_sub13[,2], df_sub13[,3], method="spearman")
    megakey$rho_salmon[i] <- ct6$estimate
  } else {
    megakey$rho_salmon[i] <- NA
  }
  
}





head(megakey)

#### Add vtyp to key!! 

vtyp_key <- rawdf %>% group_by(XKOORLOK1, YKOORLOK1) %>% summarise(vtyp=first(vtyp_first))

megakey <- megakey %>% dplyr::left_join(vtyp_key, by=c("fromXKOORLOK" = "XKOORLOK1", "fromYKOORLOK" = "YKOORLOK1"))
megakey <- megakey %>% dplyr::left_join(vtyp_key, by=c("toXKOORLOK" = "XKOORLOK1", "toYKOORLOK" = "YKOORLOK1"))
names(megakey)[which(names(megakey) == "vtyp.x")] <- "vtyp_from"
names(megakey)[which(names(megakey) == "vtyp.y")] <- "vtyp_to"

megakey$vtyp_same <- 0

for(i in 1:nrow(megakey)){
  if(megakey$vtyp_from[i] == megakey$vtyp_to[i]){
    megakey$vtyp_same[i] <- 1
  }
}


head(megakey)

#### Add some stuff
megakey$flowconn01 <- 0
megakey$flowconn01[megakey$flowconn] <- 1

megakey$over_frag01 <- 0
megakey$over_frag01[megakey$over_frag] <- 1

megakey$rivdistlg <- log(megakey$rivdist)

megakey$river_id <- "74000"



# megakey_trout <- megakey[complete.cases(megakey$rho_trout),]
# megakey_minnow <- megakey[complete.cases(megakey$rho_minnow),]
# megakey_roach <- megakey[complete.cases(megakey$rho_roach),]
# megakey_perch <- megakey[complete.cases(megakey$rho_perch),]
# megakey_pike <- megakey[complete.cases(megakey$rho_pike),]
# megakey_salmon <- megakey[complete.cases(megakey$rho_salmon),]


### Test plotting 

megakey %>% filter(n_years > 5) %>% ggplot(aes(y=rho_trout, x=rivdist, color=over_frag))+
  geom_point()+
  #scale_x_continuous(trans="log10")+
  geom_smooth(method="lm")+
  theme_classic()

megakey %>% filter(n_years > 3) %>% ggplot(aes(y=rho_minnow, x=rivdist, color=over_frag))+
  geom_point()+
  #scale_x_continuous(trans="log10")+
  geom_smooth(method="lm")+
  theme_classic()

megakey %>% filter(n_years > 3) %>% ggplot(aes(y=rho_roach, x=rivdist, color=over_frag))+
  geom_point()+
  #scale_x_continuous(trans="log10")+
  geom_smooth(method="lm")+
  theme_classic()

megakey %>% filter(n_years > 3) %>% ggplot(aes(y=rho_perch, x=rivdist, color=over_frag))+
  geom_point()+
  #scale_x_continuous(trans="log10")+
  geom_smooth(method="lm")+
  theme_classic()

megakey %>% filter(n_years > 3) %>% ggplot(aes(y=rho_pike, x=rivdist, color=over_frag))+
  geom_point()+
  #scale_x_continuous(trans="log10")+
  geom_smooth(method="lm")+
  theme_classic()

megakey %>% filter(n_years > 3) %>% ggplot(aes(y=rho_salmon, x=rivdist, color=over_frag))+
  geom_point()+
  #scale_x_continuous(trans="log10")+
  geom_smooth(method="lm")+
  theme_classic()



### Test models plotting 

summary(m1 <- lm(data=megakey, rho_trout~rivdist*over_frag01))
summary(m2 <- lm(data=megakey, rho_minnow~rivdist*over_frag01))
summary(m3 <- lm(data=megakey, rho_roach~rivdist*over_frag01))
summary(m4 <- lm(data=megakey, rho_perch~rivdist*over_frag01))
summary(m5 <- lm(data=megakey, rho_pike~rivdist*over_frag01))
summary(m6 <- lm(data=megakey, rho_salmon~rivdist*over_frag01))

plot_model(m1, type="int")
plot_model(m2, type="int")
plot_model(m3, type="int")
plot_model(m4, type="int")
plot_model(m5, type="int")
plot_model(m6, type="int")



summary(m1 <- lm(data=megakey, rho~rivdist))
summary(m1 <- lm(data=megakey, rho~rivdist*flowconn01*over_frag01, weights = n_years))

plot_model(m1, type="pred", terms=c("rivdist [15000,40000,65000]", "over_frag01", "flowconn01"))

VIF(m1)
###
# Save Megakey here for consolidation of many rivers later?


write.table(megakey, file=paste0("C:/jobb/disty/out_files/megakeys/mega_key_74000_",Sys.Date(),".csv"))


head(megakey)

#Add fragmentID based on the "fromid"

fragment_table$sitefactor <- factor(fragment_table$site)

megakey3 <- left_join(megakey, fragment_table, by = c("fromid" = "sitefactor"))

portfolio <- megakey3 %>% filter(over_frag == F) %>% group_by(fragmentID) %>% 
  summarise(mean_rho_trout = mean(rho_trout, na.rm=T),
            mean_rho_minnow = mean(rho_minnow, na.rm=T),
            mean_rho_roach = mean(rho_roach, na.rm=T),
            mean_rho_perch = mean(rho_perch, na.rm=T),
            mean_rho_pike = mean(rho_pike, na.rm=T),
            mean_rho_salmon = mean(rho_salmon, na.rm=T), 
            fragment_size_max=max(rivdist), 
            fragment_size_mean=mean(rivdist),
            n_pairs=n(),
            river="74000")

ggplot(data=portfolio, aes(x=fragment_size_max, y=mean_rho_trout, size=n_pairs))+
  geom_point()

ggplot(data=portfolio, aes(x=fragment_size_max, y=mean_rho_minnow, size=n_pairs))+
  geom_point()

ggplot(data=portfolio, aes(x=fragment_size_max, y=mean_rho_roach, size=n_pairs))+
  geom_point()

ggplot(data=portfolio, aes(x=fragment_size_max, y=mean_rho_perch, size=n_pairs))+
  geom_point()

ggplot(data=portfolio, aes(x=fragment_size_max, y=mean_rho_pike, size=n_pairs))+
  geom_point()

ggplot(data=portfolio, aes(x=fragment_size_max, y=mean_rho_salmon, size=n_pairs))+
  geom_point()


cor.test(portfolio$mean_rho_trout, portfolio$fragment_size_mean)
summary(lm(data=portfolio, mean_rho_trout~fragment_size_mean+fragment_size_mean))

write.table(portfolio, file="C:/jobb/disty/out_files/74000.csv", row.names = F, sep=";", dec=".")

##### What about persistence?

step1 <- rawdf2 %>% select(id, XKOORLOK, YKOORLOK, ÖRKLASS_mean,
                           ELKLASS_mean,
                           MÖKLASS_mean,
                           ABKLASS_mean,
                           GÄKLASS_mean,
                           LXKLASS_mean)
step2 <- step1 %>% left_join(fragment_table, by = c("id" = "sitefactor"))
step3 <- step2 %>% group_by(fragmentID) %>% summarise(mean_trout_occ = mean(ÖRKLASS_mean),
                                                      mean_minnow_occ = mean(ELKLASS_mean),
                                                      mean_roach_occ = mean(MÖKLASS_mean),
                                                      mean_perch_occ = mean(ABKLASS_mean),
                                                      mean_pike_occ = mean(GÄKLASS_mean),
                                                      mean_salmon_occ = mean(LXKLASS_mean))

portfolio2 <- portfolio %>% inner_join(step3, by = c("fragmentID"))

write.table(portfolio2, file="C:/jobb/disty/out_files/occ/74000.csv", row.names = F, sep=";", dec=".")

##### Portfolio analysis

list.files("C:/jobb/disty/out_files/")

read.csv("C:/jobb/disty/out_files/AlsterÃ¥n.csv", sep=";", dec=".")

outfiles <- Sys.glob("C:/jobb/disty/out_files/occ/*.csv")

consolidate_portfolio <- read.csv(outfiles[1], sep=";", dec=".")
for(i in 2:length(outfiles)){
  consolidate_portfolio <- rbind(consolidate_portfolio, read.csv(outfiles[i], sep=";", dec="."))
}

ggplot(data=consolidate_portfolio, aes(x=fragment_size_max, y=mean_rho_trout, color=factor(river)))+
  geom_point(aes(size=n_pairs))
#geom_smooth(method="lm", se=F)

ggplot(data=consolidate_portfolio, aes(x=sqrt(fragment_size_max), y=mean_rho_trout, color=factor(river)))+
  geom_point(aes(size=n_pairs))+
  geom_smooth(method="lm", se=F)

summary(lmer(data=consolidate_portfolio, mean_rho_minnow ~ sqrt(fragment_size_max) +(1|river)))

summary(lm(data=consolidate_portfolio, mean_rho_minnow ~ fragment_size_max + I(fragment_size_max^2)))


## 

consolidate_portfolio %>% ggplot(aes(y=mean_rho_trout, x=mean_trout_occ, color=factor(river)))+
  geom_point()+
  geom_smooth(method="lm", se=F)

summary(lmer(data=consolidate_portfolio, mean_trout_occ~log(fragment_size_max)+mean_rho_trout+(1|river)))


##### Mega keys analysis

outfiles <- Sys.glob("C:/jobb/disty/out_files/megakeys/*")

consolidate_megakeys <- read.csv(outfiles[1], sep=" ", dec=".")
for(i in 2:length(outfiles)){
  consolidate_megakeys <- rbind(consolidate_megakeys, read.csv(outfiles[i], sep=" ", dec="."))
}

consolidate_megakeys %>% filter(n_years > 10) %>% ggplot(aes(x=sqrt(rivdist), y=rho_trout, color=over_frag))+
  geom_point()+
  geom_smooth(method="lm", se=F)

hmm <- consolidate_megakeys %>% dplyr::filter(vtyp_same == 1)

summary(m3 <- lmer(data=consolidate_megakeys, rho_trout~log(rivdist)*flowconn01*over_frag01+(1|river_id)))
summary(m4 <- lmer(data=hmm, rho_trout~rivdist*flowconn01*over_frag01+(1|river_id)))
summary(m5 <- lmer(data=consolidate_megakeys, rho_trout~sqrt(rivdist)*flowconn01*over_frag01+(1|river_id)))

plot_model(m3, type="pred", terms=c("rivdist [15000,40000,65000]", "over_frag01", "flowconn01"))
plot_model(m3, type="pred", terms=c("rivdist", "over_frag01", "flowconn01"))

plot_model(m4, type="pred", terms=c("rivdist", "over_frag01", "flowconn01"))

plot_model(m5, type="pred", terms=c("rivdist", "over_frag01", "flowconn01"))

summary(lm(data=consolidate_megakeys, mean_rho ~ fragment_size + n_pairs + river))


### TESTAR

consolidate_megakeys$dedw <- consolidate_megakeys$pythdist/consolidate_megakeys$rivdist

consolidate_megakeys$dedw[consolidate_megakeys$dedw > 1] <- 1

hist(consolidate_megakeys$dedw)

consolidate_megakeys %>% filter(!is.na(rho_trout) == T) %>% ggplot(aes(x=pythdist, y=dedw, color=rho_trout))+
  geom_point()+
  scale_y_reverse(limits=c(1,0))


lul <- consolidate_megakeys[complete.cases(consolidate_megakeys$rho_trout),]

wat <- interp(x=sqrt(lul$pythdist), y=lul$dedw, z=lul$rho_trout)

plot(wat)

persp(wat, theta=120)

plot(data=consolidate_megakeys, dedw~pythdist)

#SCRATCH THAT, make boxplots instead. 

consolidate_megakeys$cat <- "waat"

consolidate_megakeys$cat[which(consolidate_megakeys$pythdist <= 19001 & consolidate_megakeys$dedw > 0.5)] <- "D1"
consolidate_megakeys$cat[which(consolidate_megakeys$pythdist > 19001 & consolidate_megakeys$dedw > 0.5)] <- "D2"
consolidate_megakeys$cat[which(consolidate_megakeys$pythdist <= 19001 & consolidate_megakeys$dedw <= 0.5)] <- "D3"
consolidate_megakeys$cat[which(consolidate_megakeys$pythdist > 19001 & consolidate_megakeys$dedw <= 0.5)] <- "D4"

table(consolidate_megakeys$cat)

consolidate_megakeys %>% filter(n_years > 6) %>% filter(vtyp_same == 1) %>% filter(over_frag == F) %>% 
  ggplot(aes(x=cat, y=rho_trout))+
  geom_boxplot()


### Test!!!! Standardisering av längd per vattendrag

hmm2 <- consolidate_megakeys %>% group_by(river_id,over_frag01) %>% mutate(rivdist_norm = (rivdist-mean(rivdist))/sd(rivdist))

summary(m6 <- lmer(data=hmm2, rho_trout~rivdist_norm*flowconn01*over_frag01+vtyp_same+(1|river_id)))

plot_model(m6, type="pred", terms=c("rivdist_norm", "over_frag01", "flowconn01"))


hist(hmm2$rivdist_norm)
hist(hmm2$rivdist)


###### Functions 


is_over_frag_diagnostics<-function(fseg, fvert, tseg, tvert){
  
  from_seg <- sites$seg[fseg]
  from_vert <- sites$vert[fvert]
  to_seg <- sites$seg[tseg]
  to_vert <- sites$vert[tvert]
  
  site_to_site_route <- detectroute(from_seg, to_seg, MyR3)
  dam_seg_vector <- dams$seg
  dam_vert_vector <- dams$vert
  dire <- riverdirection(startseg=from_seg, startvert=from_vert, 
                         endseg=to_seg, endvert=to_vert, rivers=MyR3, flowconnected = T)
  
  if(is.na(dire)){
    dire <- "forking"
  }
  
  over_frag_within <- NA
  over_frag_to <- NA
  over_frag_from <- NA
  over_frag_any <- NA
  
  if(from_seg == to_seg){
    if(TRUE %in% (from_seg %in% dam_seg_vector)){
      over_frag_within <- TRUE %in% (dam_vert_vector[which(dam_seg_vector == from_seg)] %in% from_vert:to_vert)
      over_frag_any <- FALSE
    } else {
      over_frag_within <- FALSE
    }
  } else {
    if(TRUE %in% (dam_seg_vector %in% site_to_site_route)){
      #FÃ¶r frÃ¥npunkten
      if(site_to_site_route[1] %in% dam_seg_vector){
        if(dire != "up"){
          over_frag_from <- TRUE %in% (from_vert > dam_vert_vector[which(dam_seg_vector == site_to_site_route[1])])
        } else if (dire == "up") {
          over_frag_from <- TRUE %in% (from_vert < dam_vert_vector[which(dam_seg_vector == site_to_site_route[1])])
        } else {
          over_frag_from <- FALSE  
        }
      } else {
        over_frag_from <- FALSE
      }
      
      
      #FÃ¶r tillpunkten
      #If denna Ã¤r true
      if(site_to_site_route[length(site_to_site_route)] %in% dam_seg_vector){
        if(dire == "down"){
          over_frag_to <- to_vert < dam_vert_vector[which(dam_seg_vector == site_to_site_route[length(site_to_site_route)])]
        } else {
          over_frag_to <- to_vert > dam_vert_vector[which(dam_seg_vector == site_to_site_route[length(site_to_site_route)])]
        }
      } else {
        over_frag_to <- FALSE
      }
    } else {
      over_frag_to <- FALSE
    }
  }
  
  if(length(site_to_site_route)>2){
    #over_frag_any <- TRUE %in% (dam_seg_vector %in% site_to_site_route)
    over_frag_any <- TRUE %in% (dam_seg_vector %in% site_to_site_route[2:(length(site_to_site_route)-1)])
  }
  
  
  #riverdistance(startseg=from_seg, startvert=from_vert, endseg=to_seg, endvert=to_vert, rivers=MyR3, map=TRUE)
  #riverpoints(seg=Em1_dams$seg, vert=Em1_dams$vert, rivers=MyR3)
  return(list("over_frag_any"=over_frag_any,
              "over_frag_within"=over_frag_within, 
              "over_frag_to"=over_frag_to, 
              "over_frag_from"=over_frag_from))
  
  
}


is_over_frag2<-function(fseg, fvert, tseg, tvert){
  
  from_seg <- sites$seg[fseg]
  from_vert <- sites$vert[fvert]
  to_seg <- sites$seg[tseg]
  to_vert <- sites$vert[tvert]
  
  site_to_site_route <- detectroute(from_seg, to_seg, MyR3)
  dam_seg_vector <- dams$seg
  dam_vert_vector <- dams$vert
  dire <- riverdirection(startseg=from_seg, startvert=from_vert, 
                         endseg=to_seg, endvert=to_vert, rivers=MyR3, flowconnected = T)
  
  if(is.na(dire)){
    dire <- "forking"
  }
  
  over_frag_within <- NA
  over_frag_to <- NA
  over_frag_from <- NA
  over_frag_any <- NA
  
  if(from_seg == to_seg){
    if(TRUE %in% (from_seg %in% dam_seg_vector)){
      over_frag_within <- TRUE %in% (dam_vert_vector[which(dam_seg_vector == from_seg)] %in% from_vert:to_vert)
      over_frag_any <- FALSE
    } else {
      over_frag_within <- FALSE
    }
  } else {
    if(TRUE %in% (dam_seg_vector %in% site_to_site_route)){
      #FÃ¶r frÃ¥npunkten
      if(site_to_site_route[1] %in% dam_seg_vector){
        if(dire == "down"){
          over_frag_from <- TRUE %in% (from_vert > dam_vert_vector[which(dam_seg_vector == site_to_site_route[1])])
        } else if (dire == "up") {
          over_frag_from <- TRUE %in% (from_vert < dam_vert_vector[which(dam_seg_vector == site_to_site_route[1])])
        } else {
          over_frag_from <- FALSE  
        }
      } else {
        over_frag_from <- FALSE
      }
      
      
      #FÃ¶r tillpunkten
      #If denna Ã¤r true
      if(site_to_site_route[length(site_to_site_route)] %in% dam_seg_vector){
        if(dire == "down"){
          over_frag_to <- to_vert < dam_vert_vector[which(dam_seg_vector == site_to_site_route[length(site_to_site_route)])]
        } else {
          over_frag_to <- to_vert > dam_vert_vector[which(dam_seg_vector == site_to_site_route[length(site_to_site_route)])]
        }
      } else {
        over_frag_to <- FALSE
      }
    } else {
      over_frag_to <- FALSE
    }
  }
  
  if(length(site_to_site_route)>2){
    #over_frag_any <- TRUE %in% (dam_seg_vector %in% site_to_site_route)
    over_frag_any <- TRUE %in% (dam_seg_vector %in% site_to_site_route[2:(length(site_to_site_route)-1)])
  }
  
  
  #riverdistance(startseg=from_seg, startvert=from_vert, endseg=to_seg, endvert=to_vert, rivers=MyR3, map=TRUE)
  #riverpoints(seg=Em1_dams$seg, vert=Em1_dams$vert, rivers=MyR3)
  return(TRUE %in% c(over_frag_any, over_frag_within, over_frag_to, over_frag_from))
  
  
}


is_over_frag3<-function(fseg, fvert, tseg, tvert){
  
  from_seg <- sites$seg[fseg]
  from_vert <- sites$vert[fvert]
  to_seg <- sites$seg[tseg]
  to_vert <- sites$vert[tvert]
  
  site_to_site_route <- detectroute(from_seg, to_seg, MyR3)
  dam_seg_vector <- dams$seg
  dam_vert_vector <- dams$vert
  dire <- riverdirection(startseg=from_seg, startvert=from_vert, 
                         endseg=to_seg, endvert=to_vert, rivers=MyR3, flowconnected = T)
  
  if(is.na(dire)){
    dire <- "forking"
  }
  
  over_frag_within <- NA
  over_frag_to <- NA
  over_frag_from <- NA
  over_frag_any <- NA
  
  if(from_seg == to_seg){
    if(TRUE %in% (from_seg %in% dam_seg_vector)){
      over_frag_within <- TRUE %in% (dam_vert_vector[which(dam_seg_vector == from_seg)] %in% from_vert:to_vert)
      over_frag_any <- FALSE
    } else {
      over_frag_within <- FALSE
    }
  } else {
    if(TRUE %in% (dam_seg_vector %in% site_to_site_route)){
      #FÃ¶r frÃ¥npunkten
      if(site_to_site_route[1] %in% dam_seg_vector){
        if(dire != "up"){
          over_frag_from <- TRUE %in% (from_vert > dam_vert_vector[which(dam_seg_vector == site_to_site_route[1])])
        } else if (dire == "up") {
          over_frag_from <- TRUE %in% (from_vert < dam_vert_vector[which(dam_seg_vector == site_to_site_route[1])])
        } else {
          over_frag_from <- FALSE  
        }
      } else {
        over_frag_from <- FALSE
      }
      
      
      #FÃ¶r tillpunkten
      #If denna Ã¤r true
      if(site_to_site_route[length(site_to_site_route)] %in% dam_seg_vector){
        if(dire == "down"){
          over_frag_to <- to_vert < dam_vert_vector[which(dam_seg_vector == site_to_site_route[length(site_to_site_route)])]
        } else {
          over_frag_to <- to_vert > dam_vert_vector[which(dam_seg_vector == site_to_site_route[length(site_to_site_route)])]
        }
      } else {
        over_frag_to <- FALSE
      }
    } else {
      over_frag_to <- FALSE
    }
  }
  
  if(length(site_to_site_route)>2){
    #over_frag_any <- TRUE %in% (dam_seg_vector %in% site_to_site_route)
    over_frag_any <- TRUE %in% (dam_seg_vector %in% site_to_site_route[2:(length(site_to_site_route)-1)])
  }
  
  
  #riverdistance(startseg=from_seg, startvert=from_vert, endseg=to_seg, endvert=to_vert, rivers=MyR3, map=TRUE)
  #riverpoints(seg=Em1_dams$seg, vert=Em1_dams$vert, rivers=MyR3)
  return(TRUE %in% c(over_frag_any, over_frag_within, over_frag_to, over_frag_from))
  
  
}

