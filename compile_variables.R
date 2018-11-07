# comid treated as character

library(foreign) # read.dbf
library(tibble) # add_column


setwd("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis")

# --------- load variables

## load CI mean and max; convert COMID to character; specify columns as "mean" or "max"; remove non-variable columns
ci <- read.csv("CI/CI_out/pctile90_CI.csv", stringsAsFactors = FALSE)
ci$comid <- as.character(ci$comid)
ci$MAX <- apply(ci[, 4:18], 1, max, na.rm = TRUE)
ci <- ci[c("comid", "MEDIAN", "MAX")]
colnames(ci)[2:3] <- c("CI_sp90th_tmedian", "CI_sp90th_tmax")

## lake metrics - load; remove non-variable columns ----------------------------
lake_metrics <- read.csv("lake_metrics/lake_metrics_out_2018-05-22.csv", stringsAsFactors = FALSE)
lake_metrics <- lake_metrics[, -which(colnames(lake_metrics) == "X")]
colnames(lake_metrics)[which(colnames(lake_metrics) == "COMID")] <- "comid"
lake_metrics$comid <- as.character(lake_metrics$comid)
colnames(lake_metrics)[which(colnames(lake_metrics) == "x")] <- "longitude"
colnames(lake_metrics)[which(colnames(lake_metrics) == "y")] <- "latitude"


## air temp ----------------------------
air_temp <- read.csv("Temperature_air/lake_temp_2018-05-23.csv", stringsAsFactors = FALSE)
air_temp <- air_temp[, -which(colnames(air_temp) == "X")]


## precip - load; reformat names; remove non-variable columns ----------------------------
precip <- read.csv("Precip/outputs/ls_precip_2018-05-03.csv", stringsAsFactors = FALSE)
precip <- precip[c("comid", "precip_season_spMean", "precip_max72hr_spMean")]
precip$comid <- as.character(precip$comid)


## nutrients - load; remove non-variable columns ----------------------------
nutrients <- read.csv("Nutrients/nutrients_out_2018-05-01.csv", stringsAsFactors = FALSE)
nutrients <- nutrients[, -which(colnames(nutrients) %in% c("X"))]
nutrients$comid <- as.character(nutrients$comid)


## lakecat --------------------------------------------------------
lakecat_file <- read.csv("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis/LakeCat/Iiames_lakecat_data_397lakes_SALLS.csv", stringsAsFactors = FALSE)
lk_key <- lakecat_file[1:2, ]
lakecat_all <- lakecat_file[-1, ]
lakecat <- lakecat_all[, which(lk_key[1, ] == 1)]

# add combined classes
lakecat <- add_column(lakecat, PctUrbLoMd2011Ws_SUM = lakecat$PctUrbLo2011Ws + lakecat$PctUrbMd2011Ws, .after = "PctUrbMd2011Ws")
lakecat <- add_column(lakecat, PctForest2011Ws_SUM = lakecat$PctDecid2011Ws + lakecat$PctConif2011Ws + lakecat$PctMxFst2011Ws, 
                      .after = "PctMxFst2011Ws")
lakecat <- add_column(lakecat, Wetland2011Ws_SUM = lakecat$PctWdWet2011Ws + lakecat$PctHbWet2011Ws, .after = "PctHbWet2011Ws")

# remove columns
rm_lakecat <- c("PctUrbLo2011Ws", "PctUrbMd2011Ws", "PctMxFst2011Ws", "PctWdWet2011Ws", "PctHbWet2011Ws")
lakecat <- lakecat[, -which(colnames(lakecat) %in% rm_lakecat)]

# change COMID colname and to numeric to match with others
colnames(lakecat)[colnames(lakecat) == "COMID"] <- "comid"
lakecat$comid <- as.character(lakecat$comid)

# check totals
lakecat_totals <- as.numeric(rowSums(lakecat[, -which(colnames(lakecat) %in% c("PctDecid2011Ws", "PctConif2011Ws"))][, 16:26]))
max(abs(lakecat_totals - 100))
#sum(as.numeric(rowSums(lakecat[, -which(colnames(lakecat) %in% c("PctDecid2011Ws", "PctConif2011Ws"))][, 16:26])) != 100) # weird?!?!?!

## hydrologic network buffer NLCD - 90 m (doesn't include lake edges) ----------------------------
atilla <- read.csv("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis/buffer_NLCD/network_buffer/hydro_network_90mbuffer.csv", stringsAsFactors = FALSE)

# change column names
colnames(atilla) <- sub("Lakesheds_3For_RLCP_", "", colnames(atilla))

# add combined classes
atilla <- add_column(atilla, rdevlm90 = atilla$rdevl90 + atilla$rdevm90, .after = "rdevm90")
atilla <- add_column(atilla, rforTOT90 = atilla$rdfor90 + atilla$refor90 + atilla$rmfor90, .after = "rmfor90")

# remove columns
atilla_rm <- c("OBJECTID", "Shape_Length", "Shape_Area",
               "Lakesheds_RHillgenerated_AEANAD83_rmAdtlOverlaps_kmkm", "Lakesheds_RHillgenerated_AEANAD83_rmAdtlOverlaps_TempID", 
               "rNI90", "rtun90", "rwtlw90",	"rwtle90", "rmfor90", "rUI90", "rdev90", "rdevl90", "rdevm90", "ragr90")
atilla <- atilla[, -which(colnames(atilla) %in% atilla_rm)]

# reorder columns
atilla <- atilla[, c(1, 9:11, 2, 6:8, 3:4, 12:13, 5)]

# change COMID colname and to numeric to match with others
colnames(atilla)[which(colnames(atilla) == "Lakesheds_RHillgenerated_AEANAD83_rmAdtlOverlaps_COMID")] <- "comid"
atilla$comid <- as.character(atilla$comid)

# NAs to 0
for (c in 1:ncol(atilla)) {
  print(paste0(sum(is.na(atilla[, c])), ": ", colnames(atilla)[c]))
  atilla[which(is.na(atilla[, c])), c] <- 0
}

# check totals
atilla_sums <- as.numeric(rowSums(atilla[, -which(colnames(atilla) %in% c("rdfor90", "refor90"))][, 2:11]))
max(abs(atilla_sums[atilla_sums !=0] - 100))

## lake buffer NLCD - 90 m ----------------------------
lake_edge <- read.csv("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis/buffer_NLCD/lake_buffer/lakeshedbuffer90m_LC2011forWilson.csv", stringsAsFactors = FALSE)

# change column names
names_old_lake_edge <- colnames(lake_edge)
colnames(lake_edge) <- c("comid", "lkedge_devo", "lkedge_devl", "lkedge_devm", "lkedge_devh", "lkedge_bar", "lkedge_for_dec", 
                         "lkedge_for_evgr", "lkedge_for_mix", "lkedge_shrub", "lkedge_herb", "lkedge_agr_past", 
                         "lkedge_agr_crop", "lkedge_wetl_wood", "lkedge_wetl_herb", "lkedge_Total")
data.frame(old = names_old_lake_edge, new = colnames(lake_edge)) # CHECK

# add combined classes
lake_edge <- add_column(lake_edge, lkedge_devlm = lake_edge$lkedge_devl + lake_edge$lkedge_devm, .after = "lkedge_devm")
lake_edge <- add_column(lake_edge, lkedge_for_TOT = lake_edge$lkedge_for_dec + lake_edge$lkedge_for_evgr + 
                          lake_edge$lkedge_for_mix, .after = "lkedge_for_mix")
lake_edge <- add_column(lake_edge, lkedge_wetl_TOT = lake_edge$lkedge_wetl_wood + lake_edge$lkedge_wetl_herb, .after = "lkedge_wetl_herb")

# remove columns
rm_lake_edge <- c("lkedge_devl", "lkedge_devm", "lkedge_for_mix", "lkedge_wetl_wood", "lkedge_wetl_herb", "lkedge_Total")
lake_edge <- lake_edge[, -which(colnames(lake_edge) %in% rm_lake_edge)]

# change COMID to numeric to match with others
lake_edge$comid <- as.character(lake_edge$comid)

# check totals
lake_edge_sums <- as.numeric(rowSums(lake_edge[, -which(colnames(lake_edge) %in% c("lkedge_for_dec", "lkedge_for_evgr"))][, 2:11]))
max(abs(lake_edge_sums - 100))


## all NLCD colnames - CHECK
data.frame(colnames(lakecat)[17:28], colnames(atilla)[2:13], colnames(lake_edge)[2:13])


## buffer variables from Jay Christensen ----------------------------
buff <- read.csv("Christensen/buffer_metrics_atmos_2018-04-26.csv", stringsAsFactors = FALSE)
buff$comid <- sub("ws_", "", sub("_noLake", "", buff$lakesheds.l.))
buff <- buff[c("comid", "pct_ag_untrt", "avg_d8Cbw", "pct_PDrain01", "pct_sinks_trtng")]

# avg_d8Cbw - replace NA with filler - use max value
sum(is.na(buff$avg_d8Cbw))
buff$avg_d8Cbw[is.na(buff$avg_d8Cbw)] <- max(buff$avg_d8Cbw, na.rm = TRUE)



## Omernik ecoregions ----------------------------
omernik <- read.dbf("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/geospatial_final/Lakesheds_RHillgenerated_AEANAD83_369Lakesheds_centroidpt_OmernikJoin.dbf")
#omer_shp <- readOGR("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/geospatial_final", "NA_CEC_Eco_Level2_GLB_AEA")
omernik$COMID <- as.character(omernik$COMID)
omernik <- omernik[, c(1:5)]
colnames(omernik) <- c("comid", "Ecoregion_L2_code", "Ecoregion_L2", "Ecoregion_L1_code", "Ecoregion_L1")

# combine similar classes
omernik$Ecoregion_L2_elev_lat <- NA
omernik$Ecoregion_L2_elev_lat[which(omernik$Ecoregion_L2_code %in% c("8.1", "8.2", "8.3", "9.2"))] <- "lowElev"
omernik$Ecoregion_L2_elev_lat[which(omernik$Ecoregion_L2_code %in% c("5.3", "8.4"))] <- "hiElev"
omernik$Ecoregion_L2_elev_lat[which(omernik$Ecoregion_L2_code %in% c("5.2"))] <- "hiLat"

omernik$Ecoregion_L2_elev <- NA
omernik$Ecoregion_L2_elev[which(omernik$Ecoregion_L2_code %in% c("5.2", "8.1", "8.2", "8.3", "9.2"))] <- "lowElev_hiLat"
omernik$Ecoregion_L2_elev[which(omernik$Ecoregion_L2_code %in% c("5.3", "8.4"))] <- "hiElev"

omernik$Ecoregion_L2_highelev_lat <- NA
omernik$Ecoregion_L2_highelev_lat[which(omernik$Ecoregion_L2_code %in% c("8.1", "8.2", "8.3", "9.2"))] <- "lowElev"
omernik$Ecoregion_L2_highelev_lat[which(omernik$Ecoregion_L2_code %in% c("5.3", "8.4", "5.2"))] <- "hiElevHiLat"


# ----- specify variables
vars <- list(ci, lake_metrics, air_temp, precip, nutrients, lakecat, atilla, lake_edge, buff, omernik)
varnames <- c("ci", "lake_metrics", "air_temp", "precip", "nutrients", "lakecat", "atilla", "lake_edge", "buff", "omernik")

# ----------- check to make sure all comids are in all files
for (i in 1:length(vars)) {
  comidi <- as.data.frame(vars[i])$comid
  print(paste("-------- ", varnames[i], ": length", length(comidi)))
  
  for (j in 1:length(vars)) {
    comidj <- as.data.frame(vars[j])$comid
    print(paste(varnames[j], ":", sum(comidj %in% comidi)))
  }
}



# -------------- join all

joined <- merge(ci, lake_metrics, by = "comid", all.x = TRUE)
joined <- merge(joined, air_temp, by = "comid", all.x = TRUE)
joined <- merge(joined, precip, by = "comid", all.x = TRUE)
joined <- merge(joined, nutrients, by = "comid", all.x = TRUE)
joined <- merge(joined, lakecat, by = "comid", all.x = TRUE)
joined <- merge(joined, atilla, by = "comid", all.x = TRUE)
joined <- merge(joined, lake_edge, by = "comid", all.x = TRUE)
joined <- merge(joined, buff, by = "comid", all.x = TRUE)
joined <- merge(joined, omernik, by = "comid", all.x = TRUE)

# switch all -Inf values to NA (occur from calculating max for CI)
for (c in 1:ncol(joined)) {
  if ("-Inf" %in% joined[, c]) {
    print(colnames(joined)[c])
  }
}
sum(joined == "-Inf", na.rm = T)

joined[joined == "-Inf"] <- NA

# remove NA row (had been introduced from buffer data for some reason - probably was 167122250 - OK)
#joined <- joined[-which(joined$comid==""), ]



# calculate new fields -----------------------------------

# calculate lakeshed/lake area ratio
joined <- add_column(joined, lakeshed_A_ratio = joined$WsAreaSqKm / joined$SurfaceAre, .after = "MeanDepthC")
#joined$lakeshed_A_ratio <- joined$WsAreaSqKm / joined$SurfaceAre


# convert response to WHO categories (cells/ml)
# closed on left, open on right (left inclusive; low break value is included in each class)
# highest value (max) is included in highest class
who <- data.frame(cellCount_right = c(0, 10230, 20000, 100000, 10000000,
                                      max(joined$CI_sp90th_tmax, na.rm = TRUE)*10^8))
who$CI_right <- who$cellCount_right/10^8

who_labels <- c("ND", "low", "med", "hi", "very hi")

joined$ci_sp90th_tmedian_WHO <- cut(joined$CI_sp90th_tmedian, breaks = who$CI_right, 
                            right = FALSE, include.lowest = TRUE, 
                            labels = who_labels)

joined$ci_sp90th_tmax_WHO <- cut(joined$CI_sp90th_tmax, breaks = who$CI_right, 
                         right = FALSE, include.lowest = TRUE, 
                         labels = who_labels)

# convert response to Ross categories (cells/ml)
ross <- data.frame(cellCount_right = c(0, 10230, 110000, 300000, 1000000, 
                                       max(joined$CI_sp90th_tmax, na.rm = TRUE)*10^8))
ross$CI_right <- ross$cellCount_right/10^8

ross_labels <- c("ND", "low", "med", "hi", "very hi")

joined$ci_sp90th_tmedian_Ross <- cut(joined$CI_sp90th_tmedian, breaks = ross$CI_right, 
                             right = FALSE, include.lowest = TRUE, 
                             labels = ross_labels)

joined$ci_sp90th_tmax_Ross <- cut(joined$CI_sp90th_tmax, breaks = ross$CI_right, 
                          right = FALSE, include.lowest = TRUE, 
                          labels = ross_labels)


# shave down lakesheds --------------------

# shave down to 389 non-overlapping lakesheds (from 398)
joined <- joined[which(!is.na(joined$SurfaceAre)), ]

# remove lakes/lakesheds
# remove lakes with lakesheds extending outside GLB (COMIDs 16237597, 10380929 (not present anyway), 120053324, 18438992, 6729343)
rm_lk <- c("16237597", "10380929", "120053324", "18438992", "6729343")

# remove lake alongside river that (probably erroneously) shows entire river watershed as lakeshed (- not all flow enters lake)
rm_lk <- c(rm_lk, "167122250")

# remove lakesheds that still overlap (missed by Megan M)
nonOver <- read.dbf("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Lakesheds/Hill_LakeCat/Lakesheds_RHillgenerated_AEANAD83_rmAdtlOverlaps.dbf")
overlapping <- lakecat$comid[!(lakecat$comid %in% as.character(nonOver$COMID))]
rm_lk <- c(rm_lk, overlapping)

# remove NA CI
na_ci <- joined$comid[which(is.na(joined$CI_sp90th_tmedian))]
rm_lk <- c(rm_lk, na_ci)

# remove!
joined <- joined[-which(joined$comid %in% rm_lk), ]


# calculate stats; NA and zero counts ------------------------


# for each variable:

stats_df <- data.frame()
for (c in 1:ncol(joined)) {
  
  # count number of NAs and zeros
  NAs <- sum(is.na(joined[, c]))
  zeros <- sum(joined[, c] == 0, na.rm = TRUE)
  
  # if numeric variable, calculate stats and correlation with median CI
  if (is.numeric(joined[, c])) {
    
    m_ci <- lm(joined$CI_sp90th_tmedian ~ joined[, c])
    
    min = min(joined[, c], na.rm = TRUE)
    max = max(joined[, c], na.rm = TRUE)
    mean = mean(joined[, c], na.rm = TRUE)
    median = median(joined[, c], na.rm = TRUE)
    sd = sd(joined[, c], na.rm = TRUE)
    R2_CI_median = summary(m_ci)$r.squared
    slope_CI_median = m_ci$coefficients[2]

    # if non-numeric, set those fields to NA
  } else {
    
    min = NA
    max = NA
    mean = NA
    median = NA
    sd = NA
    R2_CI_median = NA
    slope_CI_median = NA
    
  }
  
  # append counts and stats to stats_df
  stats_df <- rbind(stats_df, data.frame(var = as.character(colnames(joined)[c]),
                                         NAs,
                                         zeros,
                                         records = length(joined[, c]), 
                                         valid = length(joined[, c]) - sum(NAs, zeros),
                                         min,
                                         max,
                                         mean,
                                         median,
                                         sd,
                                         R2_CI_median,
                                         slope_CI_median))
}

rownames(stats_df) <- 1:nrow(stats_df) # since rownames came out funny
stats_df$var <- as.character(stats_df$var) # since came out as factor
stats_df$VariableIndex <- 1:nrow(stats_df) # to sort into the original order later


# remove variables with 0 or 1 valid records ---------------------
# removed: "L1_AON", "L1_AOP", "L2_ANO3", "L2_AON", "L2_AMP", "L2_AOP", "PctFire2010Ws", "rtun90" 

rm_vars <- c()

for (v in 1:nrow(stats_df)) {
  if (stats_df$valid[v] <= 1) {
    rm_vars <- c(rm_vars, as.character(stats_df$var[v]))
  }
}

# remove from join table and from stats_df
joined <- joined[, -which(colnames(joined) %in% rm_vars)]
stats_df <- stats_df[-which(stats_df$var %in% rm_vars), ]


# add label, description, units, source, resolution from file
details <- read.csv("variable_key.csv", stringsAsFactors = FALSE)
#new_vars <- colnames(joined)[which(!(colnames(joined) %in% details$Variable))]


stats_df <- merge(details, stats_df, by.x = "Variable", by.y = "var", all.x = FALSE, all.y = TRUE)

stats_df <- stats_df[order(stats_df$VariableIndex), ] # order based on original order in join table
rownames(stats_df) <- stats_df$VariableIndex # set row names to original row numbers from join table
stats_df <- stats_df[, -which(colnames(stats_df) == "VariableIndex")] # remove VariableIndex field (since now redundant to row names)

# export tables --------------------------

write.csv(joined, sprintf("GLB_LandscapeAnalysis_data_%s.csv", Sys.Date()))

write.csv(stats_df, sprintf("GLB_LandscapeAnalysis_variables_%s.csv", Sys.Date()))







# checks ----------------------------------------------------------------------------------------------------

# correlations
'
num_vars <- c()
for (c in 1:ncol(joined)) {
  if (is.numeric(joined[, c])) {
    num_vars <- c(num_vars, c)
  }
}
num_data <- joined[, num_vars]
cor_vars <- cor(num_data, use = "pairwise.complete.obs")

library(corrplot)
corrplot(cor_vars)
'


# presence of each lake and date
'
ucomids <- unique(joined$comid)
udates <- unique(joined$ci_date)

for (d in 1:length(udates)) {
  print(sum(ucomids %in% joined$comid[joined$ci_date == udates[d]]))
}


dates_per_lkshd <- data.frame()
for (c in 1:length(ucomids)) {
  nrows <- nrow(joined[joined$comid == ucomids[c], ])
  dates_per_lkshd <- rbind(dates_per_lkshd, data.frame(comid = ucomids[c], 
                                                       nrows_comid = nrows))
}
table(dates_per_lkshd$nrows_comid)
'
