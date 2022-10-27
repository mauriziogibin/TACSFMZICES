#-------------------------------------------------------------------------------
#
# Script to clean, analyse and prepare the TAC FMZ FAO file
# Date: 2022-10-19
#
#
#-------------------------------------------------------------------------------

library(data.table)
library(sf)
library(ggplot2)
library(units)

#- Clear workspace
rm(list=ls())
# cDir <- setwd("~Work=-/FDI2018/")
#cDir <- "/media/maurizio/BETA_Seagate/work/CFP"
cDir <- "D://TACSFMZICES//"
options(scipen = 999)
options(digits = 9)
#- Clear workspace
#- Settings paths
codePath         <- paste0(cDir,"/scripts/"   )      # R scripts location
dataF            <- paste0(cDir,"/data/"      )      # data folder
outPath          <- paste0(cDir,"/output/"    )       # output

#########
#Maksims#
#########
setwd(dataF)
# Loading the data
# fao <- st_read('./fao/fao_areas_sta.shp', stringsAsFactors = F)
# fdi_fao_gsa <- st_read('./fao/fdi_fao_areas_sta_gsa.shp', stringsAsFactors = F)
# #plot(fao[,'F_AREA'])
# 
# fao2737 <- fao[fao$F_CODE %like% '27*'|fao$F_CODE %like% '37*',]
# fao2737 <- fao[,c("F_CODE","areafao",'geometry')]
# #plot(fao2737[,'F_CODE'])
# fao2737 <- st_transform(fao2737,3035)
# fao2737$areafao <- st_area(fao2737)
# names(fao2737) <- tolower(names(fao2737))
# #fao2737$diff.area <- fao2737$area.fao-fao2737$areafao
# fao2737DT <- as.data.table(fao2737)
# fao2737DT$geometry <- NULL

# ICES Stocks
stocks_ices <- fread('./ices/STOCK ASSESSMENT GRAPHS 2022-09-19/StockAssessmentGraphs_2022919.csv')

stocks_ices_unique <- unique(stocks_ices,by='FishStock')
names(stocks_ices_unique)
names(stocks_ices_unique)[9] <- 'ICESAreas'
stocks_ices_unique <- stocks_ices_unique[,.(FishStock,ICESAreas)]
# SPlitting the ICES Areas
setorder(stocks_ices_unique,FishStock)
ICESAReasSplit <- cbind(stocks_ices_unique$FishStock,
  setDT(tstrsplit(as.character(stocks_ices_unique$ICESArea),
                "~", fixed=TRUE))[])
#ICESAReasSplit <- ICESAReasSplit[!is.na(ICESAReasSplit),]

stocks_ices_unique_long <- melt(ICESAReasSplit,
                                id.vars = 'V1',
                                values = names(ICESAReasSplit)[-1])

stocks_ices_unique_long <- stocks_ices_unique_long[!is.na(value)]
stocks_ices_unique_long$variable <- NULL
names(stocks_ices_unique_long) <- c('FishStock','ICESAreas')
stocks_ices_unique_long[,ICESAreas:=gsub(' ','',ICESAreas)]

icesareas <- st_read('./ices/ICES_areas/ICES_Areas_20160601_cut_dense_3857.shp')

stocks_icesareas <- merge(icesareas,stocks_ices_unique_long,
                   by.x='Area_Full',
                   by.y='ICESAreas'
                    ,
                    all.y=T)
#stocks_icesareas <- st_make_valid(stocks_icesareas)

stocks_icesareas$geometry <- NULL
stocks_icesareas$OBJECTID_1 <- NULL
stocks_icesareas$OBJECTID <- NULL

missing_stocks <- stocks_icesareas[is.na(stocks_icesareas$Area_km2),]
stocks_ok <- stocks_icesareas[!is.na(stocks_icesareas$Area_km2),]

missing_areas <- unique(missing_stocks$Area_Full)

# These are areas that have been splitted in other parts and so the join will not work.
# We need to link them to their constituting oarts/geometries in the icesarea file.
# Cannot do anything on 21.1 because there is no ICES Area defined. But for the others we do something
icesareas_missing_candidates <- 
(sapply(missing_areas,function(x){
  return(t(unique(icesareas$Area_Full[icesareas$Area_Full%like% unlist(x)])))
}))


ICESAReasCandidatesSplit <- data.table(names(icesareas_missing_candidates),
                        setDT(tstrsplit((icesareas_missing_candidates),
                                        " ", fixed=TRUE))[])
names(ICESAReasCandidatesSplit)[1] <- 'ICESArea'
ICESAReasCandidatesSplit[ICESArea=='21.1',] <- NA
ICESAReasCandidatesSplit <- ICESAReasCandidatesSplit[!is.na(ICESArea),]

fixareagarbage <- function(garbage){
    garbage <- as.character(garbage)
    garbage <- gsub('\\(',"",garbage)
    garbage <- gsub('\\)',"",garbage)
    garbage <- gsub('c',"",garbage)
    garbage <- gsub(',',"",garbage)
    garbage <- gsub('\\\"',"",garbage)
    garbage <- gsub(' ','',garbage)    #return(garbage)
}

ICESAReasCandidatesSplit[,V1:=fixareagarbage(V1)]
ICESAReasCandidatesSplit[,V2:=fixareagarbage(V2)]
ICESAReasCandidatesSplit[,V3:=fixareagarbage(V3)]

ICESAReasCandidatesLong <- melt(ICESAReasCandidatesSplit,
                                id.vars = 'ICESArea',
                                values = names(ICESAReasSplit)[-1])

ICESAReasCandidatesLong <- ICESAReasCandidatesLong[!is.na(value)]
ICESAReasCandidatesLong$variable <- NULL

missing_stocks <- missing_stocks[,c("Area_Full","FishStock")]
setnames(ICESAReasCandidatesLong,old='value',new='Area_Full_NEW')
fixed_missing_stocks <- merge(missing_stocks,ICESAReasCandidatesLong,
                        by.x='Area_Full',
                        by.y='ICESArea')
fixed_missing_stocks <- as.data.table(fixed_missing_stocks)
missing_stocks <- as.data.table(missing_stocks)

# FINAL MISSING Stocks
missing_stocks <- missing_stocks[!FishStock %in% unique(fixed_missing_stocks$FishStock),]

fixed_missing_stocks$Area_Full <- NULL
names(fixed_missing_stocks)[2] <- 'Area_Full'
stocks_ok <- stocks_ok[,c("FishStock","Area_Full")]
stocks_ices_finalDT <- rbind(stocks_ok,fixed_missing_stocks)

# Merging again with ICES Areas
stocks_ices_final <- merge(icesareas,stocks_ices_finalDT,
                          by.x='Area_Full',
                          by.y='Area_Full',
                          all.y=T)
stocks_ices_final$OBJECTID_1 <- NULL
stocks_ices_final$OBJECTID <- NULL
#stocks_ices_final <- st_make_valid(stocks_ices_final)

stocks_ices               <- NULL
stocks_ices_unique  <- NULL
stocks_ices_unique_long  <- NULL
stocks_icesareas  <- NULL
stocks_ok  <- NULL
ICESAReasCandidatesLong  <- NULL
ICESAReasCandidatesSplit  <- NULL
ICESAReasSplit  <- NULL
fixed_missing_stocks  <- NULL
icesareas_missing_candidates  <- NULL

stocks_ices_final$Stock_Area <- toupper(substring(stocks_ices_final$FishStock,5))
stocks_ices_final$Species_ICES <- toupper(substring(stocks_ices_final$FishStock,1,3))

stocks_areas_by_stock_ass_graphs_list <- stocks_ices_final
stocks_areas_by_stock_ass_graphs_list$FishStock <- NULL;


stocks_areas_by_stock_ass_graphs_list <- unique(as.data.table(`st_geometry<-`(stocks_areas_by_stock_ass_graphs_list,NULL)),
                                                          by=c('Area_Full','Stock_Area'))
                                            
stocks_areas_by_stock_ass_graphs_list <- merge(stocks_areas_by_stock_ass_graphs_list,
                                           icesareas[,c("Area_Full","geometry")])
# Loading the ices filtered stock list
ices_filtered_stock_list <- fread('../data/ices/STOCK ASSESSMENT GRAPHS 2022-09-19/ICES_FilteredStocklist.csv')
ices_filtered_stock_list[,Stock_Area:=toupper(substring(StockCode,5))]

missing_ices_filtered_stock_list <- 
  ices_filtered_stock_list[!Stock_Area %in%stocks_areas_by_stock_ass_graphs_list$Stock_Area,]

ices_ecoreg <- st_read('../data/ices/ICES_ecoregions/ICES_ecoregions_20171207_erase_ESRI.shp')

ices_filtered_stock_list_unique <- unique(ices_filtered_stock_list,by='StockCode')

names(stocks_ices_unique)
names(stocks_ices_unique)[9] <- 'ICESAreas'
ices_filtered_stock_list_unique <- ices_filtered_stock_list_unique[,.(StockCode,EcoRegion)]
# SPlitting the ICES Areas
setorder(ices_filtered_stock_list_unique,StockCode)
EcoregionsSplit <- cbind(ices_filtered_stock_list_unique$StockCode,
                        setDT(tstrsplit(as.character(ices_filtered_stock_list_unique$EcoRegion),
                                        ",", fixed=TRUE))[])

ices_filtered_stock_list_unique_long <- melt(EcoregionsSplit,
                                id.vars = 'V1',
                                values = names(EcoregionsSplit)[-1])

ices_filtered_stock_list_unique_long <- ices_filtered_stock_list_unique_long[!is.na(value)]
ices_filtered_stock_list_unique_long$variable <- NULL
names(ices_filtered_stock_list_unique_long) <- c('StockCode','EcoRegion')
ices_filtered_stock_list_unique_long[,EcoRegion:=gsub('Ecoregion','',EcoRegion)]

# Intersecting ICES Ecoregions with ICES Areas
st_crs(ices_ecoreg)
ices_ecoreg <- st_transform(ices_ecoreg,3035)
ices_ecoreg <- st_make_valid(ices_ecoreg)

EcoRegion_by_ICES_Areas <- st_intersection(ices_ecoreg,icesareas)

EcoRegion_by_ICES_Areas$Shape_Leng <- NULL
EcoRegion_by_ICES_Areas$Shape_Le_1 <- NULL
EcoRegion_by_ICES_Areas$Shape_Leng <- NULL
EcoRegion_by_ICES_Areas$Shape_Area <- NULL
EcoRegion_by_ICES_Areas$OBJECTID_1 <- NULL
EcoRegion_by_ICES_Areas$OBJECTID.1 <- NULL
EcoRegion_by_ICES_Areas$Area_km2 <- NULL

#EcoRegion_by_ICES_Areas <- st_transform(EcoRegion_by_ICES_Areas,3035)
# Did we get all ICES EcoRegions?
EcoRegion_without_ICES_Areas <- ices_ecoreg[!ices_ecoreg$Ecoregion%in%unique(EcoRegion_by_ICES_Areas$Ecoregion),]

EcoRegion_without_ICES_Areas$Shape_Le_1 <- NULL
EcoRegion_without_ICES_Areas$Shape_Leng <- NULL
EcoRegion_without_ICES_Areas$Shape_Area <- NULL

st_crs(EcoRegion_by_ICES_Areas) <- st_crs(EcoRegion_without_ICES_Areas)

EcoRegion_by_ICES_Areas <- st_transform(EcoRegion_by_ICES_Areas,4326)
EcoRegion_without_ICES_Areas <- st_transform(EcoRegion_without_ICES_Areas,4326)
length(unique(EcoRegion_by_ICES_Areas$Ecoregion))
length(unique(EcoRegion_without_ICES_Areas$Ecoregion))

names(EcoRegion_by_ICES_Areas)[!names(EcoRegion_by_ICES_Areas)%in%names(EcoRegion_without_ICES_Areas)]
EcoRegion_without_ICES_Areas$Major_FA <- ""
EcoRegion_without_ICES_Areas$SubArea <- ""
EcoRegion_without_ICES_Areas$Division <- ""
EcoRegion_without_ICES_Areas$SubDivisio<- ""
EcoRegion_without_ICES_Areas$Unit<- ""
EcoRegion_without_ICES_Areas$Area_Full<- ""
EcoRegion_without_ICES_Areas$Area_27<- ""

ices_ecoregion_by_ices_areas <- rbind(EcoRegion_by_ICES_Areas,
                                      EcoRegion_without_ICES_Areas)
                                      

#EcoRegion_by_ICES_Areas <- unique(EcoRegion_by_ICES_Areas,by='geometry')

fwrite(missing_ices_filtered_stock_list,'../output/Stocks_and_Areas_in_ICES_filtered_stock_list_not_present_in_stock_ass_graph_data.csv')
st_write(stocks_ices_final,'../output/ICES_Stocks_by_ICES_Areas_20160601_3857.shp',append = F)
st_write(stocks_areas_by_ices_areas_unique,'../output/ICES_Stocks_by_ICES_Areas_Unique_20160601_3857.shp',append = F)
fwrite(missing_stocks,'../output/stocks_ices_unique_long_with_missing_ICES_Area_geometry.csv')
fwrite(`st_geometry<-`(stocks_ices_final,NULL),'../output/stocks_ices_unique_long.csv')

tac_year <- 2022
# Loading the FMZ_Stocks_v_7_1
fmz <- st_read('./FMZ_v7_1/FMZ_v7_1.shp')#[fmz_stocks_2022$Year==2022,]
tac_stocks <- st_read('./FMZ_v7_1/Stocks_v7_1.dbf')
tac_stocks_year <- tac_stocks[tac_stocks$Year_ID==tac_year,]

fmz_stocks_year <- merge(fmz,tac_stocks_year,by='FMZ_ID')
fmz_stocks_year <- st_transform(fmz_stocks_year,3035)
fmz_stocks_year <- st_make_valid(fmz_stocks_year)

icesareas <- st_transform(icesareas,3035)
icesareas <- st_make_valid(icesareas)

fmz_stocks_year_unique <- fmz_stocks_year[,c('FMZ_ID',"Descriptio","Def_analyt",'geometry')]
fmz_stocks_year_unique <- unique(fmz_stocks_year_unique,by='FMZ_ID')
fmz_stocks_year_unique$FMZ_Area_km2 <- st_area(fmz_stocks_year_unique)

icesareas_fmz_stocks_year <- st_intersection(fmz_stocks_year_unique,icesareas)
icesareas_fmz_stocks_year$OBJECTID_1 <- NULL
icesareas_fmz_stocks_year$OBJECTID<- NULL
icesareas_fmz_stocks_year$ICES_Area_km2<- st_area(icesareas_fmz_stocks_year)

#icesareas_fmz_stocks_2022 <- unique(icesareas_fmz_stocks_2022)
icesareas_fmz_stocks_year <- st_transform(icesareas_fmz_stocks_year,4326)
icesareas_fmz_stocks_year <- st_make_valid(icesareas_fmz_stocks_year)

ices_ecoregion_by_ices_areas <- st_transform(ices_ecoregion_by_ices_areas,3035)
ices_ecoregion_by_ices_areas_fmz_stocks_year <- st_intersection(fmz_stocks_year_unique,ices_ecoregion_by_ices_areas)
#icesareas_fmz_stocks_year$Species_TAC <- substring(icesareas_fmz_stocks_year$Stocks_v7_,1,3)

ices_ecoregion_by_ices_areas_fmz_stocks_year$OBJECTID <- NULL

fwrite(,
       '../output/FMZ_by_ICES_Ecoregions_and_Areas_20160601_4326.csv')


st_write(ices_ecoregion_by_ices_areas_fmz_stocks_year,
         '../output/FMZ_by_ICES_Ecoregions_and_Areas_20160601_4326.shp',
         append=F,
         delete_dsn = T)

st_write(icesareas_fmz_stocks_year,
         '../output/FMZ_by_ICES_Areas_20160601_4326.shp',
         append=F,
         delete_dsn = T)

icesareas_fmz_stocks_year_tableau <- unique(icesareas_fmz_stocks_year,by=c('FMZ_ID','Area_Full'))
icesareas_fmz_stocks_year_tableau <- icesareas_fmz_stocks_year_tableau[,c("FMZ_ID",'Descriptio',
                                                                          'Def_analyt',
                                                                          "Area_Full",'FMZ_Area_km2',
                                                                          'ICES_Area_km2',"geometry")]
#icesareas_fmz_stocks_2022_tableau <- st_transform(icesareas_fmz_stocks_2022_tableau,4326)

st_write(icesareas_fmz_stocks_year_tableau,'../output/FMZ_by_ICES_Areas_20160601_4326_TABLEAU.shp'
         ,append = F,
         delete_dsn = T,
         overwrite_layer=T)

fmz_stocks_year_tableau <- `st_geometry<-`(fmz_stocks_year[,c('FMZ_ID','STOCK_ID')],NULL)
#fmz_stocks_2022_tableau$geometry <- NULL
fmz_stocks_year_tableau <- unique(fmz_stocks_year_tableau,by=c('FMZ_ID','STOCK_ID'))
stocks_ices_finalDT <- as.data.table(stocks_ices_finalDT)
stocks_ices_finalDT[,Species_ICES:=toupper(substring(FishStock,1,3))]
fmz_stocks_year_tableau <- as.data.table(fmz_stocks_year_tableau)
fmz_stocks_year_tableau[,Species_TAC:=substring(STOCK_ID,1,3)]
sort(unique(fmz_stocks_year_tableau$Species_TAC))
sort(unique(stocks_ices_finalDT$Species_ICES))

# Common species
common_species <- sort(unique(fmz_stocks_year_tableau$Species_TAC))[sort(unique(fmz_stocks_year_tableau$Species_TAC))%in%
sort(unique(stocks_ices_finalDT$Species_ICES))]
not_in_common_species <-  sort(unique(fmz_stocks_year_tableau$Species_TAC))[!sort(unique(fmz_stocks_year_tableau$Species_TAC))%in%
                                                                          sort(unique(stocks_ices_finalDT$Species_ICES))]
fwrite(fmz_stocks_year_tableau,paste0('../output/FMZ_Stocks_',tac_year,'_TABLEAU.csv'))
fwrite(stocks_ices_finalDT,'../output/Stocks_ICES_TABLEAU.csv')

              
FMZ_Stocks_v7_ICES_Stocks_tableau <- merge(
  merge(icesareas_fmz_stocks_year_tableau,
                            fmz_stocks_year_tableau,by='FMZ_ID'),stocks_ices_finalDT,
  by='Area_Full')
setnames(FMZ_Stocks_v7_ICES_Stocks_tableau,old=c('species.x','species.y'),
      new=c('species_TAC','species_ICES'))
setnames(FMZ_Stocks_v7_ICES_Stocks_tableau,old=c('STOCK_ID','FishStock'),
         new=c('Stock_TAC','Stock_ICES'))

# They are not really duplicates
duplicates <- `st_geometry<-`(FMZ_Stocks_v7_ICES_Stocks_tableau[duplicated(FMZ_Stocks_v7_ICES_Stocks_tableau)|
                                                  duplicated(FMZ_Stocks_v7_ICES_Stocks_tableau,fromLast = T),],NULL)

#`st_geometry<-`(FMZ_Stocks_v7_ICES_Stocks_tableau,NULL)
# FMZ_Stocks_v7_ICES_Stocks_tableau <- st_make_valid(FMZ_Stocks_v7_ICES_Stocks_tableau)
# st_write(FMZ_Stocks_v7_ICES_Stocks_tableau,'../output/FMZ_Stocks_v7_ICES_Stocks_4326_TABLEAU.shp'
#          ,append = F,
#          #delete_dsn = T,
#          overwrite_layer=T)
fwrite(`st_geometry<-`(FMZ_Stocks_v7_ICES_Stocks_tableau,NULL),
       paste0('../output/FMZ_Stocks_',tac_year,'_ICES_Stocks_TABLEAU.csv'))
