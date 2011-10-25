#' First letter
a <- function() {}
  
#' Second letter
"b" <- function() {}

#' Third letter
`c` <- function() {}

#' Extracts environmental covariate data for SMI Zc analysis
#' : by extracting data from ACCESS database and creating a series of
#' anomaly summaries for sea surface temperature(SST), upwelling index (UWI) and
#' multivariate ENSO index (MEI).
#'
#' @export
#' @import CIPinnipedAnalysis
#' @param average.years years to use for average for anomaly creationccc
#' @param fdir directory containing environmental.data.mdb
#' @param sites SST sites to be used in SST averaging (from create.SST.anomalies)
#'        1) ESB, 2)WSB, 3)PtArg, 4)PtSM, 5)PtSL, 6)CSM, 7)MB, 8)PtReyes
#' @return dataframe with rows as years from 1987 to last year in data and the columns are:
#'      time,OcttoJuneSSTAnomalies,ApriltoJuneSSTAnomalies,JulytoJuneSSTAnomalies,
#'                      OcttoJuneUWI33Anomalies, ApriltoJuneUWI33Anomalies, JulytoJuneUWI33Anomalies,
#'                      OcttoJuneUWI36Anomalies, ApriltoJuneUWI36Anomalies, JulytoJuneUWI36Anomalies,
#'                      OcttoJuneMEI, ApriltoJuneMEI, JulytoJuneMEI
#' @author Jeff Laake
"EnvironCovariates"<-function(average.years=c(1994:1996,1998:2008),fdir="",sites=1:5)
{
#   SST Anomalies
       if(fdir=="")fdir=system.file(package = "CIPinnipedAnalysis")
       anomalies=create.SST.anomalies(average.years,fdir=fdir,store=FALSE)
       fpath=file.path(fdir,"environmental.data.mdb")
       connection=odbcConnectAccess(fpath)
       SSTAnomalies=t(apply(anomalies[,,sites],c(2,1),mean,na.rm=TRUE))
       SSTAnomalies[is.nan(SSTAnomalies)]=NA
       nyears=nrow(SSTAnomalies)
       OcttoJuneSSTAnomalies=rowMeans(cbind(SSTAnomalies[1:(nyears-1),c("Oct","Nov","Dec")],SSTAnomalies[2:nyears,c("Jan","Feb","Mar","Apr","May","June")]),na.rm=TRUE)
       ApriltoJuneSSTAnomalies=rowMeans(SSTAnomalies[,c("Apr","May","June")],na.rm=TRUE)
       names(ApriltoJuneSSTAnomalies)=as.character(as.numeric(names(ApriltoJuneSSTAnomalies))-1)
       JulytoJuneSSTAnomalies=rowMeans(cbind(SSTAnomalies[1:(nyears-1),c("July","Aug","Sept","Oct","Nov","Dec")],SSTAnomalies[2:nyears,c("Jan","Feb","Mar","Apr","May","June")]),na.rm=TRUE)
#   UpwellingIndex for 33N & 36N
       UWI=sqlFetch(connection,"UWIAnomaly")
       UWI=UWI[order(UWI$Year,UWI$Month),]
       UWI=tapply(UWI$UWI,list(UWI$Year,UWI$Month,UWI$Location),unique)
       minyear=min(as.numeric(dimnames(UWI)[[1]]))
       maxyear=max(as.numeric(dimnames(UWI)[[1]]))
       nyears=maxyear-minyear+1
       OcttoJuneUWI33Anomalies=rowMeans(cbind(UWI[1:(nyears-1),as.character(10:12),1],UWI[2:nyears,as.character(1:6),1]),na.rm=TRUE)
       ApriltoJuneUWI33Anomalies=rowMeans(UWI[,as.character(4:6),1],na.rm=TRUE)[1:nyears]
       names(ApriltoJuneUWI33Anomalies)=as.character(as.numeric(names(ApriltoJuneUWI33Anomalies))-1)
       JulytoJuneUWI33Anomalies=rowMeans(cbind(UWI[1:(nyears-1),as.character(7:12),1],UWI[2:nyears,as.character(1:6),1]),na.rm=TRUE)
       OcttoJuneUWI36Anomalies=rowMeans(cbind(UWI[1:(nyears-1),as.character(10:12),2],UWI[2:nyears,as.character(1:6),2]),na.rm=TRUE)
       ApriltoJuneUWI36Anomalies=rowMeans(UWI[,as.character(4:6),2],na.rm=TRUE)
       names(ApriltoJuneUWI36Anomalies)=as.character(as.numeric(names(ApriltoJuneUWI36Anomalies))-1)
       JulytoJuneUWI36Anomalies=rowMeans(cbind(UWI[1:(nyears-1),as.character(7:12),2],UWI[2:nyears,as.character(1:6),2]),na.rm=TRUE)
#   Multivariate ENSO Index - lagged by 3 months
       MEI=sqlFetch(connection,"MEI")
       minyear=min(MEI$Year)
       maxyear=max(MEI$Year)
       nyears=maxyear-minyear+1
       MEI=tapply(MEI$MEI,list(MEI$Year,MEI$Month),unique)
       OcttoJuneMEI=rowMeans(cbind(MEI[1:(nyears-1),as.character(7:12)],MEI[2:nyears,as.character(1:3)]),na.rm=TRUE)
       ApriltoJuneMEI=rowMeans(MEI[,as.character(1:3)],na.rm=TRUE)
       names(ApriltoJuneMEI)=as.character(as.numeric(names(ApriltoJuneMEI))-1)
       JulytoJuneMEI=rowMeans(cbind(MEI[1:(nyears-1),as.character(4:12)],MEI[2:nyears,as.character(1:3)]),na.rm=TRUE)
#   Create dataframe with values from 1987:2009
       maxyear=max(as.numeric(names(JulytoJuneMEI)))
#   time is meant to match the beginning year of a survival interval from time to time+1;  that is why the
#   April-to-June is set as the previous year to model survival of pup cohort born in the previous year
   envcovdf=data.frame(time=1987:maxyear,
                       OcttoJuneSSTAnomalies=OcttoJuneSSTAnomalies[as.character(1987:maxyear)],
                       ApriltoJuneSSTAnomalies=ApriltoJuneSSTAnomalies[as.character(1987:maxyear)],
                       JulytoJuneSSTAnomalies=JulytoJuneSSTAnomalies[as.character(1987:maxyear)],
                       OcttoJuneUWI33Anomalies=OcttoJuneUWI33Anomalies[as.character(1987:maxyear)],
                       ApriltoJuneUWI33Anomalies=ApriltoJuneUWI33Anomalies[as.character(1987:maxyear)],
                       JulytoJuneUWI33Anomalies=JulytoJuneUWI33Anomalies[as.character(1987:maxyear)],
                       OcttoJuneUWI36Anomalies=OcttoJuneUWI36Anomalies[as.character(1987:maxyear)],
                       ApriltoJuneUWI36Anomalies=ApriltoJuneUWI36Anomalies[as.character(1987:maxyear)],
                       JulytoJuneUWI36Anomalies=JulytoJuneUWI36Anomalies[as.character(1987:maxyear)],
                       OcttoJuneMEI=OcttoJuneMEI[as.character(1987:maxyear)],
                       ApriltoJuneMEI=ApriltoJuneMEI[as.character(1987:maxyear)],
                       JulytoJuneMEI=JulytoJuneMEI[as.character(1987:maxyear)]
                       )
       odbcClose(connection)
   return(envcovdf)
}