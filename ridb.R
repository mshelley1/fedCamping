library(jsonlite)
library(tidyr)
library(reshape)
library(dplyr)
library(stringr)

# API Key
key=

#
# Get all facilities in a state {FacilityID, FacilityName}
#
# WV: 119 items
wv1<-fromJSON('https://ridb.recreation.gov/api/v1/facilities?limit=50&offset=0&full=false&state=WV&lastupdated=10-01-2018&sort=%22Name%22&apikey=d82b9ce0-00e6-4b62-9aff-952e9bae78af')
wv2<-fromJSON('https://ridb.recreation.gov/api/v1/facilities?limit=50&offset=50&full=false&state=WV&lastupdated=10-01-2018&sort=%22Name%22&apikey=d82b9ce0-00e6-4b62-9aff-952e9bae78af')
wv3<-fromJSON('https://ridb.recreation.gov/api/v1/facilities?limit=50&offset=100&full=false&state=WV&lastupdated=10-01-2018&sort=%22Name%22&apikey=d82b9ce0-00e6-4b62-9aff-952e9bae78af')

wv1dat<-as.data.frame(cbind(wv1[1]$RECDATA$FacilityID,wv1[1]$RECDATA$FacilityName,wv1[1]$RECDATA$FacilityLongitude,wv1[1]$RECDATA$FacilityLatitude))
wv2dat<-as.data.frame(cbind(wv2[1]$RECDATA$FacilityID,wv2[1]$RECDATA$FacilityName,wv2[1]$RECDATA$FacilityLongitude,wv2[1]$RECDATA$FacilityLatitude))               
wv3dat<-as.data.frame(cbind(wv3[1]$RECDATA$FacilityID,wv3[1]$RECDATA$FacilityName,wv3[1]$RECDATA$FacilityLongitude,wv3[1]$RECDATA$FacilityLatitude))                

wvdat<-as.data.frame(rbind(wv1dat,wv2dat,wv3dat))
names(wvdat)<-c('FacilityID','FacilityName','FacilityLongitude','FacilityLatitude')
wvdat$FacilityID<-as.numeric(as.character(wvdat$FacilityID))

#
# For each facitliy ID in a state that have campsites, get all the campsites
#

# Determine how many sites each facility has
numSites<-data.frame(FacilityID=9999,NumSites=9999)
for (i in 1:nrow(wvdat)) {
  facId=wvdat$FacilityID[i]
  campsitesurl<-paste0('https://ridb.recreation.gov/api/v1/facilities/',facId,'/campsites?limit=50&offset=0&apikey=',key)
  campsites<-fromJSON(campsitesurl)
  
# Record number of campsites for each facility
   numSites<-rbind(numSites,c(facId,campsites$METADATA$RESULTS$TOTAL_COUNT))
}

# Keep only facilities that have at least one campsite and cleanup
facWithCamp<-filter(numSites,NumSites>0 & FacilityID!=9999)
rm(wv1,wv2,wv3,wv1dat,wv2dat,wv3dat,numSites,campistes,facId)

x<-getSites(facWithCamp)

#
# getSites: For each facility that has campsites, return the attributes of each campsite
#
getSites <- function(facilityData) {
  
  # Return row sets of up to 50 records for each facility
    for (i in 1:nrow(facilityData)) {
      facId=facilityData$FacilityID[i]
      print(c(facilityData[i,1],facilityData[i,2]))
      # API record limit is 50, so need offset to get all sites (at large facilities)
          for (j in 1:ceiling(facilityData[i,2]/50)) {   
            offset=50*(j-1)
            campsitesurl<-paste0('https://ridb.recreation.gov/api/v1/facilities/',facId,'/campsites?limit=50&offset=',0,'&apikey=',key)
            tempsites<-fromJSON(campsitesurl)
            If (j==1) {
              subInfo<-list(subSites(tempsites))
              
            if (j==3) {return(subSites(tempsites))}
          }
    }
}

#
# subSites: Call from getSites - for each site, extract and transform the attributes
#

subSites<- function(tempsites) {

  # Extract attribute values and permitted EQ for each non-MANAGEMENT/overnight campsite;
    sites<- select(tempsites$RECDATA,c(1:11)) %>%
            filter(CampsiteType != "MANAGEMENT" & TypeOfUse=="Overnight")
    
    attribchk<-tempsites$RECDATA$CampsiteType !="MANAGEMENT" & tempsites$RECDATA$TypeOfUse=="Overnight" #Check each attrib list for corresponding record in sites
    attribs<-tempsites$RECDATA$ATTRIBUTES[attribchk==T]
    eq<-tempsites$RECDATA$PERMITTEDEQUIPMENT[attribchk==T]
    
    attribs_mrg<-Reduce(function(x,y)merge(x,y,by="AttributeName",all=TRUE),attribs)
    names(attribs_mrg)<-c("VarName", as.character(sites$CampsiteID))
           
    eq_mrg<-Reduce(function(x,y)merge(x,y,by="EquipmentName",all=TRUE),eq)
    names(eq_mrg)<-c("VarName",as.character(sites$CampsiteID))
    
    attrib_eq<-rbind(attribs_mrg,eq_mrg)
    
  # Transpose and rename, cleaning up attrib names to turn into colnames
    attribeq_t<-t(attrib_eq)
    atn<-gsub('\\s|/','',attribeq_t[1,])
    attribeq_t<-as.data.frame(attribeq_t[-1,])
    CampSiteID<-row.names(attribeq_t)
    names(attribeq_t)<-atn
    siteAttribsEq<-as.data.frame(cbind(CampSiteID,as.data.frame(attribeq_t)))

  # Convert factors numeric where needed
    siteAttribsEq$MaxNumofPeople<-as.numeric(as.character(siteAttribsEq$MaxNumofPeople))
    siteAttribsEq$MaxVehicleLength<-as.numeric(as.character(siteAttribsEq$MaxVehicleLength))
    siteAttribsEq$RV<-as.numeric(as.character(siteAttribsEq$RV))
    siteAttribsEq$Tent<-as.numeric(as.character(siteAttribsEq$Tent))
    siteAttribsEq$Trailer<-as.numeric(as.character(siteAttribsEq$Trailer))
    siteAttribsEq$CampSiteID<-as.character(siteAttribsEq$CampSiteID)
      
  # Merge sites with attrib_eq
    sitesInfo<-merge(sites,siteAttribsEq,by.x='CampsiteID',by.y='CampSiteID')
  
 return(sitesInfo)  
}
  
