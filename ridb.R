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
            
          if (j==3) { test<-subSites(tempsites)} #if for testing
      }
  }
}

#
# subSites: Call from getSites - for each site, extract and transform the attributes
#

subSites<- function(tempsites) {

  # Extract attribute values and permitted EQ for each non-MANAGEMENT campsite;
    sites<- select(tempsites$RECDATA,c(1:11)) %>%
            filter(CampsiteType != "MANAGEMENT" & TypeOfUse=="Overnight")
    
    attribs<-cbind(tempsites$RECDATA$CampsiteType,tempsites[1]$RECDATA$ATTRIBUTES[1]) #initialize dataframe with first record
    
    attrib<-as.data.frame(tempsites[1]$RECDATA$ATTRIBUTES[1]) #initialize dataframe with first record
    names(attrib)<-c('AttributeName',tempsites[1]$RECDATA$CampsiteID[1]) #colnames for each will be campsiteID
  
    permiteq<-as.data.frame(tempsites[1]$RECDATA$PERMITTEDEQUIPMENT[1])
    names(permiteq)<-c('EquipmentName',tempsites[1]$RECDATA$CampsiteID[1])
  
    
  # Merge data for each campsite by attribute name since order, completeness of attributes vary
  # Use same approach for permitted eq
    for (i in 2:nrow(tempsites$RECDATA)) {
       temp<-as.data.frame(tempsites[1]$RECDATA$ATTRIBUTES[i])
       names(temp)<-c('AttributeName',tempsites[1]$RECDATA$CampsiteID[i])
       attrib<-merge(attrib,temp,by='AttributeName')
      
       tempeq<-as.data.frame(tempsites[1]$RECDATA$PERMITTEDEQUIPMENT[i])
       names(tempeq)<-c(names(permiteq)<-c('EquipmentName',campsites[1]$RECDATA$CampsiteID[i]))
       permiteq<-merge(permiteq,tempeq,by='EquipmentName')
      }
      
  # Transpose and rename, cleaning up attrib names to turn into colnames
    attrib_t<-t(attrib)
    atn<-gsub('\\s|/','',attrib_t[1,])
    attrib_t<-as.data.frame(attrib_t[-1,])
    CampSiteID<-row.names(attrib_t)
    names(attrib_t)<-atn
    siteAttribs<-as.data.frame(cbind(CampSiteID,as.data.frame(attrib_t)))
      
    permiteq_t<-t(permiteq)
    eqn<-permiteq_t[1,] #No funky chars in eq names
    permiteq_t<-as.data.frame(permiteq_t[-1,])
    eqCampsiteID<-row.names(permiteq_t)
    names(permiteq_t)<-eqn
    eq<-as.data.frame(cbind(eqCampsiteID,as.data.frame(permiteq_t)))
      
  # Merge attibs and eq
    attrib_eq<-merge(siteAttribs,eq,by.x='CampSiteID',by.y='eqCampsiteID')
      
  # Convert factors numeric where needed
    attrib_eq$MaxNumofPeople<-as.numeric(as.character(attrib_eq$MaxNumofPeople))
    attrib_eq$MaxVehicleLength<-as.numeric(as.character(attrib_eq$MaxVehicleLength))
    attrib_eq$RV<-as.numeric(as.character(attrib_eq$RV))
    attrib_eq$Tent<-as.numeric(as.character(attrib_eq$Tent))
    attrib_eq$Trailer<-as.numeric(as.character(attrib_eq$Trailer))
    attrib_eq$CampSiteID<-as.character(attrib_eq$CampSiteID)
      
  # Merge sites with attrib_eq
    sitesInfo<-merge(sites,attrib_eq,by.x='CampsiteID',by.y='CampSiteID')
  
 return(sitesInfo)  
}
  
