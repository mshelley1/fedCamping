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
  
#
# For each facility that has campsites, return and extract campsite attributes.
# cbind facility ID to create a flat file
#

#for (i in 1:nrow(facWithCamp)) {
for (i in 1:3) {
    tempsites<-list()
    facId=facWithCamp$FacilityID[i]
  
  # If a facility has more than 50 sites, then need to adjust offset
    if (facWithCamp$NumSites[i] > 50)  {
        print(paste('Note: Facility',facId,'has more than 50 sites.'))
        for (j in 1:ceiling(facWithCamp[i,2]/50)) {   
          offset=50*(j-1)
          print(c(ceiling(facWithCamp[i,2]), j, offset))
           campsitesurl<-paste0('https://ridb.recreation.gov/api/v1/facilities/',facId,'/campsites?limit=50&offset=',0,'&apikey=',key)
           tempsites<-fromJSON(campsitesurl)
         }

      
      
getSites <- function(facilityData) {
  for (i in 1:nrow(facilityData)) {
    facId=facWithCamp$FacilityID[i]
    
    # If a facility has more than 50 sites, then need to adjust offset
    if (facilityData$NumSites[i] > 50)  {
      print(paste('Note: Facility',facId,'has more than 50 sites.'))
      for (j in 1:ceiling(facilityData[i,2]/50)) {   
        offset=50*(j-1)
        print(c(ceiling(facilityData[i,2]), j, offset))
        campsitesurl<-paste0('https://ridb.recreation.gov/api/v1/facilities/',facId,'/campsites?limit=50&offset=',0,'&apikey=',key)
        tempsites<-fromJSON(campsitesurl)
      }
  
}    
           
##TODO:
      # create a function to process each set of 50 records returned from campsites
      # call from this if/for loop
      
  #else
    #{campsitesurl<-paste0('https://ridb.recreation.gov/api/v1/facilities/',facId,'/campsites?limit=50&offset=0&apikey=',key)
    # campsites<-fromJSON(campsitesurl)
   }
}  
    # 

sites<-campsites$RECDATA[1:11]

#
#Extract attribute values and permitted EQ for each campsite
#
attrib<-as.data.frame(campsites[1]$RECDATA$ATTRIBUTES[1]) #initialize dataframe with first record
names(attrib)<-c('AttributeName',campsites[1]$RECDATA$CampsiteID[1]) #colnames for each will be campsiteID

permiteq<-as.data.frame(campsites[1]$RECDATA$PERMITTEDEQUIPMENT[1])
names(permiteq)<-c('EquipmentName',campsites[1]$RECDATA$CampsiteID[1])

# Merge data for each campsite by attribute name since order, completeness of attributes vary
# Use same approach for permitted eq
for (i in 2:nrow(campsites$RECDATA)) {
 temp<-as.data.frame(campsites[1]$RECDATA$ATTRIBUTES[i])
 names(temp)<-c('AttributeName',campsites[1]$RECDATA$CampsiteID[i])
 attrib<-merge(attrib,temp,by='AttributeName')

 tempeq<-as.data.frame(campsites[1]$RECDATA$PERMITTEDEQUIPMENT[i])
 names(tempeq)<-c(names(permeq)<-c('EquipmentName',campsites[1]$RECDATA$CampsiteID[i]))
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

