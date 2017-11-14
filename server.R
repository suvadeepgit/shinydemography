# !! Warning: More beautification needed !!

library(shiny)
library(datasets)
library(ggplot2)
library(plyr)
library(maps)
library(ggmap)
#library(RODBC)

# -- Set up connection to Postgres database and read -------------

#conn.dma = odbcConnect("dma", uid='postgres',
#                      pwd='postgres2015', rows_at_time = 1,
#                      believeNRows = FALSE)

#dma_borders = sqlQuery(conn.dma, paste("SELECT 
#                                       dma_borders.group,
#                                       dma_borders.lon,
#                                       dma_borders.lat,
#                                       dma_borders.dma_id,
#                                       dma_borders.dma_lable
#                                       FROM dma_borders ORDER BY id
#                                       "))

dma_borders <- read.csv("dma_borders.csv",sep="|")

#-------------  AGE Sex --------------------
data_age_sex <- read.csv("./ageSexData.csv",sep=",")
data_edu <- read.csv("./educationData.csv",sep=",")
data_earnings <- read.csv("./earningsData2.csv",sep=",")
data_race <- read.csv("./raceData.csv",sep=",")
data_zip_dma <- read.csv("./dma_to_payroll.csv",sep="|")

data_age_sex_bydma <- merge(data_age_sex, data_zip_dma, by.x = "Id2", by.y = "zip")

short_data_age_sex_bydma <- data.frame(
  zip             = data_age_sex_bydma$Id2,
  dma_id          = data_age_sex_bydma$dma_code,
  mf              = data_age_sex_bydma$M_Percent,
  ff              = data_age_sex_bydma$F_Percent,
  M_5To14Years    = data_age_sex_bydma$M_5To14Years     ,	
  F_5To14Years    = data_age_sex_bydma$F_5To14Years     ,	
  M_15To17Years   = data_age_sex_bydma$M_15To17Years    ,
  F_15To17Years   = data_age_sex_bydma$F_15To17Years    ,
  M_18To24Years   = data_age_sex_bydma$M_18To24Years    ,
  F_18To24Years   = data_age_sex_bydma$F_18To24Years    ,
  M_15To44Years   = data_age_sex_bydma$M_15To44Years    ,
  F_15To44Years   = data_age_sex_bydma$F_15To44Years    ,
  M_18YearsAndOver= data_age_sex_bydma$M_18YearsAndOver ,
  F_18YearsAndOver= data_age_sex_bydma$F_18YearsAndOver
)

new<-aggregate(short_data_age_sex_bydma[,3:14], by = list(short_data_age_sex_bydma$dma), FUN=mean, na.rm=TRUE)
names(new)[names(new) == "Group.1"] <- "dma_id"

dma_age_sex <- merge(dma_borders,new,by="dma_id")

#---- Education -------------------------

colnames(data_edu) <- c("X","zip","M_gr1","F_gr1","M_gr2","F_gr2","M_gr3","F_gr3","M_gr4","F_gr4","M_gr5","F_gr5")
data_education_bydma <- merge(data_edu, data_zip_dma, by = "zip")
education_bydma <- subset(data_education_bydma, select = -c(X, name, employment, annual_payroll))
aggr_edu<-aggregate(education_bydma[,2:11], by = list(education_bydma$dma_code), FUN=mean, na.rm=TRUE)
names(aggr_edu)[names(aggr_edu) == "Group.1"] <- "dma_id"
aggr_edu_wsum <- ddply(aggr_edu, .(dma_id,M_gr1,M_gr2,M_gr3,M_gr4,M_gr5,F_gr1,F_gr2,F_gr3,F_gr4,F_gr5), summarise, M_sum = sum(M_gr1+M_gr2+M_gr3+M_gr4+M_gr5), F_sum = sum(F_gr1+F_gr2+F_gr3+F_gr4+F_gr5))

education_bydma_Pcent <- ddply(aggr_edu_wsum, .(dma_id), summarise,
                               M_gr1 = M_gr1/M_sum,
                               M_gr2 = M_gr2/M_sum,
                               M_gr3 = M_gr3/M_sum,
                               M_gr4 = M_gr4/M_sum,
                               M_gr5 = M_gr5/M_sum,
                               F_gr1 = F_gr1/F_sum,
                               F_gr2 = F_gr2/F_sum,
                               F_gr3 = F_gr3/F_sum,
                               F_gr4 = F_gr4/F_sum,
                               F_gr5 = F_gr5/F_sum
                               )

education_bydma_Pcent <- merge(dma_borders,education_bydma_Pcent,by="dma_id")

#aggr_edu_wsumF <- aggr_edu_wsum[,c("F_gr1","F_gr2","F_gr3","F_gr4","F_gr5")] / aggr_edu_wsum[,"F_sum"]
#ggr_edu_wsumM <- aggr_edu_wsum[,c("M_gr1","M_gr2","M_gr3","M_gr4","M_gr5")] / aggr_edu_wsum[,"M_sum"]

# --  Earnings ------------

colnames(data_earnings) <- c("X","zip","M_gr1","F_gr1","M_gr2","F_gr2","M_gr3","F_gr3","M_gr4","F_gr4","M_gr5","F_gr5","M_gr6","F_gr6","M_gr7","F_gr7","M_gr8","F_gr8","M_gr9","F_gr9")
data_earnings_bydma <- merge(data_earnings, data_zip_dma, by = "zip")
earnings_bydma <- subset(data_earnings_bydma, select = -c(X, name, employment, annual_payroll))
aggr_earnings<-aggregate(earnings_bydma[,2:19], by = list(earnings_bydma$dma_code), FUN=mean, na.rm=TRUE)
names(aggr_earnings)[names(aggr_earnings) == "Group.1"] <- "dma_id"
earnings_bydma_Pcent <- merge(dma_borders,aggr_earnings,by="dma_id")

# --- Race ------------

colnames(data_race) <- c("X","White","Black","AmInd","Asian","Hawaii","Others","Two","zip")
data_race_bydma <- merge(data_race, data_zip_dma, by = "zip")
race_bydma <- subset(data_race_bydma, select = -c(X, name, employment, annual_payroll))
aggr_race<-aggregate(race_bydma[,2:8], by = list(race_bydma$dma_code), FUN=mean, na.rm=TRUE)
names(aggr_race)[names(aggr_race) == "Group.1"] <- "dma_id"
race_bydma_Pcent <- merge(dma_borders,aggr_race,by="dma_id")

# --- Load US States -----------------
us.dat<-map_data("state")

# -----------------------------------
shinyServer(function(input, output){
  input_agesex <- reactive({
    if(input$sex == "Female" & input$age == "All") 
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","ff")]
    else if(input$sex == "Female" & input$age == "5-14 years") 
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","F_5To14Years")]
    else if(input$sex == "Female" & input$age == "15-17 years") 
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","F_15To17Years")]
    else if(input$sex == "Female" & input$age == "18-24 years") 
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","F_18To24Years")]
    else if(input$sex == "Female" & input$age == "15-44 years") 
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","F_15To44Years")]
    else if(input$sex == "Female" & input$age == "18 years and Over") 
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","F_18YearsAndOver")]
    else if(input$sex == "Male" & input$age == "All")
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","mf")]
    else if(input$sex == "Male" & input$age == "5-14 years")
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","M_5To14Years")]
    else if(input$sex == "Male" & input$age == "15-17 years")
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","M_15To17Years")]
    else if(input$sex == "Male" & input$age == "18-24 years")
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","M_18To24Years")]
    else if(input$sex == "Male" & input$age == "15-44 years")
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","M_15To44Years")]
    else if(input$sex == "Male" & input$age == "18 years and Over")
      df = dma_age_sex[, c("dma_id","group","lon","lat","dma_lable","M_18YearsAndOver")]
  })
  
# -- for Demography map with DMA boundaries ----
  plot.age_sex_map <- reactive({
    tmp = input_agesex()
    colnames(tmp)[6] <- "percent"
    tmp$percent[1]
    ggplot() +
    geom_polygon(aes(long,lat, group=group), color='black', fill=NA, data=us.dat) + 
    theme(axis.text = element_blank(), axis.title=element_blank()) + theme_bw() + 
    geom_polygon(data=tmp,aes(lon,lat,group=group,fill=percent),color="black") + 
    theme(legend.position="right",axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +
    scale_x_continuous("") + scale_y_continuous("") + scale_fill_gradient(low = "yellow", high = "red")
  }) 

  output$plot_agesex <- renderPlot({
   plot.age_sex_map()
  },width=900, height=600)

# --- Education by DMA --------------
  
  input_education <- reactive({

    if(input$sex_e == "Female" & input$education == "Less than High School Graduate") 
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr1")] 
    else if(input$sex_e == "Female" & input$education == "High School Graduate") 
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr2")]
    else if(input$sex_e == "Female" & input$education == "Some College or Associate's Degree") 
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr3")]
    else if(input$sex_e == "Female" & input$education == "Bachelor's Degree") 
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr4")]
    else if(input$sex_e == "Female" & input$education == "Graduate or Professional Degree") 
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr5")]
    else if(input$sex_e == "Male" & input$education == "Less than High School Graduate")
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr1")]
    else if(input$sex_e == "Male" & input$education == "High School Graduate")
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr2")]
    else if(input$sex_e == "Male" & input$education == "Some College or Associate's Degree")
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr3")]
    else if(input$sex_e == "Male" & input$education == "Bachelor's Degree")
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr4")]
    else if(input$sex_e == "Male" & input$education == "Graduate or Professional Degree")
      df = education_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr5")]
    
    })
  
# -- for Demography map with DMA boundaries ----
  plot.education_map <- reactive({
    tmp = input_education()
    colnames(tmp)[6] <- "percent"
   
    ggplot() +
    geom_polygon(aes(long,lat, group=group), color='black', fill=NA, data=us.dat) + 
    theme(axis.text = element_blank(), axis.title=element_blank()) + theme_bw() + 
    geom_polygon(data=tmp,aes(lon,lat,group=group,fill=percent*100),color="black") + 
    theme(legend.position="right",axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +
    scale_x_continuous("") + scale_y_continuous("") + scale_fill_gradient(low = "yellow", high = "red")
  }) 

  output$plot_education <- renderPlot({
   plot.education_map()
},width=900, height=600)

# --- Earnings by DMA --------------
  
  input_earnings <- reactive({
      
    if(input$sex_er == "Female" & input$earnings == "Less than 10000") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr1")] 
    else if(input$sex_er == "Female" & input$earnings == "10000-14999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr2")]
    else if(input$sex_er == "Female" & input$earnings == "15000-24999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr3")]
    else if(input$sex_er == "Female" & input$earnings == "25000-34999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr4")]
    else if(input$sex_er == "Female" & input$earnings == "35000-49999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr5")]
    else if(input$sex_er == "Female" & input$earnings == "50000-64999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr6")]
    else if(input$sex_er == "Female" & input$earnings == "65000-74999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr7")]
    else if(input$sex_er == "Female" & input$earnings == "75000-99999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr8")]
    else if(input$sex_er == "Female" & input$earnings == "100000 or More") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","F_gr9")]
    
    else if(input$sex_er == "Male" & input$earnings == "Less than 10000") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr1")] 
    else if(input$sex_er == "Male" & input$earnings == "10000-14999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr2")]
    else if(input$sex_er == "Male" & input$earnings == "15000-24999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr3")]
    else if(input$sex_er == "Male" & input$earnings == "25000-34999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr4")]
    else if(input$sex_er == "Male" & input$earnings == "35000-49999") 
        df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr5")]
    else if(input$sex_er == "Male" & input$earnings == "50000-64999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr6")]
    else if(input$sex_er == "Male" & input$earnings == "65000-74999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr7")]
    else if(input$sex_er == "Male" & input$earnings == "75000-99999") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr8")]
    else if(input$sex_er == "Male" & input$earnings == "100000 or More") 
      df = earnings_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","M_gr9")]                        

    })
  
# -- for Demography map with DMA boundaries ----
  plot.earnings_map <- reactive({
    tmp = input_earnings()
    colnames(tmp)[6] <- "percent"
    ggplot() +
    geom_polygon(aes(long,lat, group=group), color='black', fill=NA, data=us.dat) + 
    theme(axis.text = element_blank(), axis.title=element_blank()) + theme_bw() + 
    geom_polygon(data=tmp,aes(lon,lat,group=group,fill=percent),color="black") + 
    theme(legend.position="right",axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +
    scale_x_continuous("") + scale_y_continuous("") + 
#    scale_fill_brewer(palette = "Blues")
    scale_fill_gradient(low = "yellow", high = "red")
  }) 

  output$plot_earnings <- renderPlot({
   plot.earnings_map()
  },width=900, height=600)

# --  Race -- q()


    input_race <- reactive({
 
    if(input$race == "White" ) 
      df = race_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","White")] 
    else if(input$race == "BlackOrAfricanAmerican" ) 
      df = race_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","Black")]
    else if(input$race == "AmericanIndianAndAlaskaNative" ) 
      df = race_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","AmInd")]
    else if(input$race == "Asian" ) 
      df = race_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","Asian")]
    else if(input$race == "NativeHawaiianAndOtherPacificIslander" ) 
      df = race_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","Hawaii")]
    else if(input$race == "SomeOtherRace" ) 
      df = race_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","Others")]
    else if(input$race == "TwoOrMoreRaces" ) 
      df = race_bydma_Pcent[, c("dma_id","group","lon","lat","dma_lable","Two")]
                       })
  
# -- for Demography map with DMA boundaries ----
  plot.race_map <- reactive({
    tmp = input_race()
    colnames(tmp)[6] <- "percent"
    ggplot() +
    geom_polygon(aes(long,lat, group=group), color='black', fill=NA, data=us.dat) + 
    theme(axis.text = element_blank(), axis.title=element_blank()) + theme_bw() + 
    geom_polygon(data=tmp,aes(lon,lat,group=group,fill=percent),color="black") + 
    theme(legend.position="right",axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()) +
    scale_x_continuous("") + scale_y_continuous("") + scale_fill_gradient(low = "yellow", high = "red")
  }) 

  output$plot_race <- renderPlot({
   plot.race_map()
  },width=900, height=600)
  
})
