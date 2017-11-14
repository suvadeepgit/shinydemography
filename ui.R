library(shiny)

shinyUI(navbarPage('Demography by DMA',
  tabPanel('Age and Gender',
  sidebarLayout(
    sidebarPanel(
      helpText("Ranking Metropolitan area based on NAICS Type (2012)"),
      
      selectInput("sex", 
                  label = "Choose Gender",
          	          choices = c(
                  	      "Female",
			                    "Male"
                              ),
                  	      selected = "Female"),
      selectInput("age",
                  label = "Choose Age group",
                  choices = c(
                    "All",
                    "5-14 years",
                    "15-17 years",
                    "18-24 years",
                    "15-44 years",
                    "18 years and Over"
                  ), 
                  selected = "All")       
      ),  
      mainPanel(
          plotOutput("plot_agesex")       
      )
    )),
 tabPanel('Education',
         sidebarLayout(
           sidebarPanel(
             helpText("Select Education"),
             selectInput("sex_e", 
                  label = "Choose Gender",
          	          choices = c(
                  	      "Female",
			                     "Male"
                              ),
                  	      selected = "Female"),
             selectInput("education", 
                         label = "Choose an Education Level",
                         choices = c(
                         "Less than High School Graduate",
                         "High School Graduate",
                         "Some College or Associate's Degree",
                         "Bachelor's Degree",
                         "Graduate or Professional Degree"  
                         ),
                         selected = "Less than High School Graduate")         
           ),
           mainPanel(
             plotOutput("plot_education")
           )
 )),
 tabPanel('Earnings',
         sidebarLayout(
           sidebarPanel(
             helpText("Select Earning"),
             selectInput("sex_er",
                  label = "Choose Gender",
                          choices = c(
                              "Female",
                              "Male"
                              ),
                              selected = "Female"),
             selectInput("earnings", 
                         label = "Choose an Earning Type (USD)",
                         choices = c(
                          "Less than 10000",
                          "10000-14999",
                          "15000-24999",
                          "25000-34999",
                          "35000-49999",
                          "50000-64999",
                          "65000-74999",
                          "75000-99999",
                          "100000 or More"
                         ),
                         selected = "Less than 10000")         
           ),
           mainPanel(
             plotOutput("plot_earnings")
           )
 )),
 tabPanel('Race',
         sidebarLayout(
           sidebarPanel(
             helpText("Select Race"),
             selectInput("race", 
                         label = "Choose a race",
                         choices = c(
                           "White",
                           "BlackOrAfricanAmerican",
                           "AmericanIndianAndAlaskaNative",
                           "Asian",
                           "NativeHawaiianAndOtherPacificIslander",
                           "SomeOtherRace",
                           "TwoOrMoreRaces"
                         ),
                         selected = "White")         
           ),
           mainPanel(
             plotOutput("plot_race")
           )
 ))
))
