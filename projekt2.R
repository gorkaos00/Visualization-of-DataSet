library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(base)
library(stats)
data=read.table("HappinessAlcoholConsumption.csv",sep=",",header=TRUE)
data$Country<-as.factor(data$Country)
data$Region<-as.factor(data$Region)
data$Hemisphere<-as.factor(data$Hemisphere)
By<-c("Region","Hemisphere")
dzieleniekorelacji<-c("HappinessScore","HDI","GDP_PerCapita","Beer_PerCapita","Spirit_PerCapita","Wine_PerCapita")

ui<-fluidPage(
  useShinyjs(),
  titlePanel("Influence of Alcohol Consumption"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="ploty",label="Choose plot:",choices=c("Happiness Score by -","Most Happiest and Saddest Country","Correlation between -","The effect of alcohol consumption on -","Countries and their alcohol consumption")),
      selectInput(inputId="ByHS",label="Happiness Score by:",choices=By),
      selectInput(inputId="kor1",label="Correlation",choices=dzieleniekorelacji),
      selectInput(inputId="kor2",label="Correlation",choices=dzieleniekorelacji),
      sliderInput(inputId="top",label="How many country you would like to show",min=1,max=15,value=5),
      checkboxGroupInput("Alc", 
                         ("Select what you want to display "), 
                         choices = list("HDI" = 1, 
                                        "Happiness Score" = 2, 
                                        "GPD_PerCapita" = 3)),
    
    radioButtons("Region", 
                       ("Select Regions you want to display "), 
                       choices = list("Sub-Saharan Africa",
                                      "Central and Eastern Europe",
                                      "Latin America and Caribbean",
                                      "Western Europe",
                                      "Middle East and Northern Africa",
                                      "North America",
                                      "Australia and New Zealand",
                                      "Southeastern Asia",
                                      "Eastern Asia")),
    
  ),
    mainPanel(
      plotOutput(outputId = "wykres1"),
      plotOutput(outputId = "wykres2"),
      plotOutput(outputId = "wykres3")
    )
  )
)

server<-function(input,output)
{
  observe({
    if(input$ploty=="Happiness Score by -"){
        hide("wykres2")
        hide("wykres3")
        hide("kor1")
        hide("kor2")
        hide("top")
        hide("Alc")
        show("ByHS")
        hide("Region")
      
      output$wykres1<-renderPlot({
        if(input$ByHS=="Region"){
        mean_scores <- data %>%
          group_by(Region) %>% 
          summarise(mean = mean(HappinessScore))
        }
        else{
        mean_scores <- data %>%
          group_by(Hemisphere) %>% 
          summarise(mean = mean(HappinessScore))
        }
        ggplot(mean_scores, aes_string(x = input$ByHS, y = "mean")) +
          geom_col(fill = "#0073C2FF") +  # add mean points
          labs(title = paste("Happiness Scores by", input$ByHS), x =input$ByHS , y = "Mean Happiness Score") + 
          theme_minimal()+
          theme(text = element_text(size = 15, color = "black"),axis.text.x = element_text(angle = 30))
  })
    }
    else if(input$ploty=="Most Happiest and Saddest Country"){
      show("wykres2")
      hide("wykres3")
      hide("kor1")
      hide("kor2")
      hide("ByHS")
      hide("Alc")
      show("top")
      hide("Region")
      
      sorted_data <- data %>% 
        arrange(HappinessScore)
      
      # Select the top 10 happiest and saddest countries
      top_10_happiest <- sorted_data %>% 
        tail(input$top)
      top_10_saddest <- sorted_data %>% 
        head(input$top)
      
      # Bind the two datasets together
      output$wykres1<-renderPlot({
      # Create the plot
      ggplot(top_10_happiest, aes(x = Country, y = HappinessScore, fill = HappinessScore)) +
        geom_col() +
        labs(title =paste( "Top",input$top,  "Happiest Countries"), x = "Country", y = "Happiness Score") +
        scale_fill_gradient(low = "light green", high = "green") +
        theme_minimal()
      })
      output$wykres2<-renderPlot({
        # Create the plot
        ggplot(top_10_saddest, aes(x = Country, y = HappinessScore, fill = HappinessScore)) +
          geom_col() +
          labs(title = paste( "Top",input$top,  "Sadest Countries"), x = "Country", y = "Happiness Score") +
          scale_fill_gradient(low = "blue", high = "light blue") +
          theme_minimal()
      })
    }
    else if(input$ploty=="Correlation between -"){
      hide("wykres2")
      hide("wykres3")
      show("kor1")
      show("kor2")
      hide("ByHS")
      hide("top")
      hide("Alc")
      hide("Region")
      output$wykres1<-renderPlot({
        ggplot(data, aes_string(x = input$kor1, y = input$kor2)) +
          geom_point(aes(color = input$kor1),
                     fill="#69b3a2",
                     shape=21,
                     alpha=0.5,
                     size=6,
                     stroke = 2) +
          geom_smooth(method = "lm", color = "red", size = 1, se = FALSE)+
          labs(title=paste("Correlation between",input$kor1,input$kor2))
        })
      
    }
    else if(input$ploty=="The effect of alcohol consumption on -"){
      hide("kor1")
      hide("kor2")
      hide("ByHS")
      hide("top")
      show("Alc")
      hide("Region")
      temp_data<-data
      temp_data["Total_Alcohol_Consumption"] = temp_data["Beer_PerCapita"] + temp_data["Spirit_PerCapita"] + temp_data["Wine_PerCapita"]
      temp_data <- temp_data %>% 
        arrange(Total_Alcohol_Consumption)
      output$wykres1<-renderPlot({
        ggplot( temp_data, aes(x=Total_Alcohol_Consumption, y=HDI)) +
          geom_point(fill="grey",shape=21,size=3) +
          geom_line(linetype="dashed", color="purple",size=1)+
          geom_smooth(method = "lm", color = "red", size = 1, se = FALSE)+
          ggtitle("HDI of Total Alchohol Consumption")
      })
      output$wykres2<-renderPlot({
        ggplot( temp_data, aes(x=Total_Alcohol_Consumption, y=HappinessScore)) +
          geom_point(fill="grey",shape=21,size=3) +
          geom_line(linetype="dashed", color="purple",size=1)+
          geom_smooth(method = "lm", color = "red", size = 1, se = FALSE)+
          ggtitle("Happiness Score of Total Alcohol Consumption")
      })
      output$wykres3<-renderPlot({
        ggplot( temp_data, aes(x=Total_Alcohol_Consumption, y=GDP_PerCapita)) +
          geom_point(fill="grey",shape=21,size=3) +
          geom_line(linetype="dashed", color="purple",size=1)+
          geom_smooth(method = "lm", color = "red", size = 1, se = FALSE)+
          ggtitle("GDP Per Capita of Total Alcohol Consumption")
      })
      
      
      if(is.null(input$Alc)){
        hide("wykres2")
        hide("wykres3")
        hide("wykres1")
      }
      else if(length(input$Alc)==1){
        if(input$Alc=="1"){
          hide("wykres2")
          hide("wykres3")
          show("wykres1")
        }
        else if(input$Alc=="2"){
          hide("wykres1")
          hide("wykres3")
          show("wykres2")
        
        }
        else if(input$Alc=="3"){
          hide("wykres1")
          hide("wykres2")
          show("wykres3")
        }
      }
      else if(length(input$Alc)==2){
        if(input$Alc[1]=="1" && input$Alc[2]=="2"){
          show("wykres2")
          hide("wykres3")
          show("wykres1")
        }
        else if(input$Alc[1]=="1" && input$Alc[2]=="3"){
          show("wykres1")
          hide("wykres2")
          show("wykres3")
        }
        else if(input$Alc[1]=="2" && input$Alc[2]=="3"){
          hide("wykres1")
          show("wykres2")
          show("wykres3")
        }
      }
     else if(length(input$Alc)==3){
       show("wykres1")
       show("wykres2")
       show("wykres3")
      } 
    }
    else if(input$ploty=="Countries and their alcohol consumption"){
        show("Region")
        hide("kor1")
        hide("kor2")
        hide("ByHS")
        hide("top")
        hide("Alc")
        hide("wykres2")
        hide("wykres3")
        
        temp_data<-data
        temp1<- subset(temp_data, select = c("Region","Country", "Wine_PerCapita"))
        temp1["Alcohol_Consumption"] =temp_data["Wine_PerCapita"]
        temp2<- subset(temp_data, select = c("Region","Country", "Spirit_PerCapita"))
        temp2["Alcohol_Consumption"] =temp_data["Spirit_PerCapita"] 
        temp3<- subset(temp_data, select = c("Region","Country", "Beer_PerCapita"))
        temp3["Alcohol_Consumption"] =temp_data["Beer_PerCapita"]
        temp <- bind_rows(temp1, temp2, temp3)
          if(input$Region=="Sub-Saharan Africa"){
            temp<- subset(temp, Region == "Sub-Saharan Africa", select = c("Country", "Alcohol_Consumption"))
          }else if(input$Region=="Central and Eastern Europe"){
            temp <- subset(temp, Region == "Central and Eastern Europe", select = c("Country", "Alcohol_Consumption"))
          }
          else if(input$Region=="Latin America and Caribbean"){
            temp <- subset(temp, Region == "Latin America and Caribbean", select = c("Country", "Alcohol_Consumption"))
          }
          else if(input$Region=="Western Europe"){
            temp <- subset(temp, Region == "Western Europe", select = c("Country", "Alcohol_Consumption"))
          }
          else if(input$Region=="Middle East and Northern Africa"){
            temp <- subset(temp, Region == "Middle East and Northern Africa", select = c("Country", "Alcohol_Consumption"))
          }
          else if(input$Region=="North America"){
            temp <- subset(temp, Region == "North America", select = c("Country", "Alcohol_Consumption"))
          }
          else if(input$Region=="Australia and New Zealand"){
            temp <- subset(temp, Region == "Australia and New Zealand", select = c("Country", "Alcohol_Consumption"))
          }
          else if(input$Region=="Southeastern Asia"){
            temp <- subset(temp, Region == "Southeastern Asia", select = c("Country", "Alcohol_Consumption"))
            
          }else if(input$Region=="Eastern Asia"){
            temp <- subset(temp, Region == "Eastern Asia", select = c("Country", "Alcohol_Consumption"))
          }
        output$wykres1<-renderPlot({
        ggplot(temp, aes(x=Country, y=Alcohol_Consumption, fill=Country)) + 
          geom_boxplot(alpha=0.3) +
          theme(legend.position="none")+
            ggtitle(paste("Cuntries in", input$Region,"Alcohol Consumption"))+
            labs(y="Alcohol Consumption")+theme(text = element_text(size = 15, color = "black"),axis.text.x = element_text(angle = 30))
        })
      
      }
})
}

shinyApp(ui = ui, server = server)   