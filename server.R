#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(leaflet)
library(treemap)
library(shinythemes)




load("~/CPS-Analytics/2020 Fall Quarter/ALY 6070/team/Tree_1/tree.RData")# load the dataframe
tree$created_at <- as.Date(tree$created_at, "%m/%d/%Y")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        
    
        # generate bins based on input$bins from ui.R
        output$Plot <-  renderPlot({
          
           u <- tree %>%
               filter(health !="") %>% 
               filter(guards !="") %>% 
               group_by(health,guards) %>% 
               count() %>% 
               filter(#health %in% input$Health,
                      guards %in% input$Guard)
               
           ggplot(u, aes(x = health,y = n, fill = guards)) +
           geom_bar(stat = "identity",position = "dodge") +
           geom_text(aes(label = n, y=n),position = position_dodge(1),
                     family = "Georgia", size = 3, vjust = -1.5)+
           #guides(fill=FALSE) + 
           ylab("count") + 
           ggtitle(" Health influenced by each guards type")
            
        })
        
        output$PlotTS <-  renderPlot({
            
            selectdate <- seq.Date(input$Date[1],input$Date[2], by = "weeks")
            
            tree %>%
            filter(status %in% input$Status, user_type %in% input$User_Type ,
                   created_at %in% selectdate,borough %in% input$Borough) %>% 
            ggplot(aes(created_at,colour = factor(user_type))) +
            geom_line(stat = "count") + 
                xlab("Date") + 
                ylab("Value") +
                theme_minimal() +
                theme(legend.title = element_blank())+
            facet_grid(rows = vars(borough),scales = "free_x")+
            ggtitle("Condition of Trees in each Borough")
        
        
})
       
        
        output$STmap <-  renderPlot({
            
            counties <- rgdal::readOGR("~/CPS-Analytics/2020 Fall Quarter/ALY 6070/team/Tree_1/nybb", layer="nybb")
            
            sp::coordinates(tree) = ~longitude+latitude
            
            sp::proj4string(tree)<-sp::CRS("+proj=longlat +datum=NAD83")
            ntree<- sp::spTransform(tree, sp::CRS(sp::proj4string(counties)))
            
            ntree<-data.frame(ntree)
            
            s <- names(head(sort(table(ntree$spc_common), decreasing = TRUE), n=5))
            ntree$spc_common <- ifelse(ntree$spc_common != s,"others",s)
            
            d <- ntree %>%
                #select(spc_common) %>% 
                filter(spc_common != "others") %>%
                #table()
                filter(spc_common %in% input$Species,borough %in% input$Boroughd)
            
            
            ggplot(#data = d, aes(x = longitude, y = latitude, fill=spc_common)
                ) +
                geom_polygon(data=counties[which(counties$BoroName %in% input$Boroughd),],
                             aes(x=long, y=lat, group=group), 
                             fill="grey40", colour="grey90", alpha=1) +
                labs(x="", y="", title="Locations of the Largest 5 Species of Trees")+ #labels
                theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
                      axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
                      plot.title = element_text(lineheight=.8, face="bold", vjust=1))+ # make title bold and add space
                geom_point(aes(x=longitude, y=latitude, color=factor(spc_common)), 
                           data=d, alpha=0.5, size=0.3)+ 
                #scale_colour_gradient() +
                #labs(shape="Speices of Tree") +
                scale_color_discrete(name = "Species of Tree") +
                coord_equal(ratio=1) # square plot to avoid the distortion
        })
        
        output$PlotH <- renderPlot({
            
            s <- names(head(sort(table(tree$spc_common), decreasing = TRUE), n=5))
            
            h1 <- tree %>% 
                select(health,spc_common)%>% 
                filter(health != "") %>% 
                group_by(spc_common, health) %>%
                count() %>%
                group_by(spc_common) %>% 
                transmute(health,Percent = round(n/sum(n),2)) %>% 
                filter(spc_common %in% s) 
            
            p1<- ggplot(h1, aes(x = spc_common,y= Percent,fill=health)) +
                geom_bar(stat = "identity",position = "dodge") +
                geom_text(aes(label = Percent, y=Percent),position = position_dodge(1),
                          family = "Georgia", size = 3, hjust = 0.2) +
                xlab("Species") +
                ylab("Percent of health condition") +
                coord_flip() +
                theme_minimal() +
                ggtitle("The largest number of 5 species of health conditions ")
            
            
            h2 <- tree %>% 
                select(health,spc_common) %>% 
                group_by(spc_common,health) %>% 
                filter(health !="") %>% 
                filter(spc_common !="") %>% 
                count() %>% 
                group_by(spc_common) %>% 
                transmute(health,percent = round(n/sum(n),2)) 
            h3<- h2[order(h2$percent,decreasing=TRUE),][1:5,]
            h4 <- h2 %>% filter(spc_common %in% h3$spc_common)
            p2 <- ggplot(h4, aes(x = spc_common,y= percent,fill=health)) +
                geom_bar(stat = "identity",position = "dodge") +
                geom_text(aes(label = percent, y=percent),position = position_dodge(1),
                          family = "Georgia", size = 3, hjust = 0.2) +
                xlab("Species") +
                ylab("Percent of health condition") +
                guides(fill=FALSE) +
                coord_flip() +
                theme_minimal() +
                ggtitle("Species of Top 5 Good Health")
            gridExtra::grid.arrange(p1, p2, ncol = 2)   
            
        })
        
        
        output$mymap <- renderLeaflet({
            t <- tree %>% 
                filter(zip_city == input$Zip_City)#%>%group_by(created_at)
                
            getColor <- function(x) {
                lapply(x$status,function(status){
                    if(status == "Alive"){
                        "green"
                    } else if (status == "Dead") {
                        "red"
                    } else {
                        "orange"
                    }
                })
            }
           
            icons <- awesomeIcons(
                icon = "tree",
                library = "fa",
                markerColor  = getColor(t)
            )
            
            
           m <- leaflet() %>% 
                setView(lng = min(t$longitude), lat = max(t$latitude), zoom = 2)  %>% 
                #setting the view over ~ center of New york
                addTiles() %>%
                addAwesomeMarkers(lng = t$longitude, 
                                  lat = t$latitude,
                                  icon = icons,
                                  label = t$status,
                                  clusterOptions = markerClusterOptions()
                                  )
           m
           
        })
            
        
        output$PlotT <- renderPlot({
            
            Gtree <- tree %>%
                filter(status != "Alive") %>% 
                select(borough,zip_city) %>%
                group_by(borough,zip_city) %>%
                summarise(value = n())
                #mutate(value = n())
                #filter(status == "Alive") %>%
                 #%>%
                #aggregate(x=c('status'), by = list(borough, zip_city), FUN=sum)
            
            treemap(# Data
                    Gtree, 
                    index = c("borough","zip_city"),
                    vSize = "value",
                    
                    # Main
                    title="Number of Non-Alive trees in each borough and city",
                    palette="Dark2",
                    
                    # Borders:
                    border.col=c("black", "grey"),             
                    border.lwds=c(1,0.5),                         
                    
                    # Labels
                    fontsize.labels=c(0.7, 0.4),
                    fontcolor.labels=c("white", "black"),
                    fontface.labels=1,            
                    bg.labels=c("transparent"),              
                    align.labels=list( c("left", "top"), 
                                       c("right", "bottom")),                                  
                    overlap.labels=0.5,
                    inflate.labels=T   
                    )
})
        output$Plotp1 <- renderLeaflet({
            
            pr <- tree %>% 
                 filter(problems !="" & problems!="None") 
            prn <- names(head(sort(table(pr$problems),decreasing = TRUE), n=5))
            problempal <- colorFactor(topo.colors(5), prn)
            
             pr %>% 
                 filter(problems %in% prn) %>%
                 filter(problems == input$Problems) %>% 
                 leaflet() %>% 
                 addTiles() %>%
                 addCircleMarkers(
                     color = ~problempal(problems),
                     stroke = FALSE, fillOpacity = 0.5,
                     clusterOptions = markerClusterOptions()
                 )
            })
        
        output$Plotp2<-renderPlot({
            
           pr2<- tree %>% 
               select(borough,sidewalk,problems) %>% 
                group_by(borough,sidewalk,problems) %>% 
                count() %>% 
               filter(sidewalk !="" & problems !="") 
           pr3<- pr2 %>% 
               filter(problems !="None",sidewalk !="NoDamage") %>%
               group_by(borough,problems) %>% 
               select(borough,problems,n) %>%
               #filter(rank(desc(n)) <= 5) %>% 
               arrange(borough,desc(n)) %>% 
               group_by(borough) %>% 
               slice_max(n,n=5)
          pr3plot <- ggplot(pr3, aes(x=borough,y=n,fill=problems)) +
              geom_bar(stat = "identity",position = "dodge")+
              xlab("Borough") +
              ylab("Count") +
              theme_minimal() +
              ggtitle("Problems of Top 5 in each Borough with Damage Sidewalk")
            
          pr4 <-pr2 %>% 
              filter(problems !="None",sidewalk !="Damage") %>%
              group_by(borough,problems) %>% 
              select(borough,problems,n) %>%
              #filter(rank(desc(n)) <= 5) %>% 
              arrange(borough,desc(n)) %>% 
              group_by(borough) %>% 
              slice_max(n,n=5)
          pr4plot <- ggplot(pr4, aes(x=borough,y=n,fill=problems)) +
              geom_bar(stat = "identity",position = "dodge")+
              xlab("Borough") +
              ylab("Count") +
              theme_minimal() +
              ggtitle("Problems of Top 5 in each Borough with NoDamage Sidewalk")
          gridExtra::grid.arrange(pr3plot, pr4plot, nrow = 2) 
          
        })
        
})
