library(googleVis)
library(dplyr)
library(reshape2)
library(highcharter)
library(visNetwork)

setwd("~/Courses/R/Refugees Europe")

refugees <- read.csv("unhcr_popstats_export_asylum_seekers_monthly_2016_05_25_195810.csv", sep = ",", 
                     skip = 3, header = TRUE, stringsAsFactors = FALSE)
str(refugees)
refugees$Value <- as.integer(refugees$Value)
sum(refugees$Value, na.rm = TRUE)

#Remove the NA's:
refugees <- refugees[complete.cases(refugees), ]
colnames(refugees)[1] <- "Country"
#We have 2 USA in the data, merge them together:

refugees$Country <- replace(refugees$Country, refugees$Country=="USA (INS/DHS)", "USA")
refugees$Country <- replace(refugees$Country, refugees$Country=="USA (EOIR)", "USA")

refugees$Origin <- replace(refugees$Origin, refugees$Origin == "United States of America", "USA")
refugees$Country <- replace(refugees$Country, refugees$Country == "United Kingdom of Great Britain and Northern Ireland", "UK")
refugees$Origin <- replace(refugees$Origin, refugees$Origin == "United Kingdom of Great Britain and Northern Ireland", "UK")


refugees <- aggregate(Value~Country+Origin+Year+Month, data = refugees, FUN=sum)

#The data is cleaned up. Let's do the top 7 of countries of origin:
ref_top7_yearly <- data.frame(refugees %>%
                                group_by(Year, Origin) %>%
                                summarize(Total = sum(Value)) %>%
                                top_n(7, wt = Total))
str(ref_top7_yearly)
ref_top7_yearly$Year <- as.integer(ref_top7_yearly$Year)
ref_top7_yearly$Year <- as.character(ref_top7_yearly$Year)

#Remake ref_top7_yearly to a dcast data frame (to be used in highcharter):
d_top7 <- dcast(ref_top7_yearly, Year ~ Origin, value.var = "Total")

d_top7[is.na(d_top7)] <- 0
colnames(d_top7) <- c("Year", "Afghanistan", "Albania", "China", 
                      "Eritrea", "Haiti", "India", "Iran", "Iraq", 
                      "Mexico", "Nigeria", "Pakistan", "Russia", 
                      "SerbiaKosovo", "Somalia", "SriLanka", "Syria", 
                      "Turkey", "Unknown")

col_pal <- c("#E0F2F1", "#B2DFDB", "#80CBC4", "#4DB6AC", 
             "#26A69A", "#009688","#00897B", "#00796B", "#EEEEEE", 
             "#E0E0E0", "#9E9E9E", "#757575","#E0F7FA","#0097A7",
             "#006064","#424242", "#607D8B", "#263238")

hc_ref <- highchart() %>%
  hc_xAxis(categories = d_top7$Year) %>%
  hc_add_series(name = "Afghanistan", data = d_top7$Afghanistan, type = "column", color = col_pal[1]) %>%
  hc_add_series(name = "Albania", data = d_top7$Albania, type = "column", color = col_pal[2]) %>%
  hc_add_series(name = "China", data = d_top7$China, type = "column", color = col_pal[3]) %>%
  hc_add_series(name = "Eritrea", data = d_top7$Eritrea, type = "column", color = col_pal[4]) %>%
  hc_add_series(name = "Haiti", data = d_top7$Haiti, type = "column", color = col_pal[5]) %>%
  hc_add_series(name = "India", data = d_top7$India, type = "column", color = col_pal[6]) %>%
  hc_add_series(name = "Iran", data = d_top7$Iran, type = "column", color = col_pal[7]) %>%
  hc_add_series(name = "Iraq", data = d_top7$Iraq, type = "column", color = col_pal[8]) %>%
  hc_add_series(name = "Mexico", data = d_top7$Mexico, type = "column", color = col_pal[9]) %>%
  hc_add_series(name = "Nigeria", data = d_top7$Nigeria, type = "column", color = col_pal[10]) %>%
  hc_add_series(name = "Pakistan", data = d_top7$Pakistan, type = "column", color = col_pal[11]) %>%
  hc_add_series(name = "Russia", data = d_top7$Russia, type = "column", color = col_pal[12]) %>%
  hc_add_series(name = "Serbia & Kosovo", data = d_top7$SerbiaKosovo, type = "column", color = col_pal[13]) %>%
  hc_add_series(name = "Somalia", data = d_top7$Somalia, type = "column", color = col_pal[14]) %>%
  hc_add_series(name = "Sri Lanka", data = d_top7$SriLanka, type = "column", color = col_pal[15]) %>%
  hc_add_series(name = "Syria", data = d_top7$Syria, type = "column", color = col_pal[16]) %>%
  hc_add_series(name = "Turkey", data = d_top7$Turkey, type = "column", color = col_pal[17]) %>%
  hc_add_series(name = "Various & Unknown", data = d_top7$Unknown, type = "column", color = col_pal[18])
hc_ref

hc_ref %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_xAxis(title = list(text = "Year (1999-2016/03)")) %>%
  hc_yAxis(title = list(text = "Total number of refugees")) %>%
  hc_title(text = "Countries of origin (yearly top 7)")
  
#Do the same for the top 7 most popular asylum countries: 
asy_top7_yearly <- data.frame(refugees %>%
                                group_by(Year, Country) %>%
                                summarize(Total = sum(Value)) %>%
                                top_n(7, wt = Total))
str(asy_top7_yearly)
asy_top7_yearly$Year <- as.integer(asy_top7_yearly$Year)
asy_top7_yearly$Year <- as.character(asy_top7_yearly$Year)

#Remake ref_top7_yearly to a dcast data frame:
c_top7 <- dcast(asy_top7_yearly, Year ~ Country, value.var = "Total")

c_top7[is.na(c_top7)] <- 0
colnames(c_top7)[c(11,15)] <- c("SerbiaKosovo", "UK")

col_pal2 <- c("#E0F2F1", "#B2DFDB", "#80CBC4", "#4DB6AC", "#26A69A", 
             "#009688","#00897B", "#00796B", "#EEEEEE", "#E0E0E0", 
             "#9E9E9E", "#757575","#424242", "#607D8B", "#263238")
hc_asy <- highchart() %>%
  hc_xAxis(categories = c_top7$Year) %>%
  hc_add_series(name = "Austria", data = c_top7$Austria, type = "column", color = col_pal2[1]) %>%
  hc_add_series(name = "Belgium", data = c_top7$Belgium, type = "column", color = col_pal2[2]) %>%
  hc_add_series(name = "Canada", data = c_top7$Canada, type = "column", color = col_pal2[3]) %>%
  hc_add_series(name = "France", data = c_top7$France, type = "column", color = col_pal2[4]) %>%
  hc_add_series(name = "Germany", data = c_top7$Germany, type = "column", color = col_pal2[5]) %>%
  hc_add_series(name = "Greece", data = c_top7$Greece, type = "column", color = col_pal2[6]) %>%
  hc_add_series(name = "Hungary", data = c_top7$Hungary, type = "column", color = col_pal2[7]) %>%
  hc_add_series(name = "Italy", data = c_top7$Italy, type = "column", color = col_pal2[8]) %>%
  hc_add_series(name = "Netherlands", data = c_top7$Netherlands, type = "column", color = col_pal2[9]) %>%
  hc_add_series(name = "Serbia & Kosovo", data = c_top7$SerbiaKosovo, type = "column", color = col_pal2[10]) %>%
  hc_add_series(name = "Sweden", data = c_top7$Sweden, type = "column", color = col_pal2[11]) %>%
  hc_add_series(name = "Switzerland", data = c_top7$Switzerland, type = "column", color = col_pal2[12]) %>%
  hc_add_series(name = "Turkey", data = c_top7$Turkey, type = "column", color = col_pal2[13]) %>%
  hc_add_series(name = "UK", data = c_top7$UK, type = "column", color = col_pal2[14]) %>%
  hc_add_series(name = "USA", data = c_top7$USA, type = "column", color = col_pal2[15])
hc_asy

hc_asy %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_xAxis(title = list(text = "Year (1999-2016/03)")) %>%
  hc_yAxis(title = list(text = "Total number of asylum seekers")) %>%
  hc_title(text = "Countries of asylum (yearly top 7)")


#For the visNetork
#We need to do some things: 
#1. Group all origin countries and label them refugees and summarize.
#2. Group all asylum countries and label them asylum seeking country and 
#summarize.
#3. Calculate how many for each origin went to each asylym country.
#4. Merge the data frames all together
#5. Look only to 2015 data:

origin_2015 <- data.frame(refugees %>%
  subset(Year == 2015) %>%
  group_by(Origin, Year) %>%
  summarize(Total = sum(Value)))
colnames(origin_2015) <- c("Origin", "Year", "Origin_total")

country_2015 <- data.frame(refugees %>%
  subset(Year == 2015) %>%
  group_by(Country, Year) %>%
  summarize(Total = sum(Value)))
colnames(country_2015) <- c("Country", "Year", "Country_total")

origin_country_2015 <- data.frame(refugees %>%
  subset(Year == 2015) %>%
    group_by(Origin, Country, Year) %>%
    summarize(Total = sum(Value)))

#Data looks fine, merge the three files into one superfile: 
total_2015 <- merge(origin_2015, origin_country_2015, by=c("Origin", "Year"))
total_2015 <- merge(total_2015, country_2015, by=c("Country", "Year"))

#make a data frame with all the unique countries (this can be done oh so 
#much easier than I did...): 
countries <- data.frame(total_2015$Country)
colnames(countries) <- "Country"
origin <- data.frame(total_2015$Origin)
colnames(countries) <- "Country"
countries <- data.frame(countries[!duplicated(countries),])
colnames(countries) <- "Country"
origin <- data.frame(Country = origin[!duplicated(origin),])
colnames(origin) <- "Country"
countries_origin <- rbind(countries, origin)
countries_origin <- as.character(countries_origin[order(as.character(countries_origin$Country)),])
countries_origin <- data.frame(countries_origin)
colnames(countries_origin) <- "Country"
countries_origin <- data.frame(countries_origin[!duplicated(countries_origin),])
countries_origin$id <- seq.int(nrow(countries_origin))
colnames(countries_origin) <- c("Country", "id")

total_2015 <- merge(total_2015, countries_origin, by.x = "Country", by.y = "Country")
total_2015 <- merge(total_2015, countries_origin, by.x = "Origin", by.y = "Country")

colnames(total_2015)[c(7,8)] <- c("To", "From")
total_2015 <- total_2015[,c(1,8,2,7,3:6)]

#Create a node and edge data frame:
node_2015 <- (data.frame(Country = countries_origin$Country, 
           Origin_total = total_2015[match(countries_origin$Country,
                                           total_2015$Origin), 6]))
node_2015 <- data.frame(Country = node_2015$Country,
                        Origin_total = node_2015$Origin_total,
                        Country_total = total_2015[match(countries_origin$Country,
                                            total_2015$Country), 8])
node_2015$id <- seq.int(nrow(node_2015))
str(node_2015)
node_2015[is.na(node_2015)] <- 0

node_2015$group <- ifelse(node_2015$Origin_total == 0 & node_2015$Country_total >0, "Asylum Country",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total == 0, "Refugee Country",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 100, "Mainly Refugee Country",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 10, "Dual Flow Country",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Country_total/node_2015$Origin_total > 100, "Mainly Asylum Country", "Dual Flow Country")))))

node_2015$value <- ifelse(node_2015$Origin_total > node_2015$Country_total, node_2015$Origin_total, node_2015$Country_total)
node_2015$color <- ifelse(node_2015$Origin_total == 0 & node_2015$Country_total >0, "#80CBC4",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total == 0, "#EF9A9A",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 100, "#FFE082",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 10, "#FFF59D",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Country_total/node_2015$Origin_total > 100, "#B2DFDB", "#FFF59D")))))
node_2015$title <- paste0("<p>",node_2015$Country," ","2015:","<br>",
                          "Refugees to ",node_2015$Country,": ",
                          node_2015$Country_total,"<br>",
                          "Asylum seekers coming from ",node_2015$Country,
                          ":",node_2015$Origin_total,"</p>", sep="")
node_2015$shadow <- FALSE
node_2015$shape <- ifelse(node_2015$Origin_total == 0 & node_2015$Country_total >0, "dot",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total == 0, "triangle",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 100, "triangle",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 10, "square",
                   ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Country_total/node_2015$Origin_total > 100, "dot", "square")))))


node_2015_2 <- node_2015[,c(4, 1, 5, 6, 10, 8, 7, 9)]
colnames(node_2015_2)[2] <- "label"

#Done with the node, let's do the edges:
edge_2015 <- total_2015
edge_2015$arrows <- "to"
edge_2015$smooth <- TRUE
edge_2015$shadow <- FALSE
edge_2015$dashes <- FALSE
edge_2015$title <- paste0(edge_2015$Origin, " to ", edge_2015$Country, ": ", edge_2015$Total, sep = "")
edge_2015$label <- c(as.character(edge_2015$Total))


edge_2015_2 <- edge_2015[, c(2, 4, 9, 12, 13, 10, 11)]
colnames(edge_2015_2)[c(1,2)] <- c("from", "to")

#Done with edges, let's do the network:
visnetwork_refugees <- visNetwork(node_2015_2, edge_2015_2, width = "100%", height = "600px") %>%
  visOptions(nodesIdSelection = TRUE, selectedBy = "group", highlightNearest = list(enabled = TRUE, degree = 1)) %>%
  visEdges(physics = FALSE, arrows =list(to = list(enabled = TRUE, 
                                      scaleFactor = 0.5))) %>%
  visIgraphLayout(type = "full", layout = "layout_in_circle") %>%
  visInteraction(hover = TRUE) #%>%
visnetwork_refugees

#If we want to we can save it in a html file:
visSave(visnetwork_refugees, file = "refugees_network.html")