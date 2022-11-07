library(data.table)
library(dplyr)
library(ggplot2)
library(highcharter)
library(plotly)
library(leaflet)
library(visNetwork)
library(maps)
library(Metrics)
library(d3heatmap)
library(circlize)
library(waffle)
library(edgebundleR)
library(corrplot)
library(tidyr)
library(reshape2)
library(DT)
library(highcharter)
library(lubridate)
library(chorddiag)
library(viridisLite)
library(geosphere)
library(broom)


train <- fread("train.csv")
test <- fread("test.csv")


train[, filter:= 0]
test[, filter:= 1]

dataset <- bind_rows(train, test)

colSums(is.na(train))
colSums(is.na(test))


summary(dataset)

str(dataset)

hchart(dataset$vendor_id, type = "column", colorByPoint = T) %>% hc_colors(c("red", "blue")) %>% hc_title(text = "Counts of each Vendor Id") %>% hc_subtitle(text = "The vendor 2 had 200000 more trips as compared to vendor 1") %>% hc_add_theme(hc_theme_darkunica())
train %>% group_by(vendor_id) %>% summarise(Count = n()) %>% mutate(percent = Count/sum(Count)) %>% plot_ly(values = ~percent, labels = ~vendor_id, type = "pie") %>% layout(showlegend = T, title = "<b>Percentage of counts of both vendor</b>", xaxis = list(showgrid = F, zeroline = F, showline = F, showticklabels = F), yaxis = list(showgrid = F, zeroline = F, showline = F, showticklabels = F))



#hchart(dataset$store_and_fwd_flag, type = "column", colorByPoint = T) %>% hc_colors(c("red", "blue")) %>% hc_title(text = "Counts of each Vendor Id") %>% hc_subtitle(text = "The vendor 2 has  10000 more count than the vendor 1")

dataset[,":="(pickup_datetime = ymd_hms(pickup_datetime),
              dropoff_datetime = ymd_hms(dropoff_datetime),
              pick_year = year(pickup_datetime),
              pick_month = month(pickup_datetime),
              pick_day = day(pickup_datetime),
              pick_wday = wday(pickup_datetime),
              pick_hour = hour(pickup_datetime))]




hchart(density(train$pick_month),type = "area", color = "#B71C1C", name = "monthly distributin of rides") %>% hc_add_theme(hc_theme_ffx())
hchart(density(train$pick_wday),type = "area", color = "#c1c1c9", name = "weekly distributin of rides") %>% hc_add_theme(hc_theme_ffx())
hchart(density(train$pick_day), type = "area", color = "#f08080", name = "daily distributin of rides") %>% hc_add_theme(hc_theme_ffx())
hchart(density(train$pick_hour), type = "area", color = "#c8cbcc",name = "hourly distributin of rides") %>% hc_add_theme(hc_theme_ffx())



g <- train %>% group_by(pickup_latitude, pickup_longitude) %>% summarise(count = n()) %>% arrange(desc(count))

col_top <- colorNumeric(topo.colors(100), g$count[1:50])
leaflet(g[1:50,]) %>% addTiles() %>% addCircles(~pickup_longitude, ~pickup_latitude, color= ~col_top(count)) %>% addLegend("bottomright",pal = col_top, values= g$count[1:50], title = "50 most popular pickup points", opacity = 0.3)

train <- train %>% arrange(desc(trip_duration))
plot_ly(train[1:20,], x = ~passenger_count, y = ~trip_duration, text = ~paste(as.character(pickup_datetime))) %>% add_markers() %>% add_text(textfont = list(family = "sans serif", size = 18, colour = ~month), textposition = "top middle")%>% layout(title = "<b>Name of countries of top 20 runners</b>", xaxis = list(title = "<i>overall.position</i>"), yaxis = list(title = "<i>official.seconds</i>"))

g <- train[1:20,]
g$numb <- 1:20
leaflet(g) %>% addTiles() %>% addCircles(~pickup_longitude, ~pickup_latitude, color = "red", popup =~paste("pickup:", as.character(pickup_datetime), " numb: ", numb, "trip: ", trip_duration), radius = 30)%>% addCircles(~dropoff_longitude, ~dropoff_latitude, color = "blue", popup =~paste("dropoff:", as.character(dropoff_datetime), " numb: ", numb), radius = 30)   %>%addLegend("bottomright",colors = c("red", "blue"), label= c("<b>pickup</b>", "<b>dropoff</b>"), title = "Distance between pickup and dropoff points of outliers", opacity = 0.3)


g <- g[with(g, order(count)),]
col_bottom <- colorNumeric(topo.colors(200), g$count[1:50])
leaflet(g[1:50,]) %>% addTiles() %>% addMarkers(~pickup_longitude, ~pickup_latitude, color= ~col_bottom(count)) %>% addLegend("bottomright",pal = col_bottom, values= g$count[1:50], title = "50 most popular pickup points", opacity = 0.3)


col <- viridis(6)
col <- substr(col, 0, 7)

h <- train %>% group_by(pick_month) %>% summarise(count = n()) %>% hchart(h, hcaes(x = count, y = pick_month), type = "bar", colorByPoint = T) %>% hc_colors(col) %>% hc_title(text = "Counts of each Vendor Id") %>% hc_subtitle(text = "The vendor 2 had 200000 more trips as compared to vendor 1") %>% hc_add_theme(hc_theme_db())

train %>% group_by(pick_wday) %>% summarise(count = n()) %>% hchart(h, hcaes(x = count, y = pick_month), type = "bar", colorByPoint = T) %>% hc_colors(col) %>% hc_title(text = "Counts of each Vendor Id") %>% hc_subtitle(text = "The vendor 2 had 200000 more trips as compared to vendor 1") %>% hc_add_theme(hc_theme_db())



edgebundle(cor(dataset[filter == 0, c(2, 5, 6, 7,8, 9, 11, 13:16)]), cutoff = 0.2, tension = 0.5)

#radarDf <- gather(train, key = Label, value = trip_duration, -passenger_count) %>% spread(key = passenger_count, value = trip_duration)

#chartJSRadar(scores = radarDf, showLegend = F, showToolTipLabel = T, maxScale = 5)


dataset[, pickup_point := paste(pickup_latitude, pickup_longitude)]
dataset[, dropoff_point:= paste(dropoff_latitude, dropoff_longitude)]

train <- dataset[filter == 0, ]
test <- dataset[filter == 1, ]


networkDF <- train %>% 
  arrange(pickup_datetime) %>%
  group_by(pickup_point,dropoff_point) %>% 
  summarise(pickup = first(pickup_point), low_trip = min(trip_duration), high_trip = max(trip_duration), min_passenger = min(passenger_count), max_passenger = max(passenger_count), early_year = min(pick_month), late_year = max(pick_month), early_month= min(pick_day), late_month = max(pick_day), early_wday = min(pick_wday), late_wday = max(pick_day), vendor = first(vendor_id)) %>% 
  mutate(drop_prev = lag(dropoff_point), drop_now = dropoff_point, vendor_now = vendor, vendor_prev = lag(vendor))%>% 
  select(pickup, drop_prev, drop_now, low_trip, high_trip, min_passenger, max_passenger, early_year, late_year, early_month, late_month, early_wday, late_wday, vendor_now, vendor_prev) %>% as.data.frame()



visNetworkPerPickup <- function(df,seconds,passenger, mon, day, vendor){
    edges <- df %>% 
    filter(low_trip <= seconds) %>%
    filter(min_passenger <= passenger) %>%
    filter(early_month <= mon) %>% 
    filter(early_wday <= day) %>%
    filter(vendor_now == vendor) %>%
    select(pickup, drop_now) %>%
    rename(from = pickup) %>%
    rename(to = drop_now) %>%
    sample_n(30, replace = F) %>%
    ungroup() %>%
    mutate(arrows = "from")
  
  
  edgesMelt <- edges %>%
    mutate(shape = "") %>%
    melt(id = "shape", measure= c("to", "from"), value.name = "id")
  
  
  nodesPickup <- edgesMelt %>% 
    filter(variable == "from") %>%
    mutate(group = "pickup")
  
  
  nodesDrop <- edgesMelt %>%
    filter(variable == "to") %>%
    mutate(group = "drop")
  
  
  nodes <- rbind(nodesPickup, nodesDrop) %>% select(variable, id, group) %>% unique()
  
   visNetwork(nodes, edges, main = list(text = paste0("Pickup and drop off points"),
                                       style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;")) %>%
    visGroups( groupname = "pickup", color = "lightgreen") %>%
    visGroups( groupname = "drop", color = "lightblue") %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree =1), nodesIdSelection = T) %>%
    visInteraction(dragNodes = T, dragView = T, zoomView = T)  %>%
    visGroups(groupname = "pickup", shape = "icon", icon = list(code = "f0ac", size =100)) %>%
    visGroups(groupname = "drop", shape = "icon", icon = list(code = "f162", color = "green")) %>%
    addFontAwesome() %>%
    visInteraction(navigationButtons = TRUE) 
}


  SECONDS <- 200
  PASSENGER <- 2
  MONTH <- 4
  WDAY <- 2
  VENDOR <- 2

  edges <- networkDF %>% 
  filter(low_trip <= SECONDS) %>%
  filter(min_passenger <= PASSENGER) %>%
  filter(early_month <= MONTH) %>% 
  filter(early_wday <= WDAY) %>% 
  filter(vendor_now == VENDOR) %>%
  select(pickup, drop_now) %>%
  rename(from = pickup) %>%
  rename(to = drop_now) %>%
  ungroup() %>%
  mutate(arrows = "from")
  
    
  edgesMelt <- edges %>%
  mutate(shape = "") %>%
  melt(id = "shape", measure= c("to", "from"), value.name = "id")
  
  
  nodesPickup <- edgesMelt %>% 
  filter(variable == "from") %>%
  mutate(group = "pickup")
  
  
  nodesDrop <- edgesMelt %>%
  filter(variable == "to") %>%
  mutate(group = "drop")
  
  
  nodes <- rbind(nodesPickup, nodesDrop) %>% select(variable, id, group) %>% unique()
  

  visNetwork(nodes, edges, main = list(text = paste0("Pickup and drop off points"),
                                       style = "font-family:Comic Sans MS;color:#ff0000;font-size:15px;text-align:center;")) %>%
    visGroups( groupname = "pickup", color = "lightgreen") %>%
    visGroups( groupname = "drop", color = "lightblue") %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree =1), nodesIdSelection = T) %>%
    visInteraction(dragNodes = T, dragView = T, zoomView = T)  %>%
    visGroups(groupname = "pickup", shape = "icon", icon = list(code = "f0ac", size =100)) %>%
    visGroups(groupname = "drop", shape = "icon", icon = list(code = "f162", color = "green")) %>%
    addFontAwesome() %>%
    visInteraction(navigationButtons = TRUE) 




visNetworkPerPickup(networkDF, 200, 2, 4, 2)


TransferMatrix <- na.omit(networkDF)  %>%  sample_frac(0.001, replace = F) %>% ungroup() %>% group_by(drop_prev, drop_now) %>%
  summarise(sub = n()) %>% ungroup() %>%  na.omit()  %>%
  mutate_each(funs(factor), drop_prev:drop_now) %>% acast(drop_prev ~ drop_now, value.var = "sub") 



h <- dataset %>% group_by(passenger_count) %>% summarise(count = n())
col <- viridis(11)
col <- substr(col, 0, 7)

col <- c("limegreen", "black", "red", "green", "blue", "brown", "yellow","violet", "magenta", "purple", "pink")

parts1 <- c(`0` = 83, `1` = 1476987, `2` = 300345, `3` = 85582, `4` = 40421, `5` = 111499, `6` = 68854, `7` = 3, `8` = 1, `9` = 1)
waffle(parts1/10000, rows = 9, colors = col, legend_pos = "bottom", title = "Frequency of number of passengers", xlab= "1 square = 10000", pad =  2)

set.seed(123)

icons <- c("taxi", "bus", "plane", "motorcycle")

n <- sample(3:10, length(icons)) %>% 
  sort(decreasing = TRUE) %>% 
  {. *  seq(length(icons), 1) } 

hciconarray(icons, n, icons = icons, size = 5)



stats <- train %>% group_by(pick_month) %>% summarise(median = median(trip_duration), mean = mean(trip_duration))
stats <- train %>% group_by(pick_wday) %>% summarise(median = median(trip_duration), mean = mean(trip_duration))
stats <- train %>% group_by(pick_day) %>% summarise(median = median(trip_duration), mean = mean(trip_duration))
stats <- train %>% group_by(pick_hour) %>% summarise(median = median(trip_duration), mean = mean(trip_duration))

highchart() %>% hc_chart(type = "line") %>% hc_xAxis(categories = stats$pick_month) %>% hc_add_series(data = stats$mean, name = "Monthly Mean trip duration(in seconds)") %>% hc_add_series(data = stats$median, name = "Monthly Median trip duration(in seconds)")  %>%hc_title(text = "Center of target variable") %>% hc_subtitle(text = "Positive long-term Trend in trip duration") %>% hc_colors(c("limegreen", "red")) %>% hc_add_theme(hc_theme_economist())
highchart() %>% hc_chart(type = "line") %>% hc_xAxis(categories = stats$pick_wday) %>% hc_add_series(data = stats$mean, name = "Weekly Mean trip duration(in seconds)") %>% hc_add_series(data = stats$median, name = "Weekly Median trip duration(in seconds)")  %>% hc_title(text = "Center of target variable") %>% hc_subtitle(text = "Positive long-term Trend in trip duration") %>% hc_colors(c("limegreen", "red")) %>% hc_add_theme(hc_theme_economist())
highchart() %>% hc_chart(type = "line") %>% hc_xAxis(categories = stats$pick_day) %>% hc_add_series(data = stats$mean, name = "Daily Mean trip duration(in seconds)") %>% hc_add_series(data = stats$median, name = "DailyMedian trip duration(in seconds)")  %>% hc_title(text = "Center of target variable") %>% hc_subtitle(text = "Positive long-term Trend in trip duration") %>% hc_colors(c("limegreen", "red")) %>% hc_add_theme(hc_theme_economist())
highchart() %>% hc_chart(type = "line") %>% hc_xAxis(categories = stats$pick_hour) %>% hc_add_series(data = stats$mean, name = "Hourly Mean trip duration(in seconds)") %>% hc_add_series(data = stats$median, name = "Hourly Median trip duration(in seconds)")  %>% hc_title(text = "Center of target variable") %>% hc_subtitle(text = "Positive long-term Trend in trip duration") %>% hc_colors(c("limegreen", "red")) %>% hc_add_theme(hc_theme_economist())

train %>% plot_ly(y=~log(trip_duration),x = ~pick_month, type = "box", color = ~store_and_fwd_flag, colors = "Set1") %>% layout(boxmode = "group", title = "<b>Trip Duration(in seconds) by every value of stored variable</b>", xaxis = list(title = "<b><i>month</i></b>"), yaxis = list(title = "<b><i>trip duration</i></b>"))
train %>% plot_ly(y=~log(trip_duration),x = ~pick_wday, type = "box", color = ~store_and_fwd_flag, colors = "Set1") %>% layout(boxmode = "group", title = "<b>Trip Duration(in seconds) by every value of stored variable</b>", xaxis = list(title = "<b><i>wday</i></b>"), yaxis = list(title = "<b><i>trip duration</i></b>"))
train %>% plot_ly(y=~log(trip_duration),x = ~pick_day, type = "box", color = ~store_and_fwd_flag, colors = "Set1") %>% layout(boxmode = "group", title = "<b>Trip Duration(in seconds) by every value of stored variable</b>", xaxis = list(title = "<b><i>wday</i></b>"), yaxis = list(title = "<b><i>trip duration</i></b>"))
train %>% plot_ly(y=~log(trip_duration),x = ~pick_hour, type = "box", color = ~store_and_fwd_flag, colors = "Set1") %>% layout(boxmode = "group", title = "<b>Trip Duration(in seconds) by every value of stored variable</b>", xaxis = list(title = "<b><i>wday</i></b>"), yaxis = list(title = "<b><i>trip duration</i></b>"))


hchart(train,"scatter", hcaes(x = pickup_latitude, y = pickup_longitude), name = "Spread of coordinates in a space")

train %>% plot_ly(x = ~pickup_latitude, y= ~pickup_longitude, type = "scatter", mode = "markers")


sub <- train %>% filter(pick_hour %in% c(4,5)) %>% arrange(desc(trip_duration))
sub %>%  group_by(vendor_id) %>% summarise(count = n()) %>% mutate(percent = count/sum(count)) %>% plot_ly(values = ~percent, labels = ~vendor_id, type = "pie") %>% layout(showlegend = T, title = "<b>Percentage of counts of both vendor</b>", xaxis = list(showgrid = F, zeroline = F, showline = F, showticklabels = F), yaxis = list(showgrid = F, zeroline = F, showline = F, showticklabels = F))
sub$id <- as.numeric(as.factor(sub$id))
leaflet(sub[1:20, ]) %>% addTiles() %>% addCircles(~pickup_longitude, ~pickup_latitude, color = "red", popup =~paste("pickup:", as.character(pickup_datetime), " id: ", id, "trip: ", trip_duration), radius = 30)%>% addCircles(~dropoff_longitude, ~dropoff_latitude, color = "blue", popup =~paste("dropoff:", as.character(dropoff_datetime), " id: ", id), radius = 30)   %>%addLegend("bottomright",colors = c("red", "blue"), label= c("<b>pickup</b>", "<b>dropoff</b>"), title = "Distance between pickup and dropoff points", opacity = 0.3)


i <- cbind(longitude = dataset$pickup_longitude, latitude = dataset$pickup_latitude)
j <- cbind(longitude = dataset$dropoff_longitude, latitude= dataset$dropoff_latitude)

dataset[, distance := distHaversine(i, j)]
train <- dataset[filter == 0, ]
test <- dataset[filter == 1, ]


set.seed(0)

h <- train %>% sample_frac(0.005, replace = F)

m <- loess(trip_duration ~ distance, data = h)






h <- train %>% group_by(pick_day, vendor_id) %>% summarise(trip = mean(trip_duration), uniqueness = length(unique(pickup_point)))
h1 <- h %>% filter(vendor_id == 1)
h2 <- h %>% filter(vendor_id == 2)
hchart(h1, "treemap", hcaes(x = as.character(pick_day), value = trip, color = uniqueness)) %>% hc_title(text = "Daily mean trip Duration of both vendors") %>% hc_subtitle(text = "Vendor 2 is well ahead of Vendor 1 again")
hchart(h2, "treemap", hcaes(x = as.character(pick_day), value = trip, color = uniqueness)) %>% hc_title(text = "Daily mean trip Duration of both vendors") %>% hc_subtitle(text = "Vendor 2 is well ahead of Vendor 1 again")


h <- train %>% group_by(pick_wday, vendor_id) %>% summarise(trip = mean(trip_duration), uniqueness = length(unique(pickup_point)))
h1 <- h %>% filter(vendor_id == 1)
h2 <- h %>% filter(vendor_id == 2)
hchart(h1, "treemap", hcaes(x = as.character(pick_wday), value = trip, color = uniqueness)) %>% hc_title(text = "Daily mean trip Duration of both vendors") %>% hc_subtitle(text = "Vendor 2 is well ahead of Vendor 1 again")
hchart(h2, "treemap", hcaes(x = as.character(pick_wday), value = trip, color = uniqueness)) %>% hc_title(text = "Daily mean trip Duration of both vendors") %>% hc_subtitle(text = "Vendor 2 is well ahead of Vendor 1 again")


h <- train %>% group_by(pick_month, vendor_id) %>% summarise(trip = mean(trip_duration), uniqueness = length(unique(pickup_point)))
h1 <- h %>% filter(vendor_id == 1)
h2 <- h %>% filter(vendor_id == 2)
hchart(h1, "treemap", hcaes(x = as.character(pick_month), value = trip, color = uniqueness)) %>% hc_title(text = "Daily mean trip Duration of both vendors") %>% hc_subtitle(text = "Vendor 2 is well ahead of Vendor 1 again")
hchart(h2, "treemap", hcaes(x = as.character(pick_month), value = trip, color = uniqueness)) %>% hc_title(text = "Daily mean trip Duration of both vendors") %>% hc_subtitle(text = "Vendor 2 is well ahead of Vendor 1 again")


h <- train %>% group_by(pick_hour, vendor_id) %>% summarise(trip = mean(trip_duration), uniqueness = length(unique(pickup_point)))
h1 <- h %>% filter(vendor_id == 1)
h2 <- h %>% filter(vendor_id == 2)
hchart(h1, "treemap", hcaes(x = as.character(pick_hour), value = trip, color = uniqueness)) %>% hc_title(text = "Daily mean trip Duration of both vendors") %>% hc_subtitle(text = "Vendor 2 is well ahead of Vendor 1 again")
hchart(h2, "treemap", hcaes(x = as.character(pick_hour), value = trip, color = uniqueness)) %>% hc_title(text = "Daily mean trip Duration of both vendors") %>% hc_subtitle(text = "Vendor 2 is well ahead of Vendor 1 again")


#######################################################################################################################################################################

train %>% sample_frac(0.003)%>% plot_ly(x = ~log(trip_duration), y = ~log(distance), alpha = 0.3) %>% add_markers(marker = list(line = list(color = "black", width = 1))) %>%
  layout(
    title = "<b>Relation of journey time and distance</b>",
    xaxis = list(domain = c(0.1, 1), title = "<b><i>trip duration(log)</i></b>"),
    yaxis = list(title = "<b><i>distance(log)</i></b>"),
    updatemenus = list(
      list(
        y = 0.8,
        buttons = list(
          
          list(method = "restyle",
               args = list("type", "scatter"),
               label = "Scatter"),
          
          list(method = "restyle",
               args = list("type", "histogram2d"),
               label = "2D Histogram")))
))

####################################################################################################################

stats %>% plot_ly(x = ~pick_month) %>%
  add_lines(y = ~vendor1, name = "vendor1") %>%
  add_lines(y = ~vendor2, name = "vendor2", visible = F) %>%
  layout(
    title = "Drop down menus - Styling",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "trip duration"),
    updatemenus = list(
      list(
        y = 0.8,
        buttons = list(
          
          list(method = "restyle",
               args = list("line.color", "blue"),
               label = "Blue"),
          
          list(method = "restyle",
               args = list("line.color", "red"),
               label = "Red"))),
      
      list(
        y = 0.7,
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE, FALSE)),
               label = "vendor1"),
          
          list(method = "restyle",
               args = list("visible", list(FALSE, TRUE)),
               label = "vendor2")))
    )
  )



######################################################################################################################
x <- train
x$date <- as.Date(x$pickup_datetime)
f <- x %>% sample_frac(0.003, replace = F)
plot_ly(f, x = ~date) %>%
  add_lines(data = f %>% filter(vendor_id == 1), y = ~log(trip_duration), name = "vendor 1") %>%
  add_lines(data = f %>% filter(vendor_id == 2), y = ~log(trip_duration), name = "vendor 2") %>%
  layout(
    title = "Trip duration",
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 1,
            label = "YTD",
            step = "year",
            stepmode = "todate"),
          list(step = "all"))),
      
      rangeslider = list(type = "date")),
    
    yaxis = list(title = "journey"))



##############################################################################################################

h <- train %>% sample_frac(0.003, replace= F)



# create data
aval <- list()
for(step in 1:11){
  aval[[step]] <-list(visible = FALSE,
                      name = paste0('v = ', step),
                      x=log(h$trip_duration),
                      y=sin(step*log(h$trip_duration)))
}
aval[3][[1]]$visible = TRUE

# create steps and plot all traces
steps <- list()
p <- plot_ly()
for (i in 1:11) {
  p <- add_lines(p,x=aval[i][[1]]$x,  y=aval[i][[1]]$y, visible = aval[i][[1]]$visible, 
                 name = aval[i][[1]]$name, type = 'scatter', mode = 'lines', hoverinfo = 'name', 
                 line=list(color='00CED1'), showlegend = FALSE)
  
  step <- list(args = list('visible', rep(FALSE, length(aval))),
               method = 'restyle')
  step$args[[2]][i] = TRUE  
  steps[[i]] = step 
}  

# add slider control to plot
 p %>%
  layout(sliders = list(list(active = 3,
                             currentvalue = list(prefix = "Frequency: "),
                             steps = steps)))


#############################################################################################################################

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


d <- train  %>% sample_frac(0.001, replace= F) 
d$date <- as.Date(d$pickup_datetime)
f <- d %>% dplyr::group_by(date) %>% dplyr::summarise(med =median(trip_duration)) %>% as.data.frame()
d  <- d %>% left_join(f, by = c("date"))
d <- d %>%
  accumulate_by(~date)

d %>%
  plot_ly(
    x = ~date, 
    y = ~log(med),
    split = ~vendor_id,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  ) %>% 
  layout(
    xaxis = list(
      title = "Date",
      zeroline = F,
      range = c(8000,9000)
    ),
    yaxis = list(
      title = "Median",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    hide = F
  ) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  )


#########################################################################################################################

d1 <- as.Date("2016-06-01")
d2 <- as.Date("2016-06-30")
lag_30 <- train %>% mutate(date = as.Date(pickup_datetime)) %>% arrange(date)
lag_30 <- lag_30 %>% filter(date %in% c(d1:d2))
lag_30 <- lag_30 %>% group_by(date) %>% mutate(center = mean(trip_duration))
lag_3 <- unique(lag_30[, c("date","center")])
lag_3$id <- seq.int(nrow(lag_3))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- lag_3 %>%
  accumulate_by(~id)

 df %>%
  plot_ly(
    x = ~id, 
    y = ~center, 
    frame = ~frame,
    type = 'scatter', 
    mode = 'lines', 
    fill = 'tozeroy', 
    fillcolor='rgba(114, 186, 59, 0.5)',
    line = list(color = 'rgb(114, 186, 59)'),
    text = ~paste("Day: ", id, "<br>Close: s", center), 
    hoverinfo = 'text'
  ) %>%
  layout(
    title = "Median trip distribution of last 30 days",
    yaxis = list(
      title = "Seconds", 
      range = c(0,1400), 
      zeroline = F,
      tickprefix = "s"
    ),
    xaxis = list(
      title = "Day", 
      range = c(0,30), 
      zeroline = F, 
      showgrid = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Day "
    )
  )

################################################################################################################
 
 
h <- train %>% sample_frac(0.001, replace = F)
h$pick_date <- as.Date(h$pickup_datetime)
h$drop_date <- as.Date(h$dropoff_datetime)
 
 
p <- h  %>%  plot_ly(x = ~pick_date, y =~ drop_date, z =~trip_duration,  type = "heatmap", colorscale='Rainbow')

chart_types <- list(
  type = "buttons",
  direction = "right",
  xanchor = 'center',
  yanchor = "top",
  pad = list('r'= 0, 't'= 10, 'b' = 10),
  x = 0.5,
  y = 1.27,
  buttons = list(
    
    list(method = "restyle",
         args = list("type", "heatmap"),
         label = "Heatmap"),
    
    list(method = "restyle",
         args = list("type", "contour"),
         label = "Contour"),
    
    list(method = "restyle",
         args = list("type", "surface"),
         label = "Surface")
  ))

# color option buttons  
color_types <- list(
  type = "buttons",
  direction = "right",
  xanchor = 'center',
  yanchor = "top",
  pad = list('r'= 0, 't'= 10, 'b' = 10),
  x = 0.5,
  y = 1.17,
  buttons = list(
    
    list(method = "restyle",
         args = list("colorscale", "Rainbow"),
         label = "Rainbow"),
    
    list(method = "restyle",
         args = list("colorscale", "Jet"),
         label = "Jet"),
    
    list(method = "restyle",
         args = list("colorscale", "Earth"),
         label = "Earth"),
    
    list(method = "restyle",
         args = list("colorscale", "Electric"),
         label = "Electric")
  ))

annot <- list(list(text = "Chart<br>Type", x=0.2, y=1.25, xref='paper', yref='paper', showarrow=FALSE),
              list(text = "Color<br>Type", x=0.2, y=1.15, xref='paper', yref='paper', showarrow=FALSE))

# plot
 p %>% layout(
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"),
  updatemenus = list(chart_types,color_types),
  annotations = annot)
 
 
 ################################################################################################################################################
 
 
set.seed(23)
train %>% sample_frac(0.003, replace = F) %>%
   plot_ly(
     x = ~pickup_latitude, 
     y = ~pickup_longitude, 
     size = ~trip_duration, 
     color = ~as.factor(vendor_id), 
     frame = ~pick_month, 
     text = ~pickup_point, 
     hoverinfo = "text",
     type = 'scatter',
     mode = 'markers', 
     colors = "Set1"
   ) %>%
   animation_opts(
     1000, easing = "elastic", redraw = FALSE
   ) %>% 
   animation_button(
     x = 1, xanchor = "right", y = 0, yanchor = "bottom"
   ) %>%
   animation_slider(
     currentvalue = list(prefix = "MONTH ", font = list(color="red"))
   )
 

###################################################################################################################################################


# The function used to create the plots
sanktify <- function(x) {
  
  # Create nodes DF with the unique sources & targets from input
  
  
  nodes <- data.frame(unique(c(x$source,x$target)),stringsAsFactors=FALSE)
 
  nodes$ID <- as.numeric(rownames(nodes)) - 1 
  names(nodes) <- c("name", "ID")
  

  links <- inner_join(x, nodes, by = c("source"="name")) %>%
    rename(source_ID = ID) %>%
    inner_join(nodes, by = c("target"="name")) %>%
    rename(target_ID = ID) 
  
  # Create Sankey Plot
  sank <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source_ID",
    Target = "target_ID",
    Value = "value",
    NodeID = "name",
    units = "USD",
    fontSize = 12,
    nodeWidth = 30
  )
  
  return(sank)
  
}

############################################################################################################################

fixtable <- function(...) {
  tab <- table(...)
  if (substr(colnames(tab)[1],1,1) == "_" &
      substr(rownames(tab)[1],1,1) == "_") {
    tab2 <- tab
    colnames(tab2) <- sapply(strsplit(colnames(tab2), split=" "), `[`, 1)
    rownames(tab2) <- sapply(strsplit(rownames(tab2), split=" "), `[`, 1)
    tab2[1,1] <- 0
    # mandat w klubie
    for (par in names(which(tab2[1,] > 0))) {
      delta = min(tab2[par, 1], tab2[1, par])
      tab2[par, par] = tab2[par, par] + delta
      tab2[1, par] = tab2[1, par] - delta
      tab2[par, 1] = tab2[par, 1] - delta
    }
    # przechodzi przez niezalezy
    for (par in names(which(tab2[1,] > 0))) {
      tab2["niez.", par] = tab2["niez.", par] + tab2[1, par]
      tab2[1, par] = 0
    }
    for (par in names(which(tab2[,1] > 0))) {
      tab2[par, "niez."] = tab2[par, "niez."] + tab2[par, 1]
      tab2[par, 1] = 0
    }
    
    tab[] <- tab2[] 
  }
  tab
}

flow2 <- rbind(
  data.frame(fixtable(z = paste0(h$, " day1"), do = paste0(dat$day2_state, " day2"))),
  data.frame(fixtable(z = paste0(dat$day2_state, " day2"), do = paste0(dat$day3_state, " day3"))),
  data.frame(fixtable(z = paste0(dat$day3_state, " day3"), do = paste0(dat$day4_state, " day4"))),
  data.frame(fixtable(z = paste0(dat$day4_state, " day4"), do = paste0(dat$day5_state, " day5"))),
  data.frame(fixtable(z = paste0(dat$day5_state, " day5"), do = paste0(dat$day6_state, " day6"))),
  data.frame(fixtable(z = paste0(dat$day6_state, " day6"), do = paste0(dat$day7_state, " day7"))),
  data.frame(fixtable(z = paste0(dat$day7_state, " day7"), do = paste0(dat$day8_state, " day8"))),
  data.frame(fixtable(z = paste0(dat$day8_state, " day8"), do = paste0(dat$day9_state, " day9"))),
  data.frame(fixtable(z = paste0(dat$day9_state, " day9"), do = paste0(dat$day10_state, " day10"))))



