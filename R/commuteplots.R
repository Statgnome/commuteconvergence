#This function will use ggplot2 to make plots with very little input,
#some basic histograms of times, boxplots by category.


commuteplots <- function(commutedf, Leave ="Left", Arrive ="ArriveDesk",
                         Time = "Total", Group = "Day",timebins=6){



  #This if else statement allows the user to input either
  #their leave and arrival times as seen on a clock,
  #or a total time in minutes.
  if(missing(Leave) | missing(Arrive)) {
    mintot <- commutedf[,Time] %>% min()
    maxtot <- commutedf[,Time] %>% max()
    commutedf <- commutedf %>% dplyr::select(Time,Group) %>% data.frame()
      } else {
        #converting times to POSIX and getting total minutes,
        #stratifying times by group
    Arrive <- commutedf[,Arrive][[1]] %>% as.POSIXct(format = "%H:%M:%S")
    Depart <- commutedf[,Leave][[1]] %>% as.POSIXct(format = "%H:%M:%S")
    Time <- difftime(Arrive,Depart,units= "mins") %>% as.numeric()

    mintot <- Time %>% min()
    maxtot <- Time %>% max()
    commutedf <- data.frame(Time,commutedf %>% dplyr::select(Group,Leave))

    #kmeans cluster by length of drive to compare to time left
    Expanse <- difftime(Depart,min(Depart),units="mins") %>% as.numeric()
    clusterdf <- Time %>% data.frame(Expanse)
    timecluster <- clusterdf %>% kmeans(timebins)
    commutedf <- commutedf %>% dplyr::mutate(Cluster = timecluster[[1]],Expanse = Expanse)

          }

  #ensures that the days of the week will be ordered as expected
  #when things are compared by day of the week
  if("Day" %in% Group){
    commutedf[,Group] <- commutedf[,Group] %>% factor(levels = c("Monday","Tuesday","Wednesday",
                                                                 "Thursday","Friday"))}



  #This screens the data for possible length of commute problems.
  if(maxtot - mintot >= 66){
    bins = 3
  }else{
    bins = 2
    }

  #Makes a histogram of all the times brought in for overall comparison.
  histogram <- ggplot2::ggplot(commutedf,
                               ggplot2::aes(commutedf[,1], fill = ..count..)) +
    ggplot2::geom_histogram(show.legend = T,binwidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Commute Time, Minutes",y = "Frequency") +
    ggplot2::ggtitle("Commute Time Histogram") +
    #This is where you get the color variation in conjunction with fill in the aes.
    ggplot2::scale_fill_gradient("Frequency", low = "green", high = "red") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks =
                                  seq(mintot - 1, maxtot + 1, by = bins))
  plot(histogram)

  if(missing(Leave) == FALSE){

  scatter <- ggplot2::ggplot(commutedf,
                             ggplot2::aes(commutedf[,3],commutedf[,1], color=Cluster)) +
    ggplot2::scale_color_gradient(low="blue", high="red") +
    ggplot2::geom_point(size=5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Time Left for Work",y = "Commute Time, Minutes") +
    ggplot2::ggtitle("Commute Time Scatterplot") +
    ggplot2::theme_bw()


  plot(scatter)

  box <-  ggplot2::ggplot(commutedf,
                          ggplot2::aes(as.factor(commutedf[,4]), commutedf[,1],
                                       colour = as.factor(commutedf[,4]))) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title=paste("Commute Time by ",colnames(commutedf)[4],", Minutes",sep=""),
                  x= "Cluster", y="Home to Office Desk", colour = as.factor(colnames(commutedf)[4]))
  plot(box)}

    #Makes a basic ggplot2 boxplot by day of the week, or another group, to prepare to consider ANOVA.
 box <-  ggplot2::ggplot(commutedf,
         ggplot2::aes(commutedf[,2], commutedf[,1],
                      colour = commutedf[,2])) +
         ggplot2::geom_boxplot() +
         ggplot2::labs(title=paste("Commute Time by ",colnames(commutedf)[2],", Minutes",sep=""),
                       x= Group, y="Home to Office Desk", colour = Group)
plot(box)

}
