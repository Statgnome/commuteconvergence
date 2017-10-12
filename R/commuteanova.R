#This function will do the anova, and
#it will display the results.

commuteanova <- function(commutedf,Leave ="Left",Arrive="ArriveDesk",Time ="Total",
                         Group="Day",timebins = 6){


  if(missing(Leave) | missing(Arrive)) {
    commutedf <- commutedf %>% dplyr::select(Time,Group) %>% unclass() %>%  as.data.frame()

  } else {
    #gets times into POSIX format so we can deal with them in R

    Arrive <- commutedf[,Arrive][[1]] %>% as.POSIXct(format = "%H:%M:%S")
    Depart <- commutedf[,Leave][[1]] %>% as.POSIXct(format = "%H:%M:%S")
    driveTime <- difftime(Arrive,Depart,units= "mins") %>% as.numeric()

    commutedf <- data.frame(driveTime,commutedf %>% dplyr::select(Group,Leave) %>% unclass())

    #kmeans cluster by length of drive to compare to time left
    Expanse <- difftime(Depart,min(Depart),units="mins") %>% as.numeric()
    clusterdf <- driveTime %>% data.frame(Expanse)
    timecluster <- clusterdf %>% kmeans(timebins)
    commutedf <- commutedf %>% dplyr::mutate(Cluster = timecluster[[1]],Expanse = Expanse)

    #Runs linear model to perform Anova by a group such as Day of Week.
    lmresults <-  lm(commutedf[,1] ~ commutedf[,4])
    anovaresults <- lmresults %>% anova()

    #This will retrieve time of departure cluster
    #which has the lowest average drive time.
    lowgroup  <- commutedf[,1] %>% by(commutedf[,4],mean) %>% which.min()
    lowgroup <-  unname(lowgroup)
    mindepart <- commutedf %>%
      dplyr::filter(Cluster == lowgroup) %>%
      dplyr::filter(Expanse == min(Expanse))
    mindepart <- mindepart[,3]

    maxdepart <- commutedf %>%
      dplyr::filter(Cluster == lowgroup) %>%
      dplyr::filter(Expanse == max(Expanse))
    maxdepart <- maxdepart[,3]


    #The series of if statements begins by
    #checking for non-significant ANOVA results.
    if(anovaresults[1,5] > .05){
      paste("Your commutes are not significantly different between one another using ",timebins," clusters.",sep="") %>%
        print()
      #It sill tells minimum average drive time day.
      paste("Leave between ",mindepart," and ",maxdepart," though for the shortest average drive time.",sep="") %>%
        print()
    }else{
      #If the result is significant, then it runs Tukey's Post-Hoc test, and
      #it also displays which days were different pairwise.
      aovresults <-  aov(commutedf[,1] ~ as.factor(commutedf[,4]))
      posthoc <- TukeyHSD(aovresults)
      tukeydf <- posthoc[[1]] %>% data.frame()

      #uses select from tidyverse to find significant pairwise
      #results, eliminated a very large sequence of if else statements
      #with these 5 lines, which could arguably be one or two lines
      sig <- tukeydf %>%
        tibble::rownames_to_column('group') %>%
        dplyr::filter(p.adj < .05) %>%
        tibble::column_to_rownames('group') %>%
        rownames()

      paste("Your commutes are significantly different between these time clusters:",sep="") %>%
        print()

      sig %>% print()

      paste("Leave between ",mindepart," and ",maxdepart," for the shortest average drive time.",sep="") %>%
        print()
    }


  }


  #Runs linear model to perform Anova by a group such as Day of Week.
  lmresults <-  lm(commutedf[,1] ~ commutedf[,Group])
  anovaresults <- lmresults %>% anova()

  #This will show the user which day is their minimum average drive day.
  lowgroup  <- commutedf[,1] %>% by(commutedf[,Group],mean) %>% which.min()
  lowgroup <-  names(lowgroup)


#The series of if statements begins by
  #checking for non-significant ANOVA results.
  if(anovaresults[1,5] > .05){
    paste("Your commutes are not significantly different between one another based on ", Group ,".",sep="") %>%
      print()
    #It sill tells minimum average drive time day.
    paste(lowgroup," is the group with the shortest average drive time.",sep="") %>%
    print()
    }else{
  #If the result is significant, then it runs Tukey's Post-Hoc test, and
      #it also displays which days were different pairwise.
    aovresults <-  aov(commutedf[,1] ~ commutedf[,Group])
    posthoc <- TukeyHSD(aovresults)
    tukeydf <- posthoc[[1]] %>% data.frame()

    #uses select from tidyverse to find significant pairwise
    #results, eliminated a very large sequence of if else statements
    #with these 5 lines, which could arguably be one or two lines
    sig <- tukeydf %>%
      tibble::rownames_to_column('group') %>%
      dplyr::filter(p.adj < .05) %>%
      tibble::column_to_rownames('group') %>%
      rownames()

    paste("Your commutes are significantly different between these ", Group ," groups:",sep="") %>%
      print()

    sig %>% print()

    paste(lowgroup," is the group with the shortest average drive time.",sep="") %>%
      print()
    }



}
