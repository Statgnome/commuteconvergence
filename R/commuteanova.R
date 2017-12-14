#This function will do the anova, and
#it will display the results.
commuteanova <- function(commutedf,Leave ="Left",Arrive="ArriveDesk",Time ="Total",
                         Group="Day",usertimebins = 6){


  if(missing(Leave) | missing(Arrive)) {
    commutedf <- commutedf %>% dplyr::select(Time,Group) %>% unclass() %>%  as.data.frame()

  } else {
    #gets times into POSIX format so we can deal with them in R

    Arrive <- commutedf[,Arrive][[1]] %>% as.POSIXct(format = "%H:%M:%S")
    Depart <- commutedf[,Leave][[1]] %>% as.POSIXct(format = "%H:%M:%S")


    driveTime <- difftime(Arrive,Depart,units= "mins") %>% as.numeric()

    commutedf <- data.frame(driveTime,commutedf %>% dplyr::select(Group,Leave) %>% unclass())

    #kmeans cluster by length of drive to compare to time left
    timebins <- usertimebins
    Expanse <- difftime(Depart,min(Depart),units="mins") %>% as.numeric()
    clusterdf <- driveTime %>% data.frame(Expanse)
    timecluster <- clusterdf[,2] %>% kmeans(timebins)
    clustercounts <- timecluster[[1]] %>% table()

    #this is the initial user specified number of time bins for average lowest time
    #in case the assumptions for anova fail
    commutedf <- commutedf %>% dplyr::mutate(Cluster = timecluster[[1]],Expanse = Expanse)

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


    #adding in a kolmogorov smirnov test for normality of clusters
    ksres <- list()
    #ks test on multiple groups with list output
    for(i in 1:length(names(clustercounts))){
    datcluster <- dplyr::filter(commutedf,Cluster==i)
    ksres[[i]]<-
      suppressWarnings( ks.test(x =  datcluster[,1],y="pnorm",mean(datcluster[,1],sd(datcluster[,1]))))
    #end ks loop
    }

   #logic check for results of kolmogorov-smirnov
    if(any(lapply(ksres,'[[',2) %>% as.vector() < .05)){
      #log the drive times if they fail normality in ks
      commutedf <- commutedf %>% dplyr::mutate(logTime = log(driveTime))
      kslog <- list()

    #ks log loop
      for(i in 1:length(names(clustercounts))){
        datcluster <- dplyr::filter(commutedf,Cluster==i)
        kslog[[i]]<-
          suppressWarnings(ks.test(x=datcluster[,6],y="pnorm",mean(datcluster[,6],sd(datcluster[,6]))))
        #end ks log loop
      }

      #checking the ks on the logs for normality
      if(any(lapply(kslog,'[[',2) %>% as.vector() < .05)){
        paste("Leave between ",mindepart," and ",maxdepart,
            " for the shortest average drive time based on descriptive statistics.",sep="") %>% print()
          stop("No formal statistical test was conducted as ANOVA assumptions were not met by the data.")


            }else{
              #testing for heteroscedasticity now
          if(fligner.test(logTime ~ Cluster, data=commutedf)$p.value < .05){
            paste("Leave between ",mindepart," and ",maxdepart," for the shortest average
                  drive time based on descriptive statistics.",sep="") %>%
              print()
            stop("No formal statistical test was conducted as ANOVA assumptions were not met by the data.")
            #if data is homoscedastic the rest of the analysis follows on the logs
          }else{
            #Runs linear model to perform Anova by cluster.
            lmresults <-  lm(commutedf[,1] ~ commutedf[,4])
            anovaresults <- lmresults %>% anova()


            #The series of if statements begins by
            #checking for non-significant ANOVA results.
            if(anovaresults[1,5] > .05){
              paste("Your commutes are not significantly different between one another using ",timebins,"
                    clusters.",sep="") %>%
                print()
              #It sill tells minimum average drive time day.
              paste("Leave between ",mindepart," and ",maxdepart," though for the shortest average
                    drive time.",sep="") %>%
                print()
            }else{
              #Runs linear model to perform Anova by cluster.
              lmresults <-  lm(commutedf[,6] ~ commutedf[,4])
              anovaresults <- lmresults %>% anova()


              #The series of if statements begins by
              #checking for non-significant ANOVA results.
              if(anovaresults[1,5] > .05){
                paste("Your commutes are not significantly different between one another using ",timebins," clusters.",sep="") %>%
                  print()
                #It sill tells minimum average drive time day.
                paste("Leave between ",mindepart," and ",maxdepart," though for the shortest average drive time.",sep="") %>%
                  print()
              }else{

              #this is on the logged data after failure of ks test for normality
              #If the result is significant, then it runs Tukey's Post-Hoc test, and
              #it also displays which times were different pairwise.
              aovresults <-  aov(commutedf[,1] ~ as.factor(commutedf[,6]))
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
          }
  #end if ks log fail to rejct the null
      }
#end if any ks reject the null
    }


    if(fligner.test(driveTime ~ Cluster, data=commutedf)$p.value < .05){
      paste("Leave between ",mindepart," and ",maxdepart," for the shortest average drive time based on descriptive statistics.",sep="") %>%
        print()
      print("Your commutes are heteroscedastic, so no significance tests are performed.")
    }else{

      #Runs linear model to perform Anova by cluster.
      lmresults <-  lm(commutedf[,1] ~ commutedf[,4])
      anovaresults <- lmresults %>% anova()


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
#end if statement for using real times, i.e. left and arrival not missing
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
