##This function takes the sharing url from a Google Sheet, and it
#uses the googlesheets package to bring in the sheet as a
#googlesheet type of object.


##The url must be surrounded by quotes
read_sheet <- function(url,sheetname ="Sheet1"){

  #uses googlesheets to pass the sheet info to a variable
yoursheet <-  googlesheets::gs_url(url)

#reads the sheet into a data.frame,
#the worksheet name can be specified
sheet.df <- yoursheet %>% googlesheets::gs_read(ws = sheetname)



return(sheet.df)
}
