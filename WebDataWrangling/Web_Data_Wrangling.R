  library(Lahman)
  Awarders <- AwardsPlayers %>% 
    filter(yearID == 2016) %>% 
    select(playerID,yearID,awardID)
  top <- Batting %>% 
    filter(yearID == 2016) %>%
    arrange(desc(HR)) %>%    # arrange by descending HR count
    slice(1:10)    # take entries 1-10
  top_Awarders <- left_join(top,Awarders) %>% 
    select(playerID,yearID,awardID) %>% filter(!is.na(awardID)==TRUE) %>% 
    group_by(top_Awarders,playerID)
  
  #____________________________________
  Awarders <- AwardsPlayers %>% 
    filter(yearID == 2016) %>% 
    select(playerID,awardID)
  no_top <- Batting %>% 
    filter(yearID == 2016) %>%
    arrange(desc(HR)) %>%    # arrange by descending HR count
    slice(11:n())    # no take entries 1-10
  no_top_Awarders <- left_join(no_top,Awarders) %>% 
    select(playerID,awardID) %>% 
    filter(!is.na(awardID)==TRUE) %>% 
    length(unique(no_top_Awarders$playerID))
  length(unique(top_Awarders$playerID))
  
  
  #______________________________________________
  library(rvest)
  url <- "http://www.stevetheump.com/Payrolls.htm"
  h <- read_html(url)
  nodes <- html_nodes(h, "table")
  class(nodes)
  tabla1 <- nodes[[1]] %>% html_table()
  tabla2 <- nodes[[2]] %>% html_table()
  tabla3 <- nodes[[3]] %>% html_table()
  tabla4 <- nodes[[4]] %>% html_table()
  str(tabla1)
  str(tabla2)
  str(tabla3)
  str(tabla4)
  head(tabla1)
  head(tabla2)
  head(tabla3)
  head(tabla4)
  
  tabla20 <- nodes[[20]] %>% html_table()
  tabla21 <- nodes[[21]] %>% html_table()
  tabla22 <- nodes[[22]] %>% html_table()
  
  str(tabla20)
  str(tabla21)
  str(tabla22)
  
  head(tabla20)
  head(tabla21)
  head(tabla22)
  
  tab1 <- ""
  tab2 <- ""
  tab1 <- nodes[[10]] %>% html_table() %>% set_names(c("Team", "Payroll", "Average")) %>% select("Team")
  tab2 <- nodes[[19]] %>% html_table() %>% slice(2:n()) %>% set_names(c("Team", "Payroll", "Average"))%>% select("Team")
  str(tab1)
  str(tab2)
  head(tab1)
  head(tab2)
  tab1_2 <- full_join(tab1,tab2,match = "Team")
  str(tab1_2)
  head(tab1_2)
  length(tab1_2)
  nrow(tab1_2)
  dim(tab1_2)

  library(rvest)
  library(tidyverse)
  url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
  h <- read_html(url)
  nodes <- html_nodes(h, "table")
  class(nodes)
  str(nodes)
  
  #nodos que no son tablas 1,5,6,7,8,24,26,28,31,33,37  casi tablas 17,34,35,38,39
  #pregunta # 5---tabla 32,27,25
  
  for (k in c(2,3,4,9,10,11,12,13,14,15,16,18,19,20,21,22,23,25,27,29,30,32,36)) {
    nombre <- c("tbls",k)
    print(nombre)
    tbls <- nodes[[k]] %>% html_table()
    str(tbls)
    print(tbls)
  }
  
  library(rvest)
  strtrim("5 ' 9 ",2)
  s <- c("Five feet eight inches")
  str_to_upper(s)
  ?system.file()
  
  
  library(pdftools)
  temp_file <- tempfile()
  url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
  download.file(url, temp_file)
  txt <- pdf_text(temp_file)
  #file.remove(temp_file)
  #txt
  #raw_data_research_funding_rates <- txt[2]
  #raw_data_research_funding_rates %>% head
  #tab <- split(raw_data_research_funding_rates, "\n")
  #tab <- tab[[1]]
  #print(tab)
  #tab %>% head
  #the_names_1 <- tab[3]
  #the_names_2 <- tab[4]
  #the_names_1
  strc
  
  #____________________________________________________________________________
  library(rvest)
  library(tidyverse)
  library(stringr)
  url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
  tab <- read_html(url) %>% html_nodes("table")
  polls <- tab[[5]] %>% html_table(fill = TRUE)
  head(polls)
  polls <- polls %>% set_names(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
  polls <- slice(polls,(2:n()))
  #polls <- parse_number(polls$remain)/100
  a <- function_name(polls$undecided, "N/A", "0")
  print(a)
          
  example <- data_frame(line = c(1, 2, 3, 4),
                        text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
  example
  example %>% unnest_tokens(word, text)
  
  a <- stop_words
  b <- get_sentiments("bing")
  c <- get_sentiments("afinn")
  d <- get_sentiments("nrc")  
  
  library(rvest)
  library(tidyverse)
  url <- "http://www.promigas.com/Es/BEO/Paginas/VolumenEntregadoProductor.aspx"
  h <- read_html(url)
  nodes <- html_nodes(h, "table")
  vol <- nodes[[1]] %>% html_form() %>% 
    set_values("Fecha Inicio:" == "01/01/2020", "Fecha Fin:" == "01/04/2020", submit = "Enviar")
  vol
  
  library(dslabs)
  library(lubridate)
  library(tidyverse)
  options(digits = 3)
  data(brexit_polls)
  str(brexit_polls)
  
  start_OnApril <- brexit_polls %>%
    mutate(month = month(startdate)) %>%
    filter(month == 4) %>%
    count(month)
  
  end <- brexit_polls %>%
    mutate(week = week(enddate)) %>%
    filter(week == 24) %>%
    count(week)
  
  weekday <- brexit_polls %>%
    mutate(weekday = weekdays(enddate)) %>%
    count(weekday) %>% 
    group_by(weekday)
  
  library(dslabs)
  data(movielens)
  
  yearmovi <- movielens %>% 
    mutate(timestamp, as_datetime(timestamp))
    mutate(year, year(timestamp)) %>% 
    
  yearmovi2 <- yearmovi %>% 
    count(year) %>% 
    group_by(year)
  
  hourmovi <- movielens %>% 
    mutate(hour = hour(ymd_hms(as_datetime(movielens$timestamp)))) %>% 
    count(hour) %>% 
    group_by(hour)
  
  yearmovi <- movielens %>% 
    mutate(yearstamp = year(ymd_hms(as_datetime(movielens$timestamp)))) %>% 
    count(yearstamp) %>% 
    group_by(yearstamp)
  
  
  ##_____________________________________________________________________
  
  
  library(tidyverse)
  library(gutenbergr)
  library(tidytext)
  options(digits = 3)
  data(gutenberg_metadata)
  
  data <- data.frame(gutenberg_metadata)
  str(data)
  
  id <- data %>% 
    mutate(patron = str_detect(title, "Pride and Prejudice")) %>% 
    count(patron) %>% 
    group_by(patron)
  
  ids <- data.frame(str_detect(data$title, "Pride and Prejudice"))
  
  id3 <- gutenberg_works(title == "Pride and Prejudice")
  
  pdf_pap <- data.frame(gutenberg_download(1342)) %>% 
    select(text)
  words <- unnest_tokens(pdf_pap, word, text, token = "words", format = NULL, to_lower = TRUE, drop = FALSE, collapse = NULL)
  
  NoStop_words <- words %>% 
    left_join(stop_words) %>% 
    filter(is.na(lexicon) == TRUE)
  
  No_digits <- NoStop_words %>%
    mutate(digits = parse_number(word)) %>% 
    filter(is.na(digits) == TRUE)
  
  moda_words <- No_digits %>% 
    count(word) %>% 
    group_by(word) %>% 
    filter(n>=100)
  
  afinn <- get_sentiments("afinn")
  
  afinn_sentiments_word <- words %>% 
    inner_join(afinn) #%>% 
  #  select(word,value) %>% 
  #  unique()
  
  fourvalue_afinn_sentiments <- afinn_sentiments_word %>% 
    filter(value == 4)

  afinn_sentiments <- afinn_sentiments_word %>% 
    nrow()
  
  positive_afinn_sentiments <- afinn_sentiments_word %>% 
    filter(value > 0) %>% 
    nrow()
  
  positive_afinn_sentiments/afinn_sentiments
  
  
  
  
  