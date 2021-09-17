# Thu Aug 08 21:28:20 2019 ------------------------------
## Alberto Berral Gonzalez
scrapper <- function(url) {
  pacman::p_load(rvest, tidyverse, robotstxt, xml2, 
                 data.table)
  
  # We test the scrapability of the web
  # paths_allowed(url)
  # As it returns TRUE we can go ahead
  
  web <- read_html("http://www.elcomparador.com")
  
  # We subset the data
  ## Odd bets
  web %>% 
    html_nodes('.impar') %>% 
    html_text() %>% str_trim() -> precios
  ## Even bets
  web %>% 
    html_nodes('.par') %>% 
    html_text() %>% str_trim() -> precios2
  # Participants
  web %>% 
    html_nodes('.equipo') %>% 
    html_text() -> nombres
  # Bets
  web %>% 
    html_nodes('.apuesta') %>% 
    html_text() -> apuesta
  # Bet pages
  web %>%
    html_nodes(css = 'img') %>% 
    html_attr("src") -> c.apuestas
  
  c.apuestas <- unique(c.apuestas[grep("casas", c.apuestas)])
  rmv   <- grep("max|mini", c.apuestas)
  c.apuestas <- c.apuestas[-rmv]
  c.apuestas <- gsub("/images/casas/", "", c.apuestas)
  c.apuestas <- gsub(".png.*", "", c.apuestas)
  # grep("casas", c.apuestas))
  
  # We generate the matrix we are going to use
  dt1 <- matrix(precios,  ncol = length(c.apuestas), byrow = T)
  dt2 <- matrix(precios2, ncol = length(c.apuestas), byrow = T)
  # And the matrix we are going to return
  dtfinal <- matrix(ncol = length(c.apuestas), nrow = length(apuesta)) 
  
  # We bind together odd and even bets
  i = 1
  j = 1
  for (x in seq(1:length(apuesta))) {
    for (name in apuesta) {
      if (name == "1" | name == "2") {
        dtfinal[x,] <- dt1[i,]
        i = i + 1
        x = x + 1
      }
      else {
        dtfinal[x,] <-  dt2[j,]
        j = j + 1
        x = x + 1
      }
    }
    rm(i)
    rm(j)
    rm(x)
    break
  }
  # We assing Rownames
  ## We paste together the names of competitors...
  names <- NULL
  for (i in seq(from = 1, to = (length(nombres) - 1), by = 2)) {
    x <- paste(nombres[i], nombres[i + 1], sep = '_')
    names <- c(names, x)
    rm(x)
  }
  ## and add to them the possible bets (1,X,2)
  rnames <- NULL
  i = 1
  j = 1
  while (i <= length(apuesta)) {
    if (apuesta[i] == '2') {
      aux <- paste(names[j], apuesta[i], sep = '__')
      rnames <- c(rnames, aux)
      j = j + 1
      i = i + 1
    }
    else {
      aux <- paste(names[j], apuesta[i], sep = '__')
      rnames <- c(rnames, aux)
      i = i + 1
    }
  }
  rm(i, j, aux)
  # We assign colnames
  colnames(dtfinal) <- c.apuestas
  ## We assign this combination as rownames
  rownames(dtfinal) <- rnames
  return(dtfinal)
  
}
