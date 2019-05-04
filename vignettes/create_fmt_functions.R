# Create fmt functions 



into_list <- function(x,y){
  x[[y]]
}

names <- 
  tibble(names_01 = names(formats)) %>% 
  mutate(names_02 = map(names_01,~names(formats[[.x]]))) %>% 
  unnest() 

names$names_03 <- 
  c(1:nrow(names)) %>%
  map(~ {
    if (names$names_02[.x] == "NULL") {
      return(tibble(names_03 = "NULL"))
    } else {
      list(
        formats,
        names$names_01[[.x]],
        names$names_02[[.x]]
      ) %>%
        reduce(into_list) %>%
        names()
    }
  }) %>%
  map(~ tibble(names_03 = as.character(.x))) %>%
  map_if(~ nrow(.x) == 0, ~ tibble(names_03 = "NULL"))  

names <- names %>% unnest()

names$names_04 <- 
  c(1:nrow(names)) %>%
  map(~ {
    if (names$names_03[.x] == "NULL") {
      return(tibble(names_04 = "NULL"))
    } else {
      list(
        formats,
        names$names_01[[.x]],
        names$names_02[[.x]],
        names$names_03[[.x]]
      ) %>%
        reduce(into_list) %>%
        names()
    }
  }) %>%
  map(~ tibble(names_04 = as.character(.x))) %>%
  map_if(~ nrow(.x) == 0, ~ tibble(names_04 = "NULL"))  

names <- names %>% unnest()

names$names_05 <- 
  c(1:nrow(names)) %>%
  map(~ {
    if (names$names_04[.x] == "NULL") {
      return(tibble(names_05 = "NULL"))
    } else {
      list(
        formats,
        names$names_01[[.x]],
        names$names_02[[.x]],
        names$names_03[[.x]],
        names$names_04[[.x]]
      ) %>%
        reduce(into_list) %>%
        names()
    }
  }) %>%
  map(~ tibble(names_05 = as.character(.x))) %>%
  map_if(~ nrow(.x) == 0, ~ tibble(names_05 = "NULL"))  

names <- names %>% unnest() 

names$names_06 <- 
  c(1:nrow(names)) %>%
  map(~ {
    if (names$names_05[.x] == "NULL") {
      return(tibble(names_06 = "NULL"))
    } else {
      list(
        formats,
        names$names_01[[.x]],
        names$names_02[[.x]],
        names$names_03[[.x]],
        names$names_04[[.x]],
        names$names_05[[.x]]
      ) %>%
        reduce(into_list) %>%
        names()
    }
  }) %>%
  map(~ tibble(names_06 = as.character(.x))) %>%
  map_if(~ nrow(.x) == 0, ~ tibble(names_06 = "NULL"))  

names <- names %>% unnest()


names$names_07 <- 
  c(1:nrow(names)) %>%
  map(~ {
    if (names$names_06[.x] == "NULL") {
      return(tibble(names_07 = "NULL"))
    } else {
      list(
        formats,
        names$names_01[[.x]],
        names$names_02[[.x]],
        names$names_03[[.x]],
        names$names_04[[.x]],
        names$names_05[[.x]],
        names$names_06[[.x]]
      ) %>%
        reduce(into_list) %>%
        names()
    }
  }) %>%
  map(~ tibble(names_07 = as.character(.x))) %>%
  map_if(~ nrow(.x) == 0, ~ tibble(names_07 = "NULL"))  

names <- names %>% unnest()

names_local <- 
  names %>% filter(names_01 == "local")


format_types <-  c(1:nrow(names_local)) %>% map(~ names_local[.x,] %>% unlist() %>%  unname() %>% .[. != "NULL" & . != "local"]  )



create_format_function <- function(format_type_vec){
  paste("\r\n\r\n",
        paste0(paste0("fmt_",paste0(format_type_vec,collapse = "_"), "_single"), " <- "), 
        paste0("function(local_format_id,sheet_formats = formats){"),
        format_type_vec %>% paste0(collapse = "','") %>% paste0("format_type_vec <- c('",.,"')"),
        paste0('append(list(sheet_formats),c("local",format_type_vec)) %>%  reduce(into_list) %>% .[[local_format_id]]}'),"\r\n\r\n",
        
        paste0("fmt_",paste(format_type_vec, collapse = "_" ), " <- "),
        paste0("function(format_id_vec= local_format_id,sheet_formats = formats){"),
        
        paste0("format_id_vec %>% map_chr(possibly(" ,
               paste0("fmt_",paste0(format_type_vec,collapse = "_"), "_single"),
               ",NA_real_),sheet_formats = sheet_formats)}"), sep = "\r\n")}


functions <- 
  format_types %>% map(create_format_function)

functions[[1]] %>% cat()

functions %>% write_lines(path = "format_funcs.r", append = TRUE)