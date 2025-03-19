le_excel <- function(caminho_pasta_excel) {
  require(dplyr)
  require(lubridate)
  require(purrr)

  lista_arquivos_excel <- list.files(caminho_pasta_excel, full.names = TRUE)

  le_excel_quest <- function(caminho_excel, sheetName) {
    require(xlsx)

    quest <-
      read.xlsx(caminho_excel, sheetName = sheetName) %>%
      as_tibble()

    return(quest)
  }

  dados_quest_1 <-
    map_dfr(lista_arquivos_excel, le_excel_quest, "Form One", .id = "arquivo") %>%
    arrange(Date) %>%
    slice(12:n()) %>%
    mutate(Date = ymd_hms(Date, tz = "America/Sao_Paulo"),
           ID_pergunta = case_when(
             str_detect(Question, "^Quanto") ~ 1,
             str_detect(Question, "^Se") ~ 2,
             str_detect(Question, "^Na") ~ 3,
             str_detect(Question, "^Qual") ~ 4,
             TRUE ~ NA_real_
           ))

  dados_quest_2 <-
    map_dfr(lista_arquivos_excel, le_excel_quest, "Form Two", .id = "arquivo") %>%
    arrange(Date) %>%
    slice(11:n()) %>%
    mutate(Date = ymd_hms(Date, tz = "America/Sao_Paulo"),
           ID_pergunta = case_when(
             str_detect(Question, "^Caso") ~ 1,
             str_detect(Question, "barco\\?$") ~ 2,
             str_detect(Question, "qualidade\\?$") ~ 3,
             str_detect(Question, "^Se") ~ 4,
             str_detect(Question, "passeio\\?$") ~ 5,
             TRUE ~ NA_real_
           ))

  dados_quest <- list(
    quest_1 = dados_quest_1,
    quest_2 = dados_quest_2
  )

  return(dados_quest)
}
