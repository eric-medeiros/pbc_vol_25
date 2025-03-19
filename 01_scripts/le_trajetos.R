le_trajetos <- function(caminho_pasta_rotas) {

  library(sf)
  library(stringr)
  library(purrr)

  lista_trajetos <- list.files(caminho_pasta_rotas, "trajeto", full.names = TRUE)
  # lista_botos <- list.files(caminho_pasta_rotas, "botos", full.names = TRUE)

  # caminho_trajeto <- lista_trajetos[12]

  le_arquivo_trajeto <- function(caminho_trajeto) {

    # Coordenadas aproximadas do centro de Cananéia
    centro_cananeia <- c(-47.936110, -25.014894)
    raio_maximo_km <- 10  # Mantém pontos dentro de 30 km de Cananéia

    dados_arquivo_traj <-
      st_read(caminho_trajeto, layer = "track_points", quiet = TRUE) %>%
      mutate(arquivo = basename(caminho_trajeto),
             lng = st_coordinates(.)[,"X"],
             lat = st_coordinates(.)[,"Y"],
             ID_user = str_extract(arquivo, "(?<=_user_)\\d+"),
             perto_prev = as.numeric(st_distance(., lag(.), by_element = TRUE)) / 1000 <= 1,
             dentro_raio = as.numeric(st_distance(., st_sfc(st_point(centro_cananeia), crs = 4326))) / 1000 <= raio_maximo_km) %>%
      select(ID_user, ID_ponto = track_seg_point_id, datahora = time, lng, lat, arquivo, dentro_raio, perto_prev)

    return(dados_arquivo_traj)
  }

  dados_trajetos <-
    map_dfr(lista_trajetos, le_arquivo_trajeto, .id = "ID_arquivo") %>%
    mutate(ID_arquivo = as.integer(ID_arquivo),
           Data = date(datahora)) %>%
    arrange(datahora)

  return(dados_trajetos)
}
