install.packages("sf")
install.packages("tidyverse")
install.packages("spdep")
install.packages("ggraph")
install.packages("dplyr")
install.packages("stringi")
install.packages("lwgeom")


# PAQUETES
# _________

library(sf)
library(dplyr)
library(spdep)
library(ggplot2)
library(readr)
library(stringi)
library(tidyr)


# CARGAR DATOS
# _____________
df <- read_csv("C:/Users/User/Pictures/red de cannabis/Distribución_de_licencias_de_cannabis_vigentes_por_departamentos_20251126.csv",
               show_col_types = FALSE)

muni <- st_read("C:/Users/User/Pictures/red de cannabis/MGN2023_MPIO_POLITICO/MGN_ADM_MPIO_GRAFICO.shp",
                quiet = TRUE)


# NORMALIZAR NOMBRES (quita tildes, mayúsculas, trim)
# ___________________________________________________
df <- df %>%
  mutate(
    MUN_norm = MUNICIPIO %>%
      stri_trans_general("Latin-ASCII") %>%
      toupper() %>%
      trimws(),
    DEP_norm = DEPARTAMENTO %>%
      stri_trans_general("Latin-ASCII") %>%
      toupper() %>%
      trimws()
  )

muni <- muni %>%
  mutate(
    MUN_norm = mpio_cnmbr %>%
      stri_trans_general("Latin-ASCII") %>%
      toupper() %>%
      trimws(),
    DEP_norm = dpto_cnmbr %>%
      stri_trans_general("Latin-ASCII") %>%
      toupper() %>%
      trimws()
  )


# CREAR KEY (DEPARTAMENTO + MUNICIPIO) para join único
# _______________________________________________________
df <- df %>%
  mutate(key = paste0(DEP_norm, "_", MUN_norm))

muni <- muni %>%
  mutate(key = paste0(DEP_norm, "_", MUN_norm))


# LIMPIAR duplicados en df (agrupar por key, sumar numéricos)
# ___________________________________________________________
# Identificamos columnas numéricas relevantes en el CSV
num_cols <- c("NO PSICO", "PSICO", "SEMILLAS", "TOTAL")

num_cols_existing <- intersect(num_cols, names(df))
df <- df %>%
  mutate(across(all_of(num_cols_existing), ~ as.numeric(.)))

# Agrupar por key y sumar los valores numéricos (evita many-to-many)
df_agg <- df %>%
  group_by(key) %>%
  summarise(
    # si hay columnas no numéricas que quieras conservar, agrégalos aquí (p.ej. first(DEPARTAMENTO))
    across(all_of(num_cols_existing), ~ sum(., na.rm = TRUE)),
    .groups = "drop"
  )

# Conservar DEPARTAMENTO original:
df_meta <- df %>%
  st_drop_geometry() %>%
  select(key, DEPARTAMENTO) %>%
  distinct(key, .keep_all = TRUE)

df_clean <- left_join(df_agg, df_meta, by = "key")


# Reparar geometrías y reproyectar a CRS métrico para cálculos espaciales
# _______________________________________________________________________
sf::sf_use_s2(FALSE)
muni <- st_make_valid(muni)

# Reproyectar a CRS métrico apropiado para Colombia (ej: EPSG:3116)
muni_proj <- st_transform(muni, 3116)

# Calcular centroides en CRS proyectado (coordenadas métricas)
muni_proj <- muni_proj %>%
  mutate(centroid = st_centroid(geometry),
         cx = st_coordinates(centroid)[,1],
         cy = st_coordinates(centroid)[,2])


# Hacer el join usando la key única
# _________________________________
muni2 <- left_join(muni_proj, df_clean, by = "key")

# Reemplazar NAs numéricos por 0 (municipios sin licencias)
muni2 <- muni2 %>%
  mutate(across(all_of(num_cols_existing), ~ replace_na(., 0)))


# Crear la variable 'tipo' (dominante)
# _______________________________________
muni2 <- muni2 %>%
  mutate(tipo = case_when(
    `PSICO` > `NO PSICO` & `PSICO` > `SEMILLAS` ~ "psico",
    `NO PSICO` > `PSICO` & `NO PSICO` > `SEMILLAS` ~ "no_psico",
    `SEMILLAS` > `PSICO` & `SEMILLAS` > `NO PSICO` ~ "semillas",
    (`PSICO` + `NO PSICO` + `SEMILLAS`) == 0 ~ "sin_datos",
    TRUE ~ "empate"    # por si hay empates exactos
  ))

# Ajustar factor ordenado
muni2$tipo <- factor(muni2$tipo, levels = c("psico", "no_psico", "semillas", "empate", "sin_datos"))


# MATRIZ DE VECINDAD (k-NN por cercanía) - opción estable (k = 4)
# _______________________________________________________________
# Centroid coords (en metros)
coords <- st_coordinates(muni2$centroid)


k <- 4
knn_4 <- knearneigh(coords, k = k)
nb_knn <- knn2nb(knn_4)

# Crear líneas de edges (sf) para visualización
edges_knn <- nb2lines(nb_knn, coords = coords, as_sf = TRUE)
st_crs(edges_knn) <- st_crs(muni2)


# 9. CREAR EDGE LIST (segura) y añadir coordenadas
# _________________________________________________
edge_list <- do.call(rbind, lapply(seq_along(nb_knn), function(i) {
  neighs <- nb_knn[[i]]
  if (length(neighs) == 0) return(NULL)
  data.frame(from = rep(i, length(neighs)), to = neighs, stringsAsFactors = FALSE)
}))


edge_list$from <- as.integer(edge_list$from)
edge_list$to   <- as.integer(edge_list$to)

# Añadir coordenadas corregidas
edge_list$x    <- muni2$cx[edge_list$from]
edge_list$y    <- muni2$cy[edge_list$from]
edge_list$xend <- muni2$cx[edge_list$to]
edge_list$yend <- muni2$cy[edge_list$to]


# Mapa con red por cercanía (k-NN)
#_____________________________________________
# Escala de verdes para tipos
cols <- c("psico" = "#006400", "no_psico" = "#228B22", "semillas" = "#66C266",
          "empate" = "#8B8B00", "sin_datos" = "grey90")

map <- ggplot() +
  geom_sf(data = st_transform(muni2, 4326), fill = "grey95", color = "white", size = 0.15) + # base en lon/lat para visual
  geom_segment(data = edge_list, aes(x = x, y = y, xend = xend, yend = yend), color = "gray40", alpha = 0.25, size = 0.3, linewidth = 0.3) +
  geom_point(data = as.data.frame(st_coordinates(muni2$centroid)) %>% 
               bind_cols(st_drop_geometry(muni2) %>% select(TOTAL, tipo)),
             aes(x = X, y = Y, size = TOTAL, color = tipo), alpha = 0.9) +
  scale_color_manual(
    values = cols,
    labels = c(
      psico = "Licencia: uso psicoactivo",
      no_psico = "Licencia: uso no psicoactivo",
      semillas = "Licencia de semillas",
      empate = "Misma cantidad de licencias de uso psicoactivo y no psicoactivo",
      sin_datos = "Sin licencias"
    ),
    na.value = "grey80"
  ) +
  scale_size(range = c(1, 6), name = "Total licencias") +
  theme_minimal() +
  labs(title = "Red de cercanía (k-NN) y licencias de cannabis por municipio",
       subtitle = paste0("k = ", k, "  —  puntos: tamaño = TOTAL"),
       caption = "Fuente: Datos tomados de la Subdirección de Control y Fiscalización de Sustancias Químicas y Estupefacientes \n del Ministerio de Justicia y del Derecho")+
  coord_sf(expand = FALSE) +
theme(aspect.ratio = 1)


# Exportar objetos para inspección
#__________________________________
st_write(edges_knn, "edges_knn.shp", delete_dsn = TRUE)
st_write(muni2, "muni2_with_licenses.shp", delete_dsn = TRUE)

ggsave("C:/Users/User/Pictures/red de cannabis/red_knn.png", map, width = 11, height = 11, dpi = 300)
