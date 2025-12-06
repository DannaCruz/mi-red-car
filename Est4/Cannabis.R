# PAQUETES

library(sf)
library(dplyr)
library(spdep)
library(ggplot2)
library(readr)
library(stringi)
library(tidyr)
library(FNN)
library(igraph)


# CARGAR DATOS
df <- read_csv("C:/Users/User/Pictures/red de cannabis/Distribución_de_licencias_de_cannabis_vigentes_por_departamentos_20251126.csv",
               show_col_types = FALSE)

muni <- st_read("C:/Users/User/Pictures/red de cannabis/MGN2023_MPIO_POLITICO/MGN_ADM_MPIO_GRAFICO.shp",
                quiet = TRUE)


# NORMALIZAR NOMBRES (quita tildes, mayúsculas, trim)
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
df <- df %>%
  mutate(key = paste0(DEP_norm, "_", MUN_norm))

muni <- muni %>%
  mutate(key = paste0(DEP_norm, "_", MUN_norm))


# LIMPIAR duplicados en df (agrupar por key, sumar numéricos)

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
muni2 <- left_join(muni_proj, df_clean, by = "key")

# Reemplazar NAs numéricos por 0 (municipios sin licencias)
muni2 <- muni2 %>%
  mutate(across(all_of(num_cols_existing), ~ replace_na(., 0)))




# _______________________________
# RED SIN DISTINCIÓN DE LICENCIAS
# _______________________________

library(sf)
library(dplyr)
library(FNN)
library(igraph)

# ------------------------------------------------------------
# 1. Crear KEY única (departamento + municipio)
# ------------------------------------------------------------
muni2 <- muni2 %>%
  mutate(
    KEY = paste0(dpto_cnmbr, "_", mpio_cnmbr),
    KEY = gsub(" ", "_", KEY)   # evitar espacios
  )

# ------------------------------------------------------------
# 2. Calcular centroides y coordenadas X/Y
# ------------------------------------------------------------
muni2$centroid <- st_centroid(muni2$geometry)

coords <- st_coordinates(muni2$centroid)
# coords tiene columnas X y Y ✔✔

muni2 <- muni2 %>%
  mutate(
    cx = coords[,1],
    cy = coords[,2]
  )

# Matriz numérica para FNN
coords_mat <- as.matrix(muni2[, c("cx", "cy")])
coords_clean <- apply(coords_mat[, 1:2], 2, function(x) as.numeric(unlist(x)))

# Convertir a matriz numérica
coords_clean <- as.matrix(coords_clean)

# Verificar
str(coords_clean)
class(coords_clean)

# ------------------------------------------------------------
# 3. KNN (k vecinos más cercanos)
# ------------------------------------------------------------
k <- 4
knn_res <- get.knn(coords_clean, k = k)

# ------------------------------------------------------------
# 4. Construir tabla de aristas
# ------------------------------------------------------------
edges <- data.frame(
  from = rep(muni2$KEY, each = k),
  to   = muni2$KEY[as.vector(knn_res$nn.index)]
)

# ------------------------------------------------------------
# 5. Construcción de nodos
# rating = TOTAL
# ------------------------------------------------------------
nodes <- muni2 %>%
  mutate(
    rating = ifelse(is.na(TOTAL), 0, TOTAL),
    x = cx,
    y = cy
  ) %>%
  select(KEY, departamento = dpto_cnmbr, municipio = mpio_cnmbr, rating, x, y)

# Quitar el geometry porque igraph no lo quiere
nodes_df <- st_drop_geometry(nodes)

# ------------------------------------------------------------
# 6. Crear grafo
# ------------------------------------------------------------
G <- igraph::graph_from_data_frame(
  edges,
  vertices = nodes_df,
  directed = FALSE
)

# ------------------------------------------------------------
# 7. Layout
# ------------------------------------------------------------
pos_mat <- as.matrix(nodes_df[, c("x", "y")])

# ------------------------------------------------------------
# 8. Plot de la red
# ------------------------------------------------------------
plot(
  G,
  layout = pos_mat,
  vertex.size = 2,
  vertex.color = "forestgreen",
  vertex.label = NA,
  edge.color = rgb(0.8, 0.8, 0.8),
  main = "Red de cercanía (kNN = 4) — TOTAL de licencias por municipio"
)

G <- simplify(G, remove.multiple = TRUE, remove.loops = TRUE)
g <- G



# =====================================================
#        ANALISIS DESCRIPTIVO COMPLETO DE LA RED
# =====================================================

library(igraph)
library(dplyr)
library(ggplot2)
library(scales)

# -----------------------------------------------------
# 0. Suponemos que ya tienes tu grafo g cargado
#    y es un objeto igraph válido
# -----------------------------------------------------

# =====================================================
# 1. GRADO DEL NODO
# =====================================================

deg <- degree(g, mode = "all")

# Tabla descriptiva del grado
summary(deg)

# Histograma
ggplot(data.frame(deg), aes(deg)) +
  geom_histogram(bins = 40, fill="steelblue") +
  theme_minimal() +
  labs(title="Distribución del grado", x="Grado", y="Frecuencia")

# CDF
ggplot(data.frame(deg), aes(x = deg)) +
  stat_ecdf(geom="step") +
  theme_minimal() +
  labs(title="Función de distribución acumulada (CDF)", 
       x="Grado", y="F(deg)")

# =====================================================
# 2. COMPONENTES CONEXOS
# =====================================================

comp <- components(g)
num_comp <- comp$no
tam_comp <- sizes(comp)
giant_comp <- induced_subgraph(g, which(comp$membership == which.max(tam_comp)))

num_comp
tam_comp

# =====================================================
# 3. CENTRALIDADES
# =====================================================

cent_degree <- degree(g)
cent_close  <- closeness(g, normalized = TRUE)
cent_between <- betweenness(g, normalized = TRUE)

centralidades <- data.frame(
  nodo = names(cent_degree),
  grado = cent_degree,
  closeness = cent_close,
  betweenness = cent_between
) %>% arrange(desc(grado))

head(centralidades, 15)  # Los 15 más importantes

# =====================================================
# 4. CLUSTERING (coeficiente de agrupamiento)
# =====================================================

clustering_global <- transitivity(g, type = "global")
clustering_por_nodo <- transitivity(g, type = "local")

clustering_global
summary(clustering_por_nodo)

# =====================================================
# 5. CAMINOS MÁS CORTOS: ASP Y DIÁMETRO
# =====================================================

# Solo dentro del componente gigante
asp <- mean_distance(giant_comp, directed = FALSE)
diam <- diameter(giant_comp, directed = FALSE)

asp
diam

# =====================================================
# 6. COMUNIDADES + MODULARIDAD Q (greedy)
# =====================================================

com_greedy <- cluster_fast_greedy(g)
muni2$comunidad <- com_greedy$membership
mod_greedy <- modularity(com_greedy)
mod_greedy
install.packages("RColorBrewer")
library(RColorBrewer)

n_com <- length(unique(muni2$comunidad))
paleta_com <- colorFactor(brewer.pal(min(n_com, 12), "Set3"),
                          domain = muni2$comunidad)
muni2_wgs <- st_transform(muni2, 4326)
coords <- st_coordinates(st_centroid(muni2_wgs$geometry))
muni2_wgs$lon <- coords[,1]
muni2_wgs$lat <- coords[,2]
library(leaflet)

leaflet(muni2_wgs) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    fillColor = ~paleta_com(comunidad),
    color = "#444444",
    weight = 0.4,
    radius = 5,
    fillOpacity = 0.9,
    popup = ~paste0(
      "<b>Municipio: </b>", mpio_cnmbr, "<br>",
      "<b>Comunidad #: </b>", comunidad, "<br>",
      "<b>Departamento: </b>", dpto_cnmbr
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = paleta_com,
    values = ~comunidad,
    title = "Comunidades detectadas",
    opacity = 1
  )


# =====================================================
# 7. NODOS ESTRATÉGICOS SEGÚN CENTRALIDADES
# =====================================================

top_nodos <- centralidades %>%
  mutate(rank_grado = rank(-grado),
         rank_close = rank(-closeness),
         rank_between = rank(-betweenness),
         score = rank_grado + rank_close + rank_between) %>%
  arrange(score)

head(top_nodos, 20)

# =====================================================
# 8. RECORRIDO BFS (por capas)
# =====================================================

nodo_inicio <- V(g)[1]   # Puedes cambiar el nodo
bfs_res <- bfs(g, root = nodo_inicio, dist = TRUE)

table(bfs_res$dist)

# =====================================================
# 9. RECORRIDO DFS
# =====================================================

dfs_res <- dfs(g, root = nodo_inicio)
dfs_res$order[1:50]   # primeros 50 nodos del recorrido

# =====================================================
# 10. RUTA MÁS CORTA (DIJKSTRA)
# =====================================================

# Si la red NO tiene pesos, Dijkstra = BFS ponderado a 1
origen <- V(g)[1]
destino <- V(g)[50]

ruta <- shortest_paths(g, from = origen, to = destino, output = "vpath")
ruta

# =====================================================
# 11. ASORTATIVIDAD (HOMOFILIA)
# =====================================================

# Si tienes un atributo en los nodos, ej: "region"
# V(g)$region <- muni2_wgs$region   # ejemplo

# Asortatividad por atributo categórico
# asort1 <- assortativity_nominal(g, as.factor(V(g)$region))

# Asortatividad por grado (preferencia por nodos similares en grado)
asort_grado <- assortativity_degree(g)

asort_grado

