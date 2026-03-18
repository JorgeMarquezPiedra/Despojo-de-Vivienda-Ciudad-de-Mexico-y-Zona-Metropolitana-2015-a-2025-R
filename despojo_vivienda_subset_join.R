# Leer librerías
library(data.table)
library(sf)

# Leer el directorio
file_path <- "F:/JMP/Elaboración Cartográfica/Despojo de Viviendas/IDM_NM_feb25_municipal.csv"

# Leer csv con fread (rápido para muchas filas)
data <- fread(file_path)

# Ver nombres de columnas
colnames(data)

# Ver valores únicos de entidad
unique(data$Entidad)

# Hacer subset de las entidades de interés
subset_data <- data[Entidad %in% c("Ciudad de M\xe9xico", "M\xe9xico")]

# Head del subset
head(subset_data)

# Ver valores únicos de tipo de delito y tipo de delito
unique(subset_data$`Tipo de delito`)
unique(subset_data$`Subtipo de delito`)

# Crear nuevo subset con los tipos de delito de interés
despojo_data <- subset_data[`Tipo de delito` %in% c("Despojo", "Otros delitos contra el patrimonio", "Allanamiento de morada")]

# Ver valores únicos del subset
unique(despojo_data$`Subtipo de delito`)

# Crear columnas de meses
month_cols <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Asegurar que el tipo de columnas es numérico
despojo_data[, (month_cols) := lapply(.SD, as.numeric), .SDcols = month_cols]

# Añadir columna Total que sume todos los valores por meses
despojo_data[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = month_cols]

# Definir coiumna Año como tipo numérico
despojo_data[, Año := as.numeric(`A\xf1o`)]

# Filtrar los años entre 2015 y 2025
filtered_data <- despojo_data[`A\xf1o` >= 2015 & `A\xf1o` <= 2025]

# Agrupar por Municipio y Tipo de delito, para sumar el Total
summary_data <- filtered_data[, .(Suma_Total = sum(Total, na.rm = TRUE)),
                              by = .(`Clave_Ent`, `Cve. Municipio`, Municipio, `Tipo de delito`)]

# Guardar el csv
fwrite(summary_data, "F:/JMP/Elaboración Cartográfica/Despojo de Viviendas/despojo_data_filtrado.csv")


# Join--------------------------------------------------------------------------

# Directorio del shapefile
shapefile <- st_read("F:/JMP/Elaboración Cartográfica/Despojo de Viviendas/cdmx_zona_metropolitana.shp")

# Sumar los 3 "Tipo de delito" por municipio
agg_data <- summary_data[, .(Suma_Total = sum(Suma_Total, na.rm = TRUE)), 
                         by = .(`Cve. Municipio`, `Tipo de delito`)]

# Nombre para el shapefile
names(shapefile)

# Nombre para la agregación de los datos
names(agg_data)

# Pivot para que cada delito tenga su propia columna
agg_wide <- pivot_wider(agg_data, 
                        names_from = `Tipo de delito`, 
                        values_from = Suma_Total)

# Join espacial
shapefile_joined <- merge(shapefile, agg_wide, 
                          by.x = "CVEGEO_2", 
                          by.y = "Cve. Municipio", 
                          all.x = TRUE)

# Guardar el nuevo shapefile
st_write(shapefile_joined, "F:/JMP/Elaboración Cartográfica/Despojo de Viviendas/joined_shapefile_r.shp", delete_dsn = TRUE)
