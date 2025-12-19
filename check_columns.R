library(readxl)

# Ruta al archivo de Excel
ruta_archivo <- "BBDD/ARCLIM/riesgo/Acuicultura/ARCLIM_acuicultura_cuencas_aguadulce_acuicultura_cuencas_172.xlsx"

# Leer la hoja DATOS
datos <- read_excel(ruta_archivo, sheet = "DATOS")

# Mostrar los nombres de las columnas
print("Nombres de columnas:")
print(names(datos))

# Verificar si existen columnas específicas de latitud y longitud
latlon_cols <- grep("lat|lon|coord", names(datos), ignore.case = TRUE, value = TRUE)
print("Columnas relacionadas con coordenadas:")
print(latlon_cols)

# Mostrar algunas filas para examinar los datos
print("Primeras 3 filas de datos:")
print(head(datos, 3))

# Si se encuentran columnas de latitud/longitud, ver sus formatos
if (length(latlon_cols) > 0) {
  for (col in latlon_cols) {
    print(paste("Valores para", col, ":"))
    print(head(datos[[col]], 5))
    print(paste("Clase de", col, ":", class(datos[[col]])))
  }
} 