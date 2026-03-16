# 1. Usar una imagen oficial de R con Plumber ya instalado
FROM rstudio/plumber:latest

# 2. Instalar librerías del sistema operativo que quantmod y otras necesitan para descargar datos
RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev

# 3. Instalar tus paquetes financieros de R
RUN R -e "install.packages(c('quantmod', 'PerformanceAnalytics', 'dplyr', 'tidyr'), repos='http://cran.rstudio.com/')"

# 4. Crear una carpeta en la nube y copiar tu código adentro
RUN mkdir /app
COPY api_aetherium.R /app/api_aetherium.R

# 5. Exponer el puerto (Render lo asignará dinámicamente)
EXPOSE 8000

# 6. El comando que arranca tu API cuando el servidor se enciende
# OJO: Render exige que el host sea '0.0.0.0' para conectarse a internet
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('/app/api_aetherium.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 8000)))"]
