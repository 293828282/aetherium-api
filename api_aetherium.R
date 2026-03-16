# ==========================================
# api_aetherium.R 
# VERSIÓN DEFINITIVA - AETHERIUM PRO
# ==========================================

suppressPackageStartupMessages({
  library(plumber)
  library(quantmod)
  library(PerformanceAnalytics)
  library(dplyr)
  library(tidyr)
})

# Configuraciones Globales
options(quantmod.auto.assign = FALSE)
MY_TOKEN <- "aed55865812fe78363417505694d04c003812df8"

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Authorization")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(res)
  }
  plumber::forward()
}

# Función robusta para descargar datos de Tiingo
safe_get_prices <- function(ticker, from, to) {
  tryCatch({
    # Limpieza: Tiingo no usa símbolos como ^ ni espacios
    clean_tk <- gsub("\\^", "", ticker)
    clean_tk <- trimws(clean_tk)
    
    # Si el usuario pide el índice de Yahoo (^GSPC), lo cambiamos al ETF SPY para Tiingo
    if(clean_tk == "GSPC") clean_tk <- "SPY"
    
    # Descarga
    x <- getSymbols(clean_tk, src = "tiingo", api.key = MY_TOKEN, 
                    from = from, to = to, auto.assign = FALSE)
    
    if (is.null(x) || nrow(x) == 0) return(NULL)
    
    adj <- Ad(x)
    adj <- na.omit(adj)
    colnames(adj) <- ticker # Mantenemos el nombre original para el usuario
    return(adj)
  }, error = function(e) return(NULL))
}

#* @post /api/portafolio
function(tickers = "AAPL,MSFT", pesos = "0.5,0.5", benchmark = "SPY", rf = 0.04) {
  
  tryCatch({
    # 1. Procesar Tickers y Pesos
    tk_list <- toupper(trimws(unlist(strsplit(tickers, ","))))
    w_list <- as.numeric(unlist(strsplit(pesos, ",")))
    
    if(length(tk_list) != length(w_list)) {
      return(list(error = "La cantidad de activos y pesos no coincide."))
    }
    
    # Normalizar pesos
    w_list <- w_list / sum(w_list)
    
    # 2. Definir Fechas (Últimos 3 años)
    to_date <- Sys.Date()
    from_date <- to_date - (365 * 3)
    
    # 3. Descarga de Activos
    plist <- lapply(tk_list, safe_get_prices, from = from_date, to = to_date)
    names(plist) <- tk_list
    plist_valid <- plist[!vapply(plist, is.null, logical(1))]
    
    if(length(plist_valid) < length(tk_list)) {
      missing <- setdiff(tk_list, names(plist_valid))
      return(list(error = paste("No se encontraron datos para:", paste(missing, collapse=", "))))
    }
    
    # 4. Descarga de Benchmark (si falla, usamos SPY por defecto)
    P_bench <- safe_get_prices(benchmark, from = from_date, to = to_date)
    if(is.null(P_bench)) P_bench <- safe_get_prices("SPY", from = from_date, to = to_date)
    
    # 5. Cálculos Financieros
    P <- do.call(merge, c(plist_valid, all = FALSE))
    R <- na.omit(Return.calculate(P, method = "discrete"))
    
    # Portafolio
    port_R <- Return.portfolio(R, weights = w_list)
    colnames(port_R) <- "Portafolio"
    
    # Benchmark
    bench_R <- na.omit(Return.calculate(P_bench, method = "discrete"))
    colnames(bench_R) <- "Benchmark"
    
    # Métricas
    stats <- list(
      cagr = as.numeric(Return.annualized(port_R, scale = 252)),
      volatilidad = as.numeric(StdDev.annualized(port_R, scale = 252)),
      sharpe = as.numeric(SharpeRatio.annualized(port_R, Rf = as.numeric(rf)/252, scale = 252)),
      max_drawdown = as.numeric(maxDrawdown(port_R))
    )
    
    # 6. Preparar Gráfico (Wealth Index)
    wealth <- cumprod(1 + port_R)
    bench_wealth <- cumprod(1 + bench_R)
    
    # Unir para asegurar fechas coincidentes
    m_wealth <- na.omit(merge(wealth, bench_wealth))
    
    df_chart <- data.frame(
      date = as.character(index(m_wealth)),
      Portafolio = as.numeric(m_wealth[,1]),
      Benchmark = as.numeric(m_wealth[,2])
    )
    
    return(list(
      status = "success",
      metricas = stats,
      graficos = list(crecimiento_historico = df_chart)
    ))
    
  }, error = function(e) {
    return(list(error = paste("Error en el motor:", e$message)))
  })
}
