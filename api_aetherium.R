# ==========================================
# api_aetherium.R 
# Backend API - Aetherium (Versión Tiingo Pro)
# ==========================================

suppressPackageStartupMessages({
  library(plumber)
  library(quantmod)
  library(PerformanceAnalytics)
  library(dplyr)
  library(tidyr)
})

# Configuraciones globales
options(quantmod.auto.assign = FALSE)
MY_TIINGO_TOKEN <- "aed55865812fe78363417505694d04c003812df8"

# Función para limpiar y procesar los tickers ingresados
parse_tickers <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))
  v <- unlist(strsplit(x, ","))
  v <- toupper(trimws(v))
  unique(v[nzchar(v)])
}

# Función para descargar precios usando la API de Tiingo (Más estable que Yahoo)
safe_get_prices <- function(ticker, from, to) {
  tryCatch({
    # Intentamos descargar de Tiingo con tu Token
    x <- suppressWarnings(
      getSymbols(ticker, 
                 src = "tiingo", 
                 api.key = MY_TIINGO_TOKEN, 
                 from = from, 
                 to = to, 
                 auto.assign = FALSE)
    )
    
    if (is.null(x) || nrow(x) == 0) return(NULL)
    
    # Extraemos el precio ajustado (Ad)
    adj <- Ad(x)
    adj <- na.omit(adj)
    colnames(adj) <- ticker
    return(adj)
  }, error = function(e) {
    return(NULL)
  })
}

# Helper para calcular retornos
to_returns <- function(P) {
  R <- Return.calculate(P, method = "discrete")
  R <- na.omit(R[-1, , drop = FALSE])
  return(R)
}

# Helper para convertir xts a formato JSON amigable para la web
xts_to_df <- function(xts_obj) {
  df <- data.frame(date = as.character(index(xts_obj)), coredata(xts_obj))
  rownames(df) <- NULL
  return(df)
}

#* @filter cors
cors_setup <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  plumber::forward()
}

#* @options /api/portafolio
function() { list(status = "OK") }

#* @post /api/portafolio
function(tickers = "AAPL,MSFT", pesos = "0.5,0.5", benchmark = "^GSPC", rf = 0.04) {
  
  tryCatch({
    tk_list <- parse_tickers(tickers)
    w_list <- as.numeric(unlist(strsplit(pesos, ",")))
    rf_rate <- as.numeric(rf)
    
    if(length(tk_list) != length(w_list)) {
      return(list(error = "La cantidad de activos y pesos no coincide."))
    }
    
    # Normalizar pesos para que sumen 1
    w_list <- w_list / sum(w_list)
    names(w_list) <- tk_list
    
    # Rango de fechas: últimos 3 años
    to_date <- Sys.Date()
    from_date <- to_date - (365 * 3)
    
    # Descarga de datos
    plist <- lapply(tk_list, safe_get_prices, from = from_date, to = to_date)
    plist_valid <- plist[!vapply(plist, is.null, logical(1))]
    
    if(length(plist_valid) < length(tk_list)) {
      return(list(error = "Tiingo no pudo encontrar algunos activos. Revisa los tickers."))
    }
    
    # Unión de precios y cálculo de retornos
    P <- do.call(merge, c(plist_valid, all = FALSE))
    P_bench <- safe_get_prices(benchmark, from = from_date, to = to_date)
    
    R <- to_returns(P)
    port_R <- Return.portfolio(R, weights = w_list, rebalance_on = "months")
    colnames(port_R) <- "Portafolio"
    
    bench_R <- if(!is.null(P_bench)) to_returns(P_bench) else NULL
    if(!is.null(bench_R)) colnames(bench_R) <- "Benchmark"
    
    # Métricas Financieras (PerformanceAnalytics)
    port_cagr <- Return.annualized(port_R, scale = 252)
    port_vol <- StdDev.annualized(port_R, scale = 252)
    port_sharpe <- SharpeRatio.annualized(port_R, Rf = rf_rate/252, scale = 252)
    port_maxdd <- maxDrawdown(port_R)
    
    # Datos para el gráfico (Índice de riqueza base 1)
    wealth_index <- cumprod(1 + port_R)
    chart_data <- xts_to_df(wealth_index)
    
    if(!is.null(bench_R)) {
      bench_wealth <- cumprod(1 + bench_R)
      # Unimos portafolio y benchmark para el gráfico
      M_wealth <- na.omit(merge(wealth_index, bench_wealth))
      chart_data <- xts_to_df(M_wealth)
    }
    
    return(list(
      status = "success",
      metricas = list(
        cagr = as.numeric(port_cagr),
        volatilidad = as.numeric(port_vol),
        sharpe = as.numeric(port_sharpe),
        max_drawdown = as.numeric(port_maxdd)
      ),
      graficos = list(crecimiento_historico = chart_data)
    ))
    
  }, error = function(e) {
    return(list(error = paste("Error en el motor financiero:", e$message)))
  })
}
