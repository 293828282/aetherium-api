# ==========================================
# api_aetherium.R
# Backend API para Análisis de Portafolios
# ==========================================
install.packages(c("dplyr", "tidyr"))
suppressPackageStartupMessages({
  library(plumber)
  library(quantmod)
  library(PerformanceAnalytics)
  library(dplyr)
  library(tidyr)
})

options(quantmod.auto.assign = FALSE)

# ------------------------------------------
# 1. FUNCIONES CORE Y MANEJO DE DATOS
# ------------------------------------------

parse_tickers <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))
  v <- unlist(strsplit(x, ","))
  v <- toupper(trimws(v))
  unique(v[nzchar(v)])
}

safe_get_prices <- function(ticker, from, to) {
  tryCatch({
    x <- suppressWarnings(getSymbols(ticker, src = "yahoo", from = from, to = to, auto.assign = FALSE, warnings = FALSE))
    adj <- Ad(x)
    adj <- na.omit(adj)
    colnames(adj) <- ticker
    return(adj)
  }, error = function(e) NULL)
}

to_returns <- function(P) {
  R <- Return.calculate(P, method = "discrete")
  R <- na.omit(R[-1, , drop = FALSE])
  return(R)
}

# Función clave: Convierte series xts a dataframes planos para la web (JSON)
xts_to_df <- function(xts_obj, col_name = "value") {
  df <- data.frame(date = as.character(index(xts_obj)), coredata(xts_obj))
  rownames(df) <- NULL
  return(df)
}

# ------------------------------------------
# 2. DEFINICIÓN DE LA API (PLUMBER)
# ------------------------------------------

#* @filter cors
cors_setup <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  plumber::forward()
}

#* @options /api/portafolio
function() { list(status = "OK") }

#* Analiza el portafolio y devuelve métricas y datos para gráficos
#* @param tickers:character Tickers separados por coma (ej: "AAPL,MSFT")
#* @param pesos:character Pesos separados por coma (ej: "0.6,0.4")
#* @param benchmark:character Ticker del benchmark (ej: "^GSPC")
#* @param rf:numeric Tasa libre de riesgo (ej: 0.04)
#* @post /api/portafolio
function(tickers = "AAPL,MSFT", pesos = "0.5,0.5", benchmark = "^GSPC", rf = 0.04) {
  
  # Usar tryCatch maestro para que la API nunca se caiga y devuelva errores limpios
  tryCatch({
    
    # 1. Parsear inputs
    tk_list <- parse_tickers(tickers)
    w_list <- as.numeric(unlist(strsplit(pesos, ",")))
    rf_rate <- as.numeric(rf)
    
    if(length(tk_list) != length(w_list)) {
      return(list(error = "La cantidad de tickers y pesos no coincide."))
    }
    
    # Normalizar pesos (asegurar que sumen 1)
    w_list <- w_list / sum(w_list)
    names(w_list) <- tk_list
    
    # Fechas: Últimos 3 años por defecto para tener buena data estadística
    to_date <- Sys.Date()
    from_date <- to_date - (365 * 3)
    
    # 2. Descargar Datos
    plist <- lapply(tk_list, safe_get_prices, from = from_date, to = to_date)
    P <- do.call(merge, c(plist[!vapply(plist, is.null, logical(1))], all = FALSE))
    
    if(is.null(P) || ncol(P) < length(tk_list)) {
      return(list(error = "No se pudieron descargar todos los activos de Yahoo Finance."))
    }
    
    # Descargar Benchmark
    P_bench <- safe_get_prices(benchmark, from = from_date, to = to_date)
    
    # 3. Cálculos de Retornos
    R <- to_returns(P)
    port_R <- Return.portfolio(R, weights = w_list, rebalance_on = "months")
    colnames(port_R) <- "Portafolio"
    
    bench_R <- if(!is.null(P_bench)) to_returns(P_bench) else NULL
    if(!is.null(bench_R)) colnames(bench_R) <- "Benchmark"
    
    # 4. Métricas Estadísticas (CAGR, Vol, Sharpe, MaxDD)
    port_cagr <- Return.annualized(port_R, scale = 252)
    port_vol <- StdDev.annualized(port_R, scale = 252)
    port_sharpe <- SharpeRatio.annualized(port_R, Rf = rf_rate/252, scale = 252)
    port_maxdd <- maxDrawdown(port_R)
    
    # 5. Preparar datos para gráficos (Growth of $1)
    wealth_index <- cumprod(1 + port_R)
    chart_data <- xts_to_df(wealth_index)
    
    if(!is.null(bench_R)) {
      bench_wealth <- cumprod(1 + bench_R)
      # Alinear fechas
      M_wealth <- na.omit(merge(wealth_index, bench_wealth))
      chart_data <- xts_to_df(M_wealth)
    }
    
    # 6. Construir la respuesta JSON perfecta
    return(list(
      status = "success",
      inputs = list(
        activos = tk_list,
        pesos_normalizados = w_list
      ),
      metricas = list(
        cagr = as.numeric(port_cagr),
        volatilidad = as.numeric(port_vol),
        sharpe = as.numeric(port_sharpe),
        max_drawdown = as.numeric(port_maxdd)
      ),
      graficos = list(
        crecimiento_historico = chart_data
      )
    ))
    
  }, error = function(e) {
    return(list(error = paste("Error interno en el cálculo:", e$message)))
  })
}