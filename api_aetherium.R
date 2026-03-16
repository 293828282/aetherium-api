# ==========================================
# api_aetherium.R 
# Versión FINAL BLINDADA - Aetherium
# ==========================================

suppressPackageStartupMessages({
  library(plumber)
  library(quantmod)
  library(PerformanceAnalytics)
  library(dplyr)
  library(tidyr)
})

# Configuraciones y Token
options(quantmod.auto.assign = FALSE)
MY_TIINGO_TOKEN <- "aed55865812fe78363417505694d04c003812df8"

# 1. FILTRO DE SEGURIDAD (CORS) - ESTO ES LO QUE ESTABA BLOQUEANDO A CODEPEN
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

# 2. FUNCIONES DE APOYO
parse_tickers <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))
  v <- unlist(strsplit(x, ","))
  v <- toupper(trimws(v))
  unique(v[nzchar(v)])
}

safe_get_prices <- function(ticker, from, to) {
  tryCatch({
    # Limpiar ticker (Tiingo no usa el ^ de Yahoo)
    clean_ticker <- gsub("\\^", "", ticker)
    if(clean_ticker == "GSPC") clean_ticker <- "SPY" # Autocorrección de benchmark
    
    x <- getSymbols(clean_ticker, src = "tiingo", api.key = MY_TIINGO_TOKEN, 
                    from = from, to = to, auto.assign = FALSE)
    
    if (is.null(x) || nrow(x) == 0) return(NULL)
    adj <- Ad(x)
    adj <- na.omit(adj)
    colnames(adj) <- ticker
    return(adj)
  }, error = function(e) return(NULL))
}

# 3. EL CORAZÓN DE LA API
#* @post /api/portafolio
function(tickers = "AAPL,MSFT", pesos = "0.5,0.5", benchmark = "SPY", rf = 0.04) {
  
  tryCatch({
    tk_list <- parse_tickers(tickers)
    w_list <- as.numeric(unlist(strsplit(pesos, ",")))
    
    if(length(tk_list) != length(w_list)) {
      return(list(error = "La cantidad de activos y pesos no coincide."))
    }
    
    w_list <- w_list / sum(w_list)
    
    to_date <- Sys.Date()
    from_date <- to_date - (365 * 3)
    
    # Descarga de datos
    plist <- lapply(tk_list, safe_get_prices, from = from_date, to = to_date)
    plist_valid <- plist[!vapply(plist, is.null, logical(1))]
    
    if(length(plist_valid) < length(tk_list)) {
      return(list(error = "No se pudieron obtener datos de todos los activos. Revisa los nombres."))
    }
    
    P <- do.call(merge, c(plist_valid, all = FALSE))
    P_bench <- safe_get_prices(benchmark, from = from_date, to = to_date)
    
    # Retornos
    R <- na.omit(Return.calculate(P, method = "discrete"))
    port_R <- Return.portfolio(R, weights = w_list)
    colnames(port_R) <- "Portafolio"
    
    bench_R <- if(!is.null(P_bench)) na.omit(Return.calculate(P_bench, method = "discrete")) else NULL
    
    # Métricas
    port_cagr <- Return.annualized(port_R, scale = 252)
    port_vol <- StdDev.annualized(port_R, scale = 252)
    port_sharpe <- SharpeRatio.annualized(port_R, Rf = as.numeric(rf)/252, scale = 252)
    port_mdd <- maxDrawdown(port_R)
    
    # Datos para gráfico
    wealth <- cumprod(1 + port_R)
    df_chart <- data.frame(date = as.character(index(wealth)), Portafolio = as.numeric(wealth))
    
    if(!is.null(bench_R)) {
      bench_wealth <- cumprod(1 + bench_R)
      m_wealth <- merge(wealth, bench_wealth, all = FALSE)
      df_chart <- data.frame(
        date = as.character(index(m_wealth)),
        Portafolio = as.numeric(m_wealth[,1]),
        Benchmark = as.numeric(m_wealth[,2])
      )
    }
    
    return(list(
      status = "success",
      metricas = list(
        cagr = as.numeric(port_cagr),
        volatilidad = as.numeric(port_vol),
        sharpe = as.numeric(port_sharpe),
        max_drawdown = as.numeric(port_mdd)
      ),
      graficos = list(crecimiento_historico = df_chart)
    ))
    
  }, error = function(e) {
    return(list(error = paste("Error interno:", e$message)))
  })
}
