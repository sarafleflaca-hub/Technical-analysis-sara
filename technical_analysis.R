# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# Portfolio symbols: AMZN, GOOGL, TSLA
# Run this script from the assignment folder in RStudio.

required_packages <- c("quantmod", "TTR", "xts", "ggplot2")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# Read one ticker symbol per line from portfolio.txt.
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  if (!file.exists(file)) stop("portfolio.txt was not found in the working directory.")
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[nzchar(symbols)]
  if (length(symbols) == 0) stop("portfolio.txt contains no stock symbols.")

  stock_data <- list()
  for (symbol in symbols) {
    message("Downloading ", symbol, " ...")
    stock_xts <- tryCatch(
      getSymbols(symbol, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", symbol, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(stock_xts)) stock_data[[symbol]] <- stock_xts
  }
  if (length(stock_data) == 0) stop("No stock data was downloaded. Check symbols and internet connection.")
  return(stock_data)
}

# Statistical mode: base R's mode() reports storage type, so calculate
# the most frequently occurring numeric value instead.
statistical_mode <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) == 0) return(NA_real_)
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

calculate_statistics <- function(stock_xts, ma_period = 20) {
  close_col <- grep("\\.Close$", colnames(stock_xts), value = TRUE)
  if (length(close_col) == 0) stop("No Close price column found.")
  close_prices <- na.omit(Cl(stock_xts))
  price_vec <- as.numeric(close_prices)

  data.frame(
    Moving_Average_20 = as.numeric(last(TTR::SMA(close_prices, n = ma_period))),
    Mean = mean(price_vec),
    Mode = statistical_mode(price_vec),
    Median = median(price_vec),
    Standard_Deviation = stats::sd(price_vec),
    row.names = NULL
  )
}

display_stock_data <- function(stock_data, rows = 6) {
  for (symbol in names(stock_data)) {
    cat("\\n================ ", symbol, " ================\\n")
    print(tail(stock_data[[symbol]], rows))
    cat("\\nSummary statistics:\\n")
    print(calculate_statistics(stock_data[[symbol]]))
  }
}

plot_stock_data <- function(stock_data) {
  for (symbol in names(stock_data)) {
    close_prices <- data.frame(
      Date = as.Date(index(stock_data[[symbol]])),
      Close = as.numeric(Cl(stock_data[[symbol]]))
    )
    p <- ggplot2::ggplot(close_prices, ggplot2::aes(Date, Close)) +
      ggplot2::geom_line() +
      ggplot2::labs(title = paste(symbol, "Closing Price"),
                    x = "Date", y = "Closing price (USD)") +
      ggplot2::theme_minimal()
    print(p)
  }
}

# Execute analysis.
portfolio_data <- load_stock_data()
statistics_by_stock <- lapply(portfolio_data, calculate_statistics)
display_stock_data(portfolio_data)
print(do.call(rbind, lapply(names(statistics_by_stock), function(s) {
  cbind(Symbol = s, statistics_by_stock[[s]])
})))
plot_stock_data(portfolio_data)

# Optional: save results for submission.
write.csv(do.call(rbind, lapply(names(statistics_by_stock), function(s) {
  cbind(Symbol = s, statistics_by_stock[[s]])
})), "stock_statistics.csv", row.names = FALSE)
