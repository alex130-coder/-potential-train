
## ://blog.ephorie.de/create-return-triangle-plots-with-r



library(quantmod)

return_triangle <- function(Symbol = "^GSPC", 
                            from = 2000,
                            to = 2020) {
  symbol <- getSymbols(Symbol,
                       from = paste0(from, "-01-01"),
                       to = paste0(to, "-12-31"),
                       auto.assign = FALSE)
  symbol_y <- coredata(to.yearly(symbol)[ , c(1, 4)])
  from_to <- seq(from, to)
  M <- matrix(NA,
              nrow = length(from_to),
              ncol = length(from_to))
  rownames(M) <- colnames(M) <- from_to
  
  for (buy in seq_along(from_to)) {
    for (sell in seq(buy, length(from_to))) {
      M[buy, sell] <- (symbol_y[sell, 2] - symbol_y[buy, 1]) / symbol_y[buy, 1]
    }
  }
  round(100 * M, 1)
}

rt <- return_triangle(from = 2009, to = 2020)
print(rt, na.print = "")


library(plot.matrix)
rt <- return_triangle(from = 2000, to = 2020)
bkp_par <- par(mar = c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(rt, digits = 1,
     text.cell = list(cex = 0.5),
     breaks = 15, 
     col = colorRampPalette(c("red", "white", "green1", "green2", "green3", "green4", "darkgreen")),
     na.print = FALSE,
     border = NA,
 #    key = NULL,
     main = "S&P 500",
     xlab = "sell",
     ylab = "buy")
par(bkp_par)
