test_that("dotplot  works", {
  for(hc in c("superimpose", "overwrite")){
    for(relative in c(F, T)){
      for(es in c("constant", "hairpin")){
        compose_dotplot(
          data = ggplot2::diamonds,
          time = "carat",
          pitch = "price",
          handle_collisions = hc,
          relative = relative,
          envelope.shape = es,
          filename = paste0("dotplot_", hc,
                            "_relative=", relative,
                            "__envelope.shape=",  es,
                            ".wav"))
      }
    }
    library("ggplot2")
    ggplot(data = diamonds) +
      geom_point(aes(x = carat, y = price), alpha = 0.01) +
      ggsave("dotplot_superimpose.pdf")
  }
})

test_that("sp500 demo works", {
  for(bd in c(0.01, 0.1, 0.5)){
    for(cd in c(1, 10)){
      for(es in c("constant", "hairpin")){
        compose_dotplot(
          data = sp500,
          time = "DaysSinceStart",
          pitch = "Close",
          beep.duration = bd,
          chart.duration = cd,
          envelope.shape = es,
          filename = paste0(
            "sp500",
            "__beep.duration=", bd,
            "__chart.duration=", cd,
            "__envelope.shape=",  es,
            ".wav")
        )
      }
    }
  }
  ggplot(sp500) +
    geom_line(aes(x = DateLubridate, y = Close))+
    ggsave("sp500.pdf") +
    ggtitle("S&P 500 Performance") +
    xlab("Date") +
    ylab("Closing price")
})

