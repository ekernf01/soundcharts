test_that("dotplot  works", {
  for(hc in c("superimpose", "overwrite")){
    for(relative in c(F, T)){
      compose_dotplot(
        data = ggplot2::diamonds,
        time = "carat",
        pitch = "price",
        handle_collisions = hc,
        relative = relative,
        filename = paste0("dotplot_", hc, "_relative=", relative, ".wav"))
    }
    library("ggplot2")
    ggplot(data = diamonds) +
      geom_point(aes(x = carat, y = price), alpha = 0.01) +
      ggsave("dotplot_superimpose.pdf")
  }
})

