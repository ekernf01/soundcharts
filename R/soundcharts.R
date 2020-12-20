setwd("~/Desktop/software_projects/soundcharts")
usethis::create_package(".")
# install seewave for wav generation
# sudo apt-get install libfftw3-3 libfftw3-dev 
# sudo apt-get install libsndfile1 libsndfile1-dev 
# sudo apt-get install libglu1-mesa-dev freeglut3-dev mesa-common-dev
# install.packages(c("fftw","tuneR","rpanel", "rgl"), repos="http://cran.at.r-project.org/")
# install.packages("seewave")

# Setup 
library("seewave")

scale_pitch = function(x, lower = 500, upper = 1500){
  range = max(x) - min(x)
  if(range==0){
    return(rep((upper + lower) / 2, length(x)))
  } else{
    z = (x - min(x)) / range
    return(z * (upper - lower) + lower)
  }
}
soundchart = function(data, 
                      time,
                      pitch, 
                      filename = "demo.wav", 
                      chart.duration = 10, 
                      chart.rate = 10000, 
                      beep.duration = 0.1){
  scaled_data = data.frame( pitch = scale_pitch(data[[pitch]]), stringsAsFactors = F )
  # Figure out chart and beep sample number
  chart.length = chart.rate * chart.duration
  beep.length  = chart.rate * beep.duration
  #place all attacks early enough to avoid lengthening the chart
  chart.length.attacks = chart.length - beep.length 
  beep.support = round(chart.length.attacks * data[[time]] / max(data[[time]]))
  chart.wave = rep(0, chart.length)
  for( i in seq_along(beep.support) ){
    a = beep.support[[i]]
    chart.wave[ a + 1:beep.length ] =
      chart.wave[ a + 1:beep.length ] +
      seewave::synth2(env = rep(10, beep.length), 
                      ifreq = rep(scaled_data[["pitch"]][[i]], beep.length),
                      f = chart.rate)
   }
  seewave::savewav(wave = chart.wave, f = chart.rate, filename = filename, rescale=c(-3000,3000) )
}
soundchart(data = ggplot2::diamonds, time = "carat", pitch = "price")

library("ggplot2")
ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price), alpha = 0.01) + 
  ggsave("demo.pdf")
