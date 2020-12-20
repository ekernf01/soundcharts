## Soundcharts

The `soundcharts` project aims to make it easy for data analysts to render quantitative data accessible to any hearing person. 

#### Installation notes

One main dependency is `espeak`, for text-to-speech labeling.

  sudo apt install espeak          

Another main dependency is `seewave`, used for rendering `.wav` files.

  # install seewave for wav generation
  sudo apt-get install libfftw3-3 libfftw3-dev
  sudo apt-get install libsndfile1 libsndfile1-dev
  sudo apt-get install libglu1-mesa-dev freeglut3-dev mesa-common-dev
  install.packages(c("fftw","tuneR","rpanel", "rgl"), repos="http://cran.at.r-project.org/")
  install.packages("seewave")
