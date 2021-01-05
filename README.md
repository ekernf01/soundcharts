## Soundcharts

The `soundcharts` project aims to make it easy for data analysts to render quantitative data accessible to any hearing person.  

#### Status

This is a work in progress containing only basic "scatterplot" type features. Planned features include:

- Removal of click/buzz artifacts and experimentation with resolution of dense time-series
- Legends on the time axis
- Experimentation with tone colors, chords, or stereo to display multiple series in comparison
- More demo datasets, including a sparse/simple demo of some sort and a Covid-19 demo
- Categorical data support for time axis and pitch axis
- Axis titles

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


### Credits

S&P 500 timecourse data were downloaded from Investopedia
 on Dec 21, 2020. URL: https://www.investopedia.com/ask/answers/042415/what-average-annual-return-sp-500.asp
