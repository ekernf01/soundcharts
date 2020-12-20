div_by_max = function(x){
  if(max(x)==0){
    return(x*0)
  }
  x / max(x)
}

#' Convert an abstract set of numbers into frequencies suitable for human ears.
#'
#' @param lower @param upper Most extreme pitches.
#' Default is A440 plus or minus an octave.
#' @param relative If false, equal intervals in data are separated by equal
#' intervals in absolute pitch (equal numbers of Hz).
#' If true, equal intervals in data are separated by equal
#' intervals in relative pitch (equal numbers of cents or semitones or octaves).
#' Relative pitch is preferred for perceptual accuracy.
#'
scale_pitch = function(x, lower = 220, upper = 880, relative = T){
  # Scale/shift to [0, 1]
  range = max(x) - min(x)
  if(range==0){
    z = 0.5
  } else{
    z = (x - min(x)) / range
  }
  # Convert to pitch in Hz
  if(relative){
    return( exp( z * (log(upper) - log(lower)) + log(lower)) )
  } else {
    return(z * (upper - lower) + lower)
  }
}

#' Add a text-to-speech legend for the pitch scale.
#' text-to-speech rendering of the values ("five six seven eight") is followed by
#' arpeggiation of the corresponding pitches ("boo doe daa deep").
#'
add_pitch_legend = function(x,
                            filename,
                            break_values = pretty(x, 3),
                            stagger.duration = 0.25,
                            beep.duration = 0.75, ...){
  # Spoken values
  temp_file = tempfile()
  paste0(break_values, collapse = " ") %>%
    gsub("-", " minus ", . , fixed = T) %>%
    paste0(  "espeak -v english-us -s 150 -w ", temp_file, " '", ., "'") %>%
    system()
  legend.speech = tuneR::readWave(temp_file)
  # Corresponding arpeggio
  break_pitches = scale_pitch(c(max(x), min(x), break_values), ...)[-(1:2)]
  legend.rate = legend.speech@samp.rate
  beep.length    = legend.rate * beep.duration
  stagger.length = legend.rate * stagger.duration
  legend.wave = rep( 0, beep.length + stagger.length * length( break_pitches ) )
  for( b in seq_along( break_pitches ) ){
    idx = (b-1)*stagger.length + (1:beep.length)
    legend.wave[idx] = legend.wave[idx] +
      seewave::synth2(env =   rep(10,              beep.length ),
                      ifreq = rep(break_pitches[b], beep.length),
                      f = legend.rate)
  }
  suppressWarnings(seewave::savewav(wave = c(div_by_max(legend.speech@left), div_by_max(legend.wave)),
                                    f = legend.rate,
                                    filename = filename,
                                    rescale=c(-3000,3000) ))
  return(filename)
}

#' Make a dotplot where x axis is time, y axis is pitch, and each dot is a beep.
#'
#' @param data Dataframe in long format.
#' @param time @param pitch Columns from `data`.
#' @param filename Where to put results. This should end in `.wav`.
#' @param chart.duration How many seconds the sound file plays for.
#' @param beep.duration How many seconds an individual beep or point lasts.
#' @param ... Args sent to scale_pitch().
#'
compose_dotplot = function(
  data,
  time,
  pitch,
  filename,
  handle_collisions = "superimpose",
  chart.duration = 10,
  beep.duration = 0.1,
  ...
){
  # Sanitize input
  collision_handling_options = c("superimpose", "overwrite")
  if(!handle_collisions %in% collision_handling_options){
    stop("handle_collisions must match one of\n" %>% paste(paste(collision_handling_options, collapse = " ")))
  }
  # Set up pitch scale
  legend.wave = tuneR::readWave(add_pitch_legend(x = data[[pitch]], filename = tempfile(), ...))
  scaled_data = data.frame( pitch = scale_pitch(data[[pitch]], ...),
                            stringsAsFactors = F )
  chart.rate = legend.wave@samp.rate
  # Figure out chart and beep sample number
  chart.length = chart.rate * chart.duration
  beep.length  = chart.rate * beep.duration
  # Place all attacks early enough to avoid lengthening the chart
  chart.length.last.attack = chart.length - beep.length
  beep.attacks = round(chart.length.last.attack * data[[time]] / max(data[[time]]))
  chart.wave = rep(0, chart.length)
  # Lovingly place each beep
  for( i in seq_along(beep.attacks) ){
    a = beep.attacks[[i]]
    if( handle_collisions == "superimpose"){
      chart.wave[ a + 1:beep.length ] =
        chart.wave[ a + 1:beep.length ] +
        seewave::synth2(env = rep(10, beep.length),
                        ifreq = rep(scaled_data[["pitch"]][[i]], beep.length),
                        f = chart.rate)
    } else if( handle_collisions == "overwrite" ) {
      chart.wave[ a + 1:beep.length ] =
        seewave::synth2(env = rep(10, beep.length),
                        ifreq = rep(scaled_data[["pitch"]][[i]], beep.length),
                        f = chart.rate)
    } else {
      stop("Shouldn't reach here -- please notify Eric about bug #16.")
    }
  }
  suppressWarnings(seewave::savewav(wave = c(div_by_max(legend.wave@left), div_by_max(chart.wave)),
                                    f = chart.rate,
                                    filename = filename,
                                    rescale=c(-3000,3000) ))
}


