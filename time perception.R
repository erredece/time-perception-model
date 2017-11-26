require(ggplot2)
# require(Cairo)
# CairoWin()
# Uncomment the previous two lines to use the Cairo package. This will prompt a new window
# where the plot, antialiased, will be generated, as RStudio for Windows does not antialise plots.

# Parameters
params <- data.frame(a = 1.02, b = 0.015, t0 = 100)
values <- c(2000, 2520, 3180, 4000, 5040, 6350, 8000) # 2 to 8 seconds
trainingProportion <- c(0.02, 0, 0.12, 0.5, 0.84, 0.91, 1) # Corresponding data

# Act-R noise
actr.noise <- function(sd,n=1) {
  rand <- runif(n,min=0.0001,max=0.9999)
  sd * log((1 - rand ) / rand)
}

# Miliseconds to pulses
msecToPulses <- function(t) {
  tick <- params$t0
  timeCounter <- tick
  tickCounter <- 0
  while(timeCounter <= t){
    sd <- params$b * params$a * tick
    tick <- params$a * tick + actr.noise(sd)
    timeCounter <- timeCounter + tick
    tickCounter <- tickCounter + 1
  }
  tickCounter
}

# Pulses to miliseconds
pulsesToMsec <- function(p) {
  timeCounter <- 0
  tick <- params$t0
  while(p > 0){
    sd <- params$b * params$a * tick
    tick <- params$a * tick + actr.noise(sd)  
    timeCounter <- timeCounter + tick
    p <- p - 1
  }
  timeCounter
}

# Model
response <- numeric(7)
proportion <- numeric(0)
anchorShort <- msecToPulses(values[1])
anchorLong <- msecToPulses(values[7])


## Presenting the values and responding 
for(i in 1:7){
  valuePulses <- msecToPulses(values[i])
  if(anchorLong - valuePulses <= valuePulses - anchorShort){
    response[i] <- 1 # 1 Stands for "long", and 0 stands for "short"
  }
}

for(i in 1:7){
  proportion[i] <- sum(response[1:i]) / sum(response)
}

# Plotting the data
ggplot(data.frame(values, proportion), aes(x=values, y=proportion)) + 
  geom_line(aes(y = proportion, colour = 'Model'), size=1) + 
  geom_point(size=3, colour='blue', shape=18) + 
  geom_line(aes(y = trainingProportion, colour = 'Human data'), size=1) + 
  geom_point(y = trainingProportion, size=3, colour='red', shape=17) +
  labs(x = 'Miliseconds', y = 'Proportion of \"long\" responses ', colour='') +
  scale_color_manual(values=c('red','blue')) + theme_bw() + 
  ggtitle('Comparison of \"long\" responses between human data and model') +
  theme(text = element_text(lineheight = .9, face='bold'))