library(tidyverse)
library(readxl)

wavy = read_csv("oscillation.csv")

#Create a filtered dataset of values outside of settled range.
#Find the max time of this dataset to determine when oscillation oscillation settled.
y_in_range = filter(wavy, abs(y) >= 0.5)
max_x = max(y_in_range$x)

#Filter for maximum values and find final value in this range to determine start of oscillation.
max_y = max(wavy$y)
low_range = max_y - (0.01 * max_y)
high_range = max_y + (0.01 * max_y)
max_y = wavy |> filter(between(y, left = low_range, right = high_range))
osc_start = max(max_y$x)

#Find time to settle and print
settle_time <- max_x - osc_start
print(settle_time)

ggplot(wavy, aes(x, y)) +
  geom_line() +
  geom_hline(yintercept = low_range, color = "Blue") +
  geom_hline(yintercept = high_range, color = "Blue") +
  geom_vline(xintercept = osc_start, color = "Orange") +
  geom_vline(xintercept = max_x, color = "Red") + 
  geom_segment(x = osc_start, y = 0, xend = max_x, yend = 0, color = "Purple") +
  annotate("text", x = (osc_start+max_x)/2, y = 0.1, label = toString(settle_time)) +
  labs(title = "Position vs. Time Mock Data") +
  xlab("Time") +
  ylab("Position")
  ggsave("oscillationexample.pdf")
