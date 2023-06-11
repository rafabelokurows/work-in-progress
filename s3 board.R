install.packages(
  "paint", 
  repos = c(mm = "https://milesmcbain.r-universe.dev", getOption("repos")))
library(paint)

paint::mask_print()
head(datasets::Titanic)
options(paint_palette = my_magma)

paint(mtcars)
options(paint_palette =viridis_6())
unpaint(df = .Last.value)
head(mtcars)

library(tidyverse)
1:10 %>% sqrt()
library(tidymodels)
install.packages("pins")
library(tidymodels)
library(pins)
board = board_s3("mlandstuff", region = "eu-central-1")
