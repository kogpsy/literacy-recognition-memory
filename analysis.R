library(tidyverse)

cambridge <- read_delim("data/cambridge.tsv",
                        delim = "\t", escape_double = FALSE,
                        trim_ws = TRUE) |>
    filter(!(visual_noise %in% "learn"))


reading_scores <- read_delim("data/reading_intercepts.tsv",
                             delim = "\t", escape_double = FALSE,
                             trim_ws = TRUE) |>
    drop_na(raw_reading_score, adjusted_reading_score)


reading <- reading_scores |>
    select(pp, raw_reading_score, adjusted_reading_score)


centre <- function(x) {
    x - mean(x)
}

d <- cambridge |>
    left_join(reading, by = "pp") |>
    drop_na() |>
    mutate(across(c(raw_reading_score, adjusted_reading_score),
                  centre),
           across(c(visual_noise, category),
                  as_factor),
           visual_noise = fct_recode(visual_noise,
                                     no = "no_noise",
                                     yes = "noise"))



summary <- d |>
    group_by(literate, category, visual_noise) |>
    summarise(accuracy = mean(ACC))




## models ----
library(brms)


fit1 <- brm(ACC ~ 0 + category*visual_noise*adjusted_reading_score + (1|pp),
           family = bernoulli(),
           data = d)

# fit2 <- brm(ACC ~ category*visual_noise*raw_reading_score,
#             family = bernoulli(),
#             data = d)
#

fit1
mcmc_plot(fit1)

# conditions <- expand_grid(category = levels(d$category),
#                           visual_noise = levels(d$visual_noise)) |>
#     mutate(across(is.character, as_factor))

#
# conditions <- make_conditions(fit1, c("visual_noise", "category"))
#
# conditional_effects(fit1,
#                     effects = "adjusted_reading_score",
#                     conditions = conditions)

# theme_set(theme_grey(base_size = 14) +
#               theme(panel.grid = element_blank()))

library(ggthemes)
conditions <- make_conditions(fit1, "category")

p1 <- conditional_effects(fit1,
                    effects = "adjusted_reading_score:visual_noise",
                    conditions = conditions)

plot(p1, plot = F)[[1]] +
    scale_fill_pander() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank())




