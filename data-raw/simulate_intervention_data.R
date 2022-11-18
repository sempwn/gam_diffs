## code to prepare `simulate_intervention_data` dataset goes here
library(tidyverse)
library(mgcv)
library(ggeffects)


#' plot data
plot_gam_predictions <- function(combined_dat,predict_color = "#8695e3",
                                 by=NULL){
  g <- combined_dat %>%
    ggplot(aes(x, predicted))
  if(!is.null(by)){
    g <- g +
      geom_point(aes(y=y,color=.data[[by]])) +
      geom_line(aes(color=.data[[by]])) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high,fill=.data[[by]]),
                  alpha=0.3)
  }else{
    g <- g +
      geom_point(aes(y=y)) +
      geom_line(color = predict_color) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                  fill= predict_color, alpha=0.3)
  }
  return(g)
}

#' create intervention data for purpose of comparing model fits
create_intervention_data <- function(n=312,
                                     int_start_time=100,
                                     alpha = 2,
                                     beta = 0.1,
                                     int_strength = -0.05,
                                     secular_trend = 0.5,
                                     seasonal_strength = 1.0) {
  x <- seq(1, n, by = 1)
  # intervention time
  x_int <- x - int_start_time
  x_int <- (x_int > 0) * x_int
  # week
  week <- x %% 52

  background_rate <- seasonal_strength * sin(2 * pi * x / 52) + secular_trend * log(x) # background rate
  intervention_rate <- int_strength * x_int ^ alpha * exp(-beta * x_int)

  y <- exp(background_rate + intervention_rate)
  y <- stats::rpois(n, lambda = y)

  background_rate <- exp(background_rate)
  intervention_rate <- exp(intervention_rate)

  random_data <- dplyr::tibble(x = x, y = y,
                               week = week,
                               intervention_time = x_int,
                               intervention_rate = intervention_rate,
                               background_rate = background_rate)
  return(random_data)
}

test_data <- create_intervention_data()

g <- test_data %>%
  ggplot(aes(x=x,y=y)) +
  geom_point() +
  geom_line(aes(y=intervention_rate,color="intervention rate")) +
  geom_line(aes(y=background_rate,color="background rate"))

show(g)

# run model
m <- gam(y ~  s(x, bs = "tp", k = 3) + s(week, bs = "cc") +
            s(intervention_time, bs = "tp"),
          data = test_data,
          family = "poisson")

plot_data <- predict(m,new_data = test_data,se.fit = TRUE) %>%
  as_tibble() %>%
  mutate(conf.low = fit - 1.96 * se.fit,
         conf.high = fit + 1.96 * se.fit) %>%
  mutate(across(everything(),exp)) %>%
  rename("predicted" = "fit") %>%
  bind_cols(test_data)

g <- plot_gam_predictions(plot_data)

show(g)


g <- ggpredict(m, terms = "week", interval = "predict") %>%
  as_tibble() %>%
  inner_join(slice(test_data,1:52),by="x") %>%
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.3) +
  geom_line(color = "#8695e3") +
  geom_point(aes(x=week,y=exp(background_rate)))

show(g)
# repeat draw from simulation to get true 95% CI




#usethis::use_data(simulate_intervention_data, overwrite = TRUE)
