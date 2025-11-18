library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

creativity <- readRDS("tmp-data/creativity-all.rds")
stereotypy <- readRDS("tmp-data/stereotypy-all.rds")

x <- creativity |>
  count(condition, cue, corrected_response) |>
  group_by(condition, cue) |>
  summarize(score = mean(n == 1)) |>
  group_by() |>
  pivot_wider(id_cols = cue, names_from = condition, values_from = score)

x_contr <- x |>
  mutate(
    child_peer = child - peer,
    child_short = child - short,
    child_creative = child - creative,
    peer_short = peer - short,
    peer_creative = peer - creative,
    short_creative = short - creative
  ) |>
  select(-child, -peer, -short, -creative) |>
  pivot_longer(
    cols = c(child_peer:short_creative),
    names_to = "contrast",
    values_to = "scorediff"
  )

x_contr_avg <- x_contr |>
  group_by(contrast) |>
  summarize(
    mean_diff = mean(scorediff),
    s = sd(scorediff),
    se = s / sqrt(n()),
    ci = qt(.025, 60 - 4, lower.tail = FALSE) * se
  ) |>
  ungroup()

x_contr_avg |>
  ggplot(aes(x = contrast, y = mean_diff)) +
    geom_pointrange(aes(ymin = mean_diff - ci, ymax = mean_diff + ci))

               

d <- list(
  creativity = creativity |>
    mutate(score = if_else(condition == "peer", c_score_peer_corrected, creative_score_corrected)) |>
    select(
      participant,
      condition,
      cue,
      response = corrected_response,
      score
    ),
  stereotypy = stereotypy |>
    select(participant, condition, cue, response = corrected_response, score = stereotypy_score_corrected)
) |>
  list_rbind(names_to = "metric") |>
  mutate(metric = as.factor(metric))

d_contr <- d |>
  group_by(metric, condition, cue) |>
  summarize(score = mean(score)) |>
  pivot_wider(id_cols = c(metric, cue), names_from = condition, values_from = score) |>
  mutate(
    child_peer = child - peer,
    child_short = child - short,
    child_creative = child - creative,
    peer_short = peer - short,
    peer_creative = peer - creative,
    short_creative = short - creative
  ) |>
  select(-child, -peer, -short, -creative) |>
  pivot_longer(
    cols = c(child_peer:short_creative),
    names_to = "contrast",
    values_to = "scorediff"
  )

d_contr_avg <- d_contr |>
  group_by(metric, contrast) |>
  summarize(
    mean_diff = mean(scorediff),
    s = sd(scorediff),
    se = s / sqrt(n()),
    ci = qt(.025, 60 - 4, lower.tail = FALSE) * se
  ) |>
  ungroup()

d_contr_avg |>
  ggplot(aes(x = contrast, y = mean_diff)) +
    geom_pointrange(aes(ymin = mean_diff - ci, ymax = mean_diff + ci)) +
    facet_grid(vars(metric))

               