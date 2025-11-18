library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

d <- readRDS("tmp-data/filter-participants-corrected.rds")

d_list <- d |>
  select(condition, participant, cue, corrected_response) |>
  split(d$condition)

d |>
  select(condition, participant, cue, corrected_response) |>
  group_by(condition, cue) |>
  summarize(n = n_distinct(participant)) |>
  pivot_wider(id_cols = cue, names_from = condition, values_from = n)

d_grp <- d |>
  filter(cue == "abhor") |>
  group_by(condition)

abhor_resps <- d_grp |>
  group_split() |>
  map(~ pull(.x, corrected_response))

names(abhor_resps) <- group_keys(d_grp)$condition

d_list$child |>
  filter(cue == "abhor")


x <- d_list |>
  map(function(df_all_pp) {
    split(df_all_pp, df_all_pp$participant) |>
      imap(function(df_one_pp, pp_id, df_all_pp) {
        df_other_pp <- df_all_pp |> filter(participant != pp_id)
        map2(
          split(df_one_pp, df_one_pp$cue),
          split(df_other_pp, df_other_pp$cue),
          function(x, y) {
            x |>
              mutate(is_creative = !(corrected_response %in% y$corrected_response))
          }
        ) |>
          list_rbind(names_to = "cue")
      }, df_all_pp = df_all_pp) |>
      list_rbind(names_to = "participant")
  }) |>
  list_rbind(names_to = "condition")

x |>
  mutate(condition = factor(condition, c("child", "peer", "short", "creative"))) |>
  group_by(condition, cue) |>
  summarize(p_creative_cuecond = mean(is_creative)) |>
  pivot_wider(id_cols = cue, names_from = condition, values_from = p_creative_cuecond) |>
  mutate(peer_child = peer - child, short_child = short - child, creative_child = creative - child) |>
  pivot_longer(
    cols = c(peer_child, short_child, creative_child),
    names_to = "contrast",
    values_to = "diff_p_creative"
  ) |>
  group_by(contrast) |>
  summarize(
    mean_diff_p_creative = mean(diff_p_creative),
    s = sd(diff_p_creative),
    se = sd(diff_p_creative) / sqrt(n()),
    ci = qt(.025, df = 60 - 4, lower.tail = FALSE) * se
  ) |>
  mutate(
    contrast = factor(contrast, levels = c("peer_child", "short_child", "creative_child"))
  ) |>
  ggplot(aes(x = contrast, y = mean_diff_p_creative)) +
  ylim(c(-.05, .15)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_pointrange(aes(ymin = mean_diff_p_creative - ci, ymax = mean_diff_p_creative + ci)) +
  theme_classic()
