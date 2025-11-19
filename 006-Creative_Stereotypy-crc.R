library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)


creativity <- readRDS("tmp-data/creativity-all.rds")
stereotypy <- readRDS("tmp-data/stereotypy-all.rds")

               
# Original scoring, but within-cue contrasts  ----
d <- list(
  creativity = creativity |>
    mutate(score = if_else(condition == "peer", c_score_peer_corrected, creative_score_corrected)) |>
    dplyr::select(
      participant,
      condition,
      cue,
      response = corrected_response,
      score
    ),
  stereotypy = stereotypy |>
    dplyr::select(participant, condition, cue, response = corrected_response, score = stereotypy_score_corrected)
) |>
  list_rbind(names_to = "metric") |>
  mutate(metric = factor(metric, c("stereotypy", "creativity")))

d_avg <- d |>
  group_by(metric, condition, cue) |>
  summarize(score = mean(score))


d_avg_grouped <- group_by(d_avg, metric)

d_aov_list <- d_avg_grouped |>
  group_split() |>
  map(~{
    aov(score ~ condition + Error(cue / condition), data = .x)
  })

names(d_aov_list) <- group_keys(d_avg_grouped)$metric

map(d_aov_list, summary)

# g eta ^2
# stereotypy
0.7176/(0.7176 + 1.4271)

  
d_contr <- d_avg |>
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
  dplyr::select(-child, -peer, -short, -creative) |>
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
    tval = mean_diff / se,
    df = 60 - 1,
    pval = pt(tval, 60-1, lower.tail = FALSE) * 2,
    ci = qt(.025, 60 - 1, lower.tail = FALSE) * se
  ) |>
  ungroup()

ci_d_contr_avg <- d_contr_avg %>% 
  mutate(ci_neg = mean_diff-ci, ci_plus =  mean_diff+ci)

d_contr_avg |>
  mutate(
    contrast = factor(
      contrast,
      levels = c(
        "child_peer",
        "child_short",
        "peer_short",
        "child_creative",
        "peer_creative",
        "short_creative"
      ),
      labels = c(
        "child-peer",
        "child-short",
        "peer-short",
        "child-creative",
        "creative-peer",
        "creative-short"
      )
    )
  ) |>
  ggplot(aes(x = contrast, y = mean_diff, color = metric)) +
    geom_pointrange(aes(ymin = mean_diff - ci, ymax = mean_diff + ci), position = position_dodge(.7), size = 1.5, linewidth = 1.5) +
    geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
    facet_grid(vars(metric)) +
    ylab("Within-cue difference") +
    scale_color_manual(values = c("#f0b400","#613F9D"))+
    theme_bw(base_size = 12) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none",
      plot.background = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1),
      panel.background = element_blank()
    )
ggsave(filename = 'Figures/stereotypy_condition-crc.png' ,width = 9.77, height = 7.3, dpi = 600, units = "in", device='png')


# New way of scoring, also within-cue contrasts ----
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
  dplyr::select(-child, -peer, -short, -creative) |>
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
    tval = mean_diff / se,
    df = 60 - 1,
    pval = pt(tval, 60-1, lower.tail = FALSE) * 2,
    ci = qt(.025, 60 - 1, lower.tail = FALSE) * se
  ) |>
  ungroup()

x_contr_avg |>
  mutate(
    contrast = factor(
      contrast,
      levels = c(
        "child_peer",
        "child_short",
        "peer_short",
        "child_creative",
        "peer_creative",
        "short_creative"
      ),
      labels = c(
        "child-peer",
        "child-short",
        "peer-short",
        "child-creative",
        "creative-peer",
        "creative-short"
      )
    )
  ) |>
  ggplot(aes(x = contrast, y = mean_diff)) +
    geom_pointrange(aes(ymin = mean_diff - ci, ymax = mean_diff + ci)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    ylab("Within-cue creativity difference") +
    theme_classic() +
    theme(
      axis.title.x = element_blank()
    )
