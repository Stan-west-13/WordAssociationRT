library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(tidyr)
lst <- list.files("data",   pattern = "marginal",  full.names = TRUE)

df_marginal <- map_dfr(lst, function(x){
    measure <- sub(".*_(.*?)\\.csv$", "\\1", x)
    df <- read_csv(x) %>%
        mutate(measure = measure)
    return(df)
}) %>%
    mutate(measure = factor(measure, levels = c("aoa","wl","wf")))



d_plot <-df_marginal  %>%
    rename(CIU = ci_upper, CIL = ci_lower) %>%
    group_by(condition,measure) %>%
    pivot_wider(names_from = c(condition),
                values_from = c(mean,CIU,CIL)) %>%
    group_by(measure) %>%
    mutate( diff_cp = mean_child - mean_peer,
            diff_sp = mean_short - mean_peer,
            diff_cs = mean_child - mean_short,
             se_grp_c = (CIU_child - CIL_child)/(2*qt(0.025,59)),
             se_grp_p = (CIU_peer - CIL_peer)/(2*qt(0.025,59)),
             se_grp_s = (CIU_short - CIL_short)/(2*qt(0.025,59)),
             se_diff_cp = sqrt(se_grp_c^2 + se_grp_p^2),
             se_diff_sp = sqrt(se_grp_s^2 + se_grp_p^2),
             se_diff_cs = sqrt(se_grp_c^2 + se_grp_s^2),
             cilower_cp =  diff_cp - qt(0.025,59)*se_diff_cp,
             ciupper_cp =  diff_cp + qt(0.025,59)*se_diff_cp,
             cilower_sp = diff_sp  - qt(0.025,59)*se_diff_sp,
             ciupper_sp = diff_sp  + qt(0.025,59)*se_diff_sp,
             cilower_cs = diff_cs - qt(0.025,59)*se_diff_cs,
             ciupper_cs = diff_cs + qt(0.025,59)*se_diff_cs,
            ) %>%
    pivot_longer(cols = c("diff_cp",
                          "diff_sp",
                          "diff_cs",
                          "cilower_cp",
                          "ciupper_cp",
                          "cilower_sp",
                          "ciupper_sp",
                          "cilower_cs",
                          "ciupper_cs"),
                names_to = c("comp","contrast"),
                values_to = "value",
                names_sep = "_") %>%
    pivot_wider(names_from = "comp",
                values_from = value) %>%
    mutate(contrast = factor(contrast, levels = c("cp","sp","cs"),labels = c("child - peer", 
                                                    "short - peer", 
                                                    "child - short")),
            measure = factor(measure, levels = c("aoa", "wl", "wf")))


ggplot(aes(x = contrast, y = diff,color = contrast), data = d_plot %>% filter(!measure == "cd")) +
    stat_summary(geom = "point", fun = "identity",size = 2, position = position_dodge(0.5))+
    geom_errorbar(aes(ymin = cilower, ymax = ciupper,width = 0), position = position_dodge(0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~measure, ncol = 3,
                         scales = "free",
                         labeller = as_labeller(c("aoa" = "Age of Acquisition", "wf" = "Frequency",  "wl" = "Word Length")))+
    theme_bw()+
    theme(
    axis.title.x = element_blank(),
    legend.title = element_blank() )+
    labs(y = "model marginal mean differences")
ggsave("Figures/psycholinguistic_analysisMM_group_diff.png", width = 12, height = 6)
ggsave("Figures/psycholinguistic_analysisMM_group_diff.svg", width = 12, height = 6)
ggsave("Figures/psycholinguistic_analysisMM_group_diff.pdf", width = 12, height = 6)