stereo_cre <- d %>% 
  group_by(condition,cue) %>%
  count(response, sort = TRUE,name = "resp_count") %>%
  left_join(d, by = c("response","cue","condition")) %>%
  mutate(mst_freq = ifelse(resp_count == max(resp_count), TRUE,FALSE)) %>%
  group_by(condition,cue,response) %>%
  mutate(is_unique = ifelse(resp_count == 1,TRUE,FALSE)) %>%
  group_by(participant) %>%
  mutate(sum_stereo = sum(mst_freq),
         sum_creative = sum(is_unique))


ggplot(stereo_cre %>%
         mutate(condition = factor(condition,
           levels = c("peer", "child", "short", "creative")
         )), aes(x = condition, y = sum_stereo))+
  stat_summary(fun = "mean", 
               geom = "col", 
               position = "dodge") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(width = 0.90))+
  labs(y = "avg_stereptypy")
