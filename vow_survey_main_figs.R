library(tidyverse)
library(patchwork)
 
df <- vow_surveys

 
df_subset <- df %>% 
  select(year, starts_with("trust_"), drink_water_quality_loc)



# -----------------------------
# Compute percentages by year
# -----------------------------
# plot_data <- df_long %>%
#   group_by(year, Question, Response) %>%
#   summarise(Percent = n() / nrow(df) * 100, .groups = "drop")


plot_data <- df_long %>%
  group_by(year, Question)  %>%
  mutate(total_n_q = n())  %>% 
  group_by(year, Question, Response)  %>%
  summarise(Percent = n()/first(total_n_q)*100, .groups = "drop") %>% 
  distinct() %>% 
  # group_by(year, Question, Response)  %>%
  # summarise(Percent = n() / nrow(df) * 100, .groups = "drop") %>%
  group_by(year, Question) %>%
  mutate(check = sum(Percent))


# -----------------------------
# Plot: grouped bars across years
# -----------------------------
plot_panel_a <- ggplot(plot_data, aes(x = Response, y = Percent, fill = factor(year))) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Question, scales = "free_x") +
  labs(
    x = "",
    y = "Percent",
    fill = "Survey Year"
  ) +
  theme_minimal() +
  ylim(0,100)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        strip.text = element_text(size = 14, face = "bold") 
  )

 
#===============================================================================
#-----------------------------------PANEL B-------------------------------------
#===============================================================================

 
# -----------------------------
# Convert numeric 1–5 to labels (if needed)
# -----------------------------
response_labels <- c(
  "ufavS" = "Distrust strong",
  "ufavSW" = "Distrust a little",
  "favSW" = "Trust a little",
  "favS" = "Trust strong"
)

df_long <- df_subset %>%
  pivot_longer(
    cols = c("trust_water_safety","trust_pipes_safety"),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = recode(as.character(Response), !!!response_labels),
    Response = factor(
      Response,
      levels = c("Trust strong", "Trust a little", "Distrust a little", "Distrust strong", "Don't know / Refuse")
    ),
    Question = recode(Question,
                      trust_pipes_safety = "Trust water pipes \n in home are safe",
                      trust_water_safety = "Trust that drinking \n water delivered to \n home is safe"
    )
  )

# -----------------------------
# Compute percentages by year
# -----------------------------
# plot_data <- df_long %>%
#   group_by(year, Question, Response) %>%
#   summarise(Percent = n() / nrow(df) * 100, .groups = "drop")


plot_data <- df_long %>%
  group_by(year, Question)  %>%
  mutate(total_n_q = n())  %>% 
  group_by(year, Question, Response)  %>%
  summarise(Percent = n()/first(total_n_q)*100, .groups = "drop") %>% 
  distinct() %>% 
  # group_by(year, Question, Response)  %>%
  # summarise(Percent = n() / nrow(df) * 100, .groups = "drop") %>%
  group_by(year, Question) %>%
  mutate(check = sum(Percent))

plotb <- plot_data

# -----------------------------
# Plot: grouped bars across years
# -----------------------------
plot_panel_b <- ggplot(plot_data, aes(x = Response, y = Percent, fill = factor(year))) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Question, scales = "free_x") +
  labs(
    x = "",
    y = "Percent",
    fill = "Survey Year"
  ) +
  theme_minimal() +
  ylim(0,100)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        strip.text = element_text(size = 14, face = "bold") 
  )

 
#===============================================================================
#-----------------------------------PANEL C-------------------------------------
#===============================================================================

# -----------------------------
# Convert numeric 1–5 to labels (if needed)
# -----------------------------
response_labels <- c(
  "extremely concerned" = "Extremely Concerned",
  "very concerned" = "Very Concerned",
  "sw concerned" = "Somewhat Concerned",
  "not concerned" = "Not Concerned",
  "DK/NA" = "Don't know/NA"
)
 
# df_long <- df_subset %>%
#   pivot_longer(
#     cols = c("trust_drink_water_home"),
#     names_to = "Question",
#     values_to = "Response"
#   ) %>%
#   mutate(
#     Response = recode(as.character(Response), !!!response_labels),
#     Response = factor(
#       Response,
#       levels = c("Unfiltered tap", "Filtered tap", "Bottled water", "Other", "Don't know / Refuse")
#     ),
#     Question = recode(Question,
#                       trust_drink_water_home = "Water drunk at home most often"
#     )
#   )
df_long <- df_subset %>%
  pivot_longer(
    cols = c("drink_water_quality_loc"),
    names_to = "Question",
    values_to = "Response"
  )  %>% 
  mutate(
    Response = recode(as.character(Response), !!!response_labels),
    Response = factor(
      Response,
      levels = c("Not Concerned", "Somewhat Concerned", "Very Concerned", "Extremely Concerned", "NA")
    ),
    Question = recode(Question,
                      drink_water_quality_loc = "Concern with drinking water \n quality in local community"
    )
  )

# -----------------------------
# Compute percentages by year
# -----------------------------
 

plot_data <- df_long %>%
  group_by(year, Question)  %>%
  mutate(total_n_q = n())  %>% 
  group_by(year, Question, Response)  %>%
  summarise(Percent = n()/first(total_n_q)*100, .groups = "drop") %>% 
  distinct() %>% 
  # group_by(year, Question, Response)  %>%
  # summarise(Percent = n() / nrow(df) * 100, .groups = "drop") %>%
  group_by(year, Question) %>%
  mutate(check = sum(Percent))

plotc <- plot_data

# -----------------------------
# Plot: grouped bars across years
# -----------------------------
plot_panel_c <- ggplot(plot_data, aes(x = Response, y = Percent, fill = factor(year))) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Question, scales = "free_x") +
  labs(
    x = "",
    y = "Percent",
    fill = "Survey Year"
  ) +
  ylim(0,100)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        strip.text = element_text(size = 14, face = "bold") 
  )

 

 
plot_combined <- rbind(plotb, plotc) %>% 
  mutate(
    Question = gsub("\\s+", " ", Question),  
    Question = trimws(Question)
  )

 
ggplot(plot_combined, aes(x = Response, y = Percent, fill = factor(year))) +
  geom_col(position = position_dodge()) +
  facet_grid(~Question, scales = "free_x", labeller = label_wrap_gen(width = 25)) +
  labs(
    x = "",
    y = "Percent",
    fill = "Survey Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        strip.text = element_text(size = 14, face = "bold") 
  )

ggsave("figure4_revised.png", width = 14, height = 6, dpi = 300)







#===============================================================================
#-----------------------------------PANEL A-------------------------------------
#===============================================================================

# -----------------------------
# Convert numeric 1–5 to labels (if needed)
# -----------------------------
response_labels <- c(
  "never" = "Never",
  "some of the time" = "Some of the time",
  "most of the time" = "Most of the time",
  "always" = "Always"
)

df_long <- df_subset %>%
  pivot_longer(
    cols = starts_with("trust_gov"),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = recode(as.character(Response), !!!response_labels),
    Response = factor(
      Response,
      levels = c("Always", "Most of the time", "Some of the time", "Never", "Don't know / Refuse")
    ),
    Question = recode(Question,
                      trust_gov_local = "Trust in \n local government",
                      trust_gov_state = "Trust in \n state government"
    )
  )

plot_data <- df_long %>%
  group_by(year, Question)  %>%
  mutate(total_n_q = n())  %>% 
  group_by(year, Question, Response)  %>%
  summarise(Percent = n()/first(total_n_q)*100, .groups = "drop") %>% 
  distinct() %>% 
  # group_by(year, Question, Response)  %>%
  # summarise(Percent = n() / nrow(df) * 100, .groups = "drop") %>%
  group_by(year, Question) %>%
  mutate(check = sum(Percent))

plota <- plot_data

# -----------------------------
# Plot: grouped bars across years
# -----------------------------
plot_panel_a <- ggplot(plot_data, aes(x = Response, y = Percent, fill = factor(year))) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Question, scales = "free_x") +
  labs(
    x = "",
    y = "Percent",
    fill = "Survey Year"
  ) +
  theme_minimal() +
  ylim(0,100)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        strip.text = element_text(size = 14, face = "bold") 
  )


response_labels <- c(
  "unfiltered tap" = "Unfiltered tap",
  "filtered tap" = "Filtered tap",
  "bottled water" = "Bottled water",
  "Other" = "Other",
  "NA" = "Don't know/NA"
)

df_long <- df_subset %>%
  pivot_longer(
    cols = c("trust_drink_water_home"),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = recode(as.character(Response), !!!response_labels),
    Response = factor(
      Response,
      levels = c("Unfiltered tap", "Filtered tap", "Bottled water", "Other", "NA")
    ),
    Question = recode(Question,
                      trust_drink_water_home = "Water drunk at home \n most often"
    )
  )

plot_data <- df_long %>%
  group_by(year, Question)  %>%
  mutate(total_n_q = n())  %>% 
  group_by(year, Question, Response)  %>%
  summarise(Percent = n()/first(total_n_q)*100, .groups = "drop") %>% 
  distinct() %>% 
  # group_by(year, Question, Response)  %>%
  # summarise(Percent = n() / nrow(df) * 100, .groups = "drop") %>%
  group_by(year, Question) %>%
  mutate(check = sum(Percent))

plotd <- plot_data

# -----------------------------
# Plot: grouped bars across years
# -----------------------------
plot_panel_d <- ggplot(plot_data, aes(x = Response, y = Percent, fill = factor(year))) +
  geom_col(position = position_dodge()) +
  facet_wrap(~ Question, scales = "free_x") +
  labs(
    x = "",
    y = "Percent",
    fill = "Survey Year"
  ) +
  theme_minimal() +
  ylim(0,100)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        strip.text = element_text(size = 14, face = "bold") 
  )




plot_combined2 <- rbind(plota, plotd) %>% 
  mutate(
    Question = gsub("\\s+", " ", Question),  
    Question = trimws(Question)
  )


ggplot(plot_combined2, aes(x = Response, y = Percent, fill = factor(year))) +
  geom_col(position = position_dodge()) +
  facet_grid(~Question, scales = "free_x", labeller = label_wrap_gen(width = 25)) +
  labs(
    x = "",
    y = "Percent",
    fill = "Survey Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=13),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size=13),
        strip.text = element_text(size = 14, face = "bold") 
  )

ggsave("figure_appendix_additional_trust_vars.png", width = 14, height = 6, dpi = 300)


