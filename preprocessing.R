library(tidyverse)

# loading the data -------------------------------------------------------------

setwd("C:/sync/experiment_social_cultural_exam/d_analysis")

data_files <- list.files(path = "./offline_data", full.names = T) %>% str_subset("\\.csv")
df_raw <- data_files %>% map_df( \(file) read_csv(file) %>% mutate(file = file))



# STEPS ------------------------------------------------------------------------
  # 1. check whether all works with the participant counter and the dates
  # 2. make participant counter into unique id
  # 3. get the general variables you'll need (condition, order the tasks were presented) for each participant (unique id - the participant counter)
  # 4. split the data for separate analysis of each task
  # 5. aggregate the general scores for each participant

# what are the columns of interest?
  
  # - which order the tasks appeared in
  # - total score at each point
  # - condition
  
  # ratio_bias
  # - position_x_large, position_x_small, 
  # - n_red in the large tray
  # - win or lose frame
  # - whether they drew red or not (so whether the score changed or not)
  # - the random number (just to check if all works)
  # - the rt to choose

  # invest_task
  # - how much they bet
  # - whether they lost or won
  # - the random number (just to check if all works)
  # - the rt to choose 
  # - possibly rt to confirm depending on how the rt is recorded

  # belief_bias
  # - which syllogism 
  # - the correct answer
  # - their response
  # - (new) did they respond correctly
  # - the rt to choose


# Step 1: inspect the data for any obvious problems ----------------------------
glimpse(df_raw)
head(df_raw)
tail(df_raw)



df_raw <- df_raw %>% 
  rename_with(.cols = everything(), str_to_lower)


# Step 2: make file name into unique id-------------------------------

#check if they don't repeat
data_files %>% length()
unique(df_raw$file) %>% length()

#to_do : change later
df_raw <- df_raw %>% 
  mutate(file = str_remove(file, "./offline_data/") %>%
           str_remove("_scd_experiment_2023-") %>%
           str_remove("leave_blank") %>%
           str_remove(".csv")
         ) %>% 
  rename(id = file) %>% 
  fill(who, .direction = "down") %>% 
  mutate(condition = case_when(
    who == "You" ~ "individual",
    who == "Your group" ~ "group"
    ),
    .keep = 'unused',
    .after = 'id'
  ) %>% 
  mutate(across(c(id, condition), ~ as_factor(.x)))

# check if all works
unique(df_raw$id) %>% length()

# clean-up
rm(data_files)



# Step 3: get the general info about each participant---------------------------

df_general_info <- df_raw %>% 
  select(id, condition, resp_ratio_bias_task.keys, slider_invest.response, 
         key_syllogism.keys, total_score, 
         starts_with("survey"), feedback.feedback)
  
  
# demographics
df_general_info <- df_general_info %>% 
  rename("survey.feedback" = "feedback.feedback") %>% 
  group_by(id) %>% 
  fill(starts_with("survey"), .direction = "updown") %>% 
  rename_with(.cols = starts_with("survey"), ~ .x %>% str_remove("^.*\\.")) %>% 
  mutate(
    gender = case_when(
      gender == "item1Pre" ~ "female",
      gender == "item2" ~ "male"
    ) %>% 
      as_factor(),
    cognitive_science = case_when(
      cognitive_science == "item1" ~ "yes",
      cognitive_science == "item2" ~ "no"
    ) %>% 
      as_factor()
  )
  

# task order
df_general_info <- df_general_info %>% 
  rename(
    ratio_bias = resp_ratio_bias_task.keys,
    investment_task = slider_invest.response,
    belief_bias = key_syllogism.keys
  ) %>% 
  # NAs
  filter(!if_all(c(ratio_bias, investment_task, belief_bias), ~ is.na(.x))) %>%
  # order
    mutate(
      investment_task = as.character(investment_task),
      across(c(ratio_bias, investment_task, belief_bias), ~ if_else(!is.na(.), cur_column(), .)),
      order = coalesce(ratio_bias, investment_task, belief_bias)
    ) %>% 
  group_by(id) %>% 
  mutate(order = str_flatten(unique(order), collapse = ", ") %>% as_factor,
         ratio_bias = NULL,
         investment_task = NULL,
         belief_bias = NULL
         )
# fixing total score - when belief bias is last task it's NA is last row
df_general_info <- df_general_info %>% 
  group_by(id) %>% 
  fill(total_score, .direction = 'down') %>%
  mutate(
    total_score = if_else(
      row_number() == n() &
        # i only want to change the last row in each group
      str_detect(order, pattern = fixed("belief_bias,"), negate = TRUE), 
        # when belief bias last it is not followed by a comma
      total_score + 360,
      total_score
    ))

# one row for each
df_general_info <- df_general_info %>% 
  group_by(id) %>% 
  slice_tail(n = 1) %>%
  rename("final_score" = "total_score") %>% 
  ungroup()

write_csv(df_general_info, "background_data.csv")


# describing / summarising the demographics

head(df_general_info, n = 16)

df_general_info %>% 
  count(order)

df_general_info %>% 
  count(gender)

# check the samples sizes for each condition
df_general_info %>% 
  count(condition)
# The counterbalancing failed miserably. I'm sorry about that. 
# I explain why and how it happened in the discussion section of the paper

df_general_info %>% 
  count(condition, gender)

df_general_info %>% 
  summarise(
    across(
      .cols = c(education, age),
      .fns = list(mean = mean, sd = sd),
      .names = "{.col}_{.fn}"
    )
  )


df_general_info %>% 
  ggplot(aes(x = "", y = education)) +
  geom_count() +
  geom_boxplot(width = 0.1, alpha = 0.4)

df_general_info %>% 
  ggplot(aes(x = "", y = age)) +
  geom_count() +
  geom_boxplot(width = 0.1, alpha = 0.4)

# Step 4: Preprocessing of each task -------------------------------------------

## (a) ratio bias --------------------------------------------------------------

df_ratio_bias <- df_raw %>% 
  select(id, condition, total_score, position_x_small, position_x_large, trail_n_red, marbles_draw, contains("ratio"))

df_ratio_bias %>% glimpse()

df_ratio_bias <- df_ratio_bias %>% 
  select(!contains('loop') & !contains('feedback')) %>% 
  select(!contains('instructions') | !contains(".rt"))

df_ratio_bias %>% glimpse()

df_ratio_bias <- df_ratio_bias %>% 
  rename(
    win_frame = key_ratio_bias_instructions.keys,
    lose_frame = key_ratio_bias_instructions_lose.keys,
    n_red_large = trail_n_red,
    rt_ratio_bias = resp_ratio_bias_task.rt
  ) %>%
  # getting the order of the lose/win blocks
  mutate(
    .by = id,
    across(c(win_frame, lose_frame), ~ if_else(.x == 'space', cur_column(), .x)),
    current_frame = coalesce(win_frame, lose_frame), 
    frame_order = str_flatten(current_frame %>% na.omit(.) %>% unique(.), collapse = ', '),
    across(c(win_frame, lose_frame), ~ {.x = NULL}),
    .after = position_x_large
    ) %>%
  fill(current_frame, .direction = 'down') %>% 
  filter(if_all(contains('position_x'), ~ !is.na(.x))) %>% 
  mutate(
    n_red_small = 1,
    across(contains('position_x'), 
           ~ case_when(
             .x > 0 ~ 'right',
             .x < 0 ~ 'left'
           )
    ), 
    choice = case_when(
      resp_ratio_bias_task.keys == 'j' ~ "right",
      resp_ratio_bias_task.keys == 'f' ~ "left"
    ),
    nonoptimal = case_when(
      n_red_large == 10 ~ 0,
      current_frame == "win_frame" & position_x_large == choice ~ 1,
      current_frame == "lose_frame" & position_x_small == choice ~ 1, 
      .default = 0
    ),
    n_red_magnitude = if_else(n_red_large > 6, "high", "low"),
    # nonoptimal_scale = nonoptimal * (n_red_small/10 - n_red_large/100) * 100,
    
    # round_score = case_when(
    #   current_frame == "win_frame" & marbles_draw <= 10 ~ 100,
    #   current_frame == "lose_frame" & marbles_draw <= 10 ~ -100,
    #   .default = 0
    # ),
    .after = n_red_large
  )



df_ratio_bias <- df_ratio_bias %>% 
  select(id, condition, frame_order, nonoptimal, n_red_magnitude, current_frame, 
           choice, position_x_large, total_score, n_red_large, rt_ratio_bias)


write_csv(df_ratio_bias, "ratio_bias_data.csv")


## (b) investment task ----------------------------------------------------------
df_raw %>% glimpse()

df_investment_task <- df_raw %>% 
  select(id, condition, total_score, round_score, invest_success, contains('invest'))

df_investment_task %>% glimpse()

df_investment_task <- df_investment_task %>% 
  select(!contains("instruction") & !contains("loop") & !contains("feedback") & !key_invest_confirm.keys) %>% 
  rename(
    response_investment = slider_invest.response,
    rt_response_investment = slider_invest.rt,
    rt_confirm_investment = key_invest_confirm.rt,
    )

df_investment_task %>% glimpse()

df_investment_task <- df_investment_task %>%
  filter(if_all(.cols = !c(id, condition), ~ !is.na(.x))) %>% 
  # some bug in PsychoPY/JS forced me shift the actual range of the responses by +5 (would not work properly otherwise) so i have to reverse it now
  mutate(
    rt = rt_response_investment + rt_confirm_investment,
    response_investment = response_investment - 5,
    nonoptimal = if_else(response_investment == 100, 0, 1) %>% as_factor,
    nonoptimal_scale = 100 - response_investment
  )

glimpse(df_investment_task)
head(df_investment_task)

df_investment_task <- df_investment_task %>% 
  select(id, condition, nonoptimal, nonoptimal_scale, response_investment, round_score, rt)


write_csv(df_investment_task, "invest_task_data.csv")

## (c) belief bias task ---------------------------------------------------------
df_raw %>% glimpse()

df_belief_bias <- df_raw %>% 
  select(id, condition, contains("belief") | contains("syllogism"))

df_belief_bias %>% glimpse()  

df_belief_bias <- df_belief_bias %>% 
  select(!contains("instructions") & !contains("feedback") & !contains("loop")) %>% 
  rename(
    rt = key_syllogism.rt,
    correct = key_syllogism.corr,
    response = key_syllogism.keys,
  )

df_belief_bias %>% glimpse()

df_belief_bias <- df_belief_bias %>% 
  filter(if_all(.cols = !c(id, condition), ~ !is.na(.x))) %>% 
  mutate(
    nonoptimal = abs(correct - 1) %>% as_factor(),
    correct = correct %>% as_factor()
  )

glimpse(df_belief_bias)
head(df_belief_bias)

df_belief_bias <- df_belief_bias %>% 
  select(id, condition, nonoptimal, correct, rt, syllogism)

write_csv(df_belief_bias, "belief_bias_data.csv")
