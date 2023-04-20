# Diagnostic plots

plot_trace <- function(model, params) {
  model %>% 
    tidybayes::gather_draws(!!!syms(params)) %>% 
    ggplot(aes(x = .iteration, y = .value, color = factor(.chain))) +
    geom_line(linewidth = 0.1) +
    scale_color_viridis_d(option = "rocket", end = 0.85) +
    labs(color = "Chain") +
    facet_wrap(vars(.variable), scales = "free_y") +
    theme_pandem()
}

plot_trank <- function(model, params) {
  model %>% 
    tidybayes::gather_draws(!!!syms(params)) %>% 
    group_by(.variable) %>% 
    mutate(draw_rank = rank(.value)) %>% 
    ggplot(aes(x = draw_rank, color = factor(.chain))) +
    stat_bin(geom = "step", binwidth = 200, position = position_identity(), boundary = 0) +
    scale_color_viridis_d(option = "rocket", end = 0.85) +
    labs(color = "Chain") +
    facet_wrap(vars(.variable), scales = "free_y") +
    theme_pandem()
}

plot_pp <- function(model) {
  bayesplot::pp_check(model, ndraws = 100, type = "bars")
}




# Storing ggplot objects as rds files is BAD 
#   (https://github.com/hadley/ggplot2-book/issues/344)
#
# But it's also necessary/recommended when working with {targets}---the basic
# walkthrough in the manual even shows how to create a plot as a target
# 
# ggplot objects are strange beasts because they store a copy of the overall
# environment when serialized into rds files. When working with regular-sized
# datasets, this isn't really ever a problem. But when working with tidy
# data frames of MCMC chains with millions and millions of rows, this can create
# rds files that are hundreds or thousands of MBs, which is wild.
#
# This post goes into more details about how to fix it: 
# https://ropensci.org/blog/2022/12/06/save-ggplot2-targets/
#
# Instead of using stat_lineribbon() on tidy MCMC draws like I normally do, it's
# better to collapse the data down first with median_qi() and then use
# geom_lineribbon(). This results in much tinier data frames and plots render
# immediately in .qmd files

library(marginaleffects)

build_policies_plot_data <- function(model_df, year_week_lookup) {
  model_df_preds_mfx <- model_df %>% 
    mutate(pred_data = map(model, ~{
      datagrid(model = .x,
               year_week_num = 1:69,
               derogation_ineffect = 0:1) %>% 
        tidybayes::add_epred_draws(.x) %>% 
        left_join(year_week_lookup, by = "year_week_num") %>% 
        mutate(derogation_ineffect = factor(derogation_ineffect, 
                                            levels = 0:1,
                                            labels = c("No", "Yes"),
                                            ordered = TRUE)) %>% 
        group_by(year_week_day, derogation_ineffect) %>% 
        tidybayes::median_qi(.epred, .width = c(0.5, 0.8, 0.95))
    })) %>% 
    mutate(mfx_data = map(model, ~{
      .x %>% 
        comparisons(newdata = datagrid(year_week_num = 1:69),
                    variables = "derogation_ineffect",
                    type = "response")  %>%  
        posteriordraws() %>% 
        left_join(year_week_lookup, by = "year_week_num") %>% 
        group_by(year_week_day) %>% 
        tidybayes::median_qi(draw, .width = c(0.5, 0.8, 0.95))
    }))
  
  return(model_df_preds_mfx)
}


build_human_rights_plot_data <- function(model_df, year_week_lookup) {
  model_df_preds_mfx <- model_df %>% 
    mutate(pred_data = map2(model, family, ~{
      if (.y == "cumulative") {
        df <- datagrid(model = .x,
                       year_week_num = 1:69,
                       derogation_ineffect = 0:1) %>% 
          tidybayes::add_epred_draws(.x) %>% 
          left_join(year_week_lookup, by = "year_week_num") %>% 
          mutate(derogation_ineffect = factor(derogation_ineffect, 
                                              levels = 0:1,
                                              labels = c("No derogation", "Derogation in effect"),
                                              ordered = TRUE)) %>% 
          group_by(year_week_day, .category, derogation_ineffect) %>% 
          tidybayes::median_qi(.epred, .width = c(0.5, 0.8, 0.95))
      } else {
        df <- datagrid(model = .x,
                       year_week_num = 1:69,
                       derogation_ineffect = 0:1) %>% 
          tidybayes::add_epred_draws(.x) %>% 
          left_join(year_week_lookup, by = "year_week_num") %>% 
          mutate(derogation_ineffect = factor(derogation_ineffect, 
                                              levels = 0:1,
                                              labels = c("No", "Yes"),
                                              ordered = TRUE)) %>% 
          group_by(year_week_day, derogation_ineffect) %>% 
          tidybayes::median_qi(.epred, .width = c(0.5, 0.8, 0.95))
      }
    })) %>% 
    mutate(mfx_data = pmap(list(model, family, y), \(.model, .family, .y) {
      mfx <- .model %>% 
        comparisons(newdata = datagrid(year_week_num = 1:69),
                    variables = "derogation_ineffect",
                    type = "response")  %>%  
        posteriordraws() %>% 
        left_join(year_week_lookup, by = "year_week_num") 
      
      if (.family == "cumulative") {
        mfx <- mfx %>% 
          mutate(group = factor(group, levels = levels(.model$data[[.y]]), ordered = TRUE)) %>% 
          group_by(year_week_day, group) %>% 
          tidybayes::median_qi(draw, .width = c(0.5, 0.8, 0.95))
      } else {
        mfx <- mfx %>% 
          group_by(year_week_day) %>% 
          tidybayes::median_qi(draw, .width = c(0.5, 0.8, 0.95))
      }
      
      mfx
    }))
  
  return(model_df_preds_mfx)
}
