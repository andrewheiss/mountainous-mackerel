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
  bayesplot::pp_check(model, ndraws = 100, type = "bars") +
    theme_pandem()
}

fmt_p_inline <- function(x, direction) {
  x <- round(x, 2)
  
  if (direction == "gt") {
    out <- glue::glue(r"[$p(\Delta > 0) = {x}$]")
  } else {
    out <- glue::glue(r"[$p(\Delta < 0) = {x}$]")
  }
  
  return(out)
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
        posterior_draws() %>% 
        left_join(year_week_lookup, by = "year_week_num") %>% 
        group_by(year_week_day) %>% 
        reframe(post_medians = tidybayes::median_qi(draw, .width = c(0.5, 0.8, 0.95)),
                p_gt_0 = sum(draw > 0) / n()) %>% 
        unnest(post_medians) %>% 
        rename(draw = y, .lower = ymin, .upper = ymax)
    }))
  
  return(model_df_preds_mfx)
}


build_human_rights_plot_data <- function(model_df, year_quarter_lookup) {
  model_df_preds_mfx <- model_df %>% 
    mutate(pred_data = map2(model, family, ~{
      if (.y == "cumulative") {
        df <- datagrid(model = .x,
                       year_quarter_num = 1:6,
                       derogation_ineffect = 0:1) %>% 
          tidybayes::add_epred_draws(.x) %>% 
          left_join(year_quarter_lookup, by = "year_quarter_num") %>% 
          mutate(derogation_ineffect = factor(derogation_ineffect, 
                                              levels = 0:1,
                                              labels = c("No derogation", "Derogation in effect"),
                                              ordered = TRUE)) %>% 
          group_by(year_quarter_day, year_quarter, .category, derogation_ineffect) %>% 
          tidybayes::median_qi(.epred, .width = c(0.5, 0.8, 0.95))
      } else {
        df <- datagrid(model = .x,
                       year_quarter_num = 1:6,
                       derogation_ineffect = 0:1) %>% 
          tidybayes::add_epred_draws(.x) %>% 
          left_join(year_quarter_lookup, by = "year_quarter_num") %>% 
          mutate(derogation_ineffect = factor(derogation_ineffect, 
                                              levels = 0:1,
                                              labels = c("No", "Yes"),
                                              ordered = TRUE)) %>% 
          group_by(year_quarter_day, year_quarter, derogation_ineffect) %>% 
          tidybayes::median_qi(.epred, .width = c(0.5, 0.8, 0.95))
      }
    })) %>% 
    mutate(mfx_data = pmap(list(model, family, y), \(.model, .family, .y) {
      mfx <- .model %>% 
        comparisons(newdata = datagrid(year_quarter_num = 1:6),
                    variables = "derogation_ineffect",
                    type = "response")  %>%  
        posterior_draws() %>% 
        left_join(year_quarter_lookup, by = "year_quarter_num")
      
      if (.family == "cumulative") {
        mfx <- mfx %>% 
          mutate(group = factor(group, levels = levels(.model$data[[.y]]), ordered = TRUE)) %>% 
          group_by(year_quarter_day, year_quarter, group) %>% 
          reframe(post_medians = tidybayes::median_qi(draw, .width = c(0.5, 0.8, 0.95)),
                  p_gt_0 = sum(draw > 0) / n()) %>% 
          unnest(post_medians) %>% 
          rename(draw = y, .lower = ymin, .upper = ymax)
      } else {
        mfx <- mfx %>% 
          group_by(year_quarter_day, year_quarter) %>% 
          reframe(post_medians = tidybayes::median_qi(draw, .width = c(0.5, 0.8, 0.95)),
                  p_gt_0 = sum(draw > 0) / n()) %>% 
          unnest(post_medians) %>% 
          rename(draw = y, .lower = ymin, .upper = ymax) %>% 
          mutate(group = "Logit")
      }
      
      mfx
    }))
  
  return(model_df_preds_mfx)
}

build_policies_table_data <- function(model_df) {
  policies_pred_tbl <- model_df %>% 
    unnest(pred_data) %>% 
    filter(.width == 0.95) %>% 
    select(nice, year_week_day, derogation_ineffect, .epred, .lower, .upper) %>% 
    group_by(nice, derogation_ineffect) %>% 
    mutate(rank = dense_rank(.epred)) %>% 
    filter(rank == 1 | rank == max(rank)) %>% 
    mutate(rank = ifelse(rank == 1, "min", "max")) %>% 
    group_by(rank, derogation_ineffect) %>% 
    mutate(outcome = janitor::make_clean_names(nice)) %>%
    rename(draw = .epred, min = .lower, max = .upper) %>% 
    mutate(across(c(draw, min, max), list(nice = ~round(. * 100, 0)))) %>% 
    ungroup()

  policies_mfx_tbl <- model_df %>% 
    unnest(mfx_data) %>% 
    filter(.width == 0.95) %>% 
    select(nice, year_week_day, draw, .lower, .upper, p_gt_0, .width) %>% 
    group_by(nice) %>% 
    mutate(rank = dense_rank(draw)) %>% 
    filter(rank == 1 | rank == max(rank)) %>% 
    mutate(rank = ifelse(rank == 1, "min", "max")) %>% 
    group_by(rank) %>% 
    mutate(outcome = janitor::make_clean_names(nice)) %>%
    rename(min = .lower, max = .upper) %>% 
    mutate(across(c(draw, min, max), list(nice = ~round(. * 100, 0))),
           p_lt_0 = p_gt_0 - 1) %>% 
    mutate(p_gt = fmt_p_inline(p_gt_0, "gt"),
           p_lt = fmt_p_inline(p_lt_0, "lt")) %>% 
    ungroup()
  
  return(lst(policies_pred_tbl, policies_mfx_tbl))
}

build_hr_table_data <- function(model_df) {
  hr_pred_tbl <- model_df %>% 
    mutate(pred_data = map(pred_data, ~{
      .x %>% 
        mutate(derogation_ineffect = as.character(derogation_ineffect),
               derogation_ineffect = recode(derogation_ineffect,
                                            "No derogation" = "No",
                                            "Derogation in effect" = "Yes"))
    })) %>% 
    unnest(pred_data) %>% 
    filter(.width == 0.95) %>% 
    select(nice, year_quarter_day, derogation_ineffect, .category, .epred, .lower, .upper) %>% 
    group_by(nice, derogation_ineffect, .category) %>% 
    mutate(rank = dense_rank(.epred)) %>% 
    filter(rank == 1 | rank == max(rank)) %>% 
    mutate(rank = ifelse(rank == 1, "min", "max")) %>% 
    group_by(rank, derogation_ineffect, .category) %>% 
    mutate(outcome = janitor::make_clean_names(nice)) %>%
    rename(draw = .epred, min = .lower, max = .upper) %>% 
    mutate(across(c(draw, min, max), list(nice = ~round(. * 100, 0)))) %>% 
    ungroup() %>% 
    mutate(category = ifelse(is.na(.category), "NA", as.character(.category)))
  
  hr_mfx_tbl <- model_df %>% 
    mutate(mfx_data = map(mfx_data, ~{
      .x %>% 
        mutate(group = as.character(group))
    })) %>% 
    unnest(mfx_data) %>% 
    filter(.width == 0.95) %>% 
    select(nice, year_quarter_day, group, draw, .lower, .upper, p_gt_0, .width) %>% 
    group_by(nice, group) %>% 
    mutate(rank = dense_rank(draw)) %>% 
    filter(rank == 1 | rank == max(rank)) %>% 
    mutate(rank = ifelse(rank == 1, "min", "max")) %>% 
    group_by(rank, group) %>% 
    mutate(outcome = janitor::make_clean_names(nice)) %>%
    rename(min = .lower, max = .upper) %>% 
    mutate(across(c(draw, min, max), list(nice = ~round(. * 100, 0))),
           p_lt_0 = 1 - p_gt_0) %>% 
    mutate(p_gt = fmt_p_inline(p_gt_0, "gt"),
           p_lt = fmt_p_inline(p_lt_0, "lt")) %>% 
    ungroup()
  
  return(lst(hr_pred_tbl, hr_mfx_tbl))
}
