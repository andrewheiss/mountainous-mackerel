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

build_prelim_plot_data <- function(model_df, year_week_lookup) {
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


build_h1_plot_data <- function(model_df, year_week_lookup) {
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
    mutate(mfx_data = pmap(list(model, family, model, y), ~{
      mfx <- ..1 %>% 
        comparisons(newdata = datagrid(year_week_num = 1:69),
                    variables = "derogation_ineffect",
                    type = "response")  %>%  
        posteriordraws() %>% 
        left_join(year_week_lookup, by = "year_week_num") 
      
      if (..2 == "cumulative") {
        mfx <- mfx %>% 
          mutate(group = factor(group, levels = levels(..3$data[[..4]]), ordered = TRUE)) %>% 
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

build_h1_plots_mfx <- function(model_df, year_week_lookup, plot_funs) {
  list2env(plot_funs, env = environment())
  
  h1_models_mfx <- model_df %>% 
    mutate(mfx = map(model, ~{
      .x %>% 
        comparisons(newdata = datagrid(year_week_num = 1:69),
                    variables = "derogation_ineffect",
                    type = "response", re_formula = NA)  %>%  
        posteriordraws()
    })) %>% 
    mutate(mfx_plot = pmap(list(mfx, nice, family, model, y), ~{
      if (..3 == "cumulative") {
        df <- ..1 %>% 
          left_join(year_week_lookup, by = "year_week_num") %>% 
          mutate(group = factor(group, levels = levels(..4$data[[..5]]), ordered = TRUE)) %>% 
          select(year_week_day, draw, group)
        
        df %>% 
          ggplot(aes(x = year_week_day, y = draw * 100)) +
          tidybayes::stat_lineribbon(aes(color = group, fill = group), alpha = 0.3) +
          geom_hline(yintercept = 0, linewidth = 0.5, color = "grey50") +
          scale_color_manual(values = c(clrs[c(2, 4, 6, 7)]), guide = "none") +
          scale_fill_manual(values = c(clrs[c(2, 4, 6, 7)]), guide = "none") + 
          scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
          labs(x = NULL,
               y = "Percentage point difference\nin probability of outcome level",
               title = ..2) +
          facet_wrap(vars(group), ncol = 2) +
          theme_pandem()
      } else {
        df <- ..1 %>% 
          left_join(year_week_lookup, by = "year_week_num") %>% 
          select(year_week_day, draw)
        
        df %>% 
          ggplot(aes(x = year_week_day, y = draw * 100, )) +
          tidybayes::stat_lineribbon(alpha = 0.25, fill = clrs[5], color = clrs[5]) +
          geom_hline(yintercept = 0, linewidth = 0.5, color = "grey50") +
          scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
          labs(x = NULL,
               y = "Percentage point difference\nin probability of outcome",
               title = ..2) +
          theme_pandem()
      }
    }))
  
  return(h1_models_mfx$mfx_plot)
}
