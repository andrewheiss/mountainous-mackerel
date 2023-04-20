# Appendix {.appendix}

### H~1~: Derogations and human rights (ordered logistic regression)

$$
\begin{aligned}
&\ \mathrlap{\textbf{Model of outcome level $i$ across week $t$ within each country $j$}} \\
\text{Outcome}_{it_j} \sim&\ \operatorname{Ordered\ logit}(\phi_{it_j}, \alpha_k) \\[0.75em]
&\ \textbf{Models for distribution parameters} \\
\phi_{it_j} =&\ (\beta_0 + b_{0_j}) + \beta_1 \text{Derogation in effect}_{it} + \\
&\ \beta_2\ \text{New cases}_{it}\ + \beta_3\ \text{Cumulative cases}_{it}\ + \\
&\ \beta_4\ \text{New deaths}_{it}\ + \beta_5\ \text{Cumulative deaths}_{it}\ + \\
&\ \beta_6\ \text{Past ICCPR derogation}_{it}\ + \beta_7\ \text{Past ICCPR action}_{it}\ + \\
&\ \beta_8\ \text{Rule of law index}_{it}\ + \beta_9\ \text{Civil liberties index}_{it}\ + \\
&\ \beta_{10}\ \text{Core civil society index}_{it}\ + \beta_{11}\ \text{Week number}_{it} \\
b_{0_j} \sim&\ \mathcal{N}(0, \sigma_0) \\[0.75em]
&\ \textbf{Priors} \\
\beta_{0 \dots 11} \sim&\ \operatorname{Student\ t}(\nu = 1, \mu = 0, \sigma = 3) \\
\sigma_0 \sim&\ \operatorname{Cauchy}(x = 0, \gamma = 1) \text{, lower bound} = 0 \\
\alpha_k \sim&\ \mathcal{N}(0, 1)
\end{aligned}
$$

### H~1~: Derogations and human rights (logistic regression)

$$
\begin{aligned}
&\ \mathrlap{\textbf{Binary outcome $i$ across week $t$ within each country $j$}} \\
\text{Outcome}_{it_j} \sim&\ \operatorname{Bernoulli}(\pi_{it_j}) \\[0.75em]
&\ \textbf{Distribution parameters} \\
\pi_{it_j} =&\ (\beta_0 + b_{0_j}) + \beta_1 \text{Derogation in effect}_{it} + \\
&\ \beta_2\ \text{New cases}_{it}\ + \beta_3\ \text{Cumulative cases}_{it}\ + \\
&\ \beta_4\ \text{New deaths}_{it}\ + \beta_5\ \text{Cumulative deaths}_{it}\ + \\
&\ \beta_6\ \text{Past ICCPR derogation}_{it}\ + \beta_7\ \text{Past ICCPR action}_{it}\ + \\
&\ \beta_8\ \text{Rule of law index}_{it}\ + \beta_9\ \text{Civil liberties index}_{it}\ + \\
&\ \beta_{10}\ \text{Core civil society index}_{it}\ + \beta_{11}\ \text{Week number}_{it} \\
b_{0_j} \sim&\ \mathcal{N}(0, \sigma_0) \\[0.75em]
&\ \textbf{Priors} \\
\beta_{0 \dots 11} \sim&\ \operatorname{Student\ t}(\nu = 1, \mu = 0, \sigma = 3) \\
\sigma_0 \sim&\ \operatorname{Cauchy}(x = 0, \gamma = 1) \text{, lower bound} = 0
\end{aligned}
$$

### H~2~: Derogations, civil society, and human rights (ordered logistic regression)

$$
\begin{aligned}
&\ \mathrlap{\textbf{Model of outcome level $i$ across week $t$ within each country $j$}} \\
\text{Outcome}_{it_j} \sim&\ \operatorname{Ordered\ logit}(\phi_{it_j}, \alpha_k) \\[0.75em]
&\ \textbf{Models for distribution parameters} \\
\phi_{it_j} =&\ (\beta_0 + b_{0_j})\ + \\
&\ \beta_1 \text{Derogation in effect}_{it} + \beta_2 \text{Civil society repression}_{it} \\
&\ \beta_3\ {(\text{Derogation} \times \text{Civil society repression})}_{i}\ + \\
&\ \beta_4\ \text{New cases}_{it}\ + \beta_5\ \text{Cumulative cases}_{it}\ + \\
&\ \beta_6\ \text{New deaths}_{it}\ + \beta_7\ \text{Cumulative deaths}_{it}\ + \\
&\ \beta_8\ \text{Past ICCPR derogation}_{it}\ + \beta_9\ \text{Past ICCPR action}_{it}\ + \\
&\ \beta_{10}\ \text{Rule of law index}_{it}\ + \beta_{11}\ \text{Civil liberties index}_{it}\ + \\
&\ \beta_{12}\ \text{Week number}_{it} \\
b_{0_j} \sim&\ \mathcal{N}(0, \sigma_0) \\[0.75em]
&\ \textbf{Priors} \\
\beta_{0 \dots 12} \sim&\ \operatorname{Student\ t}(\nu = 1, \mu = 0, \sigma = 3) \\
\sigma_0 \sim&\ \operatorname{Cauchy}(x = 0, \gamma = 1) \text{, lower bound} = 0 \\
\alpha_k \sim&\ \mathcal{N}(0, 1)
\end{aligned}
$$

### H~2~: Derogations, civil society, and human rights (logistic regression)

$$
\begin{aligned}
&\ \mathrlap{\textbf{Binary outcome $i$ across week $t$ within each country $j$}} \\
\text{Outcome}_{it_j} \sim&\ \operatorname{Bernoulli}(\pi_{it_j}) \\[0.75em]
&\ \textbf{Distribution parameters} \\
\pi_{it_j} =&\ (\beta_0 + b_{0_j})\ + \\
&\ \beta_1 \text{Derogation in effect}_{it} + \beta_2 \text{Civil society repression}_{it} \\
&\ \beta_3\ {(\text{Derogation} \times \text{Civil society repression})}_{i}\ + \\
&\ \beta_4\ \text{New cases}_{it}\ + \beta_5\ \text{Cumulative cases}_{it}\ + \\
&\ \beta_6\ \text{New deaths}_{it}\ + \beta_7\ \text{Cumulative deaths}_{it}\ + \\
&\ \beta_8\ \text{Past ICCPR derogation}_{it}\ + \beta_9\ \text{Past ICCPR action}_{it}\ + \\
&\ \beta_{10}\ \text{Rule of law index}_{it}\ + \beta_{11}\ \text{Civil liberties index}_{it}\ + \\
&\ \beta_{12}\ \text{Week number}_{it} \\
b_{0_j} \sim&\ \mathcal{N}(0, \sigma_0) \\[0.75em]
&\ \textbf{Priors} \\
\beta_{0 \dots 12} \sim&\ \operatorname{Student\ t}(\nu = 1, \mu = 0, \sigma = 3) \\
\sigma_0 \sim&\ \operatorname{Cauchy}(x = 0, \gamma = 1) \text{, lower bound} = 0
\end{aligned}
$$

\newpage

```{r tbl-results-full-prelim}
#| tbl-cap: Results from models showing relationship between derogations and emergency policies
#| include: true
#| echo: false
notes <- c(
  "Estimates are median posterior log odds from ordered logistic and binary logistic regression models;",
  "95% credible intervals (highest density posterior interval, or HDPI) in brackets.",
  "Total \\(R^2\\) considers the variance of both population and group effects;",
  "marginal \\(R^2\\) only takes population effects into account."
)

if (fmt_out == "latex") {
  notes <- notes %>% 
    str_replace_all(r"[\\]", r"[\\\\]") %>% 
    str_replace_all("%", r"(\\\\%)")
} else {
  notes <- paste(notes, collapse = " ")
}

if (fmt_out == "latex") { 
  modelsummary(models_tbl_prelim,
               estimate = "{estimate}",
               statistic = "[{conf.low}, {conf.high}]",
               coef_map = coef_map,
               gof_map = gof_map,
               output = "kableExtra",
               fmt =  list(estimate = 2, conf.low = 2, conf.high = 2),
               escape = FALSE) %>% 
    kable_styling(htmltable_class = "table-sm light-border",
                  font_size = ifelse(fmt_out == "latex", 8, NA)) %>% 
    footnote(general = notes, footnote_as_chunk = FALSE, escape = FALSE) %>% 
    column_spec(1:6, width = "0.16\\\\textwidth")
} else {
  modelsummary(models_tbl_prelim,
               estimate = "{estimate}",
               statistic = "[{conf.low}, {conf.high}]",
               coef_map = coef_map,
               gof_map = gof_map,
               output = "gt",
               fmt =  list(estimate = 2, conf.low = 2, conf.high = 2)) %>% 
    tab_footnote(footnote = notes) %>% 
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels())
}
```

\newpage

```{r tbl-results-full-h1}
#| tbl-cap: Complete results from models showing relationship between derogations and human rights (H~1~)
#| include: true
#| echo: false
if (fmt_out == "latex") {
  modelsummary(models_tbl_h1,
               estimate = "{estimate}",
               statistic = "[{conf.low}, {conf.high}]",
               coef_map = coef_map,
               gof_map = gof_map,
               output = "kableExtra",
               fmt =  list(estimate = 2, conf.low = 2, conf.high = 2),
               escape = FALSE) %>% 
    kable_styling(htmltable_class = "table-sm light-border",
                  font_size = ifelse(fmt_out == "latex", 8, NA)) %>% 
    footnote(general = notes, footnote_as_chunk = FALSE, escape = FALSE) %>% 
    column_spec(1:5, width = "0.18\\\\textwidth")
} else {
  modelsummary(models_tbl_h1,
               estimate = "{estimate}",
               statistic = "[{conf.low}, {conf.high}]",
               coef_map = coef_map,
               gof_map = gof_map,
               output = "gt",
               fmt =  list(estimate = 2, conf.low = 2, conf.high = 2),
               escape = FALSE) %>% 
    tab_footnote(footnote = notes) %>% 
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels())
}
```

\newpage

```{r tbl-results-full-h2}
#| tbl-cap: Models showing relationship between derogations, civil society, and human rights (H~2~)
#| include: true
#| echo: false
if (fmt_out == "latex") {
  modelsummary(models_tbl_h2,
               estimate = "{estimate}",
               statistic = "[{conf.low}, {conf.high}]",
               coef_map = coef_map,
               gof_map = gof_map,
               output = "kableExtra",
               fmt =  list(estimate = 2, conf.low = 2, conf.high = 2),
               escape = FALSE) %>% 
    kable_styling(htmltable_class = "table-sm light-border",
                  font_size = ifelse(fmt_out == "latex", 8, NA)) %>% 
    footnote(general = notes, footnote_as_chunk = FALSE, escape = FALSE) %>% 
    column_spec(1:5, width = "0.18\\\\textwidth")
} else {
  modelsummary(models_tbl_h2,
               estimate = "{estimate}",
               statistic = "[{conf.low}, {conf.high}]",
               coef_map = coef_map,
               gof_map = gof_map,
               output = "gt",
               fmt =  list(estimate = 2, conf.low = 2, conf.high = 2),
               escape = FALSE) %>% 
    tab_footnote(footnote = notes) %>% 
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels())
}
```

\clearpage
