format_anova = function(model) {
  tab = car::Anova(model) %>%
    rownames_to_column(var = "Parameter") %>%
    mutate(`Pr(>Chisq)` = ifelse(`Pr(>Chisq)` < 0.001,
                                 paste0("< ", format(0.001, scientific = FALSE)),
                                 format(round(`Pr(>Chisq)`, 3), scientific = FALSE)),
           `Chisq` = format(round(`Chisq`, 2), scientific = FALSE)
    ) %>%
    rename(
      DF = Df
    ) %>%
    flextable() %>%
    flextable::bold(~ `Pr(>Chisq)` <= 0.05 | `Pr(>Chisq)` == '< 0.001',4) %>%
    flextable::compose(part = "header", j="Chisq", value=as_paragraph("\U1D6D8",as_sup("2"))) %>%
    flextable::compose(part = "header", j="Pr(>Chisq)", value=as_paragraph(as_i("p"))) %>%
    flextable::width(., width = dim(.)$widths * 6.5 / (flextable::flextable_dim(.)$widths))
  
  return(tab)
}