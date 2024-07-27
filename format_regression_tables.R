################################################################################
# Functions for regression table
################################################################################

# Split regression point estimates and standard errors
transform_values <- function(value) {
  
  if (str_detect(value, "\\(.+\\)")) {
    
    # Extract the parts of the value
    parts <- str_match(value, "^(.*) \\((.*)\\)$")
    
    # Combine the parts with a newline
    return(paste0("\\makecell{", parts[2], 
                  " \\ (", parts[3], ")}"))
    
  } else {
    return(value)
  }
}

# Add a caption and label to the table
add_caption_and_label <- function(table, label, caption, n_models){
  sig_label <- paste0("\\\\midrule \\\\midrule
        \\\\multicolumn{", as.character(n_models), 
        "}{l}{\\\\emph{IID standard-errors in parentheses}}\\\\\\\\
        \\\\multicolumn{", as.character(n_models), 
        "}{l}{\\\\emph{Signif. Codes: ***: 0.01, **: 0.05, *: 0.1}}\\\\\\\\")
  table <- 
    sub("\\\\bottomrule", 
        sig_label, 
        table)
  
  caption <- paste0("\\\\end{tabularx}", 
                    "\n", "\\\\caption{", caption, "}\n")
  table <- 
    sub("\\\\end\\{table\\}", 
        paste0(caption, "\\\\end{table}"), 
        table)
  
  label <- paste0("\\\\label{table:", label, "}\n")
  table <- 
    sub("\\\\end\\{table\\}", 
        paste0(label, "\\\\end{table}"), 
        table)
  
  return(table)
}

# Correct special characters that are escaped by backslash
fix_special_characters <- function(table, label, caption, n_models){
  
  table <- gsub("\\textbackslash{}makecell\\{", "\\makecell{",
                table, fixed = TRUE)
  table <- gsub("\\textbackslash{}", "\\\\",
                table, fixed = TRUE)
  table <- gsub("\\}", "}",
                table, fixed = TRUE)
  table <- gsub("Observations", "\\midrule\nObservations",
                table, fixed = TRUE)
  table <- gsub("begin{longtable}{r", "begin{longtable}{l",
                table, fixed = TRUE)
  table <- gsub(".*begin\\{longtable\\}", 
                "\\\\addtocounter\\{table\\}\\{-1\\} \\\n 
                \\\\begin\\{table\\}\\[!tbp\\] \\\n
                \\\\scriptsize \\\n
                \\\\begin\\{tabularx\\}\\{\\\\textwidth\\}",
                table)
  table <- gsub("\\\\end\\{longtable\\}", 
                "\\\\end\\{table\\}",
                table)
  table <- gsub("Dependent Var.", 
                "\\\\emph\\{Dependent Var.\\}",
                table)
  table <- add_caption_and_label(table, label, caption, n_models)
  
  return(table)
}

# Outer function to format the regression table
format_reg_tables <- function(mdls, label, caption, var_order, 
                              var_mapping, independent_vars){
  # Format model list as a table
  table <- etable(mdls, 
                  order = paste0("^", var_order, "$"),
                  se = "hetero",
                  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))
  
  # Reformat table as a gt()
  table <- table %>%
    rownames_to_column(var = "value") %>%
    gt(rowname_col = "value") %>%
    cols_align(columns = "value", align = c("left")) %>%
    as.data.frame()
  
  # Format dependent variables and model name
  row_index <- which(row.names(table) == "Dependent Var.:")
  table[row_index, ] <- c("Dependent Var.:", independent_vars)
  names(table) <- c("value", paste0("Model (", seq(1, length(mdls)), ")"))
  
  # Reformat as a gt table
  table <- table %>%
    mutate(value = dplyr::recode(value, !!!var_mapping)) %>%
    filter(value != "S.E. type") %>%
    filter(!grepl("^_+$", value)) %>%
    mutate(across(where(is.character) & !contains("value"), 
                  ~ sapply(., transform_values))) %>%
    gt(rowname_col = "value") %>%
    as_latex() %>%
    as.character()
  
  n_models <- length(mdls)
  table <- fix_special_characters(table, label, caption, n_models)
  
  return(table)
}

################################################################################
# Functions for coefficient plots
################################################################################

#################################################
# Functions to create compatibility plot

# Convert model from fixest() to lm() object
convert_model_type <- function(model){
  
  # Extract model components
  model_formula <- formula(model)
  model_data <- model$call$data
  
  # Create a new lm object
  lm_model <- lm(model_formula, data = eval(model_data))
  
  return(lm_model)
}

# Get coefficient interval based on specified confidence level
get_bounds <- function(conf_level, model, focal_coefficient){
  
  # Tidy the model to get the coefficients and confidence intervals
  tidy_model <- tidy(convert_model_type(model), 
                     conf.int = TRUE, conf.level = conf_level) %>%
    filter(term == focal_coefficient)
  
  return(c(tidy_model$conf.low, tidy_model$conf.high))
}

# Define a function to create a compatibility plot for the focal coefficient
create_compatibility_plot <- function(model, focal_coefficient) {
  
  CIs <- seq(0, 0.999, by = 0.001)
  df <- data.frame(t(sapply(CIs, get_bounds, model, focal_coefficient)))
  colnames(df) <- c("lb", "ub")
  df$level <- CIs
  
  plot <- df %>% 
    pivot_longer(cols = c("lb", "ub")) %>%
    mutate(pvalue = (1 - level),
           level = level * 100) %>%
    ggplot(aes(y = pvalue, x = value, group = name)) + 
    geom_area(fill = "grey", alpha = 0.25) + 
    geom_line(color = "red", size = 1) +
    scale_y_continuous(
      "P-Value", 
      sec.axis = sec_axis(~ (1 - .) * 100, name = "Confidence Level (%)"),
      limits = c(0, 1)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    labs(title = "Compatibility Plot for Focal Coefficient",
         subtitle = "Preferred Specification",
         x = "Coefficient Estimate") +
    
    geom_point(aes(x = model$coefficients[focal_var][[1]], y = 1), size = 2) + 
    theme_bw() 
  
  return(plot)
}

#################################################
# Functions to create confidence interval plot

get_coefficient_results <- function(models, names, conf_level){
  level <- paste0(as.character(conf_level * 100), "% Confidence Interval")
  
  model_results <- lapply(models, broom::tidy, 
                          conf.int = TRUE, conf.level = conf_level)
  names(model_results) <- names
  model_results <- plyr::ldply(model_results, rbind) %>%
    rename("Model" = ".id") %>%
    mutate(level = level)
  
  return(model_results)
}

create_ci_plot <- function(models, focal_var){
  model_names <- paste0("Model (", seq(1, length(models)), ")")
  
  # Get interval estiamtes for each confidence level
  model_results <- rbind(
    get_coefficient_results(models, model_names, 0.99),
    get_coefficient_results(models, model_names, 0.95),
    get_coefficient_results(models, model_names, 0.90),
    get_coefficient_results(models, model_names, 0.80),
    # fixest() will not give a 50% confidence interval, so below we add the 
    #   smallest possible number (.Machine$double.eps) to get the estimate
    get_coefficient_results(models, model_names, 0.50 + .Machine$double.eps)
  )
  
  # Identify unique levels in 'level' column
  unique_levels <- unique(model_results$level)
  unique_levels <- unique_levels[unique_levels != "95% Confidence Interval"]
  linetypes <-  c("dashed", "dotted", "dotdash", "longdash", "twodash")
  
  # Assign default linetypes for unspecified levels
  default_linetypes <- setNames(linetypes[seq(1, length(unique_levels))], 
                                unique_levels)
  default_linetypes["95% Confidence Interval"] <- "solid" 
  
  # Create plot
  ci_plot <- 
    model_results %>%
    filter(term == focal_var) %>%
    mutate(Model = forcats::fct_rev(factor(Model)),
           preferred = ifelse(Model == 
                                paste0("Model (", preferred_specification, ")"),
                              "Yes", "No")) %>%
    ggplot(aes(x = estimate, y = Model)) +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
                      linetype = level, color = preferred),
                  position = position_dodge(width = 0.3), width = 0.3) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    scale_linetype_manual(values = default_linetypes) +
    scale_colour_manual(values = c("Yes" = "red", "No" = "black")) + 
    labs(linetype = "Confidence Level") +
    theme_bw() +
    theme(legend.position="none")  +
    labs(title = "Confidence Intervals for Focal Coefficient",
         subtitle = "Confidence Intervals: 50%, 80%, 90%, 95%, 99%",
         x = "Coefficient Estimate",
         y = "") 
  
  return(ci_plot)
}

# Un-comment the code below for an example of how to use these functions

# ################################################################################
# # Example
# ################################################################################
# 
# # To see how this looks in a latex document:
# #   https://www.overleaf.com/read/stgdhmyvpvcp#992a22
# 
# library(fixest)
# library(tidyverse)
# library(gt)
# library(cowplot)
# library(tikzDevice)
# 
# # STEP 1: Create a list of models fit using feols() from the fixest package ####
# 
# models<-list()
# models[[1]] <- feols(as.formula("Sepal.Length ~ Sepal.Width"),
#                    data = iris)
# models[[2]] <- feols(as.formula("Sepal.Length ~ Sepal.Width + Petal.Length"),
#                    data = iris)
# models[[3]] <- feols(as.formula("Sepal.Length ~ Sepal.Width*Petal.Length"),
#                    data = iris)
# 
# # STEP 2: Specify how you want the results to appear in the printed table ######
# 
# # Set the order of the independent variables in the left column of the table
# var_order <- c("Sepal.Width",
#                "Petal.Length",
#                "Sepal.Width x Petal.Length",
#                "(Intercept)")
# 
# # Define the mapping between the variable name in the data (e.g. "Sepal.Width")
# #   and the way you want the name to display in the table ("Sepal Width)
# var_mapping <- c("Sepal.Width" = "Sepal Width",
#                  "Petal.Length" = "Petal Length",
#                  "Sepal.Width x Petal.Length" = "Sepal Width x Petal Length",
#                  "Sepal.Width:Petal.Length" = "Sepal Width x Petal Length")
# 
# # Define the label and caption of the table in latex
# label <- "iris-regression-table"
# caption <- "OLS Models for Sepal Width."
# 
# # Create a list of the names of the dependent variable names
# independent_vars <- rep("Sepal Length", length(models))
# 
# # Settings for the coefficient plots
# focal_var <- "Sepal.Width"
# preferred_specification <- 3
# 
# # STEP 3: Create latex-formatted table #########################################
# 
# formatted_models <-
#   format_reg_tables(models,
#                     label, caption,
#                     var_order, var_mapping, independent_vars)
# 
# # Write table out to latex file
# writeLines(formatted_models,
#            "table.tex")
# 
# # STEP 4: Coefficient plots ####################################################
# 
# # Create plots
# ci_plot <- create_ci_plot(models, focal_var)
# compatibility_plot <-
#   create_compatibility_plot(models[[preferred_specification]], focal_var)
# 
# # Write plots to a latex file
# tikz(file = "plot.tex",
#      width = 9, height = 4, sanitize=TRUE)
# 
# plot_grid(ci_plot, compatibility_plot, labels = c("A", "B"))
# 
# dev.off()
