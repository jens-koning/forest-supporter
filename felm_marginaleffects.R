felm_marginal_effects <- function(regression_model, data, treatment, moderator, treatment_translation, moderator_translation, dependent_variable_translation, alpha = 0.05, se = NULL) {
  library(ggplot2)
  library(ggthemes)
  library(gridExtra)
  
  ### defining function to get average marginal effects
  getmfx <- function(betas, data, treatment, moderator) {
    betas[treatment] + betas[paste0(treatment, ":", moderator)] * data[, moderator]
  }
  
  ### defining function to get marginal effects for specific levels of the treatment variable
  getmfx_high_low <- function(betas, data, treatment, moderator, treatment_val) {
    betas[treatment] * treatment_val + betas[paste0(treatment, ":", moderator)] * data[, moderator] * treatment_val
  }
  
  ### Defining function to analytically derive standard error for marginal effects
  getvarmfx <- function(my_vcov, data, treatment, moderator) {
    my_vcov[treatment, treatment] + data[, moderator]^2 * my_vcov[paste0(treatment, ":", moderator), paste0(treatment, ":", moderator)] + 2 * data[, moderator] * my_vcov[treatment, paste0(treatment, ":", moderator)]
  }
  
  ### constraining data to relevant variables
  data <- data[, c(treatment, moderator)]
  
  ### getting marginal effects
  data[, "marginal_effects"] <- getmfx(coef(regression_model), data, treatment, moderator)
  
  ### getting  marginal effects for high and low cases of treatment variable
  data[, "marginal_effects_treatment_low"] <- getmfx_high_low(coef(regression_model), data, treatment, moderator, quantile(data[,treatment], 0.05, na.rm = TRUE))
  data[, "marginal_effects_treatment_high"] <- getmfx_high_low(coef(regression_model), data, treatment, moderator, quantile(data[,treatment], 0.95, na.rm = TRUE))
  
  ### getting robust SEs
  if (is.null(se)) {
    data$se <- getvarmfx(regression_model$vcv, data, treatment, moderator)
  } else if (se == "clustered") {
    data$se <- getvarmfx(regression_model$clustervcv, data, treatment, moderator)
  } else if (se == "robust") {
    data$se <- getvarmfx(regression_model$robustvcv, data, treatment, moderator)
  }
  
  ### Getting CI bounds
  data[, "ci_lower"] <- data[, "marginal_effects"] - abs(qt(alpha/2, regression_model$df, lower.tail = TRUE)) * sqrt(data$se)
  data[, "ci_upper"] <- data[, "marginal_effects"] + abs(qt(alpha/2, regression_model$df, lower.tail = TRUE)) * sqrt(data$se)
  
  ### plotting marginal effects plot
  p_1 <- ggplot(data, aes_string(x = moderator)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey70", alpha = 0.4) +
    geom_line(aes(y = marginal_effects)) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(size = 11.5, hjust = 0.5), axis.title = element_text(size = 10)) +
    geom_rug() + 
    xlab(moderator_translation) +
    ylab(paste("Marginal effect of",treatment_translation,"on",dependent_variable_translation)) +
    ggtitle("Average marginal effects")
  
  p_2 <- ggplot(data, aes_string(x = moderator)) +
    geom_line(aes(y = marginal_effects_treatment_high, color = paste0("High ",treatment_translation))) +
    geom_line(aes(y = marginal_effects_treatment_low, color = paste0("Low ",treatment_translation))) +
    theme_fivethirtyeight() + 
    theme(plot.title = element_text(size = 11.5, hjust = 0.5), axis.title = element_text(size = 10), axis.title.y = element_blank(), legend.justification = c(0.95, 0.95), legend.position = c(1, 1), legend.direction = "vertical") +
    geom_rug() + 
    xlab(moderator_translation) +
    ylab(paste("Marginal effect of",treatment_translation,"on",dependent_variable_translation)) +
    ggtitle("Marginal effects at high / low levels of treatment") +
    scale_color_manual(name = NULL, values = c(rgb(229, 93, 89, maxColorValue = 255), rgb(75, 180, 184, maxColorValue = 255)), labels=c(paste0("High ",treatment_translation), paste0("Low ",treatment_translation)))
  
  ### exporting plots as combined grob
  return(grid.arrange(p_1, p_2, ncol = 2))
}




#####PLOTS

felm_marginal_effects(regression_model = m1_rs_interaction_1, data = Full_new, treatment = "democracy_BX", moderator = "Ruralsup", treatment_translation = "Democracy", moderator_translation = "Rural supporters", dependent_variable_translation = "Forest Change (%)")

ggsave("marginal_effects.png", plot = last_plot(), dpi = 300)
