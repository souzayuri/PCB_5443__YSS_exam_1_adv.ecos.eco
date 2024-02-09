
if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(ggpubr)) install.packages("ggpubr", dependencies = TRUE)


# Part 1 ------------------------------------------------------------------


data <- readr::read_csv("00_data/WaterQualityWaterQualityHUC12_HCCR.csv")

glimpse(data)

data_fltd <- data |> 
  dplyr::select(c(SampleDate, Parameter, MeasureValue)) |> 
  tidyr::drop_na("MeasureValue") |> 
  dplyr::mutate(SampleDates = as.Date(SampleDate, format = "%m/%d/%Y"),
                SampleDate = as.factor(SampleDate))

data_fltd_n_p <- data_fltd |> 
  dplyr::mutate(Parameter = "TDNP")

data_fltd_ttl <- rbind(data_fltd, data_fltd_n_p)
data_fltd_ttl


model_coeff <- function(x) {
    model_summary_np <- data_fltd_ttl |> 
      dplyr::group_by(model = 1) |>  
      dplyr::filter(Parameter == x) |> 
      dplyr::do({
        model <- lm(MeasureValue ~ SampleDates, data = .)  
        dplyr::tibble(
          slope = broom::tidy(model)$estimate[2],  
          r_squared = broom::glance(model)$r.squared  
        )
      }) |> 
      dplyr::ungroup() |> 
      dplyr::select(!1)

  return(model_summary_np) 
}


nt.model.coeff <- dplyr::bind_rows(model_coeff("TDNP"), model_coeff("TDN"), model_coeff("TDP")) |> 
  dplyr::mutate(Parameter = c("TDNP", "TDN", "TDP"),
                date_label = "2015-05-14") |> 
  dplyr::select(c(3, dplyr::everything()))
nt.model.coeff

lm_model_1pt <- ggplot(data_fltd_ttl, aes(y=MeasureValue, x=SampleDates)) + 
  geom_point(shape = 21, color = "black", fill = "grey20", size = 3, alpha = 0.2) +
  geom_smooth(method="lm", col="darkred", linewidth = 2) +
  theme_bw() +
  aes(ymax=Inf) +
  labs(x = "Dates", y = "Total Dissolved Nutrient(Mg/L)") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme(axis.text.x= element_text(angle=-75, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 12)) + 
  facet_wrap(.~factor(Parameter, c("TDNP", "TDN", "TDP"), labels = c("Nitrogen + Phosphorus", "Nitrogen", "Phosphorus")), 
             scales = "free") +
  geom_text(data = nt.model.coeff, aes(label = paste("Slope:", round(slope, 9), "\nR²:", round(r_squared, 8)),
                                          x = as.Date(date_label), y = -Inf),
            hjust = 0.1, vjust = -10, size = 4, color = "black")



loess_model_1pt <- ggplot(data_fltd_ttl, aes(y=MeasureValue, x=SampleDates)) + 
  geom_point(shape = 21, color = "black", fill = "grey20", size = 3, alpha = 0.2) +
  geom_smooth(method="loess", col="darkred", linewidth = 2) +
  theme_bw() +
  aes(ymax=Inf) +
  labs(x = "Dates", y = "Total Dissolved Nutrient(Mg/L)") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme(axis.text.x= element_text(angle=-75, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 12)) + 
  facet_wrap(.~factor(Parameter, c("TDNP", "TDN", "TDP"), labels = c("Nitrogen + Phosphorus", "Nitrogen", "Phosphorus")), 
             scales = "free") +
  geom_text(data = nt.model.coeff, aes(label = paste("Slope:", round(slope, 9), "\nR²:", round(r_squared, 8)),
                                       x = as.Date(date_label), y = -Inf),
            hjust = 0.1, vjust = -10, size = 4, color = "black")

my_plot_list <- list(lm_model_1pt, loess_model_1pt)

ggpubr::ggarrange(plotlist = my_plot_list, labels = c("A", "B"), nrow = 2) |> 
  annotate_figure(top = text_grob("Figure 1. Harris Creek-Choptank River", 
                                  color = "black", face = "bold", size = 16))

#ggsave("02_figures/nutrient_graph_pt1.png", w = 25, h = 27, dpi = 500, unit = "cm")



# Part 2 ------------------------------------------------------------------

data_HCCR <- readr::read_csv("G:/My Drive/Yuri/PhD/00_UM/03_graduate/00_courses/04_4th_semester/FIU_Course/PCB_5443_2024/20230207_YSS_exam_1_adv.ecos.eco/00_data/WaterQualityWaterQualityHUC12_HCCR.csv") |> 
  dplyr::select(c(1,18,20)) |> 
  tidyr::drop_na("MeasureValue")
data_HCCR

data_BR <- readr::read_csv("G:/My Drive/Yuri/PhD/00_UM/03_graduate/00_courses/04_4th_semester/FIU_Course/PCB_5443_2024/20230207_YSS_exam_1_adv.ecos.eco/00_data/WaterQualityWaterQualityHUC12_BR.csv") |> 
  dplyr::select(c(1,18,20)) |> 
  tidyr::drop_na("MeasureValue")
data_BR



data_HCCR_BR <- rbind(data_HCCR, data_BR) |> 
  dplyr::group_by(Parameter) %>%
  dplyr::mutate(Row = row_number()) %>%
  tidyr::pivot_wider(names_from = Parameter, values_from = MeasureValue) |> 
  dplyr::select(-Row) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(HUC12 = stringr::str_replace(HUC12, "20600050506", "HCCR"),
                HUC12 = stringr::str_replace(HUC12, "020600030106", "BR"))
data_HCCR_BR



model_coeff_HCCR_BR <- function(x) {
  model_summary_np_HCCR_BR <- data_HCCR_BR |> 
    dplyr::group_by(model = 1) |>  
    dplyr::filter(HUC12 == x) |> 
    dplyr::do({
      model <- lm(TDN ~ TDP, data = .)  
      dplyr::tibble(
        slope = broom::tidy(model)$estimate[2],  
        r_squared = broom::glance(model)$r.squared  
      )
    }) |> 
    dplyr::ungroup() |> 
    dplyr::select(!1)
  
  return(model_summary_np_HCCR_BR) 
}

model_coeff_HCCR_BR("HCCR")
model_coeff_HCCR_BR("BR")


nt.model.coeff_HCCR_BR <- bind_rows(model_coeff_HCCR_BR("HCCR"), model_coeff_HCCR_BR("BR")) |> 
  dplyr::mutate(HUC12 = c("HCCR", "BR"))
nt.model.coeff_HCCR_BR




# graphs

lm_model_2pt <- ggplot(data_HCCR_BR, aes(y=TDN, x=TDP)) + 
  geom_point(shape = 21, color = "black", fill = "grey20", size = 3, alpha = 0.2) +
  geom_smooth(method="lm", col="darkred", linewidth = 2) +
  theme_bw() +
  #aes(ymax=Inf) +
  labs(x = "Total Dissolved P (Mg/L)", y = "Total Dissolved N (Mg/L)") +
  theme(axis.text.x= element_text(size = 14),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 12)) + 
  facet_wrap(.~factor(HUC12, c("HCCR", "BR"), labels = c("Harris Creek-Choptank River", "Bush River")), 
             scales = "free") +
  geom_text(data = nt.model.coeff_HCCR_BR, aes(label = paste("Slope:", round(slope, 6), "\nR²:", round(r_squared, 5)),
                                               x = -Inf, y = -Inf), hjust = -0.1, vjust = -7, size = 4, color = "black")
lm_model_2pt

loess_model_2pt <- ggplot(data_HCCR_BR, aes(y=TDN, x=TDP)) +  
  geom_point(shape = 21, color = "black", fill = "grey20", size = 3, alpha = 0.2) +
  geom_smooth(method="loess", col="darkred", linewidth = 2) +
  theme_bw() +
  aes(ymax=Inf) +
  labs(x = "Total Dissolved P (Mg/L)", y = "Total Dissolved N (Mg/L)") +
  theme(axis.text.x= element_text(size = 14),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 12)) + 
  facet_wrap(.~factor(HUC12, c("HCCR", "BR"), labels = c("Harris Creek-Choptank River", "Bush River")), 
             scales = "free") +
  geom_text(data = nt.model.coeff_HCCR_BR, aes(label = paste("Slope:", round(slope, 6), "\nR²:", round(r_squared, 5)),
                                               x = -Inf, y = -Inf), hjust = -0.1, vjust = -7, size = 4, color = "black")

my_plot_list_pt2 <- list(lm_model_2pt, loess_model_2pt)

ggpubr::ggarrange(plotlist = my_plot_list_pt2, labels = c("A", "B"), nrow = 2) |> 
  annotate_figure(top = text_grob("Figure 2. Harris Creek-Choptank River & Bush River", 
                                  color = "black", face = "bold", size = 16))
#ggsave("02_figures/nutrient_graph_pt2.png", w = 20, h = 20, dpi = 500, unit = "cm")
