
# packages ----------------------------------------------------------------


if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(ggpubr)) install.packages("ggpubr", dependencies = TRUE)


# Part 1 ------------------------------------------------------------------


# Part A + C ------------------------------------------------------------------


data <- readr::read_csv("00_data/WaterQualityWaterQualityHUC12_HCCR.csv")

glimpse(data)

#station 1
unique(data$Station)
data_fltd_XFG2810 <- data |> 
  dplyr::filter(Station == "XFG2810") |> 
  dplyr::select(c(SampleDate, Parameter, MeasureValue)) |> 
  tidyr::drop_na("MeasureValue") |> 
  dplyr::mutate(SampleDates = as.Date(SampleDate, format = "%m/%d/%Y"),
                SampleDate = as.factor(SampleDate))

data_fltd_n_p_XFG2810 <- data_fltd_XFG2810 |> 
  dplyr::mutate(Parameter = "TDNP")

data_fltd_ttl_XFG2810 <- rbind(data_fltd_XFG2810, data_fltd_n_p_XFG2810)
data_fltd_ttl_XFG2810


model_coeff_XFG2810 <- function(x) {
    model_summary_np_XFG2810 <- data_fltd_ttl_XFG2810 |> 
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

  return(model_summary_np_XFG2810) 
}


nt.model.coeff_XFG2810 <- dplyr::bind_rows(model_coeff_XFG2810("TDNP"), model_coeff_XFG2810("TDN"), model_coeff_XFG2810("TDP")) |> 
  dplyr::mutate(Parameter = c("TDNP", "TDN", "TDP"),
                date_label = "2015-05-14") |> 
  dplyr::select(c(3, dplyr::everything()))
nt.model.coeff_XFG2810

lm_model_1pt_XFG2810 <- ggplot(data_fltd_ttl_XFG2810, aes(y=MeasureValue, x=SampleDates)) + 
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
  geom_text(data = nt.model.coeff_XFG2810, aes(label = paste("Slope:", round(slope, 9), "\nR²:", round(r_squared, 8)),
                                          x = as.Date(date_label), y = -Inf),
            hjust = 0.1, vjust = -10, size = 4, color = "black")



loess_model_1pt_XFG2810 <- ggplot(data_fltd_ttl_XFG2810, aes(y=MeasureValue, x=SampleDates)) + 
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
  geom_text(data = nt.model.coeff_XFG2810, aes(label = paste("Slope:", round(slope, 9), "\nR²:", round(r_squared, 8)),
                                       x = as.Date(date_label), y = -Inf),
            hjust = 0.1, vjust = -10, size = 4, color = "black")

my_plot_list_XFG2810 <- list(lm_model_1pt_XFG2810, loess_model_1pt_XFG2810)

ggpubr::ggarrange(plotlist = my_plot_list_XFG2810, labels = c("A", "B"), nrow = 2) |> 
  annotate_figure(top = text_grob("Figure 1a. Harris Creek-Choptank River - XFG2810 Station", 
                                  color = "black", face = "bold", size = 16))

#ggsave("02_figures/XFG2810_nutrient_graph_pt1.png", w = 25, h = 27, dpi = 500, unit = "cm")


# Part B + C  ------------------------------------------------------------------

data_fltd_XFG6431 <- data |> 
  dplyr::filter(Station == "XFG6431") |> 
  dplyr::select(c(SampleDate, Parameter, MeasureValue)) |> 
  tidyr::drop_na("MeasureValue") |> 
  dplyr::mutate(SampleDates = as.Date(SampleDate, format = "%m/%d/%Y"),
                SampleDate = as.factor(SampleDate))

data_fltd_n_p_XFG6431 <- data_fltd_XFG6431 |> 
  dplyr::mutate(Parameter = "TDNP")

data_fltd_ttl_XFG6431 <- rbind(data_fltd_XFG6431, data_fltd_n_p_XFG6431)
data_fltd_ttl_XFG6431


model_coeff_XFG6431 <- function(x) {
  model_summary_np_XFG6431 <- data_fltd_ttl_XFG6431 |> 
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
  
  return(model_summary_np_XFG6431) 
}


nt.model.coeff_XFG6431 <- dplyr::bind_rows(model_coeff_XFG6431("TDNP"), model_coeff_XFG6431("TDN"), model_coeff_XFG6431("TDP")) |> 
  dplyr::mutate(Parameter = c("TDNP", "TDN", "TDP"),
                date_label = "2015-05-14") |> 
  dplyr::select(c(3, dplyr::everything()))
nt.model.coeff_XFG6431

lm_model_1pt_XFG6431 <- ggplot(data_fltd_ttl_XFG6431, aes(y=MeasureValue, x=SampleDates)) + 
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
  geom_text(data = nt.model.coeff_XFG6431, aes(label = paste("Slope:", round(slope, 9), "\nR²:", round(r_squared, 8)),
                                               x = as.Date(date_label), y = -Inf),
            hjust = 0.1, vjust = -10, size = 4, color = "black")



loess_model_1pt_XFG6431 <- ggplot(data_fltd_ttl_XFG6431, aes(y=MeasureValue, x=SampleDates)) + 
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
  geom_text(data = nt.model.coeff_XFG6431, aes(label = paste("Slope:", round(slope, 9), "\nR²:", round(r_squared, 8)),
                                               x = as.Date(date_label), y = -Inf),
            hjust = 0.1, vjust = -10, size = 4, color = "black")

my_plot_list_XFG6431 <- list(lm_model_1pt_XFG6431, loess_model_1pt_XFG6431)

ggpubr::ggarrange(plotlist = my_plot_list_XFG6431, labels = c("A", "B"), nrow = 2) |> 
  annotate_figure(top = text_grob("Figure 1b. Harris Creek-Choptank River - XFG6431 Station", 
                                  color = "black", face = "bold", size = 16))

#ggsave("02_figures/XFG6431_nutrient_graph_pt1.png", w = 25, h = 27, dpi = 500, unit = "cm")



# Part 2 ------------------------------------------------------------------

data_HCCR <- readr::read_csv("00_data/WaterQualityWaterQualityHUC12_HCCR.csv") |> 
  dplyr::select(c(1,9,18,20)) |> 
  tidyr::drop_na("MeasureValue")
data_HCCR

data_BR <- readr::read_csv("00_data/WaterQualityWaterQualityHUC12_BR.csv") |> 
  dplyr::select(c(1,9,18,20)) |> 
  tidyr::drop_na("MeasureValue")
data_BR



data_HCCR_BR <- rbind(data_HCCR, data_BR) |> 
  dplyr::group_by(Parameter, SampleDate) %>%
  dplyr::mutate(Row = row_number()) %>%
  tidyr::pivot_wider(names_from = Parameter, values_from = MeasureValue) |> 
  dplyr::select(-Row) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(HUC12 = stringr::str_replace(HUC12, "20600050506", "HCCR"),
                HUC12 = stringr::str_replace(HUC12, "020600030106", "BR"),
                Ratio = TDN/TDP,
                SampleDates = as.Date(SampleDate, format = "%m/%d/%Y"),
                SampleDate = as.factor(SampleDate)) 
data_HCCR_BR



model_coeff_HCCR_BR <- function(x) {
  model_summary_np_HCCR_BR <- data_HCCR_BR |> 
    dplyr::group_by(model = 1) |>  
    dplyr::filter(HUC12 == x) |> 
    dplyr::do({
      model <- lm(Ratio ~ SampleDates, data = .)  
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
  dplyr::mutate(HUC12 = c("HCCR", "BR"),
                date_label = "2015-05-14") |> 
  dplyr::select(c(3, dplyr::everything()))
nt.model.coeff_HCCR_BR


# graphs

lm_model_2pt <- ggplot(data_HCCR_BR, aes(y=Ratio, x=SampleDates)) + 
  geom_point(shape = 21, color = "black", fill = "grey20", size = 3, alpha = 0.2) +
  geom_smooth(method="lm", col="darkred", linewidth = 2) +
  theme_bw() +
  aes(ymax=Inf) +
  labs(x = "Dates", y = "Nitrogen:Phosphorus ratio (Mg/L)") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme(axis.text.x= element_text(angle=-75, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 12)) + 
  facet_wrap(.~factor(HUC12, c("HCCR", "BR"), labels = c("Choptank River", "Bush River")), 
             scales = "free") +
  geom_text(data = nt.model.coeff_HCCR_BR, aes(label = paste("Slope:", round(slope, 4), "\nR²:", round(r_squared, 4)),
                                               x = as.Date(date_label), y = -Inf),
            hjust = 0.1, vjust = -9, size = 4, color = "black")
lm_model_2pt




loess_model_2pt <- ggplot(data_HCCR_BR, aes(y=Ratio, x=SampleDates)) + 
  geom_point(shape = 21, color = "black", fill = "grey20", size = 3, alpha = 0.2) +
  geom_smooth(method="loess", col="darkred", linewidth = 2) +
  theme_bw() +
  aes(ymax=Inf) +
  labs(x = "Dates", y = "Nitrogen:Phosphorus ratio (Mg/L)") +
  scale_x_date(date_breaks = "year", date_labels = "%Y") +
  theme(axis.text.x= element_text(angle=-75, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 12)) + 
  facet_wrap(.~factor(HUC12, c("HCCR", "BR"), labels = c("Choptank River", "Bush River")), 
             scales = "free") +
  geom_text(data = nt.model.coeff_HCCR_BR, aes(label = paste("Slope:", round(slope, 4), "\nR²:", round(r_squared, 4)),
                                               x = as.Date(date_label), y = -Inf),
            hjust = 0.1, vjust = -9, size = 4, color = "black")
loess_model_2pt




my_plot_list_pt2 <- list(lm_model_2pt, loess_model_2pt)

ggpubr::ggarrange(plotlist = my_plot_list_pt2, labels = c("A", "B"), nrow = 2) |> 
  annotate_figure(top = text_grob("Figure 2. Harris Creek-Choptank River & Bush River", 
                                  color = "black", face = "bold", size = 16))
#ggsave("02_figures/nutrient_graph_ratio_pt2.png", w = 25, h = 25, dpi = 500, unit = "cm")



# correlation graphs ------------------------------------------------------



# 
# lm_model_2pt <- ggplot(data_HCCR_BR, aes(y=TDN, x=TDP)) + 
#   geom_point(shape = 21, color = "black", fill = "grey20", size = 3, alpha = 0.2) +
#   geom_smooth(method="lm", col="darkred", linewidth = 2) +
#   theme_bw() +
#   #aes(ymax=Inf) +
#   labs(x = "Total Dissolved P (Mg/L)", y = "Total Dissolved N (Mg/L)") +
#   theme(axis.text.x= element_text(size = 14),
#         axis.text.y= element_text(size = 14),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         strip.text = element_text(size = 12)) + 
#   facet_wrap(.~factor(HUC12, c("HCCR", "BR"), labels = c("Harris Creek-Choptank River", "Bush River")), 
#              scales = "free") +
#   geom_text(data = nt.model.coeff_HCCR_BR, aes(label = paste("Slope:", round(slope, 6), "\nR²:", round(r_squared, 5)),
#                                                x = -Inf, y = -Inf), hjust = -0.1, vjust = -7, size = 4, color = "black")
# lm_model_2pt
# 
# loess_model_2pt <- ggplot(data_HCCR_BR, aes(y=TDN, x=TDP)) +  
#   geom_point(shape = 21, color = "black", fill = "grey20", size = 3, alpha = 0.2) +
#   geom_smooth(method="loess", col="darkred", linewidth = 2) +
#   theme_bw() +
#   aes(ymax=Inf) +
#   labs(x = "Total Dissolved P (Mg/L)", y = "Total Dissolved N (Mg/L)") +
#   theme(axis.text.x= element_text(size = 14),
#         axis.text.y= element_text(size = 14),
#         axis.title.x = element_text(size = 18),
#         axis.title.y = element_text(size = 18),
#         strip.text = element_text(size = 12)) + 
#   facet_wrap(.~factor(HUC12, c("HCCR", "BR"), labels = c("Harris Creek-Choptank River", "Bush River")), 
#              scales = "free") +
#   geom_text(data = nt.model.coeff_HCCR_BR, aes(label = paste("Slope:", round(slope, 6), "\nR²:", round(r_squared, 5)),
#                                                x = -Inf, y = -Inf), hjust = -0.1, vjust = -7, size = 4, color = "black")
# 
# my_plot_list_pt2 <- list(lm_model_2pt, loess_model_2pt)
# 
# ggpubr::ggarrange(plotlist = my_plot_list_pt2, labels = c("A", "B"), nrow = 2) |> 
#   annotate_figure(top = text_grob("Figure 2. Harris Creek-Choptank River & Bush River", 
#                                   color = "black", face = "bold", size = 16))
# #ggsave("02_figures/nutrient_graph_pt2.png", w = 20, h = 20, dpi = 500, unit = "cm")

# correlation graphs ------------------------------------------------------

