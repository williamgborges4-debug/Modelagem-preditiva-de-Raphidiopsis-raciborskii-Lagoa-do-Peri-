################
## Análise Exploratória - Raphidiopsis raciborskii
## Lagoa do Peri, Florianópolis/SC
## Série temporal mensal 2009–2026
##
## Objetivo: explorar a distribuição da resposta, estrutura temporal,
## sazonalidade e relações com as preditoras abióticas antes da modelagem



# packages ----------------------------------------------------------------

# install.packages(c("readxl","ggplot2","patchwork","corrplot","moments"))
library(readxl)
library(ggplot2)
library(patchwork)
library(corrplot)
library(moments)


# dirs --------------------------------------------------------------------

pasta_dados   <- "C:/Users/willi/Desktop/Modelagem Raphidiopsis/Data"
pasta_figures <- "C:/Users/willi/Desktop/Modelagem Raphidiopsis/Figures"

caminho_entrada <- file.path(pasta_dados, "D1.xlsx")


# read data ---------------------------------------------------------------

raw <- read_excel(caminho_entrada)
colnames(raw)[1] <- "data_coleta"
raw <- as.data.frame(raw)

# forçar numérico e corrigir vírgula decimal
cols_num <- c("raphi","tn","tp","nitrito","nitrato","amonia",
              "clor","alca","ph","temp_agua","transp_agua","profund",
              "zon_fot","zon_afot","od","condut","turbidez",
              "chuva_mes","temp_mes","vel_vento_mes","temp_24h","vento_24h")

for (col in cols_num) {
  x <- as.character(raw[[col]])
  x[x == "NA"] <- NA
  raw[[col]] <- suppressWarnings(as.numeric(gsub(",", ".", x)))
}

# mês numérico, tempo contínuo e período
mes_map        <- c(jan=1,fev=2,mar=3,abr=4,mai=5,jun=6,
                    jul=7,ago=8,set=9,out=10,nov=11,dez=12)
raw$mes        <- mes_map[tolower(raw$mes_de_referencia)]
raw$tempo_cont <- raw$ano + (raw$mes - 1) / 12
raw$periodo    <- ifelse(raw$ano <= 2017, "2009-2017", "2018-2026")


# prep --------------------------------------------------------------------

# preditoras do modelo principal
# removidas por colinearidade: transp_agua (r=1.0 com zon_fot),
# temp_mes e temp_24h (r>0.88 com temp_agua) serão avaliadas pelo VIF
# na modelagem e removidas se necessário
preds_main <- c("tn","tp","clor","alca","ph","temp_agua","transp_agua",
                "profund","zon_fot","zon_afot","od","condut",
                "chuva_mes","temp_mes","vel_vento_mes","temp_24h","vento_24h")

labels_pred <- c(
  tn="N total (µg/L)", tp="P total (µg/L)", clor="Clorofila-a (µg/L)",
  alca="Alcalinidade (mEq/L)", ph="pH", temp_agua="Temperatura (°C)",
  transp_agua="Transparência (m)", profund="Profundidade (m)",
  zon_fot="Zona fótica (m)", zon_afot="Zona afótica (m)",
  od="O₂ dissolvido (mg/L)", condut="Condutividade (µS/cm)",
  chuva_mes="Chuva mensal (mm)", temp_mes="Temp. média mensal (°C)",
  vel_vento_mes="Vel. vento mensal (m/s)",
  temp_24h="Temp. 24h (°C)", vento_24h="Vento 24h (m/s)")

cores_estacao <- c("Verão"="#E07B3F", "Outono"="#8B6914",
                   "Inverno"="#4A90D9", "Primavera"="#5BAD72")

# dataset sem NAs nas preditoras
dados <- raw[complete.cases(raw[, c("raphi", preds_main)]), ]
dados <- dados[order(dados$tempo_cont), ]
dados$log_raphid <- log(dados$raphi)

cat("n =", nrow(dados), "| período:", min(dados$ano), "–", max(dados$ano), "\n")
cat("anos ausentes: 2015, 2020, 2021, 2022\n")


# 1. resumo da resposta ---------------------------------------------------
# assimetria e curtose indicam necessidade de transformação ou família não-gaussiana

summary(dados$raphi)
cat("skewness original:", round(skewness(dados$raphi),      3), "\n")
cat("skewness log     :", round(skewness(dados$log_raphid),  3), "\n")
cat("kurtosis original:", round(kurtosis(dados$raphi),      3), "\n")
cat("zeros            :", sum(dados$raphi == 0),              "\n")
cat("outliers (IQR)   :", sum(dados$raphi > quantile(dados$raphi, .75) +
                                1.5 * IQR(dados$raphi)),       "\n")


# 2. distribuição da resposta ---------------------------------------------
# shapiro-wilk rejeita normalidade nas 3 transformações → justifica família Gamma

# histogramas
p1 <- ggplot(dados, aes(x = raphi)) +
  geom_histogram(bins = 25, fill = "grey70", color = "white") +
  labs(title = "Original", x = "ind.mL⁻¹", y = "Frequência") +
  theme_classic()

p2 <- ggplot(dados, aes(x = sqrt(raphi))) +
  geom_histogram(bins = 25, fill = "steelblue", color = "white") +
  labs(title = "sqrt(raphi)", x = "sqrt(ind.mL⁻¹)", y = "Frequência") +
  theme_classic()

p3 <- ggplot(dados, aes(x = log_raphid)) +
  geom_histogram(bins = 25, fill = "seagreen", color = "white") +
  labs(title = "log(raphi)", x = "log(ind.mL⁻¹)", y = "Frequência") +
  theme_classic()

p_hist <- p1 | p2 | p3
print(p_hist)
ggsave(file.path(pasta_figures, "01_histogramas_resposta.png"),
       plot = p_hist, width = 12, height = 4, dpi = 300)

# q-q plots
png(file.path(pasta_figures, "02_qqplots_resposta.png"),
    width = 12, height = 4, units = "in", res = 300)
par(mfrow = c(1, 3))
qqnorm(dados$raphi,       main = "Q-Q Original"); qqline(dados$raphi,       col = "firebrick")
qqnorm(sqrt(dados$raphi), main = "Q-Q Raiz");     qqline(sqrt(dados$raphi), col = "firebrick")
qqnorm(dados$log_raphid,  main = "Q-Q log");      qqline(dados$log_raphid,  col = "firebrick")
par(mfrow = c(1, 1))
dev.off()

# shapiro-wilk — H0: normalidade; p < 0.05 rejeita
cat("shapiro original:", shapiro.test(dados$raphi)$p.value,       "\n")
cat("shapiro raiz    :", shapiro.test(sqrt(dados$raphi))$p.value,  "\n")
cat("shapiro log     :", shapiro.test(dados$log_raphid)$p.value,   "\n")


# 3. série temporal -------------------------------------------------------

p_serie_orig <- ggplot(dados, aes(x = tempo_cont, y = raphi)) +
  geom_line(alpha = 0.4, color = "grey50") +
  geom_point(aes(color = estacao_do_ano), size = 1.8) +
  scale_color_manual(values = cores_estacao) +
  labs(title = "Série temporal — Raphidiopsis (original)",
       x = "Ano", y = "ind.mL⁻¹", color = "Estação") +
  theme_classic()

p_serie_sqrt <- ggplot(dados, aes(x = tempo_cont, y = sqrt(raphi))) +
  geom_line(alpha = 0.4, color = "grey50") +
  geom_point(aes(color = estacao_do_ano), size = 1.8) +
  scale_color_manual(values = cores_estacao) +
  labs(title = "Série temporal — sqrt(Raphidiopsis)",
       x = "Ano", y = "sqrt(ind.mL⁻¹)", color = "Estação") +
  theme_classic()

p_series <- p_serie_orig / p_serie_sqrt
print(p_series)
ggsave(file.path(pasta_figures, "03_serie_temporal.png"),
       plot = p_series, width = 12, height = 7, dpi = 300)

# acf e pacf — lag-1 dominante no PACF justifica AR(1) no GAMM
# lag-2 também significativo → testar AR(2) na modelagem
png(file.path(pasta_figures, "04_acf_pacf.png"),
    width = 10, height = 5, units = "in", res = 300)
par(mfrow = c(1, 2))
acf(dados$log_raphid,  main = "ACF — log(raphi)",  lag.max = 24)
pacf(dados$log_raphid, main = "PACF — log(raphi)", lag.max = 24)
par(mfrow = c(1, 1))
dev.off()

# mapa de cobertura temporal — visualiza os gaps da série
anos_esperados <- min(dados$ano):max(dados$ano)
df_gaps <- data.frame(
  ano      = anos_esperados,
  presente = anos_esperados %in% unique(dados$ano)
)
p_gaps <- ggplot(df_gaps, aes(x = ano, y = 1, fill = presente)) +
  geom_tile(color = "white", height = 0.6) +
  scale_fill_manual(values = c("TRUE"="#4A90D9","FALSE"="#E24B4A"),
                    labels = c("TRUE"="Com dados","FALSE"="Sem dados")) +
  labs(title = "Cobertura temporal", x = "Ano", y = "", fill = "") +
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
print(p_gaps)
ggsave(file.path(pasta_figures, "05_cobertura_temporal.png"),
       plot = p_gaps, width = 10, height = 3, dpi = 300)


# 4. sazonalidade ---------------------------------------------------------

# média mensal — avalia se há padrão sazonal que justifique spline cíclico no GAM
med_mes     <- aggregate(log_raphid ~ mes, data = dados, FUN = mean)
sd_mes      <- aggregate(log_raphid ~ mes, data = dados, FUN = sd)
med_mes$sd  <- sd_mes$log_raphid
med_mes$mes_nome <- factor(
  c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"),
  levels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))

p_saz <- ggplot(med_mes, aes(x = mes_nome, y = log_raphid, group = 1)) +
  geom_ribbon(aes(ymin = log_raphid - sd, ymax = log_raphid + sd),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2.5) +
  labs(title = "Sazonalidade média — log(Raphidiopsis)",
       subtitle = "Média ± 1 DP por mês", x = NULL, y = "log(ind.mL⁻¹)") +
  theme_classic()
print(p_saz)
ggsave(file.path(pasta_figures, "06_sazonalidade_mensal.png"),
       plot = p_saz, width = 10, height = 5, dpi = 300)

# decomposição STL — separa tendência, sazonalidade e resíduo
# gaps interpolados linearmente só para esta análise (não altera dados de modelagem)
serie_full <- data.frame(
  tempo_cont = seq(min(dados$tempo_cont), max(dados$tempo_cont), by = 1/12))
serie_full <- merge(serie_full, dados[, c("tempo_cont","log_raphid")],
                    by = "tempo_cont", all.x = TRUE)
serie_full <- serie_full[order(serie_full$tempo_cont), ]
serie_full$log_raphid <- approx(
  x      = serie_full$tempo_cont[!is.na(serie_full$log_raphid)],
  y      = serie_full$log_raphid[!is.na(serie_full$log_raphid)],
  xout   = serie_full$tempo_cont,
  method = "linear")$y

stl_res  <- stl(ts(serie_full$log_raphid, frequency = 12),
                s.window = "periodic", robust = TRUE)
var_tend <- var(stl_res$time.series[, "trend"])
var_saz  <- var(stl_res$time.series[, "seasonal"])
var_res  <- var(stl_res$time.series[, "remainder"])
var_tot  <- var_tend + var_saz + var_res

cat(sprintf("STL — tendência: %.1f%% | sazonalidade: %.1f%% | resíduo: %.1f%%\n",
            var_tend/var_tot*100, var_saz/var_tot*100, var_res/var_tot*100))

png(file.path(pasta_figures, "07_stl_decomposicao.png"),
    width = 10, height = 8, units = "in", res = 300)
plot(stl_res, main = "Decomposição STL — log(Raphidiopsis)")
dev.off()


# 5. preditoras ao longo do tempo -----------------------------------------
# identifica tendência temporal em cada preditora (potencial confundidor)

plots_tempo <- lapply(preds_main, function(v) {
  ggplot(dados, aes(x = tempo_cont, y = .data[[v]])) +
    geom_line(alpha = 0.3, color = "grey50") +
    geom_point(aes(color = estacao_do_ano), size = 1.2) +
    geom_smooth(method = "loess", se = FALSE, color = "black",
                linewidth = 0.8, linetype = "dashed") +
    scale_color_manual(values = cores_estacao, guide = "none") +
    labs(x = NULL, y = NULL, title = labels_pred[v]) +
    theme_classic(base_size = 9)
})
p_tempo <- wrap_plots(plots_tempo, ncol = 4)
print(p_tempo)
ggsave(file.path(pasta_figures, "08_preditoras_tempo.png"),
       plot = p_tempo, width = 16, height = 12, dpi = 300)


# 6. preditoras vs log(raphi) ---------------------------------------------
# curva loess: reta = relação linear; curvatura = justifica spline no GAM

plots_scatter <- lapply(preds_main, function(v) {
  ggplot(dados, aes(x = .data[[v]], y = log_raphid)) +
    geom_point(color = "grey40", size = 1.2, alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "firebrick",
                fill = "firebrick", alpha = 0.15, linewidth = 0.9) +
    labs(x = labels_pred[v], y = "log(raphi)", title = labels_pred[v]) +
    theme_classic(base_size = 9)
})
p_scatter <- wrap_plots(plots_scatter, ncol = 4)
print(p_scatter)
ggsave(file.path(pasta_figures, "09_preditoras_vs_raphi.png"),
       plot = p_scatter, width = 16, height = 12, dpi = 300)


# 7. preditoras por estação -----------------------------------------------
# identifica quais preditoras variam sazonalmente

plots_box <- lapply(preds_main, function(v) {
  ggplot(dados, aes(x = estacao_do_ano, y = .data[[v]],
                    fill = estacao_do_ano)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
    scale_fill_manual(values = cores_estacao, guide = "none") +
    labs(x = NULL, y = NULL, title = labels_pred[v]) +
    theme_classic(base_size = 9) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
})
p_box <- wrap_plots(plots_box, ncol = 4)
print(p_box)
ggsave(file.path(pasta_figures, "10_preditoras_estacao.png"),
       plot = p_box, width = 16, height = 12, dpi = 300)


# 8. correlação entre preditoras ------------------------------------------
# pares com |r| > 0.7 serão removidos antes da modelagem para evitar VIF alto

cor_mat <- cor(dados[, preds_main], use = "complete.obs")

png(file.path(pasta_figures, "11_correlacao_preditoras.png"),
    width = 10, height = 9, units = "in", res = 300)
corrplot(cor_mat, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.55,
         tl.col = "black", tl.srt = 45, tl.cex = 0.7,
         title = "Correlação entre preditoras", mar = c(0,0,1,0))
dev.off()

cat("\npares com |r| > 0.7:\n")
for (i in 1:(nrow(cor_mat)-1))
  for (j in (i+1):ncol(cor_mat))
    if (abs(cor_mat[i,j]) > 0.7)
      cat(sprintf("  %s x %s: r = %.3f\n",
                  rownames(cor_mat)[i], colnames(cor_mat)[j], cor_mat[i,j]))


# 9. correlação com raphi por período -------------------------------------
# verifica se as relações mudaram entre 2009-2017 e 2018-2026
# inversão de sinal indica confundimento com tendência temporal

dados_p1 <- dados[dados$periodo == "2009-2017", ]
dados_p2 <- dados[dados$periodo == "2018-2026", ]

cor_p1 <- sapply(preds_main, function(v)
  cor(dados_p1[[v]], dados_p1$log_raphid, use = "complete.obs"))
cor_p2 <- sapply(preds_main, function(v)
  cor(dados_p2[[v]], dados_p2$log_raphid, use = "complete.obs"))

df_cor <- data.frame(
  variavel = rep(preds_main, 2),
  periodo  = rep(c("2009-2017","2018-2026"), each = length(preds_main)),
  r        = c(cor_p1, cor_p2))

p_cor_periodo <- ggplot(df_cor, aes(x = reorder(variavel, abs(r)),
                                    y = r, fill = periodo)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = c(-0.3, 0.3), linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("2009-2017"="#4A90D9","2018-2026"="#E07B3F")) +
  coord_flip() +
  labs(title = "Correlação com log(raphi) por período",
       x = NULL, y = "r de Pearson", fill = "Período") +
  theme_classic()
print(p_cor_periodo)
ggsave(file.path(pasta_figures, "12_correlacao_por_periodo.png"),
       plot = p_cor_periodo, width = 10, height = 7, dpi = 300)


# 10. resumo --------------------------------------------------------------

cat("\n=== resumo EDA ===\n")
cat("skewness original:", round(skewness(dados$raphi),      3), "\n")
cat("skewness raiz    :", round(skewness(sqrt(dados$raphi)), 3), "\n")
cat(sprintf("STL — tendência: %.1f%% | sazonalidade: %.1f%% | resíduo: %.1f%%\n",
            var_tend/var_tot*100, var_saz/var_tot*100, var_res/var_tot*100))

cat("\npreditoras com |r| > 0.3 com raphi (algum período):\n")
for (v in preds_main)
  if (abs(cor_p1[v]) > 0.3 | abs(cor_p2[v]) > 0.3)
    cat(sprintf("  %-18s 2009-2017: %+.3f  2018-2026: %+.3f\n",
                v, cor_p1[v], cor_p2[v]))

cat("\nfiguras salvas em:", pasta_figures, "\n")