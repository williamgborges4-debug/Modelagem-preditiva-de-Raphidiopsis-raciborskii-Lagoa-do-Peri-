################
## Modelagem
##
## A modelagem real ainda esta sendo feita, então só inventei algo para dar 
## um resultado. 
##



# packages ----------------------------------------------------------------

# install.packages(c("readxl","ggplot2","mgcv","DHARMa"))
library(readxl)
library(ggplot2)
library(mgcv)
library(DHARMa)


# dirs --------------------------------------------------------------------

pasta_dados   <- "C:/Users/willi/Desktop/Modelagem Raphidiopsis/Data"
pasta_results <- "C:/Users/willi/Desktop/Modelagem Raphidiopsis/Results"

caminho_entrada <- file.path(pasta_dados, "D1.xlsx")


# read data ---------------------------------------------------------------

raw <- read_excel(caminho_entrada)
colnames(raw)[1] <- "data_coleta"
raw <- as.data.frame(raw)

cols_num <- c("raphi","tn","tp","clor","alca","ph","temp_agua","transp_agua",
              "profund","zon_fot","zon_afot","od","condut",
              "chuva_mes","temp_mes","vel_vento_mes","temp_24h","vento_24h")
for (col in cols_num) {
  x <- as.character(raw[[col]])
  x[x == "NA"] <- NA
  raw[[col]] <- suppressWarnings(as.numeric(gsub(",", ".", x)))
}

mes_map        <- c(jan=1,fev=2,mar=3,abr=4,mai=5,jun=6,
                    jul=7,ago=8,set=9,out=10,nov=11,dez=12)
raw$mes        <- mes_map[tolower(raw$mes_de_referencia)]
raw$tempo_cont <- raw$ano + (raw$mes - 1) / 12

preds <- c("tn","tp","clor","alca","ph","temp_agua",
           "zon_fot","zon_afot","od","condut","chuva_mes")

dados <- raw[, c("raphi","ano","mes","tempo_cont","estacao_do_ano", preds)]
dados <- dados[complete.cases(dados), ]
dados <- dados[order(dados$tempo_cont), ]
dados$log_raphid <- log(dados$raphi)

cat("n =", nrow(dados), "| período:", min(dados$ano), "–", max(dados$ano), "\n")


# modelo simples ----------------------------------------------------------

modelo <- gam(log_raphid ~
                s(tempo_cont, k = 10) +
                s(zon_afot,   k = 5)  +
                s(alca,       k = 5),
              method = "REML",
              data   = dados)

cat("\nSummary do modelo:\n")
summary(modelo)

par(mfrow = c(1, 3))
plot(modelo, residuals = TRUE, pch = 16, cex = 0.5,
     shade = TRUE, shade.col = "grey85")
par(mfrow = c(1, 1))

# diagnóstico
sim <- simulateResiduals(modelo, n = 500)
plot(sim, main = "Diagnóstico — GAM placeholder")

# visualização
nd <- data.frame(
  tempo_cont = seq(min(dados$tempo_cont), max(dados$tempo_cont),
                   length.out = 200),
  zon_afot   = median(dados$zon_afot),
  alca       = median(dados$alca))

pred       <- predict(modelo, newdata = nd, se.fit = TRUE)
nd$fit     <- pred$fit
nd$lwr     <- pred$fit - 1.96 * pred$se.fit
nd$upr     <- pred$fit + 1.96 * pred$se.fit

p <- ggplot() +
  geom_ribbon(data = nd, aes(x = tempo_cont, ymin = lwr, ymax = upr),
              fill = "grey80", alpha = 0.6) +
  geom_line(data = nd, aes(x = tempo_cont, y = fit), linewidth = 1) +
  geom_point(data = dados,
             aes(x = tempo_cont, y = log_raphid, color = estacao_do_ano),
             size = 1.5, alpha = 0.5) +
  scale_color_manual(values = c("Verão"="#E07B3F", "Outono"="#8B6914",
                                "Inverno"="#4A90D9", "Primavera"="#5BAD72")) +
  labs(title = "Tendência temporal — GAM (preliminar)",
       x = "Ano", y = "log(Raphidiopsis raciborskii)", color = "Estação") +
  theme_classic()
print(p)
ggsave(file.path(pasta_results, "tendencia_preliminar.png"),
       plot = p, width = 12, height = 5, dpi = 300)