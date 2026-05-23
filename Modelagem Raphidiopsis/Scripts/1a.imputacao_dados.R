################
## Imputação de dados faltantes — Lagoa do Peri
## Raphidiopsis | Série mensal 2009–2026
##
## Métodos:
##   interpolação linear : (anterior + posterior) / 2
##   média sazonal       : média do mesmo mês em outros anos
##
##



# packages ----------------------------------------------------------------

library(readxl)
library(writexl)


# dirs --------------------------------------------------------------------

pasta_dados     <- "C:/Users/willi/Desktop/Modelagem Raphidiopsis/Data"
caminho_entrada <- file.path(pasta_dados, "D0.xlsx")
caminho_saida   <- file.path(pasta_dados, "D1.xlsx")


# read data ---------------------------------------------------------------

df <- read_excel(caminho_entrada, sheet = "Banco_dados_Limnos_Raphidiopsis")
colnames(df)[1] <- "data_coleta"
df <- as.data.frame(df)  # tibble não aceita atribuição por índice

# forçar numérico e corrigir vírgula decimal
cols_num <- c("raphi","tn","tp","clor","alca","ph","temp_agua","transp_agua",
              "profund","zon_fot","zon_afot","od","condut","turbidez",
              "chuva_mes","temp_mes","vel_vento_mes","temp_24h","vento_24h",
              "nitrito","nitrato","amonia")
for (col in cols_num) {
  x <- as.character(df[[col]])
  x[x == "NA"] <- NA
  df[[col]] <- suppressWarnings(as.numeric(gsub(",", ".", x)))
}

# padronizar meses para abreviação de 3 letras
meses_ext <- c("janeiro","fevereiro","março","abril","maio","junho",
               "julho","agosto","setembro","outubro","novembro","dezembro")
meses_abr <- c("jan","fev","mar","abr","mai","jun",
               "jul","ago","set","out","nov","dez")
for (i in seq_along(meses_ext))
  df$mes_de_referencia[tolower(df$mes_de_referencia) == meses_ext[i]] <- meses_abr[i]

cat("leitura OK:", nrow(df), "linhas\n")


# função auxiliar ---------------------------------------------------------

# média histórica do mesmo mês, excluindo a própria linha
media_mes <- function(df, col, linha) {
  mes  <- df$mes_de_referencia[linha]
  vals <- df[[col]][df$mes_de_referencia == mes & seq_len(nrow(df)) != linha]
  mean(vals, na.rm = TRUE)
}


# interpolação linear -----------------------------------------------------
# usado quando os vizinhos anterior e posterior estão disponíveis

# ph
df$ph[57]  <- (df$ph[56]  + df$ph[58])  / 2   # abr/2016
df$ph[99]  <- (df$ph[98]  + df$ph[100]) / 2   # mai/2023

# zon_afot
df$zon_afot[68] <- (df$zon_afot[67] + df$zon_afot[69]) / 2   # mar/2017

# vento_24h
df$vento_24h[72] <- (df$vento_24h[71] + df$vento_24h[73]) / 2  # jul/2017

# od
df$od[61] <- (df$od[60] + df$od[62]) / 2   # ago/2016 — isolado

# tp
df$tp[53] <- (df$tp[52] + df$tp[54]) / 2   # mar/2014
df$tp[55] <- (df$tp[54] + df$tp[56]) / 2   # fev/2016
df$tp[99] <- (df$tp[98] + df$tp[100]) / 2  # mai/2023

# tn
df$tn[32]  <- (df$tn[31]  + df$tn[33])  / 2   # mar/2012
df$tn[53]  <- (df$tn[52]  + df$tn[54])  / 2   # mar/2014
df$tn[100] <- (df$tn[99]  + df$tn[101]) / 2   # jun/2023

# clor
df$clor[94] <- (df$clor[93] + df$clor[95]) / 2  # mai/2019 — isolado

# alca
df$alca[5]   <- (df$alca[4]   + df$alca[6])   / 2  # nov/2009
df$alca[58]  <- (df$alca[57]  + df$alca[59])  / 2  # mai/2016
df$alca[110] <- (df$alca[109] + df$alca[111]) / 2  # abr/2024

# nitrito
df$nitrito[9] <- (df$nitrito[8] + df$nitrito[10]) / 2   # mar/2010

# nitrato
df$nitrato[9]  <- (df$nitrato[8]  + df$nitrato[10]) / 2   # mar/2010
df$nitrato[70] <- (df$nitrato[69] + df$nitrato[71]) / 2   # mai/2017
df$nitrato[81] <- (df$nitrato[80] + df$nitrato[82]) / 2   # abr/2018

# amonia
df$amonia[9]  <- (df$amonia[8]  + df$amonia[10]) / 2   # mar/2010
df$amonia[13] <- (df$amonia[12] + df$amonia[14]) / 2   # jul/2010
df$amonia[32] <- (df$amonia[31] + df$amonia[33]) / 2   # mar/2012
df$amonia[60] <- (df$amonia[59] + df$amonia[61]) / 2   # jul/2016


# média sazonal -----------------------------------------------------------
# usado quando não há vizinho disponível ou o bloco é consecutivo

# alca — início de série sem vizinho anterior
df$alca[1] <- media_mes(df, "alca", 1)   # jul/2009
df$alca[2] <- media_mes(df, "alca", 2)   # ago/2009

# alca — bloco mar-out/2023 (out/2023 era valor anômalo 0.001)
for (i in 97:104) df$alca[i] <- media_mes(df, "alca", i)

# clor — bloco jan-mar/2013
for (i in 42:44) df$clor[i] <- media_mes(df, "clor", i)

# od — bloco dez/2016 e jan/2017
# interpola entre od[60]=nov/2016 e od[67]=fev/2017
df$od[65] <- df$od[60] + (df$od[67] - df$od[60]) * (1/3)   # dez/2016
df$od[66] <- df$od[60] + (df$od[67] - df$od[60]) * (2/3)   # jan/2017

# tn — extremo de série 2026
df$tn[130] <- media_mes(df, "tn", 130)   # jan/2026
df$tn[131] <- media_mes(df, "tn", 131)   # fev/2026

# tp — extremo de série 2026
df$tp[130] <- media_mes(df, "tp", 130)   # jan/2026
df$tp[131] <- media_mes(df, "tp", 131)   # fev/2026

# nitrato — início de série 2009 sem vizinhos
for (i in 1:6) df$nitrato[i] <- media_mes(df, "nitrato", i)

# nitrato — ago e set/2017 com apenas um vizinho
df$nitrato[73] <- media_mes(df, "nitrato", 73)
df$nitrato[74] <- media_mes(df, "nitrato", 74)

# amonia — abr e mai/2018 sem vizinho disponível
df$amonia[81] <- media_mes(df, "amonia", 81)
df$amonia[82] <- media_mes(df, "amonia", 82)


# verificação -------------------------------------------------------------

vars_imp <- c("ph","zon_afot","vento_24h","od","tp","tn","clor","alca",
              "nitrito","nitrato","amonia")

cat("\nNAs restantes:\n")
ok <- TRUE
for (v in vars_imp) {
  n <- sum(is.na(df[[v]]))
  if (n > 0) { cat(sprintf("  ⚠ %s: %d NA(s)\n", v, n)); ok <- FALSE }
}
if (ok) cat("  ✓ nenhum NA nas variáveis imputadas\n")
cat("  nota: NAs de nitrito/nitrato/amonia de 2018+ são esperados\n")


# export ------------------------------------------------------------------

writexl::write_xlsx(df, caminho_saida)
cat(sprintf("\nexportado: %s\n", caminho_saida))