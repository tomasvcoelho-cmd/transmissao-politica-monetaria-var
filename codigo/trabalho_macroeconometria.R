# =============================================================================
# TRANSMISSÃO DA POLÍTICA MONETÁRIA: UMA ANÁLISE COM MODELO VAR
# Tomás Valença Coelho
# UFRJ - Instituto de Economia
#
# Versão organizada para reprodutibilidade e disponibilização no GitHub.
# A lógica econométrica e os resultados do trabalho original foram preservados.
# =============================================================================


# 1. Pacotes -----------------------------------------------------------------

library(readxl)
library(magrittr)
library(dplyr)
library(forecast)
library(ggplot2)
library(patchwork)
library(urca)
library(vars)


# 2. Tratamento dos dados -----------------------------------------------------

# Para reprodução no GitHub, coloque a base na pasta "dados/".
dados <- read_excel("dados/ipca_selic_pib.xlsx")

dados$Data <- dados$Data %>%
  as.Date()

# Observação:
# A coluna IPCA da planilha corresponde ao índice de preços construído
# a partir da série 433 do SGS/BCB por meio de mudança de base.

ipca_ts <- dados %>%
  select(IPCA) %>%
  ts(., frequency = 12, start = c(2014, 1)) %>%
  window(., start = c(2014, 1), end = c(2024, 12))

selic_ts <- dados %>%
  select(SELIC) %>%
  ts(., frequency = 12, start = c(2014, 1)) %>%
  window(., start = c(2014, 1), end = c(2024, 12))

# Câmbio e IBC-Br são transformados em log antes dos testes e diferenças.
cambio_ts <- dados %>%
  select(CAMBIO) %>%
  ts(., frequency = 12, start = c(2014, 1)) %>%
  window(., start = c(2014, 1), end = c(2024, 12)) %>%
  log()

ibc_ts <- dados %>%
  select(IBCBR) %>%
  ts(., frequency = 12, start = c(2014, 1)) %>%
  window(., start = c(2014, 1), end = c(2024, 12)) %>%
  log()

# Primeiras diferenças
diff_ipca <- diff(ipca_ts, differences = 1)
diff_selic <- diff(selic_ts, differences = 1)
diff_cambio <- diff(cambio_ts, differences = 1)
diff_ibc <- diff(ibc_ts, differences = 1)


# 3. Análise gráfica ----------------------------------------------------------

# IPCA em nível
g_ipca <- autoplot(ipca_ts, colour = "darkred", size = 0.8) +
  labs(title = "IPCA - nível") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# IPCA na diferença
g_d_ipca <- autoplot(diff_ipca, colour = "darkred", size = 0.8) +
  labs(title = "IPCA - diferença") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Selic em nível
g_selic <- autoplot(selic_ts, colour = "darkblue", size = 0.8) +
  labs(title = "SELIC - nível") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Selic na diferença
g_d_selic <- autoplot(diff_selic, colour = "darkblue", size = 0.8) +
  labs(title = "SELIC - diferença") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# IBC-Br em nível
g_ibc <- autoplot(ibc_ts, colour = "orange", size = 0.8) +
  labs(title = "IBC-Br (log) - nível") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# IBC-Br na diferença
g_d_ibc <- autoplot(diff_ibc, colour = "orange", size = 0.8) +
  labs(title = "IBC-Br (log) - diferença") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Câmbio em nível
g_cambio <- autoplot(cambio_ts, colour = "darkgreen", size = 0.8) +
  labs(title = "Câmbio (log) - nível") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Câmbio na diferença
g_d_cambio <- autoplot(diff_cambio, colour = "darkgreen", size = 0.8) +
  labs(title = "Câmbio (log) - diferença") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

painel_nivel <- (g_ipca | g_selic) /
  (g_ibc | g_cambio)

painel_diferenca <- (g_d_ipca | g_d_selic) /
  (g_d_ibc | g_d_cambio)

painel_nivel
painel_diferenca


# 4. Testes de raiz unitária --------------------------------------------------

# IPCA em nível
summary(ur.df(ipca_ts, type = "none", selectlags = "AIC"))
summary(ur.df(ipca_ts, type = "drift", selectlags = "AIC"))
summary(ur.df(ipca_ts, type = "trend", selectlags = "AIC"))
summary(ur.za(ipca_ts, model = "both"))

# IPCA na primeira diferença
summary(ur.df(diff_ipca, type = "none", selectlags = "AIC"))
summary(ur.df(diff_ipca, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_ipca, type = "trend", selectlags = "AIC"))
summary(ur.za(diff_ipca, model = "both"))

# Selic em nível
summary(ur.df(selic_ts, type = "none", selectlags = "AIC"))
summary(ur.df(selic_ts, type = "drift", selectlags = "AIC"))
summary(ur.df(selic_ts, type = "trend", selectlags = "AIC"))
summary(ur.za(selic_ts, model = "both"))

# Selic na primeira diferença
summary(ur.df(diff_selic, type = "none", selectlags = "AIC"))
summary(ur.df(diff_selic, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_selic, type = "trend", selectlags = "AIC"))
summary(ur.za(diff_selic, model = "both"))

# Selic na segunda diferença
diff2_selic <- diff(diff_selic)

summary(ur.df(diff2_selic, type = "none", selectlags = "AIC"))
summary(ur.df(diff2_selic, type = "drift", selectlags = "AIC"))
summary(ur.df(diff2_selic, type = "trend", selectlags = "AIC"))
summary(ur.za(diff2_selic, model = "both"))

# IBC-Br em nível
summary(ur.df(ibc_ts, type = "none", selectlags = "AIC"))
summary(ur.df(ibc_ts, type = "drift", selectlags = "AIC"))
summary(ur.df(ibc_ts, type = "trend", selectlags = "AIC"))
summary(ur.za(ibc_ts, model = "both"))

# IBC-Br na primeira diferença
summary(ur.df(diff_ibc, type = "none", selectlags = "AIC"))
summary(ur.df(diff_ibc, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_ibc, type = "trend", selectlags = "AIC"))
summary(ur.za(diff_ibc, model = "both"))

# Câmbio em nível
summary(ur.df(cambio_ts, type = "none", selectlags = "AIC"))
summary(ur.df(cambio_ts, type = "drift", selectlags = "AIC"))
summary(ur.df(cambio_ts, type = "trend", selectlags = "AIC"))
summary(ur.za(cambio_ts, model = "both"))

# Câmbio na primeira diferença
summary(ur.df(diff_cambio, type = "none", selectlags = "AIC"))
summary(ur.df(diff_cambio, type = "drift", selectlags = "AIC"))
summary(ur.df(diff_cambio, type = "trend", selectlags = "AIC"))
summary(ur.za(diff_cambio, model = "both"))


# 5. Teste de cointegração - Engle-Granger ----------------------------------

df_eg <- data.frame(
  inflacao = as.numeric(ipca_ts)[-1],
  juros = as.numeric(diff_selic),
  produto = as.numeric(ibc_ts)[-1],
  cambio = as.numeric(cambio_ts)[-1]
)

modelo_eg <- lm(inflacao ~ juros + produto + cambio, data = df_eg)
summary(modelo_eg)

erro_eg <- resid(modelo_eg)
plot(erro_eg)

eg <- ur.df(erro_eg, type = "drift")
summary(eg)


# 6. Construção das dummies ---------------------------------------------------

tempo <- time(ipca_ts)
anos <- floor(tempo)
meses <- round(12 * (tempo - anos) + 1)

dummy <- ifelse(
  ((anos == 2020 & meses >= 3) |
     (anos == 2021 & meses <= 3) |
     (anos == 2015 & meses == 10)),
  1,
  0
)

# Mantém o mesmo alinhamento temporal utilizado no código original.
dummy_ts <- ts(dummy[-1], frequency = 12, start = c(2014, 1))


# 7. Modelo VAR ---------------------------------------------------------------

# Variáveis estacionárias utilizadas no modelo:
# Δlog(Câmbio), Δlog(IBC-Br), Δ²Selic e ΔIPCA.
matriz_var <- cbind(
  diff_cambio[-1],
  diff_ibc[-1],
  diff2_selic,
  diff_ipca[-1]
)

# Seleção da defasagem
lags <- VARselect(matriz_var, lag.max = 6)
lags$selection

# Estimação do VAR(2), preservando a especificação do trabalho original.
modelo_var <- VAR(
  matriz_var,
  type = "none",
  p = 2,
  exogen = matrix(dummy_ts[-1])
)

# Diagnósticos
serial.test(modelo_var, lags.pt = 9, type = "PT.adjusted")
arch.test(modelo_var)

# Resultados
summary(modelo_var)


# 8. Funções impulso-resposta -------------------------------------------------

# As funções abaixo são não ortogonalizadas (ortho = FALSE),
# conforme a especificação utilizada no trabalho original.

ir_selic_ipca <- irf(
  modelo_var,
  impulse = "diff2_selic",
  response = "diff_ipca..1.",
  ortho = FALSE
)

ir_selic_ipca_cum <- irf(
  modelo_var,
  impulse = "diff2_selic",
  response = "diff_ipca..1.",
  ortho = FALSE,
  cumulative = TRUE
)

ir_selic_ibc <- irf(
  modelo_var,
  impulse = "diff2_selic",
  response = "diff_ibc..1.",
  ortho = FALSE
)

ir_selic_ibc_cum <- irf(
  modelo_var,
  impulse = "diff2_selic",
  response = "diff_ibc..1.",
  ortho = FALSE,
  cumulative = TRUE
)

ir_selic_cambio <- irf(
  modelo_var,
  impulse = "diff2_selic",
  response = "diff_cambio..1.",
  ortho = FALSE
)

ir_selic_cambio_cum <- irf(
  modelo_var,
  impulse = "diff2_selic",
  response = "diff_cambio..1.",
  ortho = FALSE,
  cumulative = TRUE
)

ir_ibc_ipca <- irf(
  modelo_var,
  impulse = "diff_ibc..1.",
  response = "diff_ipca..1.",
  ortho = FALSE
)

ir_ibc_ipca_cum <- irf(
  modelo_var,
  impulse = "diff_ibc..1.",
  response = "diff_ipca..1.",
  ortho = FALSE,
  cumulative = TRUE
)

ir_cambio_ipca <- irf(
  modelo_var,
  impulse = "diff_cambio..1.",
  response = "diff_ipca..1.",
  ortho = FALSE
)

ir_cambio_ipca_cum <- irf(
  modelo_var,
  impulse = "diff_cambio..1.",
  response = "diff_ipca..1.",
  ortho = FALSE,
  cumulative = TRUE
)

# Gráficos
plot(ir_selic_ipca)
plot(ir_selic_ipca_cum)

plot(ir_selic_ibc)
plot(ir_selic_ibc_cum)

plot(ir_selic_cambio)
plot(ir_selic_cambio_cum)

plot(ir_ibc_ipca)
plot(ir_ibc_ipca_cum)

plot(ir_cambio_ipca)
plot(ir_cambio_ipca_cum)
