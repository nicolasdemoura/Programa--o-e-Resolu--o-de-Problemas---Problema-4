###############################################################################
# Topic: Programação e Resolução de Problemas com R
# Goal: Analisar comparativa entre IBOV e S&P500 
# Keywords: Análise de Dados, Time Series, CAPM, R
# Autor: Carolina Guimarães, Matheus Espindola, Nícolas de Moura, Michel Wachsmann
# Date: 2025-06-10
###############################################################################

------------------------------------------------------------------------------------
####Clean working envinroment and load necessary packages#### 
rm(list = ls())

# Install required packages if not already installed
my_libs <- c(
  "quantmod", "PerformanceAnalytics", "zoo", "scales", "ggplot2", "tidyr",
  "tseries", "urca", "dplyr", "psych", "xtable", "knitr", "ggcorrplot"
)
missing_libs <- my_libs[!my_libs %in% installed.packages()]
for (lib in missing_libs) install.packages(lib, dependencies = TRUE)
sapply(my_libs, require, character.only = TRUE)

# Clean up
rm(missing_libs, lib, my_libs)
gc()

----------------------------------------------------------------------------------------
####Load and process data from Yahoo Finance####
getSymbols("^GSPC", from = "1995-01-01")     # S&P 500 (USD)
getSymbols("^BVSP", from = "1995-01-01")     # Ibovespa (BRL)

ibov <- Cl(BVSP)  #get the closing prices 
sp500 <- Cl(GSPC)

# Merge time series using all available dates (holidays included)
data <- merge(sp500, ibov, all = TRUE)
colnames(data) <- c("S&P500", "Ibovespa")

# Forward-fill missing prices to handle holiday gaps
data_filled <- na.locf(data, na.rm = FALSE)
data_clean <- na.omit(data_filled)

df <- data_clean %>%
  data.frame() %>%
  tibble::rownames_to_column("date") %>%
  mutate(date = as.Date(date)) %>%
  rename(
    sp500 = S.P500,         # Adjust if col name uses `S&P500` or similar
    ibov = Ibovespa
  )

-----------------------------------------------------------------------------------------
####calculate returns####
df <- df %>%
  arrange(date) %>%
  
  # 1. Retornos simples (diários em %)
  mutate(
    ibov_ret_simple = (ibov / lag(ibov) - 1) * 100,
    sp_ret_simple   = (sp500 / lag(sp500) - 1) * 100,
    
    # 2. Retornos logarítmicos compostos (em %)
    ibov_ret_log = c(NA, diff(log(ibov)) * 100),
    sp_ret_log   = c(NA, diff(log(sp500)) * 100)
  ) %>%
  
  # 3. Remove initial NAs 
  drop_na()


----------------------------------------------------------------------------------------
#### Statistics and Correlations####
# Período
cat("🗓️  Período:", format(min(df$date)), "a", format(max(df$date)), "\n\n")
cat("\n📊 Estatísticas dos Retornos Diários\n")

# Estatísticas
df_returns <- df %>%
  dplyr::select(
    `IBOV - Retorno Simples (%)`   = ibov_ret_simple,
    `IBOV - Retorno Log (%)`       = ibov_ret_log,
    `S&P500 - Retorno Simples (%)` = sp_ret_simple,
    `S&P500 - Retorno Log (%)`     = sp_ret_log
  )

# Calcular todas as estatísticas disponíveis
estatisticas_psych <- describe(df_returns)

# Arredondar para 4 casas decimais
estatisticas_psych_round <- round(estatisticas_psych, 4)

# Gerar tabela LaTeX com xtable
tabela_latex <- xtable(estatisticas_psych_round,
                       caption = "Estatísticas Descritivas Completas dos Retornos Diários",
                       label = "tab:estatisticas_psych")

# 6. Exibir em LaTeX
print(tabela_latex, include.rownames = TRUE)

# Correlações
cor_nivel    <- cor(df$ibov, df$sp500)
cor_retornos_simples <- cor(df$ibov_ret_simple, df$sp_ret_simple)
cor_retornos_compostos <- cor(df$ibov_ret_log, df$sp_ret_log)

cat("🔗 Correlação em nível: ", round(cor_nivel, 4), "\n")
cat("🔁 Correlação nos retornos simples: ", round(cor_retornos_simples, 4), "\n")
cat("🔁 Correlação nos retornos compostos: ", round(cor_retornos_compostos, 4), "\n")

# 7. Gráficos
# Série em nível
ggplot(df, aes(x = date, y = ibov)) +
  geom_line(color = "#d62728") +
  labs(
    title = "Série em Nível: Ibovespa",
    x = "Data",
    y = "Ibovespa (Pontos)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

ggplot(df, aes(x = date, y = sp500)) +
  geom_line(color = "#1f77b4") +
  labs(title = "Série em Nível: S&P 500",
       x = "Data", y = "S&P 500 (Pontos)") +
  theme_minimal()


# Retorno simples 
ggplot(df, aes(x = date)) +
  geom_line(aes(y = ibov_ret_simple, color = "Ibovespa")) +
  geom_line(aes(y = sp_ret_simple, color = "S&P 500")) +
  labs(
    title = "Retornos Simples Diários: Ibovespa vs S&P 500",
    x = "Data",
    y = "Retorno Diário (%)",
    color = "Índice"
  ) +
  theme_minimal()


# Retorno composto 
ggplot(df, aes(x = date)) +
  geom_line(aes(y = ibov_ret_log, color = "Ibovespa")) +
  geom_line(aes(y = sp_ret_log, color = "S&P 500")) +
  labs(
    title = "Retornos Compostos (Logarítmicos) Diários: Ibovespa vs S&P 500",
    x = "Data",
    y = "Retorno Logarítmico Diário (%)",
    color = "Índice"
  ) +
  theme_minimal()



#descritivas das séries
#Tabela 1- séries em nível em nível
describe(df[, c("ibov", "sp500")]) %>% 
  kable(digits = 4, format = "latex", caption = "Estatísticas descritivas – Nível dos índices")

-----------------------------------------------------------------------------------------------------------------
####Correlation Matrix####
#matriz de correlação

matriz <- cor(df[, c("ibov", "sp500", "ibov_ret_simple", "sp_ret_simple", "ibov_ret_log", "sp_ret_log")])
ggcorrplot(matriz, lab = TRUE, type = "lower", colors = c("blue", "white", "red"))


# Janela de 90 dias (pode ajustar)
df <- df %>%
  mutate(roll_corr_level = rollapplyr(
    data = cbind(ibov, sp500),
    width = 90,
    FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
    by.column = FALSE,
    fill = NA
  ))

ggplot(df, aes(x = date, y = roll_corr_level)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Correlação Móvel (90 dias) — Níveis IBOV em USD vs S&P 500 TR",
    x = "Data", y = "Correlação"
  ) +
  theme_minimal()

df <- df %>%
  mutate(roll_corr_return = rollapplyr(
    data = cbind(ibov_ret_log, sp_ret_log),
    width = 90,
    FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
    by.column = FALSE,
    fill = NA
  ))

ggplot(df, aes(x = date, y = roll_corr_return)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Correlação Móvel (90 dias) — Retornos IBOV em USD vs S&P 500 TR",
    x = "Data", y = "Correlação"
  ) +
  theme_minimal()

df_long <- df %>%
  select(date, `Correlação em Nível` = roll_corr_level, `Correlação nos Retornos Compostos` = roll_corr_return) %>%
  pivot_longer(-date, names_to = "Tipo", values_to = "Correlacao")

ggplot(df_long, aes(x = date, y = Correlacao)) +
  geom_line() +
  facet_wrap(~Tipo, ncol = 1) +
  labs(title = "Correlação Móvel (90 dias)", x = "Data", y = "Correlação") +
  theme_minimal()
------------------------------------------------------------------------------------------------------------
####EXTRAS: retornos acumulados####
df <- df %>%
  mutate(
    ibov_cum_ret_simple = cumprod(1 + ibov_ret_simple / 100),
    sp_cum_ret_simple   = cumprod(1 + sp_ret_simple / 100)
  )
df <- df %>%
  mutate(
    log_ibov_cum_ret = log(ibov_cum_ret_simple),
    log_sp_cum_ret   = log(sp_cum_ret_simple)
  )
ggplot(df, aes(x = date)) +
  geom_line(aes(y = log_ibov_cum_ret, color = "Ibovespa")) +
  geom_line(aes(y = log_sp_cum_ret, color = "S&P 500")) +
  labs(
    title = "Log do Retorno Acumulado – Ibovespa vs S&P 500",
    x = "Data",
    y = "Log do Índice Acumulado",
    color = "Índice"
  ) +
  theme_minimal()

#CAGR
# Calcular número de anos
anos <- as.numeric(difftime(max(df$date), min(df$date), units = "days")) / 365.25

# CAGR IBOV
cagr_ibov <- (last(df$ibov_cum_ret_simple) / first(df$ibov_cum_ret_simple))^(1 / anos) - 1

# CAGR S&P 500
cagr_sp500 <- (last(df$sp_cum_ret_simple) / first(df$sp_cum_ret_simple))^(1 / anos) - 1

# Exibir resultados formatados
cat("📈 CAGR médio anual:\n")
cat("   IBOV: ", scales::percent(cagr_ibov, accuracy = 0.01), "\n")
cat("   S&P:  ", scales::percent(cagr_sp500, accuracy = 0.01), "\n")
 




