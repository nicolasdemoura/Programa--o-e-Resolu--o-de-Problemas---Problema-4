###############################################################################
# Topic: Programação e Resolução de Problemas com R
# Goal: Analisar comparativa entre IBOV e S&P500 
# Keywords: Análise de Dados, Time Series, CAPM, R
# Autor: Carolina Guimarães, Matheus Espindola, Nícolas de Moura, Michel Wachsmann
# Date: 2025-06-10
###############################################################################

###############################################################################
# Organize the working environment
###############################################################################

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

#################################################################################
# Create relevant functions 
#################################################################################

# Plot template function
gg_template_save <- function(p, filename) {
  p +
    theme_bw(base_size = 25) +
    theme(plot.margin = unit(c(5, 7, 2, 2), "mm"),
          legend.position = "bottom",
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 16),
          legend.key.size = unit(1, "cm"),
          legend.background = element_rect(color = "black", size = 0.5)) -> p2
  ggsave(filename, plot = p2, width = 16, height = 9)
}

#################################################################################
# Load the data
#################################################################################

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

#################################################################################
# Process the data
#################################################################################

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

#################################################################################
# Descriptive Statistics 
#################################################################################

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

#################################################################################
# Plot series and returns 
#################################################################################

# Série em nível
gg <- ggplot(df, aes(x = date, y = ibov)) +
  geom_line(color = "#d62728") +
  labs(
    x = "Data",
    y = "Ibovespa (Pontos)"
  ) +
  scale_y_continuous(labels = comma) 
gg_template_save(gg, filename = "figures/serie_nivel_ibov.png")

gg <- ggplot(df, aes(x = date, y = sp500)) +
  geom_line(color = "steelblue") +
  labs(
    x = "Data",
    y = "S&P 500 (Pontos)"
  )   
gg_template_save(gg, filename = "figures/serie_nivel_sp500.png")

# Retorno simples 
gg <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = ibov_ret_simple, color = "Ibovespa")) +
  geom_line(aes(y = sp_ret_simple, color = "S&P 500")) +
  labs(
    x = "Data",
    y = "Retorno Diário (%)",
    color = "Índice"
  ) +
  scale_color_manual(values = c("Ibovespa" = "#d62728", "S&P 500" = "steelblue")) 
gg_template_save(gg, filename = "figures/retornos_simples_diarios.png")

gg <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = ibov_ret_log, color = "#d62728")) +
  geom_line(aes(y = sp_ret_log, color = "S&P500")) +
  labs(
    x = "Data",
    y = "Retorno Logarítmico Diário (%)",
    color = "Índice"
  ) +
  scale_color_manual(values = c("Ibovespa" = "#d62728", "S&P 500" = "steelblue")) 
gg_template_save(gg, filename = "figures/retornos_logaritmicos_diarios.png")

# descritivas das séries
# Tabela 1- séries em nível em nível
describe(df[, c("ibov", "sp500")]) %>% 
  kable(digits = 4, format = "latex", caption = "Estatísticas descritivas – Nível dos índices")


#################################################################################
# Matriz de Correlação e Correlações Móveis 
#################################################################################

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

gg <- ggplot(df, aes(x = date, y = roll_corr_level)) +
  geom_line(color = "#eeff11") +
  labs(
    x = "Data", y = "Correlação"
  ) 
gg_template_save(gg, filename = "figures/roll_corr_level.png")


df <- df %>%
  mutate(roll_corr_return = rollapplyr(
    data = cbind(ibov_ret_log, sp_ret_log),
    width = 90,
    FUN = function(x) cor(x[,1], x[,2], use = "complete.obs"),
    by.column = FALSE,
    fill = NA
  ))

gg <- ggplot(df, aes(x = date, y = roll_corr_return)) +
  geom_line(color = "darkgreen") +
  labs(
    x = "Data", y = "Correlação"
  ) 
gg_template_save(gg, filename = "figures/roll_corr_return.png")

df_long <- df %>%
  select(date, `Correlação em Nível` = roll_corr_level, `Correlação nos Retornos Compostos` = roll_corr_return) %>%
  pivot_longer(-date, names_to = "Tipo", values_to = "Correlacao")

gg <- ggplot(df_long, aes(x = date, y = Correlacao)) +
  geom_line() +
  facet_wrap(~Tipo, ncol = 1) +
  labs(x = "Data", y = "Correlação") 
gg_template_save(gg, filename = "figures/roll_corr_comparativa.png")

#################################################################################
# Autocorrelogramas 
#################################################################################

# Autocorrelogramas dos Retornos Simples

# Ibovespa
acf_ibov <- acf(df$ibov_ret_simple, plot = FALSE)
gg <- ggplot(data.frame(lag = acf_ibov$lag, acf = acf_ibov$acf), aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "#d62728") +
  labs(x = "Lag", y = "ACF") +
  theme_minimal()
gg_template_save(gg, "figures/acf_ibov_ret_simple.png")

# S&P 500
acf_sp <- acf(df$sp_ret_simple, plot = FALSE)
gg <- ggplot(data.frame(lag = acf_sp$lag, acf = acf_sp$acf), aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Lag", y = "ACF") +
  theme_minimal()
gg_template_save(gg, "figures/acf_sp500_ret_simple.png")

#################################################################################
# Spurious Regression
#################################################################################

# Regredir IBOV no S&P 500 e plotar os resíduos
model_spurious <- lm(ibov ~ sp500, data = df)
df <- df %>%
  mutate(residuos_spurious = residuals(model_spurious))
gg <- ggplot(df, aes(x = date, y = residuos_spurious)) +
  geom_line(color = "purple") +
  labs(
    x = "Data",
    y = "Resíduos (IBOV ~ S&P 500)"
  ) 
gg_template_save(gg, filename = "figures/residuos_regressao_spurious.png")

# Regredir retornos simples do IBOV no S&P 500 e plotar os resíduos
model_spurious_ret <- lm(ibov_ret_simple ~ sp_ret_simple, data = df)
df <- df %>%
  mutate(residuos_spurious_ret = residuals(model_spurious_ret))
gg <- ggplot(df, aes(x = date, y = residuos_spurious_ret)) +
  geom_line(color = "purple") +
  labs(
    x = "Data",
    y = "Resíduos (Retornos Simples IBOV ~ Retornos Simples S&P 500)"
  )
gg_template_save(gg, filename = "figures/residuos_regressao_spurious_ret.png")

#################################################################################
# Extras
#################################################################################

### CAGR e Retornos Acumulados ####

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

gg <- ggplot(df, aes(x = date)) +
    geom_line(aes(y = log_ibov_cum_ret, color = "Ibovespa")) +
    geom_line(aes(y = log_sp_cum_ret, color = "S&P500")) +
    scale_color_manual(values = c("Ibovespa" = "#d62728", "S&P500" = "steelblue")) +
    labs(
      x = "Data",
      y = "Log do Índice Acumulado",
      color = "Índice"
    )    
gg_template_save(gg, filename = "figures/log_retorno_acumulado_ibov_sp500.png")

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
 
### Volatilidade ####

# Calcular squared returns do retorno simples
df <- df %>%
  mutate(
    ibov_sq_ret_simple = ibov_ret_simple^2,
    sp_sq_ret_simple   = sp_ret_simple^2
  )

# Plotar squared returns
gg <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = ibov_sq_ret_simple, color = "Ibovespa")) +
  geom_line(aes(y = sp_sq_ret_simple, color = "S&P 500")) +
  labs(
    x = "Data",
    y = "Retornos Simples ao Quadrado (%)",
    color = "Índice"
  ) +
  scale_color_manual(values = c("Ibovespa" = "#d62728", "S&P 500" = "steelblue"))   
gg_template_save(gg, filename = "figures/squared_returns_ibov_sp500.png")

# Plot autocorrelogramas dos squared returns
acf_ibov_sq <- acf(df$ibov_sq_ret_simple, plot = FALSE)
gg <- ggplot(data.frame(lag = acf_ibov_sq$lag, acf = acf_ibov_sq$acf), aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "#d62728") +
  labs(x = "Lag", y = "ACF")
gg_template_save(gg, "figures/acf_ibov_sq_ret_simple.png")

acf_sp_sq <- acf(df$sp_sq_ret_simple, plot = FALSE)
gg <- ggplot(data.frame(lag = acf_sp_sq$lag, acf = acf_sp_sq$acf), aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Lag", y = "ACF")
gg_template_save(gg, "figures/acf_sp500_sq_ret_simple.png")
