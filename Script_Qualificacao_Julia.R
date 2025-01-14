#################
# Análises Dança
#################

# Pré análise

# Importação de pacotes
library(ggplot2)
library(dplyr)
library(rlang)
library(ggalluvial)
library(patchwork)

# Versão do R
R.version.string

# Versão dos pacotes 
packageVersion("ggplot2")
packageVersion("dplyr")

# Função para monstar uma lista com "nome do pacote (versão do pacote)"

  ## Lista de pacotes
  packages <- c("ggplot2","dplyr")

  ## Função para obter o nome do pacote e a versão
  get_package_version <- function(pkg) {
    version <- as.character(packageVersion(pkg))
    return(paste(pkg, "(", version, ")", sep = ""))
  }

  ## Aplicar a função a cada pacote e imprimir o resultado
  versions <- sapply(packages, get_package_version)
  cat(versions, sep = "\n")

# Importar Matriz com tratamento de NA
Matriz <- read.csv2("C:/Users/Gabriel Costa/Desktop/Analises danca Julia/T1_T2_danca.csv", 
                    stringsAsFactors = TRUE, na.strings = c("", "NA"))

# Número de observações na Matriz
num_observacoes <- nrow(Matriz)
print(num_observacoes)

# Listando nomes das variáveis

  ## Todas as variáveis
  nome_todas_variaveis <- colnames(Matriz)
  nome_todas_variaveis

# Modificar as variáveis no banco de dados
DB <- Matriz %>%
  mutate(
    # Converter para quantitativas contínuas
    dplyr::across(c(Idade,
                    Capacidade_funcional_12, 
                    Limitacao_por_aspectos_fisicos_12,
                    Dor_12,
                    Estado_geral_de_saude_12,
                    Vitalidade_12,
                    Aspectos_sociais_12,
                    Aspectos_emocionais_12,
                    Saude_mental_12,
                    Capacidade_funcional_36, 
                    Limitacao_por_aspectos_fisicos_36,
                    Dor_36,
                    Estado_geral_de_saude_36,
                    Vitalidade_36,
                    Aspectos_sociais_36,
                    Aspectos_emocionais_36,
                    Saude_mental_36,
                    Depressao_ponts, 
                    Ansiedade_ponts, 
                    Estresse_ponts, 
                    AF_vigorosa.sem,
                    AF_moderada.sem,
                    Caminhada.sem,
                    total_AF_min.sem,
                    Sentado_min.sem,
                    Peso,
                    IMC,
                    Cintura,
                    Quadril,
                    RCQ,
                    Sentar_alcancar_final,
                    Maos_costas_final,
                    Sentar_e_levantar,
                    Flexao_de_cotovelo,
                    Agil_equil_final,
                    Caminhada_6.), as.numeric),
    
    # Converter para categóricas
    dplyr::across(c(Tempo,
                    ID,
                    Raca.cor_da_pele,
                    Sexo_biologico,
                    Escolaridade,
                    Ocupacao,
                    Depressao_risco,
                    Ansiedade_risco,
                    Estresse_risco,
                    IPAQ_class, 
                    IMC_class,
                    RCQ_class,
                    Sentar_alcancar_class,
                    Maos_costas_class,
                    Sentar_e_levantar_class,
                    Flexao_de_cotovelo_class,
                    Agil_equil_class,
                    Caminhada_6._class), as.factor)
  )

## Ordenar categorias
DB$IMC_class <- factor(DB$IMC_class, levels = c("Obesidade", "Sobrepeso", "Eutrofia"))
DB$IPAQ.class <- factor(DB$IPAQ_class, levels = c("LOW", "MODERATE", "HIGH"))





# CRIAR SUBSET SÓ COM T1 E FAZER A DESCRITIVA DE CARACTERIZAÇÃO




 
# Gráficos var categóricas

  # Lista das variáveis categóricas
  variaveis_categoricas <- c("Depressao_risco", "Ansiedade_risco", "Estresse_risco",
                             "IPAQ_class", "IMC_class", "RCQ_class",
                             "Sentar_alcancar_class", "Maos_costas_class",
                             "Sentar_e_levantar_class", "Flexao_de_cotovelo_class",
                             "Agil_equil_class", "Caminhada_6._class")

  # Loop para gerar gráficos para cada variável
  for (variavel in variaveis_categoricas) {
    dados_fluxo <- DB %>%
      select(ID, Tempo, !!sym(variavel)) %>%
      tidyr::pivot_wider(names_from = Tempo, values_from = !!sym(variavel))
  
    p <- ggplot(dados_fluxo,
                aes(axis1 = T1, axis2 = T2)) +
      geom_alluvium(aes(fill = T1), width = 0.25) +
      geom_stratum(width = 0.25, fill = "gray", color = "black") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c("Pré-Intervenção", "Pós-Intervenção"), expand = c(.1, .1)) +
      labs(title = paste("Fluxo da variável", variavel, "- Pré e Pós-Intervenção"),
           x = "Momento",
           y = "Número de Participantes") +
      theme_minimal() +
      theme(legend.position = "none")
  
    print(p)
  }

# Gráficos variáveis contínuas
  
  variaveis <- c("Capacidade_funcional_12", 
                 "Limitacao_por_aspectos_fisicos_12",
                 "Dor_12",
                 "Estado_geral_de_saude_12",
                 "Vitalidade_12",
                 "Aspectos_sociais_12",
                 "Aspectos_emocionais_12",
                 "Saude_mental_12",
                 "Capacidade_funcional_36", 
                 "Limitacao_por_aspectos_fisicos_36",
                 "Dor_36",
                 "Estado_geral_de_saude_36",
                 "Vitalidade_36",
                 "Aspectos_sociais_36",
                 "Aspectos_emocionais_36",
                 "Saude_mental_36",
                 "Depressao_ponts", 
                 "Ansiedade_ponts", 
                 "Estresse_ponts", 
                 "AF_vigorosa.sem", 
                 "AF_moderada.sem", 
                 "Caminhada.sem", 
                 "total_AF_min.sem", 
                 "Sentado_min.sem", 
                 "Peso", 
                 "IMC", 
                 "Cintura", 
                 "Quadril", 
                 "RCQ", 
                 "Sentar_alcancar_final", 
                 "Maos_costas_final", 
                 "Sentar_e_levantar", 
                 "Flexao_de_cotovelo", 
                 "Agil_equil_final", 
                 "Caminhada_6.")
  
  rotulos <- c("Capacidade funcional (SF-12)",
               "Limitação por aspectos físicos (SF-12)",
               "Dor (SF-12)",
               "Estado geral de saúde (SF-12)",
               "Vitalidade (SF-12)",
               "Aspectos sociais (SF-12)",
               "Aspectos emocionais (SF-12)",
               "Saúde mental (SF-12)",
               "Capacidade funcional (SF-36)",
               "Limitação por aspectos físicos (SF-36)",
               "Dor (SF-36)",
               "Estado geral de saúde (SF-36)",
               "Vitalidade (SF-36)",
               "Aspectos sociais (SF-36)",
               "Aspectos emocionais (SF-36)",
               "Saude mental (SF-36)",
               "Depressão (DASS-21)",
               "Ansiedade (DASS-21)",
               "Estresse (DASS-21)",
               "AF vigorosa (min/sem)",
               "AF moderada (min/sem)",
               "Caminhada (min/sem)",
               "Total AF min (min/sem)",
               "Tempo sentado (min/sem)",
               "Peso (kg)",
               "Índice de massa corporal",
               "Cintura (cm)",
               "Quadril (cm)",
               "Relação cintura-quadril",
               "Sentar e alcancar (cm)",
               "Mãos nas costas (cm)",
               "Sentar e levantar (rep)",
               "Flexão de cotovelo (rep)",
               "Agilidade e equilíbrio (s)",
               "Caminhada de 6min (m)")
  
  df_rotulos <- data.frame(variaveis, rotulos, stringsAsFactors = FALSE)
  
  plot_variavel <- function(var) {
    rotulo <- df_rotulos$rotulos[df_rotulos$variaveis == var]
    
    # Calcular o delta relativo para cada ID com tratamento de T1 = 0
    delta_data <- DB %>%
      filter(Tempo %in% c("T1", "T2")) %>%
      select(ID, Tempo, !!sym(var)) %>%
      spread(Tempo, !!sym(var)) %>%
      mutate(delta_relativo = case_when(
        T1 == 0 & is.na(T2) ~ NA_real_,
        T1 != 0 & is.na(T2) ~ NA_real_,
        T1 == 0 & T2 == 0 ~ 0,
        !is.na(T1) & is.na(T2) ~ NA_real_,
        TRUE ~ ((abs(T2) - abs(T1)) / T1) * 100,
      ))
    
    # Remover valores NA ou ajustar conforme necessário
    delta_data <- delta_data %>% mutate(delta_relativo = ifelse(is.na(delta_relativo), NA, delta_relativo))
    
    # Inverter a ordem dos IDs
    delta_data$ID <- factor(delta_data$ID, levels = rev(unique(delta_data$ID)))
    
    n_ids <- length(unique(DB$ID))
    cores <- scales::hue_pal()(n_ids)
    names(cores) <- unique(DB$ID)
    
    dodge <- position_dodge(width = 0.3)  # Definir a largura do deslocamento
    
    p1 <- ggplot(DB, aes(x = Tempo, y = !!sym(var), group = ID, color = ID)) +
      geom_point(size = 3, position = dodge) +
      geom_line(size = 1, position = dodge) +
      scale_color_manual(values = cores) +
      labs(title = "Mudanças Pré e Pós Intervenção",
           x = "Tempo",
           y = rotulo) +
      theme_minimal()
    
    p2 <- ggplot(delta_data, aes(y = ID, x = delta_relativo, fill = ID)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = paste0(round(delta_relativo, 0), "%")), 
                hjust = ifelse(delta_data$delta_relativo > 0, 1.1, -0.2), 
                color = "black") +
      scale_fill_manual(values = cores) +
      labs(title = "Mudança Relativa (%)",
           x = "Delta Relativo (%)",
           y = "ID") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    
    combined_plot <- p1 + p2
    return(combined_plot)
  }
  
  pasta_destino <- "C:/Users/Gabriel Costa/Desktop/Analises danca Julia/Figuras"
  
  for (var in variaveis) {
    file_name <- paste0(gsub("[^[:alnum:]_]", "_", var), ".svg")
    caminho_completo <- file.path(pasta_destino, file_name)
    
    ggsave(filename = caminho_completo,
           plot = plot_variavel(var),
           width = 10,
           height = 6,
           device = "svg")
    print(plot_variavel(var))
  }



###################
# Análises Extensão
###################

# Pré análise

# Importação de pacotes
library(rmarkdown)
library(lmtest)
library(car)
library(boot)
library(ggplot2)
library(MASS)
library(sandwich)
library(robustbase)
library(dplyr)
library(purrr)
library(broom)
library(tidyr)
library(knitr)
library(kableExtra)
library(openxlsx)
library(Hmisc)
library(performance)
library(interactions)
library(emmeans)

# Versão do R
R.version.string

# Versão dos pacotes 
packageVersion("lmtest")
packageVersion("car")
packageVersion("boot")
packageVersion("ggplot2")
packageVersion("MASS")
packageVersion("sandwich")
packageVersion("robustbase")
packageVersion("dplyr")
packageVersion("purrr")
packageVersion("broom")
packageVersion("tidyr")
packageVersion("knitr")
packageVersion("kableExtra")
packageVersion("openxlsx")
packageVersion("Hmisc")
packageVersion("performance")
packageVersion("interactions")
packageVersion("emmeans")

# Função para monstar uma lista com "nome do pacote (versão do pacote)"

## Lista de pacotes
packages <- c("lmtest", "car", "boot", "ggplot2", "MASS", "sandwich", "robustbase", "dplyr", "purrr", "broom", "tidyr", "knitr", "kableExtra", "openxlsx", "Hmisc", "performance", "interactions", "emmeans") # adicionar nomes conforme os pacotes carregados no código

## Função para obter o nome do pacote e a versão
get_package_version <- function(pkg) {
  version <- as.character(packageVersion(pkg))
  return(paste(pkg, "(", version, ")", sep = ""))
}

## Aplicar a função a cada pacote e imprimir o resultado
versions <- sapply(packages, get_package_version)
cat(versions, sep = "\n")

# Importar Matriz com tratamento de NA
Matriz <- read.csv2("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Matriz_AF-Adultos - 29.12.24.csv", 
                    stringsAsFactors = TRUE, na.strings = c("", "NA"))

# Número de observações na Matriz
num_observacoes <- nrow(Matriz)
print(num_observacoes)

# Modificar as variáveis no banco de dados
DB <- Matriz %>%
  mutate(
    # Converter para quantitativas contínuas
    dplyr::across(c(PHYSICAL.FUNCTION..0.100._T1, 
                    ROLE.PHYSICAL..0.100._T1, 
                    BODILY.PAIN..0.100._T1, 
                    GENERAL.HEALTH..0.100._T1, 
                    VITALITY..0.100._T1, 
                    SOCIAL.FUNCTIONING..0.100._T1, 
                    ROLE.EMOTIONAL..0.100._T1, 
                    MENTAL.HEALTH..0.100._T1, 
                    Depressão_ponts_T1, 
                    Ansiedade_ponts_T1, 
                    Estresse_ponts_T1, 
                    Preensao_manual_dominante_T1, 
                    Preensao_manual_n_dominante_T1, 
                    Sentar.e.levantar_T1, 
                    Flexão.de.cotovelo_T1, 
                    Agil_equil_T1_final, 
                    Caminhada.6._T1), as.numeric),
    
    # Converter para categóricas
    dplyr::across(c(IPAQ.class_T1, 
                    IMC_class_T1, 
                    Preensao_manual_dominante_class_T1, 
                    Preensao_manual_n_dominante_class_T1, 
                    Sentar.e.levantar_T1_class, 
                    Flexão.de.cotovelo_T1_class, 
                    Agil_equil_T1_class, 
                    Caminhada.6._T1_class), as.factor)
  )

# Filtrar para manter apenas valores não faltantes
DB <- Matriz %>%
  filter(!is.na(IMC_class_T1) & 
           !is.na(Flexão.de.cotovelo_T1_class) & 
           !is.na(Idade) & 
           !is.na(IPAQ.class_T1) &
           !is.na(PHYSICAL.FUNCTION..0.100._T1) & 
           !is.na(Sentar.e.levantar_T1) &
           !is.na(Preensao_manual_dominante_class_T1))

## Ordenar categorias
DB$IMC_class_T1 <- factor(DB$IMC_class_T1, levels = c("Obesidade", "Sobrepeso", "Eutrofia"))
DB$IPAQ.class_T1 <- factor(DB$IPAQ.class_T1, levels = c("LOW", "MODERATE", "HIGH"))

# Número de observações no DB (Matriz filtrada)
num_observacoes_filtrado <- nrow(DB)
print(num_observacoes_filtrado)

# Medidas descritivas

# Lista das variáveis quantitativas
variaveis_quantitativas <- c("PHYSICAL.FUNCTION..0.100._T1", "ROLE.PHYSICAL..0.100._T1", "BODILY.PAIN..0.100._T1",
                             "GENERAL.HEALTH..0.100._T1", "VITALITY..0.100._T1", "SOCIAL.FUNCTIONING..0.100._T1",
                             "ROLE.EMOTIONAL..0.100._T1", "MENTAL.HEALTH..0.100._T1", "Depressão_ponts_T1",
                             "Ansiedade_ponts_T1", "Estresse_ponts_T1", "Preensao_manual_dominante_T1",
                             "Preensao_manual_n_dominante_T1", "Sentar.e.levantar_T1", "Flexão.de.cotovelo_T1",
                             "Agil_equil_T1_final", "Caminhada.6._T1")

# Função para calcular as medidas descritivas e formatar para 2 casas decimais
calcular_medidas_quantitativas <- function(var) {
  n <- sum(!is.na(DB[[var]]))
  media <- round(mean(DB[[var]], na.rm = TRUE), 2)
  desvio_padrao <- round(sd(DB[[var]], na.rm = TRUE), 2)
  mediana <- round(median(DB[[var]], na.rm = TRUE), 2)
  Q1 <- round(quantile(DB[[var]], 0.25, na.rm = TRUE), 2)
  Q3 <- round(quantile(DB[[var]], 0.75, na.rm = TRUE), 2)
  
  return(c(n = n, media = media, desvio_padrao = desvio_padrao, mediana = mediana, Q1 = Q1, Q3 = Q3))
}

# Gerar tabela descritiva para variáveis quantitativas
tabela_quantitativas <- sapply(variaveis_quantitativas, calcular_medidas_quantitativas) %>%
  t() %>%
  as.data.frame()

# Ajustar nomes das colunas
colnames(tabela_quantitativas) <- c("N", "Média", "Desvio Padrão", "Mediana", "1º Quartil", "3º Quartil")

# Adicionar coluna com os nomes das variáveis
tabela_quantitativas$Variável <- rownames(tabela_quantitativas)

# Reordenar colunas
tabela_quantitativas <- tabela_quantitativas %>%
  select(Variável, N, Média, `Desvio Padrão`, Mediana, `1º Quartil`, `3º Quartil`)

# Exibir tabela descritiva de variáveis quantitativas
print(tabela_quantitativas)

# Lista das variáveis categóricas
variaveis_categoricas <- c("IPAQ.class_T1", "IMC_class_T1", "Preensao_manual_dominante_class_T1", 
                           "Preensao_manual_n_dominante_class_T1", "Sentar.e.levantar_T1_class", 
                           "Flexão.de.cotovelo_T1_class", "Agil_equil_T1_class", "Caminhada.6._T1_class")

# Função para calcular as frequências
calcular_frequencias_categoricas <- function(var) {
  tabela <- DB %>%
    group_by(.data[[var]]) %>%
    summarise(N = n()) %>%
    mutate(`Frequência Absoluta` = N,
           `Frequência Relativa` = round((N / sum(N)) * 100, 2))
  
  return(tabela)
}

# Gerar tabelas de frequências para cada variável categórica
tabelas_frequencias_categoricas <- lapply(variaveis_categoricas, calcular_frequencias_categoricas)

# Nomear as tabelas com os nomes das variáveis
names(tabelas_frequencias_categoricas) <- variaveis_categoricas

# Exibir tabelas de frequências de variáveis categóricas
for (var in variaveis_categoricas) {
  cat("Tabela de Frequências para", var, ":\n")
  print(tabelas_frequencias_categoricas[[var]])
  cat("\n")
}

# Definir as variáveis quantitativas e categóricas
quant_vars <- c("PHYSICAL.FUNCTION..0.100._T1", "ROLE.PHYSICAL..0.100._T1", "BODILY.PAIN..0.100._T1", 
                "GENERAL.HEALTH..0.100._T1", "VITALITY..0.100._T1", "SOCIAL.FUNCTIONING..0.100._T1", 
                "ROLE.EMOTIONAL..0.100._T1", "MENTAL.HEALTH..0.100._T1", "Depressão_ponts_T1", 
                "Ansiedade_ponts_T1", "Estresse_ponts_T1", "Preensao_manual_dominante_T1", 
                "Preensao_manual_n_dominante_T1", "Sentar.e.levantar_T1", "Flexão.de.cotovelo_T1", 
                "Agil_equil_T1_final", "Caminhada.6._T1")

cat_vars <- c("IPAQ.class_T1", "IMC_class_T1", "Preensao_manual_dominante_class_T1", 
              "Preensao_manual_n_dominante_class_T1", "Sentar.e.levantar_T1_class", 
              "Flexão.de.cotovelo_T1_class", "Agil_equil_T1_class", "Caminhada.6._T1_class")

# Medidas descritivas para variáveis quantitativas
descriptive_quant <- DB %>%
  select(all_of(quant_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  reframe(
    N = sum(!is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Medidas descritivas para variáveis categóricas
descriptive_cat <- DB %>%
  select(all_of(cat_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Value) %>%
  summarise(
    Count = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    Total = sum(Count),
    Percent = round(Count / Total * 100, 2)
  )

# Exportar para uma planilha Excel
wb <- createWorkbook()
addWorksheet(wb, "Quantitative")
writeData(wb, "Quantitative", descriptive_quant)
addWorksheet(wb, "Categorical")
writeData(wb, "Categorical", descriptive_cat)
saveWorkbook(wb, "C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Descriptive_Statistics.xlsx", overwrite = TRUE)


# Comparação de distribuições

# Comparando a distribuição da "Physical function" bruta e via bootstrap

# Bootstrap 5.000

## Função para realizar o bootstrap não paramétrico (5.000)
bootstrap_function_1 <- function(data, indices) {
  ## Subamostra com reamostragens
  data_bootstrap_1 <- data[indices, ]
  
  ## Exemplo usando a média para reamostragem
  return(mean(data_bootstrap_1$PHYSICAL.FUNCTION..0.100._T1)) 
}

## Realizando o bootstrap não paramétrico
set.seed(123)
bootstrap_results_1 <- boot(data = DB, statistic = bootstrap_function_1, R = 5000)

## Estimativas do bootstrap (média das amostras reamostradas)
bootstrap_estimates_1 <- bootstrap_results_1$t

#Q-Q plot

## Distribuição antes do bootstrap - Q-Q Plot original
qqnorm(DB$PHYSICAL.FUNCTION..0.100._T1, main = "Q-Q Plot Original", 
       ylab = "Quantis da amostra",
       xlab = "Quantis teóricos")
qqline(DB$PHYSICAL.FUNCTION..0.100._T1, col = "red")


## Q-Q Plot após o bootstrap - usando as estimativas do bootstrap (5.000)
qqnorm(bootstrap_estimates_1, main = "Q-Q Plot após Bootstrap (5.000)",
       ylab = "Quantis da amostra",
       xlab = "Quantis teóricos")
qqline(bootstrap_estimates_1, col = "blue")

# HIstograma

## Histograma variável original

histograma_PHYSICAL_original <- hist(DB$PHYSICAL.FUNCTION..0.100._T1,
                                     prob=TRUE,
                                     col="lightblue",
                                     xlab="Função física (original)",
                                     ylab = "Densidade",
                                     main="Histograma com curva de densidade")
curva_densidade_PHYSICAL_original <- lines(density(DB$PHYSICAL.FUNCTION..0.100._T1),
                                           col="red",
                                           lwd=2)

## Histograma do bootstrap

histograma_PHYSICAL_bootstrap <- hist(bootstrap_estimates_1,
                                     prob=TRUE,
                                     col="lightblue",
                                     xlab="Função física (bootstrap)",
                                     ylab = "Densidade",
                                     main="Histograma com curva de densidade")
curva_densidade_PHYSICAL_bootstrap <- lines(density(bootstrap_estimates_1),
                                           col="red",
                                           lwd=2)

# Comparando a distribuição da "Depressão points" bruta e via bootstrap

# Bootstrap 5.000

## Função para realizar o bootstrap não paramétrico (5.000)
bootstrap_function_2 <- function(data, indices) {
  ## Subamostra com reamostragens
  data_bootstrap_2 <- data[indices, ]
  
  ## Exemplo usando a média para reamostragem
  return(mean(data_bootstrap_2$Depressão_ponts_T1)) 
}

## Realizando o bootstrap não paramétrico
set.seed(123)
bootstrap_results_2 <- boot(data = DB, statistic = bootstrap_function_2, R = 5000)

## Estimativas do bootstrap (média das amostras reamostradas)
bootstrap_estimates_2 <- bootstrap_results_2$t

#Q-Q plot

## Distribuição antes do bootstrap - Q-Q Plot original
qqnorm(DB$Depressão_ponts_T1, main = "Q-Q Plot Original",
       ylab = "Quantis da amostra",
       xlab = "Quantis teóricos")
qqline(DB$Depressão_ponts_T1, col = "red")

## Q-Q Plot após o bootstrap - usando as estimativas do bootstrap (5.000)
qqnorm(bootstrap_estimates_2, main = "Q-Q Plot após Bootstrap (5.000)",
       ylab = "Quantis da amostra",
       xlab = "Quantis teóricos")
qqline(bootstrap_estimates_2, col = "blue")

# HIstograma

## Histograma variável original

histograma_PHYSICAL_original <- hist(DB$Depressão_ponts_T1,
                                     prob=TRUE,
                                     col="lightblue",
                                     xlab="Depressão (original)",
                                     ylab = "Densidade",
                                     main="Histograma com curva de densidade")
curva_densidade_PHYSICAL_original <- lines(density(DB$Depressão_ponts_T1),
                                           col="red",
                                           lwd=2)

## Histograma do bootstrap

histograma_PHYSICAL_bootstrap <- hist(bootstrap_estimates_2,
                                      prob=TRUE,
                                      col="lightblue",
                                      xlab="Depressão (bootstrap)",
                                      ylab = "Densidade",
                                      main="Histograma com curva de densidade")
curva_densidade_PHYSICAL_bootstrap <- lines(density(bootstrap_estimates_2),
                                            col="red",
                                            lwd=2)

# Modelos univariados

# PHYSICAL FUNCTION

# Variável dependente
var_dep <- DB$PHYSICAL.FUNCTION..0.100._T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1",
  Preensao_manual_dominante_class_T1 = "Preensao_manual_dominante_class_T1",
  Preensao_manual_n_dominante_class_T1 = "Preensao_manual_n_dominante_class_T1",
  Sentar_e_levantar_T1_class = "Sentar.e.levantar_T1_class",
  Flexao_de_cotovelo_T1_class = "Flexão.de.cotovelo_T1_class",
  Agil_equil_T1_class = "Agil_equil_T1_class",
  Caminhada_6_T1_class = "Caminhada.6._T1_class"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("PHYSICAL.FUNCTION..0.100._T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)

  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("Funcao_fisica_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("Funcao_fisica_emmeans_", nome, ".csv")))

  }

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### ROLE PHYSICAL


# Variável dependente
var_dep <- DB$ROLE.PHYSICAL..0.100._T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1",
  Preensao_manual_dominante_class_T1 = "Preensao_manual_dominante_class_T1",
  Preensao_manual_n_dominante_class_T1 = "Preensao_manual_n_dominante_class_T1",
  Sentar_e_levantar_T1_class = "Sentar.e.levantar_T1_class",
  Flexao_de_cotovelo_T1_class = "Flexão.de.cotovelo_T1_class"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("ROLE.PHYSICAL..0.100._T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)

# Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("ROLE_PHYSICAL_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("ROLE_PHYSICAL_emmeans_", nome, ".csv")))

  }

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### BODILY PAIN

# Variável dependente
var_dep <- DB$BODILY.PAIN..0.100._T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1",
  Preensao_manual_dominante_class_T1 = "Preensao_manual_dominante_class_T1",
  Preensao_manual_n_dominante_class_T1 = "Preensao_manual_n_dominante_class_T1",
  Sentar_e_levantar_T1_class = "Sentar.e.levantar_T1_class",
  Flexao_de_cotovelo_T1_class = "Flexão.de.cotovelo_T1_class"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("BODILY.PAIN..0.100._T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)

  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("BODILY_PAIN_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("BODILY_PAIN_emmeans_", nome, ".csv")))

  }

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### GENERAL HEALTH

# Variável dependente
var_dep <- DB$GENERAL.HEALTH..0.100._T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1",
  Preensao_manual_dominante_class_T1 = "Preensao_manual_dominante_class_T1",
  Preensao_manual_n_dominante_class_T1 = "Preensao_manual_n_dominante_class_T1",
  Sentar_e_levantar_T1_class = "Sentar.e.levantar_T1_class",
  Flexao_de_cotovelo_T1_class = "Flexão.de.cotovelo_T1_class"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("GENERAL.HEALTH..0.100._T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  check_model(modelo) # Análise de resíduos

  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("GENERAL_HEALTH_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("GENERAL_HEALTH_emmeans_", nome, ".csv")))
  
  }

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### VITALITY

# Variável dependente
var_dep <- DB$VITALITY..0.100._T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1",
  Preensao_manual_dominante_class_T1 = "Preensao_manual_dominante_class_T1",
  Preensao_manual_n_dominante_class_T1 = "Preensao_manual_n_dominante_class_T1",
  Sentar_e_levantar_T1_class = "Sentar.e.levantar_T1_class",
  Flexao_de_cotovelo_T1_class = "Flexão.de.cotovelo_T1_class"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("VITALITY..0.100._T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)

  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("VITALITY_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("VITALITY_emmeans_", nome, ".csv")))

  }

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### SOCIAL FUNCTIONING

# Variável dependente
var_dep <- DB$SOCIAL.FUNCTIONING..0.100._T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1",
  Preensao_manual_dominante_class_T1 = "Preensao_manual_dominante_class_T1",
  Preensao_manual_n_dominante_class_T1 = "Preensao_manual_n_dominante_class_T1",
  Sentar_e_levantar_T1_class = "Sentar.e.levantar_T1_class",
  Flexao_de_cotovelo_T1_class = "Flexão.de.cotovelo_T1_class"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("SOCIAL.FUNCTIONING..0.100._T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)

  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("SOCIAL_FUNCTIONING_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("SOCIAL_FUNCTIONING_emmeans_", nome, ".csv")))
  
  }

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### ROLE EMOTIONAL

# Variável dependente
var_dep <- DB$ROLE.EMOTIONAL..0.100._T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1",
  Preensao_manual_dominante_class_T1 = "Preensao_manual_dominante_class_T1",
  Preensao_manual_n_dominante_class_T1 = "Preensao_manual_n_dominante_class_T1",
  Sentar_e_levantar_T1_class = "Sentar.e.levantar_T1_class",
  Flexao_de_cotovelo_T1_class = "Flexão.de.cotovelo_T1_class"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("ROLE.EMOTIONAL..0.100._T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  check_model(modelo) # Análise de resíduos

  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("ROLE_EMOTIONAL_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("ROLE_EMOTIONAL_emmeans_", nome, ".csv")))
  
  }

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### MENTAL HEALTH


# Variável dependente
var_dep <- DB$MENTAL.HEALTH..0.100._T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1",
  Preensao_manual_dominante_class_T1 = "Preensao_manual_dominante_class_T1",
  Preensao_manual_n_dominante_class_T1 = "Preensao_manual_n_dominante_class_T1",
  Sentar_e_levantar_T1_class = "Sentar.e.levantar_T1_class",
  Flexao_de_cotovelo_T1_class = "Flexão.de.cotovelo_T1_class"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("MENTAL.HEALTH..0.100._T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("MENTAL_HEALTH_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("MENTAL_HEALTH_emmeans_", nome, ".csv")))
  
  }

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### Depressão

# Variável dependente
var_dep <- DB$Depressão_ponts_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("Depressão_ponts_T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  print(resultados)
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("Depressao_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("Depressao_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### Ansiedade

# Variável dependente
var_dep <- DB$Ansiedade_ponts_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("Ansiedade_ponts_T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  print(resultados)
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("Ansiedade_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("Ansiedade_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


### Estresse

# Variável dependente
var_dep <- DB$Estresse_ponts_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IMC_class_T1 = "IMC_class_T1",
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1",
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind, nome) {
  equacao <- as.formula(paste("Estresse_ponts_T1 ~", var_ind))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  resultados
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("Estresse_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_var_ind <- emmeans(modelo, as.formula(paste("~", var_ind)))
  print(emmeans_var_ind)
  
  # Salvando resultados do emmeans em uma planilha
  emmeans_interaction_df <- as.data.frame(emmeans_var_ind)
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos univariados", paste0("Estresse_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos(modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos(modelos_cont[[i]], paste0("modelo_cont_", i))
}


# Modelos multivariados


### PHYSICAL FUNCTION

# Variável dependente
var_dep <- DB$PHYSICAL.FUNCTION..0.100._T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("PHYSICAL.FUNCTION..0.100._T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("PHYSICAL_FUNCTION_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("PHYSICAL_FUNCTION_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### ROLE.PHYSICAL..0.100._T1

# Variável dependente
var_dep <- DB$ROLE.PHYSICAL..0.100._T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("ROLE.PHYSICAL..0.100._T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("ROLE_PHYSICAL_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("ROLE_PHYSICAL_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### BODILY PAIN

# Variável dependente
var_dep <- DB$BODILY.PAIN..0.100._T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("BODILY.PAIN..0.100._T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("BODILY_PAIN_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("BODILY_PAIN_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### GENERAL HEALTH

# Variável dependente
var_dep <- DB$GENERAL.HEALTH..0.100._T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("GENERAL.HEALTH..0.100._T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("GENERAL_HEALTH_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("GENERAL_HEALTH_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### VITALITY

# Variável dependente
var_dep <- DB$VITALITY..0.100._T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("VITALITY..0.100._T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("VITALITY_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("VITALITY_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### SOCIAL FUNCTIONING

# Variável dependente
var_dep <- DB$SOCIAL.FUNCTIONING..0.100._T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("SOCIAL.FUNCTIONING..0.100._T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("SOCIAL_FUNCTIONING_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("SOCIAL_FUNCTIONING_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### ROLE_EMOTIONAL

# Variável dependente
var_dep <- DB$ROLE.EMOTIONAL..0.100._T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("ROLE.EMOTIONAL..0.100._T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("ROLE_EMOTIONAL_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("ROLE_EMOTIONAL_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### MENTAL HEALTH

# Variável dependente
var_dep <- DB$MENTAL.HEALTH..0.100._T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("MENTAL.HEALTH..0.100._T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("MENTAL_HEALTH_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("MENTAL_HEALTH_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}



### Depressao

# Variável dependente
var_dep <- DB$Depressão_ponts_T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("Depressão_ponts_T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("Depressao_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("Depressao_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### Ansiedade

# Variável dependente
var_dep <- DB$Ansiedade_ponts_T1

var_ind_1 <- DB$IMC_class_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("Ansiedade_ponts_T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("Ansiedade_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("Ansiedade_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}


### Estresse

# Variável dependente
var_dep <- DB$Estresse_ponts_T1

var_ind_1 <- DB$Estresse_ponts_T1

# Lista de modelos com variáveis categóricas
modelos_cat <- list(
  IPAQ.class_T1 = "IPAQ.class_T1"
)

# Lista de modelos com variáveis contínuas
modelos_cont <- list(
  Preensao_manual_D_T1_final = "Preensao_manual_dominante_T1",
  Preensao_manual_E_T1_final = "Preensao_manual_n_dominante_T1", # não esquecer de adicionar vírgula
  Sentar_e_levantar_T1 = "Sentar.e.levantar_T1",
  Flexao_de_cotovelo_T1 = "Flexão.de.cotovelo_T1",
  Agil_equil_T1_final = "Agil_equil_T1_final",
  Caminhada_6_T1 = "Caminhada.6._T1"
)

# Função para ajustar modelos e salvar resultados
ajustar_modelos <- function(var_ind_1, var_ind_2, nome) {
  equacao <- as.formula(paste("Depressão_ponts_T1 ~", var_ind_1, "*", var_ind_2))
  
  # Modelo linear
  modelo <- lm(equacao, data = DB)
  
  # Bootstrap
  modelo_boot <- function(data, indices) {
    d <- data[indices,]
    fit <- lm(equacao, data = d)
    return(coef(fit))
  }
  
  resultado_boot <- boot(data = DB, statistic = modelo_boot, R = 5000)
  
  # Estimativas e ICs
  coef_est <- colMeans(resultado_boot$t, na.rm = TRUE)
  conf_int <- apply(resultado_boot$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  p_vals <- summary(modelo)$coefficients[,4]
  
  # Resultados estruturados
  resultados <- data.frame(
    Coeficientes = coef_est,
    "IC 2.5%" = conf_int[1,],
    "IC 97.5%" = conf_int[2,],
    "p-valor" = p_vals
  )
  
  write.csv(resultados, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("Estresse_coeficientes_", nome, ".csv")))
  
  # Marginal Means
  emmeans_interaction <- emmeans(modelo, as.formula(paste("~", var_ind_1, "*", var_ind_2)))
  emmeans_interaction_df <- as.data.frame(emmeans_interaction)
  emmeans_interaction_df$IC_inf <- emmeans_interaction_df$emmean - qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  emmeans_interaction_df$IC_sup <- emmeans_interaction_df$emmean + qt(0.975, emmeans_interaction_df$df) * emmeans_interaction_df$SE
  
  # Salvando resultados do emmeans em uma planilha
  write.csv(emmeans_interaction_df, file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Resultados exportados/Modelos multivariados", paste0("Estresse_emmeans_", nome, ".csv")))
  
}

# Ajustando modelos categóricos
for (i in seq_along(modelos_cat)) {
  ajustar_modelos("IMC_class_T1", modelos_cat[[i]], paste0("modelo_cat_", i))
}

# Ajustando modelos contínuos
for (i in seq_along(modelos_cont)) {
  ajustar_modelos("IMC_class_T1", modelos_cont[[i]], paste0("modelo_cont_", i))
}

