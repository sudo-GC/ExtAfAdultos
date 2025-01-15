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

# Filtrar para manter apenas valores não faltantes
DB <- Matriz %>%
  filter(!is.na(IMC_class_T1) & 
           !is.na(Flexão.de.cotovelo_T1_class) & 
           !is.na(Idade) & 
           !is.na(IPAQ.class_T1) &
           !is.na(PHYSICAL.FUNCTION..0.100._T1) & 
           !is.na(Sentar.e.levantar_T1) &
           !is.na(Preensao_manual_dominante_class_T1))

# Modificar as variáveis no banco de dados
DB <- DB %>%
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



## Ordenar categorias
DB$IMC_class_T1 <- factor(DB$IMC_class_T1, levels = c("Obesidade", "Sobrepeso", "Eutrofia"))
DB$IPAQ.class_T1 <- factor(DB$IPAQ.class_T1, levels = c("LOW", "MODERATE", "HIGH"))

# Número de observações no DB (Matriz filtrada)
num_observacoes_filtrado <- nrow(DB)
print(num_observacoes_filtrado)

# Tabelas descritivas 

  # Descritivas quantitativas
  
    # Lista das variáveis quantitativas
    variaveis_quantitativas <- c("PHYSICAL.FUNCTION..0.100._T1", 
                                 "ROLE.PHYSICAL..0.100._T1", 
                                 "BODILY.PAIN..0.100._T1",
                                 "GENERAL.HEALTH..0.100._T1", 
                                 "VITALITY..0.100._T1", 
                                 "SOCIAL.FUNCTIONING..0.100._T1",
                                 "ROLE.EMOTIONAL..0.100._T1", 
                                 "MENTAL.HEALTH..0.100._T1", 
                                 "Depressão_ponts_T1",
                                 "Ansiedade_ponts_T1", 
                                 "Estresse_ponts_T1", 
                                 "Preensao_manual_dominante_T1",
                                 "Preensao_manual_n_dominante_T1", 
                                 "Sentar.e.levantar_T1", 
                                 "Flexão.de.cotovelo_T1",
                                 "Agil_equil_T1_final", 
                                 "Caminhada.6._T1"
    )
    
    # Função para calcular as medidas descritivas e formatar para 2 casas decimais
    calcular_medidas_quantitativas <- function(var) {
      n <- sum(!is.na(DB[[var]]))
      media <- round(mean(DB[[var]], na.rm = TRUE), 2)
      desvio_padrao <- round(sd(DB[[var]], na.rm = TRUE), 2)
      mediana <- round(median(DB[[var]], na.rm = TRUE), 2)
      Q1 <- round(quantile(DB[[var]], 0.25, na.rm = TRUE), 2)
      Q3 <- round(quantile(DB[[var]], 0.75, na.rm = TRUE), 2)
      erro_padrao <- desvio_padrao / sqrt(n)
      ic_inferior <- media - qt(0.975, df = n - 1) * erro_padrao
      ic_superior <- media + qt(0.975, df = n - 1) * erro_padrao
      Min <- round(min(DB[[var]], na.rm = TRUE), 2)
      Max <- round(max(DB[[var]], na.rm = TRUE), 2)
      
      return(c(n = n,
               media = media,
               desvio_padrao = desvio_padrao,
               mediana = mediana,
               Q1 = Q1, 
               Q3 = Q3, 
               ic_inferior = round(ic_inferior, 2), 
               ic_superior = round(ic_superior, 2),
               Min = Min,
               Max = Max))
    }
    
    # Gerar tabela descritiva para variáveis quantitativas
    tabela_quantitativas <- sapply(variaveis_quantitativas, 
                                   calcular_medidas_quantitativas) %>%
      t() %>%
      as.data.frame()
    
    # Ajustar nomes das colunas
    colnames(tabela_quantitativas) <- c("N", "Média", "Desvio Padrão", "Mediana", "1º Quartil", "3º Quartil", "IC 95% Inferior", "IC 95% Superior", "Min", "Max")
    
    # Adicionar coluna com os nomes das variáveis
    tabela_quantitativas$Variável <- rownames(tabela_quantitativas)
    
    # Reordenar colunas
    tabela_quantitativas <- tabela_quantitativas %>%
      select(Variável, N, Média, `Desvio Padrão`, Mediana, `1º Quartil`, `3º Quartil`, `IC 95% Inferior`, `IC 95% Superior`, `Min`, `Max`)
    
    # Exibir tabela descritiva de variáveis quantitativas
    print(tabela_quantitativas)
    
    # Salvando resultados das variáveis quantitativas em uma planilha
    write.xlsx(tabela_quantitativas,
               file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Julia Resultados exportados", 
                                paste0("Dados_descritivos_var_quant_v2", ".xlsx")))
    
  # Descritivas categóricas
  
    # Lista das variáveis categóricas
    variaveis_categoricas <- c("IPAQ.class_T1", 
                               "IMC_class_T1", 
                               "Preensao_manual_dominante_class_T1", 
                               "Preensao_manual_n_dominante_class_T1", 
                               "Sentar.e.levantar_T1_class", 
                               "Flexão.de.cotovelo_T1_class", 
                               "Agil_equil_T1_class", 
                               "Caminhada.6._T1_class"
    )
    
    # Função para calcular as frequências
    calcular_frequencias_categoricas <- function(var) {
      tabela <- DB %>%
        group_by(.data[[var]]) %>%
        summarise(N = n()) %>%
        mutate(`Frequência Absoluta` = N,
               `Frequência Relativa` = round((N / sum(N)) * 100, 2))
      
      # Calcular intervalos de confiança para proporções
      tabela <- tabela %>%
        mutate(`Frequência Absoluta` = N,
               `Frequência Relativa (%)` = round((N / sum(N)) * 100, 2),
               erro_padrao = sqrt((`Frequência Relativa (%)` / 100) * (1 - (`Frequência Relativa (%)` / 100)) / sum(N)),
               `IC Inferior 95%` = round((`Frequência Relativa (%)` / 100 - 1.96 * erro_padrao) * 100, 2),
               `IC Superior 95%` = round((`Frequência Relativa (%)` / 100 + 1.96 * erro_padrao) * 100, 2)
        )
      
      return(tabela)
    }
    
    # Gerar tabelas de frequências para cada variável categórica
    tabelas_frequencias_categoricas <- lapply(variaveis_categoricas, 
                                              calcular_frequencias_categoricas
    )
    
    # Nomear as tabelas com os nomes das variáveis
    names(tabelas_frequencias_categoricas) <- variaveis_categoricas
    
    # Criar um workbook
    wb <- createWorkbook()
    
    # Adicionar uma aba ao workbook
    addWorksheet(wb, "Frequências")
    
    # Inicializar a posição inicial para escrever as tabelas
    startRow <- 1
    
    # Escrever as tabelas no workbook
    for (var in variaveis_categoricas) {
      writeData(wb, 
                sheet = "Frequências", 
                tabelas_frequencias_categoricas[[var]], 
                startRow = startRow, 
                startCol = 1, 
                rowNames = TRUE
      )
      startRow <- startRow + nrow(tabelas_frequencias_categoricas[[var]]) + 2 # Atualizar a posição para a próxima tabela
    }
    
    # Caminho para salvar o workbook
    caminho <- "C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Julia Resultados exportados/Dados_descritivos_var_cat_v2.xlsx"
    
    # Salvar o workbook no arquivo Excel
    saveWorkbook(wb, 
                 file = caminho, 
                 overwrite = TRUE
    )
    
    # Exibir tabelas de frequências de variáveis categóricas (opcional)
    for (var in variaveis_categoricas) {
      cat("Tabela de Frequências para", var, ":\n")
      print(tabelas_frequencias_categoricas[[var]])
      cat("\n")
    }

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
