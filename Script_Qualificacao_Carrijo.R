library(pheatmap)
library(Hmisc)
library(dplyr)
library(openxlsx)

packageVersion("pheatmap")
packageVersion("Hmisc")
packageVersion("dply")

# Importando matriz

  # Importar Matriz com tratamento de NA
  Matriz <- read.csv2("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Carrijo/Matriz_AF-Adultos - 14.01.25.csv",
                      stringsAsFactors = TRUE, na.strings = c("", "NA"))

  ## Número de observações na Matriz
  num_observacoes <- nrow(Matriz)
  num_observacoes

# Identificando o nome das variáveis
  
  ## Listando nomes das variáveis
  
  ### Todas as variáveis
  nome_todas_variaveis <- colnames(Matriz)[1:507]
  nome_todas_variaveis
  
  ### Todas as variáveis numéricas
  variaveis_numericas <- Matriz[, sapply(Matriz, is.numeric)]
  nome_variaveis_numericas <- colnames(variaveis_numericas)
  nome_variaveis_numericas
  
  ### Variáveis de certos intervalos (n-ésima até a k-ésima posição)
  nome_variaveis_intervalo <- colnames(Matriz)[c(1:6, 39:44, 211:507)]
  nome_variaveis_intervalo

# Criando DB (Data Base)

  ## Modificar as variáveis no banco de dados
  DB <- DB %>%
    mutate(
      ### Converter para quantitativas contínuas
      dplyr::across(c(
                      Idade,
                      Idade_v2_T1,
                      AF.vigorosa.sem_T1,
                      AF.moderada.sem_T1,
                      Caminhada.sem_T1,
                      total.AF.min.sem_T1,
                      PAS_final_T1,
                      PAD_final_T1,
                      IMC_T1,
                      RCQ_T1,
                      Preensao_manual_dominante_T1, 
                      Preensao_manual_n_dominante_T1, 
                      Sentar.e.levantar_T1, 
                      Flexão.de.cotovelo_T1, 
                      Agil_equil_T1_final, 
                      Caminhada.6._T1,
                      glicose_mg.dl_T1,
                      hdl_mg.dl_T1,
                      triglicerides_mg.dl_T1,
                      c.reativa_mg.L_T1,
                      colesterol.total_mg.dl_T1,
                      Cortisol_T1,
                      Ângulo.de.fase_T1,
                      GC.._T1,
                      MME.._T1,
                      Andróide.Massa.de.Gordura_T1,
                      Ginóide.Massa.de.Gordura_T1,
                      Total.Massa.de.Gordura_T1,
                      Andróide.Massa.Magra_T1,
                      Ginóide.Massa.Magra_T1,
                      Total.Massa.Magra_T1,
                      Andróide.Região..Gordura_T1,
                      Ginóide.Região..Gordura_T1,
                      Total.Região..Gordura_T1,
                      Massa.VAT_T1,
                      Volume.VAT_T1,
                      Mean_total_t1,
                      SD_total_t1,
                      Variance_total_t1,
                      RMSSD_total_t1,
                      Mean_t1,
                      SD_t1,
                      VLF.abs_t1,
                      LF.abs_t1,
                      HF.abs_t1,
                      VLF.._t1,
                      LF.._t1,
                      HF.._t1,
                      LF..nu._t1,
                      HF..nu._t1,
                      LF.HF_t1,
                      X0V.Abs_t1,
                      X1V.Abs_t1,
                      X2LV.Abs_t1,
                      X2UV.Abs_t1,
                      X0V.._t1,
                      X1V.._t1,
                      X2LV.._t1,
                      X2UV.._t1), as.numeric),
      
      ### Converter para categóricas
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
  DB$IPAQ.class_T1 <- factor(DB$IPAQ.class_T1, levels = c("LOW", "MODERATE", "HIGH"))

  # Selecionar as variáveis específicas e calcular a quantidade de observações não nulas
  quantidade_observacoes <- Matriz %>%
    select(Idade, IMC_T1, AF.vigorosa.sem_T1, Flexão.de.cotovelo_T1, glicose_mg.dl_T1, 
           Ângulo.de.fase_T1, Andróide.Massa.de.Gordura_T1, Mean_total_t1) %>%
    summarise_all(~ sum(!is.na(.)))
  
  # Visualizar as quantidades
  quantidade_observacoes
  
  ## Filtrar para manter apenas valores não faltantes
  DB <- Matriz %>%
    filter(!is.na(Idade) &
             !is.na(AF.vigorosa.sem_T1) &
             !is.na(IMC_T1) &
             !is.na(Sindrome.metabolica_T1) &
             !is.na(Ângulo.de.fase_T1) &
             !is.na(Mean_total_t1)
           )
  
  # Número de observações no DB (Matriz filtrada)
  num_observacoes_filtrado <- nrow(DB)
  num_observacoes_filtrado

  # Selecionar as variáveis específicas e calcular a quantidade de observações não nulas
  quantidade_observacoes <- DB %>%
    select(Idade, IMC_T1, AF.vigorosa.sem_T1, Flexão.de.cotovelo_T1, glicose_mg.dl_T1, 
           Ângulo.de.fase_T1, Andróide.Massa.de.Gordura_T1, Mean_total_t1) %>%
    summarise_all(~ sum(!is.na(.)))
  
  # Visualizar as quantidades
  quantidade_observacoes
  
# Tabelas descritivas 
  
  # Descritivas quantitativas
  
    # Lista das variáveis quantitativas
    variaveis_quantitativas <- c("Idade_v2_T1",
                                 "AF.vigorosa.sem_T1",
                                 "AF.moderada.sem_T1",
                                 "Caminhada.sem_T1",
                                 "total.AF.min.sem_T1",
                                 "PAS_final_T1",
                                 "PAD_final_T1",
                                 "IMC_T1",
                                 "RCQ_T1",
                                 "glicose_mg.dl_T1",
                                 "hdl_mg.dl_T1",
                                 "triglicerides_mg.dl_T1",
                                 "c.reativa_mg.L_T1",
                                 "colesterol.total_mg.dl_T1",
                                 "Ângulo.de.fase_T1",
                                 "GC.._T1",
                                 "MME.._T1",
                                 "Mean_total_t1",
                                 "SD_total_t1",
                                 "Variance_total_t1",
                                 "RMSSD_total_t1",
                                 "VLF.abs_t1",
                                 "LF.abs_t1",
                                 "HF.abs_t1",
                                 "LF..nu._t1",
                                 "HF..nu._t1",
                                 "LF.HF_t1"
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
              file = file.path("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Carrijo", 
                               paste0("Dados_descritivos_var_quant_v2", ".xlsx")))
  
  # Descritivas categóricas
    
    # Lista das variáveis categóricas
    variaveis_categoricas <- c("Sexo.biológico",
                               "IPAQ.class_T1",
                               "Sindrome.metabolica_T1"
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
    caminho <- "C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Carrijo/Dados_descritivos_var_cat_v2.xlsx"
    
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
  
# Heatmap e Dendrograma
  
  # Selecionando as variáveis de interesse
  DB_subset <- DB[, c("Idade_v2_T1",
                      "AF.vigorosa.sem_T1",
                      "AF.moderada.sem_T1",
                      "Caminhada.sem_T1",
                      "total.AF.min.sem_T1",
                      "PAS_final_T1",
                      "PAD_final_T1",
                      "IMC_T1",
                      "RCQ_T1",
                      "glicose_mg.dl_T1",
                      "hdl_mg.dl_T1",
                      "triglicerides_mg.dl_T1",
                      "c.reativa_mg.L_T1",
                      "colesterol.total_mg.dl_T1",
                      "Ângulo.de.fase_T1",
                      "GC.._T1",
                      "MME.._T1",
                      "Mean_total_t1",
                      "SD_total_t1",
                      "Variance_total_t1",
                      "RMSSD_total_t1",
                      "VLF.abs_t1",
                      "LF.abs_t1",
                      "HF.abs_t1",
                      "LF..nu._t1",
                      "HF..nu._t1",
                      "LF.HF_t1"
                      )]
  
  
  
  # Renomeando as variáveis do banco de dados
  colnames(DB_subset) <- c("Idade",
                           "AF vig",
                           "AF mod",
                           "AF caminhada",
                           "AF total",
                           "PAS",
                           "PAD",
                           "IMC",
                           "RCQ",
                           "Glic",
                           "HDL",
                           "TRI",
                           "C-r",
                           "Colesterol",
                           "Ângulo de fase",
                           "GC",
                           "MME",
                           "R-R média",
                           "R-R DP",
                           "R-R variância",
                           "RMSSD",
                           "VLF abs",
                           "LF abs",
                           "HF abs",
                           "LF nu",
                           "HF nu",
                           "LF/HF"
  )
 
# Heatmap com correlação de Pearson e Dendrograma
   
  # Calculando matriz de correlação e valores de p, ignorando NAs
  corr_results <- rcorr(as.matrix(DB_subset), type = "pearson")
  cor_matrix <- corr_results$r        # Matriz de correlação
  p_matrix <- corr_results$P          # Matriz de valores de p
  
  # Função para formatar os coeficientes de correlação
  formatted_corr <- ifelse(p_matrix < 0.05, 
                           sprintf("%.2f*", cor_matrix),  # Adiciona '*' se p < 0.05
                           sprintf("%.2f", cor_matrix))   # Apenas exibe o coeficiente
  
  # Gerando o heatmap com pheatmap
  pheatmap(cor_matrix,
           display_numbers = formatted_corr,   # Exibe coeficientes no heatmap
           clustering_method = "complete",     # Método de cluster hierárquico
           clustering_distance_rows = "euclidean", 
           clustering_distance_cols = "euclidean",
           color = colorRampPalette(c("#FEFFD9", "#41B7C4"))(10),
           fontsize_number = 10,               # Tamanho dos números
           #legend_breaks = c(-1, 0, 1),        # Legenda para -1, 0 e 1
           #legend_labels = c("-1", "0", "1"),
           fontsize = 12,
           border_color = "grey50")
  
# Heatmap com correlação de Spearman e Dendrograma  
  
  # Calculando matriz de correlação e valores de p, ignorando NAs
  corr_results <- rcorr(as.matrix(DB_subset), type = "spearman")
  cor_matrix <- corr_results$r        # Matriz de correlação
  p_matrix <- corr_results$P          # Matriz de valores de p
  
  # Função para formatar os coeficientes de correlação
  formatted_corr <- ifelse(p_matrix < 0.05, 
                           sprintf("%.2f*", cor_matrix),  # Adiciona '*' se p < 0.05
                           sprintf("%.2f", cor_matrix))   # Apenas exibe o coeficiente
  
  # Gerando o heatmap com pheatmap
  pheatmap(cor_matrix,
           display_numbers = formatted_corr,   # Exibe coeficientes no heatmap
           clustering_method = "complete",     # Método de cluster hierárquico
           clustering_distance_rows = "euclidean", 
           clustering_distance_cols = "euclidean",
           color = colorRampPalette(c("#FEFFD9", "#41B7C4"))(10),
           fontsize_number = 10,               # Tamanho dos números
           #legend_breaks = c(-1, 0, 1),        # Legenda para -1, 0 e 1
           #legend_labels = c("-1", "0", "1"),
           fontsize = 12,
           border_color = "grey50")
