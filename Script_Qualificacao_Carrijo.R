library(pheatmap)
library(Hmisc)
library(dplyr)

packageVersion("pheatmap")
packageVersion("Hmisc")
packageVersion("dply")

# Importando matriz

  ## Importar Matriz com tratamento de NA
  Matriz <- read.csv2("C:/Users/Gabriel Costa/Desktop/USP/GEPEFSC/Projeto - Atividade Fisica para Adultos/Analise de Dados/R/Matriz_AF-Adultos - 08.01.25.csv",
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
  DB <- Matriz %>%
    mutate(
      ### Converter para quantitativas contínuas
      dplyr::across(c(
                      Idade,
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
             !is.na(IMC_T1) &
             !is.na(AF.vigorosa.sem_T1) &
             !is.na(IMC_T1) &
             !is.na(Flexão.de.cotovelo_T1) &
             !is.na(glicose_mg.dl_T1) &
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
  
# Selecionando as variáveis de interesse
DB_subset <- DB[, c("Idade",
                    "AF.vigorosa.sem_T1",
                    "AF.moderada.sem_T1",
                    "Caminhada.sem_T1",
                    "total.AF.min.sem_T1",
                    "PAS_final_T1",
                    "PAD_final_T1",
                    "IMC_T1",
                    "RCQ_T1",
                    "Preensao_manual_dominante_T1",
                    "Preensao_manual_n_dominante_T1",
                    "Sentar.e.levantar_T1",
                    "Flexão.de.cotovelo_T1",
                    "Agil_equil_T1_final",
                    "Caminhada.6._T1",
                    "glicose_mg.dl_T1",
                    "hdl_mg.dl_T1",
                    "triglicerides_mg.dl_T1",
                    "c.reativa_mg.L_T1",
                    "colesterol.total_mg.dl_T1",
                    "Cortisol_T1",
                    "Ângulo.de.fase_T1",
                    "Andróide.Massa.de.Gordura_T1",
                    "Ginóide.Massa.de.Gordura_T1",
                    "Total.Massa.de.Gordura_T1",
                    "Total.Massa.Magra_T1",
                    "Massa.VAT_T1",
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
                         "AF vigorosa",
                         "AF moderada",
                         "AF caminhada",
                         "AF total",
                         "PAS",
                         "PAD",
                         "IMC",
                         "RCQ",
                         "PM dominante",
                         "PM não dominante",
                         "SL",
                         "FC",
                         "AGI",
                         "C6'",
                         "Glic",
                         "HDL",
                         "TRI",
                         "C-r",
                         "Colesterol",
                         "Cortisol",
                         "Ângulo fase",
                         "Andróide gordura",
                         "Ginóide gordura",
                         "Massa gorda",
                         "Massa magra",
                         "Massa VAT",
                         "VFC média",
                         "VFC DP",
                         "VFC variância",
                         "RMSSD",
                         "VLF abs",
                         "LF abs",
                         "HF abs",
                         "LF nu",
                         "HF nu",
                         "LF/HF"
)

# Calculando matriz de correlação e valores de p, ignorando NAs automaticamente
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
