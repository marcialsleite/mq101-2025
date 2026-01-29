# ============================================================
# MQ101 - Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #02 - TEMPLATE DE SCRIPT (R)
# ------------------------------------------------------------

# Nome: Márcia de Lima Santos Leite
# Matrícula/RA: ______________________________
# Turma: _____________________________________
# Data: 13/10/2025
# Descrição: Respostas da Lista #02 
# ============================================================
install.packages(c("tidyverse", "readr"))
library(tidyverse)
options(scipen = 999)

caminho_csv <- "C:/Users/marci/OneDrive/Área de Trabalho/Template/educ_saude.csv"
caminho_csv <- "D:/Documentos/Márcia/UFABC Mestrado/Métodos Quantitativos/Templates/Dados/educ_saude (1).csv"

minha_base_de_dados_T1 <- readr::read_delim(caminho_csv, delim = ";", locale = locale(decimal_mark = ","), show_col_types = FALSE)	
glimpse(minha_base_de_dados_T1)	
View(minha_base_de_dados_T1)


caminho_csv <- "C:/Users/marci/OneDrive/Área de Trabalho/Template/educ_saude.csv"

minha_base_de_dados_T1 <- readr::read_delim(
  caminho_csv,
  delim = ";",                     
  locale = locale(decimal_mark = ","), 
  show_col_types = FALSE
)

# Consegui rodar o arquivo, mas o environment está buscando os dados ainda, mesmo tentado rodar duas vezes o comando 'minha_base_de_dados_T1'. Estou tentando outra opção sugerida pela IA.

glimpse(minha_base_de_dados_T1)

# O código 'delim = ";"' não está funcionando para separar as variáveis em colunas. IA sugere modificar para 'delim = ","'.

minha_base_de_dados_T1 <- readr::read_delim(
  caminho_csv,
  delim = ",",                     
  locale = locale(decimal_mark = ","), 
  show_col_types = FALSE
)
glimpse(minha_base_de_dados_T1)
base_final <-minha_base_de_dados_T1
caminho_para_salvar <- "D:/Documentos/Márcia/UFABC Mestrado/Métodos Quantitativos/Templates/CSV superado/educ_saude_FINAL.rds"
saveRDS(base_final, file = caminho_para_salvar)
cat("A base de dados foi salva com sucesso no pendrive F:")
# Aprendendo a salvar o que está em outra aba.


summary(minha_base_de_dados_T1)

# Formaram-se 13 colunas, separadas, com as informações de cada variável. Sexo, escolaridade, rede escolar, município, UF, diagnóstico e plano de saúde são qualitativas; Id, anos de estudo, idade, faltas escolares,pressão sistólica são quantitativas. 
# Dentre as qualitativas, são nominais: sexo, rede escolar, município, UF, diagnóstico e plano de saúde. 
# A escolaridade é ordinal. 
# Já nas quantitativas, as discretas são Id, idade, anos de estudo. As contínuas são tempo de estudo em horas e pressão sistólica.
# Não encontrei identificação de NA nas colunas.

#3. 

dados <- minha_base_de_dados_T1 |>
   mutate(
     sexo = factor(sexo),
     rede_escolar = factor(rede_escolar),
     plano_saude = factor(plano_saude),
     diagnostico = factor(diagnostico),
     escolaridade = factor(escolaridade,
                             levels = c("Fundamental","Médio","Superior"),
                             ordered = TRUE)
      )
 str(dados)

 dfl <- data.frame(
   variavel=c("id", "sexo", "escolaridade", "anos_estudo", "rede_escolar","municipio", "UF", "idade", "faltas_esc", "tempo_estudo_h", "pressao_sistolica", "diagnostico", "plano_saude"),
   Tipo_teorico=c("discreta", "nominal", "ordinal", "discreta", "nominal", "nominal", "nominal","discreta", "discreta", "contínua","contínua", "nominal", "nomina"),
   stringAsFactors=FALSE
 )
dfl

# Qualitativa, porque explica uma característica, através de um argumento não numérico. Nominal, porque usa um nome, que pode ser um substantivo ou adjetivo.
# Ordinal, porque usa um nome com característica hierarquizante.
# Quantitativa, porque demonstra um valor numérico, que pode ser discreto, que se vale de números inteiros para a contagem. Contínuas faz mensurações com números fracionados para contabilizar as variáveis correspondentes.

# 4.
tab_plano <- table(dados$plano_saude)
prop_plano <- prop.table(tab_plano)
cbind(FA = tab_plano, FR = round(100 * prop_plano, 1))

# 4A.
# table () cria tabela de frequências absolutas (de contagem); tab_plano e prop_plano são os nomes atribuídos aos objetos; dados$plano_saude é a variável de análise.
# prop.table produz os percentuais a partir das contagens absolutas.
help("cbind")
# combina os argumentos e combinam em colunas. Se quiser combinar em linhas, usa-se rbind. FA é a frequência absoluta e FR é a frequência relativa.

# A moda é o valor correspondente ao SUS / 5024 e a proporção dominante 50,2%. 

# 4B. 
dados |>
count(plano_saude) |>
ggplot(aes(x = plano_saude, y = n)) +
geom_col() +
labs(x = "Plano de saúde", y = "Frequência",
   title = "Distribuição de plano de saúde")
# Produziu um gráfico de barras.

# 4C.
dados |>
  count(escolaridade) |>
  ggplot(aes(x = escolaridade, y = n)) +
  geom_col() +
  labs(x = "Escolaridade (ordem substantiva)", y = "Frequência")
# Produziu um gráfico de barras, que demonstra que dentre os dados analisados, a maior frequência ocorreu na escolaridade de ensino médio.

# No caso do gráfico B, foi importante que se ordenasse da menor para a maior frequência para a visualização da proporção dominante.
# No gráfico C, mesmo que as colunas não esteja representadas em ordem crescente ou decrescente, dá para fazer a leitura e compreensão de qual é maior frequência.
# A ordem facilita a visualização, mas a sua ausência pode dificultar a interpretação da distribuição.
 
# 5.

# 5A.
sumario_idade <- dados |>
  summarise(
    n  =sum(!is.na(idade)),
    media = mean(idade, na.rm = TRUE),
    mediana= median(idade, na.rm = TRUE),
    min = min(idade, na.rm = TRUE),
    max = max(idade, na.rm = TRUE),
    dp = sd(idade, na.rm = TRUE)
  )
sumario_idade

sumario_pressao_sistolica <- dados |>
  summarise(
    n  =sum(!is.na(pressao_sistolica)),
    media = mean(pressao_sistolica, na.rm = TRUE),
    mediana= median(pressao_sistolica, na.rm = TRUE),
    min = min(pressao_sistolica, na.rm = TRUE),
    max = max(pressao_sistolica, na.rm = TRUE),
    dp = sd(pressao_sistolica, na.rm = TRUE)
  )
sumario_pressao_sistolica
    
# Considerando os valores de média e mediana da variável idade e observando o desvio padrão de 12.5, pode-se dizer que a diferença de 0.4 indica uma quase simetria.
# Na pressão sistólica, os valores de média e mediana estão idênticos, mesmo com dp = 14.1, nota-se simetria.

# 5B.
# 5B1.

ggplot(dados, aes(x = idade)) +
   geom_histogram(bins = 20) +
   labs(title = "Histograma de Idade")

# Histograma de distribuição assimétrica, com distribuição de frequência central, destaque para os maiores valores localizados entre 35 e 50 anos.
# A cauda decresce mais à direita.

# 5B2.

ggplot(dados, aes(y = idade)) +
   geom_boxplot() +
   labs(title = "Boxplot de Idade")
# Boxplot de idade com grande distribuição dos valores na faixa entre 30 e 50 anos, com outliers próximos aos 80 anos.

# 5B3.
ggplot(dados, aes(x = pressao_sistolica)) +
  geom_histogram(bins = 20) +
  labs(title = "Histograma de Pressão Sistólica")
# Histograma de distribuição simétrica, com distribuição de frequência central, destaque para os maiores valores localizados próximos a 125.

# 5B4.
ggplot(dados, aes(y = pressao_sistolica)) +
  geom_boxplot() +
  labs(title = "Boxplot de Pressão Sistólica")
# Boxplot de pressão sistólica com grande distribuição dos valores próximos ao valor de 125, com a mediana evidentemente abaixo de 125, e os outliers acima de 160, com destaque ao valor de 175.

# 6.
# 6A.
tab_cross <- table(dados$diagnostico, dados$plano_saude)
 tab_cross
 round(100 * prop.table(tab_cross, margin = 2), 1) 
 
# O comando dividiu em duas tabelas os valores de frequência relacionado com o tipo de enfermidade (diabetes mellitus, hipertensão arterial, outras e saudável), tanto em valores absolutos quanto em relativos.
# A maior frequência é de pessoas sem enfermidades, e em segundo lugar, no SUS, os hipertensos, em números absolutos.Percentualmente, há um equilíbrio entre os planos, entre os sem enfermidades.
 
# 6B
# 6B1 - Idade com sexo
 dados |>
    group_by(sexo) |>
    summarise(
      n = n(),
      media_idade = mean(idade, na.rm = TRUE),
      dp_idade = sd(idade, na.rm = TRUE),
      mediana_idade = median(idade, na.rm = TRUE)
      )
 # Dentre as 5192 pessoas do sexo feminino, a média de idade é de 40,3 anos, sendo a mediana de 40 anos, com desvio padrão de 12,5, sendo uma distribuição quase simétrica.
 # Dentre as 4808 pessoas do sexo masculino, a média de idade é de 40,5 anos, sendo a mediana de 40 anos, com desvio padrão de 12,4, sendo uma distribuição também quase simetrica.
 
 dados |>
   group_by(escolaridade) |>
   summarise(
     n = n(),
     media_idade = mean(idade, na.rm = TRUE),
     dp_idade = sd(idade, na.rm = TRUE),
     mediana_idade = median(idade, na.rm = TRUE)
   )
 
 # Há maior quantidade de indivíduos com escolaridade em ensino médio (4056), sendo a média de idade de 40,5 anos, mediana de 40 anos e o desvio padrão é de 12,6, valor que pode caracterizar uma 
 #distribuição quase simétrica.
 
 # Tanto idade x sexo quanto idade x escolaridade apresentaram distribuições quase simétricas.
 
 # 7.
 # 7A.
 colSums(is.na(dados))
 
 # Através deste comando, não foram identificados valores NA entre os dados disponíveis.
 # No exercício anterior, foi dado um comando para remover os NAs, para realizar os cálculos.
 
 # 7B.
 Q <- quantile(dados$tempo_estudo_h, probs = c(.25, .75), na.rm = TRUE)
  IQRv <- IQR(dados$tempo_estudo_h, na.rm = TRUE)
  lim_inf <- Q[1] - 1.5 * IQRv
  lim_sup <- Q[2] + 1.5 * IQRv
  subset_out <- dados |>
    filter(tempo_estudo_h < lim_inf | tempo_estudo_h > lim_sup)
  nrow(subset_out); head(subset_out)
 
  # Deu erro em que não reconhece o argumento não-numérico para o operador binário. Consultei a IA, que passou diversas sugestões de ação.
  glimpse(dados$tempo_estudo_h)
  
  dados <- dados |>
    mutate(
       tempo_estudo_h = as.numeric(tempo_estudo_h) 
    )
  
  rlang::last_trace()
  
  Q <- quantile(dados$tempo_estudo_h, probs = c(.25, .75), na.rm = TRUE)
  IQRv <- IQR(dados$tempo_estudo_h, na.rm = TRUE)
  lim_inf <- Q[1] - 1.5 * IQRv
  lim_sup <- Q[2] + 1.5 * IQRv
  subset_out <- dados |>
    filter(tempo_estudo_h < lim_inf | tempo_estudo_h > lim_sup)
  nrow(subset_out); head(subset_out)
  
# Formou-se uma tabela.
# Ela tinha suas colunas nomeadas por id, sexo, escolaridade, anos_estudo, rede_escola, município, UF, idade e faltas_escolares. 
# Informou que havia mais quatro variáveis: tempo_estudo_h, pressao_sistolica, diagnostico e plano_ saúde.
# Esta função foi produzida pelo 'tidyverse'. 
# Compreendendo os códigos: 'quantile () calcula o Q1 e o Q3, com está determinado no restante do código. 'IQR' é o cálculo 'Q3 - Q1'.
  
# A pergunta é se outliers são erros, casos raros ou informação válida é que, segundo Anderson, Sweeney e Williams (2002), outliers ou pontos fora da curva,
#podem ser valores de dados registrados de forma equivocada, com possibilidade de correção e prosseguimento da análise; podem ser conclusões incluída de forma 
#errada entre os dados, podendo ser desconsiderado; ou mesmo, um valor que repreesenta uma situação incomum - informação válida, que deve permanecer para a análise correta de um 
#conjunto de dados.Anderson, Sweeney e Williams (2002), Estatística Aplicada à administração e economia.
  
# 8.
# a)Rede escolar
  tab_rede_escolar <- table(dados$rede_escolar)
  prop_rede_escolar <- prop.table(rede_escolar)
  cbind(FA = tab_rede_escolar, FR = round(100 * prop_rede_escolar, 1))
  
  
  dados <- minha_base_de_dados_T1 |>
    mutate(
      sexo = factor(sexo),
      rede_escolar = factor(rede_escolar),
      plano_saude = factor(plano_saude),
      diagnostico = factor(diagnostico),
      escolaridade = factor(escolaridade,
                            levels = c("Fundamental","Médio","Superior"),
                            ordered = TRUE)
    )
  str(dados)
  
  tab_rede_escolar <- table(dados$rede_escolar)
  prop_rede_escolar <- prop.table(tab_rede_escolar)
  cbind(FA = tab_rede_escolar, FR = round(100 * prop_rede_escolar, 1))
  
  library(tidyverse)
  options(scipen = 999)
  names(dados)
  
# Reiniciando o script novamente, encontrei um erro, pela não detecção do dado 'rede_escolar'.
# Pedi instrução para IA, que indicou a necessidade de verificar se na base de dados os valores exibidos estavam em caracteres.
# Por isso, havia o risco de, ao digitar, ocorrerem diferenças com espaços a mais ou outra forma que o resultado estivesse escrito.

  dados_brutos <- minha_base_de_dados_T1
   dados_ajustados <- dados_brutos |>
       mutate(
           sexo = as.factor(sexo),
           rede_escolar = as.factor(rede_escolar),
           plano_saude = as.factor(plano_saude),
           diagnostico = as.factor(diagnostico),
           escolaridade = factor(escolaridade,
                                  levels = c("Fundamental","Médio","Superior"),
                                  ordered = TRUE),
          tempo_estudo_h = as.numeric (tempo_estudo_h),
          pressao_sistolica = as.numeric(pressao_sistolica)
       )
cat("\n---Distribuição de Frequência da Rede Escolar ---\n")

tab_rede_escolar <- table(dados_ajustados$rede_escolar)
prop_rede_escolar <- prop.table(tab_rede_escolar)
tabela_final_rede <- cbind(FA = tab_rede_escolar, FR = round(100 * prop_rede_escolar, 1))
cat("\n---Distribuição de Frequência da Rede Escolar ---\n")
tabela_final_rede

# b) Tempo_estudo_h com escolaridade 
ggplot(dados, aes(x= escolaridade, y = tempo_estudo_h)) +
  geom_boxplot() +
  labs(title = "Tempo de Estudo em Horas x Escolaridade",
   x= "Grau de escolaridade",
   y= "Tempo de estudo em horas"
   )

# O gráfico produzido não apresentou estética de boxplot e precisou de ajustes.
glimpse(dados_ajustados$tempo_estudo_h)

# Corrigindo conforme IA
ggplot(dados_ajustados, aes(x= escolaridade, y = tempo_estudo_h, fill = escolaridade)) +
  geom_boxplot() +
  labs(title = "Tempo de Estudo em Horas x Escolaridade",
       x= "Grau de escolaridade",
       y= "Tempo de estudo (expresso em horas)"
  )
theme_minimal()

#O erro estava de onde os dados estavam sendo extraídos, uma vez que foi criada um novo local de armazenamento.

# c) O gráfico demonstra que à medida que o nível educacional aumenta, o tempo de estudos aumentam proporcionalmente, portanto segue um padrão monotônico.

# 8B
# a)
ggplot(dados_ajustados, aes(x = pressao_sistolica)) +
  geom_histogram(bins = 20) +
  labs(title = "Histograma de Pressão Sistólica")
# Histograma de distribuição simétrica, com distribuição de frequência central, destaque para os maiores valores localizados próximos a 125.
# Para reportar média, mediana e desvio padrão, é preciso calcular estes valores 

sumario_pressao_sistolica <- dados_ajustados |>
  summarise( 
    n = sum(!is.na(pressao_sistolica)),
    media = mean(pressao_sistolica, na.rm = TRUE),
    mediana = median(pressao_sistolica, na.rm = TRUE),
    dp = sd(pressao_sistolica, na.rm = TRUE)
    )
  sumario_pressao_sistolica 
  
  valores_pressao_sistolica <-dados_ajustados
  summarise( 
    n = sum(!is.na(pressao_sistolica)),
    media = mean(pressao_sistolica, na.rm = TRUE),
    mediana = median(pressao_sistolica, na.rm = TRUE),
    dp = sd(pressao_sistolica, na.rm = TRUE)
  )
  valores_pressao_sistolica
# Agora, a construção do gráfico com destaque para média, mediana e dp.
  
  ggplot(dados_ajustados, aes(x = pressao_sistolica)) +
    geom_histogram( 
     bins = 40,
     fill = "skyblue",
     color = "darkblue"
  ) +
  geom_vline(
    xintercept = valores_pressao_sistolica$Mediana,
    linetype = "solid",
    color = "darkorange",
    linewidth = 1
  ) +
    
    geom_vline(
      xintercept = valores_pressao_sistolica$Media,
      linetype = "dashed",
      color = "yellow",
      linewidth = 1
    )  +
    
  annotate(
    "text",
    x = max(dados_ajustados$pressao_sistolica, na.rm = TRUE)*0.9, 
    y = Inf,
    label = paste0(
      "Média:", round(valores_pressao_sistolica$Media, 1),
      "\nMediana:", round(valores_pressao_sistolica$Mediana, 1),
      "\nDP:", round(valores_pressao_sistolica$DP, 1)
    ),
    vjust = 2,
    hjust = 0.5,
    size = 4,
    color = "black"
  ) +
    
    labs(
      title = "Histograma de Pressão Sistólica",
      subtitle = "Linha Laranja: Mediana | Linha Amarela: Média",
      x = "Pressão Sistólica (mmHg)",
      y = "Frequência Absoluta"
    ) +
    theme_minimal()
  names(dados_ajustados)
   names(dados_brutos)
   
   dados_brutos <- minha_base_de_dados_T1
   dados_ajustados <- dados_brutos |>
     mutate(
       sexo = as.factor(sexo),
       rede_escolar = as.factor(rede_escolar),
       plano_saude = as.factor(plano_saude),
       diagnostico = as.factor(diagnostico),
       escolaridade = factor(escolaridade,
                             levels = c("Fundamental","Médio","Superior"),
                             ordered = TRUE),
       tempo_estudo_h = as.numeric (tempo_estudo_h),
       pressao_sistolica = as.numeric(pressao_sistolica)
     )
   cat("\n---Distribuição de Frequência da Rede Escolar ---\n")
   
   
   dados_ajustados <- dados_brutos |>                                                     mutate(
     pressao_sistolica = as.numeric(pressao_sistolica)
   )                                                                                                                                valores_pressao_sistolica <- dados_ajustados |>
     summarise(
       n = sum(!is.na(pressao_sistolica)),
       media = mean(pressao_sistolica, na.rm = TRUE),
       mediana = median(pressao_sistolica, na.rm = TRUE),
       dp = sd(pressao_sistolica, na.rm = TRUE)
     ) |>
     identity()
   print(valores_pressao_sistolica)
   
   #Alguns erros aconteceram desde que eu decidi renomear os dados . Pedi ajuda à IA ...
   
   # Certifique-se que o objeto 'dados_ajustados' está na memória antes de rodar este bloco.
   # ----------------------------------------------------------------------------------
   dados_brutos <- minha_base_de_dados_T1
   dados_ajustados <- dados_brutos |>
   
   # 1. ATRIBUIÇÃO: O resultado do pipe é salvo em 'valores_pressao_sistolica'
   valores_pressao_sistolica <- dados_ajustados |>
     
     # A. MUTATE (Garante que a pressão sistólica é numérica para o cálculo)
     # B. SUMMARISE (Calcula as estatísticas)
     # 2. EXIBIÇÃO: Imprime o resultado do sumário no console
     mutate(
       pressao_sistolica = as.numeric(pressao_sistolica)
     )
   valores_pressao_sistolica <- dados_ajustados |> 
    summarise(
       n = sum(!is.na(pressao_sistolica)),
       media = mean(pressao_sistolica, na.rm = TRUE),
       mediana = median(pressao_sistolica, na.rm = TRUE),
       dp = sd(pressao_sistolica, na.rm = TRUE)
     ) 
   
   print(valores_pressao_sistolica)
# Passo 1 
   dados_ajustados <- dados_ajustados |> 
     mutate(
       pressao_sistolica = as.numeric(pressao_sistolica)
     )
# Passo 2
   valores_pressao_sistolica <- dados_ajustados |>
     summarise(
       n = sum(!is.na(pressao_sistolica)),
       media = mean(pressao_sistolica, na.rm = TRUE),
       mediana = median(pressao_sistolica, na.rm = TRUE),
       dp = sd(pressao_sistolica, na.rm = TRUE)
     )
# Passo 3
   print(valores_pressao_sistolica)
   
# Histograma
   dados_ajustados <- dados_ajustados |> 
     mutate( 
       pressao_sistolica = as.numeric(pressao_sistolica)
       )
   
   valores_pressao_sistolica <- dados_ajustados |>
     summarise(
       media = mean(pressao_sistolica, na.rm = TRUE),
       mediana = median(pressao_sistolica, na.rm = TRUE),
       dp = sd(pressao_sistolica, na.rm = TRUE)
     )
   
   print(valores_pressao_sistolica)

   
   ggplot(dados_ajustados, aes(x = pressao_sistolica)) +
     geom_histogram( 
       bins = 40,
       fill = "skyblue",
       color = "darkblue"
     ) +
     geom_vline(
       xintercept = valores_pressao_sistolica$mediana,
       linetype = "solid",
       color = "darkorange",
       linewidth = 1
     ) +
     
     geom_vline(
       xintercept = valores_pressao_sistolica$media,
       linetype = "dashed",
       color = "yellow",
       linewidth = 1
     )  +
     
     annotate(
       "text",
       x = max(dados_ajustados$pressao_sistolica, na.rm = TRUE)*0.9, 
       y = Inf,
       label = paste0(
         "Média:", round(valores_pressao_sistolica$media, 1),
         "\nMediana:", round(valores_pressao_sistolica$mediana, 1),
         "\nDP:", round(valores_pressao_sistolica$dp, 1)
       ),
       vjust = 2,
       hjust = 0.5,
       size = 4,
       color = "black"
     ) +
     
     labs(
       title = "Histograma de Pressão Sistólica",
       subtitle = "Linha Laranja: Mediana | Linha Amarela: Média",
       x = "Pressão Sistólica (mmHg)",
       y = "Frequência Absoluta"
     ) +
     theme_minimal()
   
   #Depois de muita luta, consegui fazer o histograma, somente pela mudança de letras maiúsculas em um comando por minúsculas.
   
# 8B. 
#   b)Cruzamento diagnostico × plano_saude (% por coluna).

   tab_cross <- table(dados_ajustados$diagnostico, dados_ajustados$plano_saude)
   prop_column <- prop.table(tab_cross, margin = 2)
      round(100 * prop_column, 1)
      
    ggplot(dados_ajustados, aes(x = plano_saude, fill = diagnostico))+
      geom_bar(position = fill)+
      scale_y_continuous(labels = scales::percent_format())+
      labs(
        title = "Distribuiçao de Diagnóstico por Plano de Saúde",
        x = "Plano de Saúde",
        y = "Diagnóstico"
      ) +
      theme_minimal()
    
#    Corrigindo o erro
    
    ggplot(dados_ajustados, aes(x = plano_saude, fill = diagnostico))+
      geom_bar(position = "fill")+
      scale_y_continuous(labels = scales::percent_format())+
      labs(
        title = "Distribuiçao de Diagnóstico por Plano de Saúde",
        x = "Plano de Saúde",
        y = "Diagnóstico"
      ) +
      theme_minimal()
    
    # Analisando tanto a tabela quanto o gráfico, é possível perceber quem em todas as situações selecionadas - com plano privado, com acesso ao SUS, com ambos os acessos e sem nenhum acesso, prevalecem os pacientes sem enfermidades.
    # Apresentaram-se valores que comprovam esta conclusão os valores que variam entre 80,6% a 81,4% nesta variável.
    # Dentre os pacientes com enfermidade, a hipertensão arterial sistêmica foi predominante, com valores entre 12,1% e 12,5%.
    
    # 9.
    # a)
    
    tab_synth_idade <- dados_ajustados |>
      summarise(
        n = sum (!is.na(idade)),
        media = mean(idade, na.rm = TRUE),
        mediana = median(idade, na.rm = TRUE),
        dp = sd(idade, na.rm = TRUE)
      )
    print(tab_synth_idade)
    
#    Precisava construir uma tabela. Nomeei dados ajustados, de onde o R puxa os valores; então o summarise apresenta somente os valores dos cálculos realizados.
    # Por fim, para que sejam calculadas a média, mediana e o dp, deve-se pedir para o R identificar os NAs, para utilizar somente os valores válidos para utilizar.
    # O na.rm =TRUE faz com o que o cálculo seja feito somente com os não NAs.
    
#  b)
    tab_synth_escolaridade <- dados_ajustados |>
      group_by(escolaridade, plano_saude) |>
      summarise(
        (FA = n()), .groups = "drop") |>
        group_by(escolaridade) |>
        mutate(FR = round(FA/sum(FA) *100,1))
        
        tab_synth_escolaridade <- table(dados$escolaridade)
        prop_escolaridade <- prop.table(tab_synth_escolaridade)
        cbind
        
    tab_cross <- table(dados$escolaridade, dados$plano_saude)
    prop_cross <- prop.table(tab_cross, margin=1)
    round(100*tab_cross,1)
    
    
    tab_fa <- table(dados$escolaridade, dados$plano_saude) 
    tab_fa <- table(dados$escolaridade, dados$plano_saude) 
    tab_fr <- prop.table(tab_fa, margin = 1) 
    print(tab_fa) 
    print(round(100 * tab_fr, 1))
    
    
# Faltam   
#    Produza dois gráficos:
#    1. Barras empilhadas de plano_saude por escolaridade (proporções).
#   2. Boxplot de tempo_estudo_h por escolaridade.
#    Dica: use group_by(), summarise(), count(), mutate(prop = n/sum(n)) e position
#    = "fill" no geom_col.
    
 glimpse(dados_ajustados)   
    
    ggplot(dados_ajustados, aes(x = plano_saude, fill = escolaridade))+
      geom_bar(position = "fill")+
      scale_y_continuous(labels = scales::percent_format())+
      labs(
        title = "Distribuiçao de Escolaridade por Plano de Saúde",
        x = "Plano de Saúde",
        y = "Proporção"
      ) +
      theme_minimal()
    
    ggplot(minha_base_de_dados_T1, aes(x = plano_saude, fill = escolaridade))+
      geom_bar(position = "fill")+
      scale_y_continuous(labels = scales::percent_format())+
      labs(
        title = "Distribuiçao de Escolaridade por Plano de Saúde",
        x = "Plano de Saúde",
        y = "Proporção"
      ) +
      theme_minimal()
    
    minha_base_de_dados_T1 <- readr::read_delim(
      caminho_csv,
      delim = ",",                     # SEPARADOR DE COLUNAS = PONTO E VÍRGULA
      locale = locale(decimal_mark = ","), # SEPARADOR DECIMAL = VÍRGULA (Formato BR)
      show_col_types = FALSE
    )
    glimpse(minha_base_de_dados_T1)
    base_final <-minha_base_de_dados_T1
    caminho_para_salvar <- "F:/Documentos/Márcia/UFABC Mestrado/Métodos Quantitativos/Templates/educ_saude_FINAL.rds"
    saveRDS(base_final, file = caminho_para_salvar)
    cat("A base de dados foi salva com sucesso no pendrive F:")
    
    summary(minha_base_de_dados_T1)
    
    
    
    ggplot(minha_base_de_dados_T1, aes(x = plano_saude, fill = escolaridade))+
      geom_bar(position = "fill")+
      scale_y_continuous(labels = scales::percent_format())+
      labs(
        title = "Distribuiçao de Escolaridade por Plano de Saúde",
        x = "Plano de Saúde",
        y = "Proporção"
      ) +
      theme_minimal()
    
    
  # 2. Boxplot de tempo_estudo_h por escolaridade.
    #    Dica: use group_by(), summarise(), count(), mutate(prop = n/sum(n)) e position
    #    = "fill" no geom_col.
    

    
    glimpse(minha_base_de_dados_T1$tempo_estudo_h)
    
   # Reiniciei o exercício rodando : 
    
    library(tidyverse)
    options(scipen = 999)
    
     glimpse(minha_base_de_dados_T1)       
                 
                 
    caminho_csv <- "C:/Users/marci/OneDrive/Área de Trabalho/Template/educ_saude.csv"

    minha_base_de_dados_T1 <- readr::read_delim(
      caminho_csv,
      delim = ",",                     # SEPARADOR DE COLUNAS = PONTO E VÍRGULA
      locale = locale(decimal_mark = ","), # SEPARADOR DECIMAL = VÍRGULA (Formato BR)
      show_col_types = FALSE
    )
    glimpse(minha_base_de_dados_T1) 
  
    help("str")   
    str(minha_base_de_dados_T1$tempo_estudo_h)
    summary(minha_base_de_dados_T1$tempo_estudo_h)
    
#    Que sofrimento para entender o que foi o problema da rodagem deste gráfico ...
    minha_base_de_dados_T1 <- minha_base_de_dados_T1 |>
      mutate(
        tempo_estudo_h = as.numeric(tempo_estudo_h),
        escolaridade = factor(escolaridade,
                              levels = c("Fundamental","Médio","Superior"),
                              ordered = TRUE)
      )
    str(minha_base_de_dados_T1) 
 
    
    ggplot(minha_base_de_dados_T1, aes(x = escolaridade, y = tempo_estudo_h)) +
      geom_boxplot(aes(fill = escolaridade), outlier.size = 1, outlier.alpha = 0.3) +
      coord_flip() +
      labs(
        title = "Tempo de Estudo por Grau de Escolaridade",
        x = "Escolaridade",
        y = "Tempo de Estudo (horas)"
      ) +
      theme_minimal() 
  