#Nome: Márcia de Lima Santos Leite
# Lista 03 - Gráficos e Visualização de Dados
# (ggplot2)

set.seed(101)

install.packages(c("tidyverse", "readr","ggplot2", "scale", "viridis", "electionsBR"))
library(tidyverse)

# Aquecimento – Tabela vs. Gráfico (dados internos)
data(cars)
head(cars, 10)

ggplot(cars, aes(speed, dist)) +
  geom_point(alpha = 0.6) +
  labs(title = "Speed x Distance", x = "Speed", y = "Distance" ) +
  theme_minimal()

# Relação positiva: quanto maior a velocidade desempenhada, maior a distancia necessária para frear um veículo.

# 2. 
data(mtcars)
 mtcars <- mtcars |>
 mutate(cyl = as.factor(cyl))
 # Elaborar: histogramas/densidades de mpg e boxplot por cyl. Interpretação breve.

 ggplot(mtcars, aes(x = mpg)) +
       geom_histogram( 
           bins = 10,
           fill = "skyblue",
           color = "darkblue"
         ) +
       labs(
           title = "Histograma de consumo de combustivel",
           x = "Milhas por galão (mpg)", 
           y = "Frequência"
         ) +
      theme_minimal() 
 
 ggplot(mtcars, aes(x = mpg)) +
   geom_density(alpha = 0.4) +
      labs(
     title = "Densidade de Consumo",
     x = "Milhas por galão (mpg)",
     y = "Densidade"
   ) +
   theme_minimal() 
 
 
ggplot(mtcars, aes(x = cyl, y = mpg)) +
   geom_boxplot(aes(fill = cyl), outlier.size = 1, outlier.alpha = 0.3) +
   coord_flip() +
   labs(
     title = "Consumo de Combustível por Número de Cilindro",
     x = "Cilindros",
     y = "MPG"
   ) +
   theme_minimal() 

ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_boxplot(aes(fill = cyl), outlier.size = 1, outlier.alpha = 0.3) +
    labs(
    title = "Consumo de Combustível por Número de Cilindro",
    x = "Cilindros",
    y = "MPG"
  ) +
  theme_minimal()

# 3. Série temporal simples
# Identificar tendência no tempo.
# Escolha uma base.

# Airpassengers
library (tibble)

install.packages("zoo")
library(zoo)

ap <- tibble(
  date = as.Date(as.yearmon(time(AirPassengers))),
  n = as.numeric(AirPassengers),
  year = format(as.Date(as.yearmon(time(AirPassengers))), "%Y"),
  month = format(as.Date(as.yearmon(time(AirPassengers))), "%m")
)
 
 # Gráfico de linhas
ggplot(ap, aes(x = date, y = n)) +
  geom_line(color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", size = 1) +
  labs(
    title = "Número de Passageiros ao Longo do Tempo",
    x = "Ano",
    y = "Passageiros (milhares)"
  ) +
  theme_minimal()


ggplot(ap, aes(x = date, y = n)) +
  geom_line(color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linewidth = 1) +
  labs(
    title = "Número de Passageiros ao Longo do Tempo",
    x = "Ano",
    y = "Passageiros (milhares)"
  ) +
  theme_minimal()


# Opção 2

data("airquality")
aq <- airquality |>
  as_tibble() |>
  drop_na(Ozone) |>
  mutate(Month = factor(Month),
         Day = as.integer(Day))

ggplot(aq, aes(x = Day, y = Ozone)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Month,scale = "free_x")
labs(title = "Níveis de ozone",
     x = "Dia", y = "Ozone") +
  theme_minimal()

set.seed(101)
library(tidyverse)

# 4.Relações bivariadas e transformações

ggplot(mtcars, aes(x=wt, y =mpg)) +
  geom_point(color = "darkorange" , size = 2) +
  geom_smooth(
    method = "lm",
    color = "green",
    linetype = "dashed",
    se = FALSE
  ) +
  geom_smooth(
    method = "loess",
    color = "purple",
    linetype = "solid",
    se = FALSE
  ) +
  labs(
    title = "Relação entre Peso e Consumo de Combustível (mtcars)",
    subtitle = "Tendências: Reta (Verde) versus Curva Suave (Roxa)",
    x = "Peso (milhares de libras)",
    y = "Milhas por Galão (MPG)"
  ) + 
  theme_minimal()


# Tentativa de visualizar o gráfico log10.

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  
  scale_x_log10() +
  
  labs(
    title = "MPG versus Peso (Eixo X em Escala Log)",
    subtitle = "Verifique se a dispersão dos pontos se torna mais uniforme",
    x = "Log10 (Peso em milhares de libras)",
    y = "Milhas por Galão (MPG)"
  ) +
  theme_minimal()

# O gráfico formou uma imagem de tendência negativa.
# Fazendo a comparação entre os gráficos com e sem Log10, o posicionamento dos pontos em dispersão no log10 apresentam-se mais ajustados à reta lm.
# A dispersão dos pontos no gráfico log10 fica mais uniforme.

# 5. Facetas
mtcars <- mtcars |>
   mutate(am = factor(am, labels = c("Automático","Manual")))

ggplot(mtcars,aes(x =wt, y = mpg)) +
  geom_point(aes(color = am), size = 2) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  
  facet_wrap(~am,
             ncol = 3,
             scales = "fixed") +
  
  labs(
    title = " MPG versus Peso, Segmentado por Número de Cilindros",
    subtitle = "Observe a diferença na inclinação (Tendência) entre os grupos",
    x = "Peso (milhares de libras)",
    y = "Milhas por Galão (MPG)",
    color = "Cilindros"
  ) +
theme_minimal()

# A inclinação da reta no grupo automático é mais acentuada que no grupo manual, ambas com tendência negativa.
# A dispersão é mais acentuada no grupo automático.

mtcars <- mtcars |>
  dplyr::mutate(cyl_f = factor(cyl)
                )

ggplot(mtcars,aes(x =wt, y = mpg)) +
  geom_point(aes(color = factor (am)), size = 2) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "chocolate"
  ) +
  
  
  facet_wrap(~cyl_f,
             ncol = 3,
             scales = "fixed") +
  
  labs(
    title = " MPG versus Peso, Segmentado por Cilindros",
    subtitle = "Linha de Tendência mostra a relação de Peso dentro de cada motor",
    x = "Peso (milhares de libras)",
    y = "Milhas por Galão (MPG)",
    color = "Transmissão (0=Auto, 1=Manual)"
  ) +
  theme_minimal()

# Análise dos resultados: com 4 cilindros, o mecanismo manual prevalente e o peso menor do motor coincidem melhor aproveitamento do combustível.
# Com 6 cilindros o desempenho em relação ao peso do veículo e o consumo do combustível mantêm-se quase equivalentes, em uma faixa mediana, 
# nos mecanismos manual e automático.
# Com 8 cilindros, o desempenho dos motores com mecanismo automático como prevalente, tem alto consumo de combustível, rodando muito menos
# e com evidente índice de dispersão.

# Exercicio 6 - Simulação

n <- 1000; rhos <- c(0.2, 0.6, 0.9)
 sim <- purrr::map_dfr(rhos, \(rho) {
   x <- rnorm(n); e <- rnorm(n)
   y <- rho*x + sqrt(1 - rho^2)*e
   tibble(rho = rho, x = x, y = y)
   })
 
 install.packages("MASS")
 library(MASS)

simular_dados <- function(rho, n_obs = 100) {
  sigma <- matrix(c(1, rho, rho, 1), 2, 2)
  dados_simulados <-mvrnorm(n = n_obs, mu = c(0, 0), Sigma = sigma) |>
    as.data.frame() |>

    rename(X= V1, Y = V2) |>
    mutate(
      Rotulo = paste0("p= ", rho)
    )
  return(dados_simulados)
}

dados_r02 <- simular_dados(rho = 0.2)
dados_r06 <- simular_dados(rho = 0.6)
dados_r09 <- simular_dados(rho = 0.9)

dados_finais <-bind_rows(dados_r02, dados_r06, dados_r09)


ggplot(dados_finais,aes(x =X, y = Y)) +
  geom_point(alpha = 0.7, color = "pink") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "darkgray"
  ) +
  
  
  facet_wrap(~Rotulo,
             ncol = 3,
            )  +
  
  labs(
    title = " Simulação da Relação entre a Correlação (sigma) e a Força da Relação Linear",
    x = "Variável X",
    y = "Variável Y",
  ) +
theme_minimal()

# A dispersão do ρ=0.2 destacou uma reta quase constante, com tendência levemente positiva, com a maioria dos pontos próximos a lm, e com alguns outliers, nos dois campos das variáveis, com valores positivos e negativos.
# Com o ρ=0.6, a inclinação da reta altera-se sensivelmente, com tendência positiva, com menor número de outliers.
# Ainda assim, há diversos pontos tanto na área de valores positivos quanto dos negativos.
# No ρ=0.9, a inclinação fica mais evidente, sendo tendência fortemente positiva, com maior concentração de pontos relacionais em torno do ponto ) da variável Y.

# 7. Simulação II

# Passos: simule grupo A: N (0,1); grupo B:N(1,1.8);
# faça histogramas/densidades; boxplot/violin; interprete

n <-1000
df_grupos <- tibble(grupo = rep(c("A", "B"),
                                each =n),valor = c(rnorm (n, 0 , 1), rnorm (n, 1, 1.8)))

# Histograma

ggplot(df_grupos,aes(x = valor , fill = grupo)) +
     geom_histogram(aes(y = after_stat(density)), 
         binwidth = 0.4, alpha = 0.6, position = "identity") +
    labs(
      title = "Comparação de Distribuições",
      x = "Valor",
      y = "Densidade",
      fill = "Grupo") +
  theme_minimal()

# Densidade

ggplot(df_grupos,aes(x = valor , fill = grupo)) +
  geom_density(alpha = 0.5)+ 
               
  labs(
    title = "Comparação de Distribuições 2",
    x = "Valor",
    y = "Densidade",
    fill = "Grupo") +
  theme_minimal()

# Boxplot

ggplot(df_grupos,aes(x = grupo , y= valor, fill = grupo)) +
  geom_boxplot(alpha = 0.7) + 
                 
  labs(
    title = "Comparação de Distribuições 3",
    x = "Grupo",
    y = "Valor",
    fill = "Grupo") +
  theme_minimal()

# Violin

ggplot(df_grupos, aes(x = grupo , y=valor)) +
geom_violin(fill = "lightblue" , color = "navy" ) +
  
  labs(
    title = "Comparação de Distribuições 4",
    x = "Grupo",
    y = "Valor" ) +
   theme_minimal()

# Gráfico comparativo : as médias

library(readr)
library(dplyr)
library(ggplot2)
educ <- read_csv("C:/Users/marci/Desktop/educ_saude_1.csv")

glimpse(educ)
summary(educ)

names(educ)

# Histograma
 # Educação
ggplot(educ, aes(x = anos_estudo)) + 
  geom_histogram(
    binwidth = 1, 
    fill = "skyblue", 
    color = "darkorchid") + 
  labs(title = "Distribuição dos anos de estudo", 
       x = "Anos de estudo", 
       y = "Frequência")
theme_minimal()

#Saúde
ggplot(educ, aes(x = pressao_sistolica)) + 
  geom_histogram(
    binwidth = 5, 
    fill = "gold", 
    color = "chocolate") + 
  labs(title = "Distribuição da pressão sistólica", 
       x = "Pressão Sistólica (mmHg)", 
       y = "Frequência")
theme_minimal()

# Densidades
# Educação
ggplot(educ, aes(x = anos_estudo)) + 
  geom_density(fill = "lightblue") + 
  labs(title = "Densidade dos anos de estudo")
theme_minimal()

#Saúde
ggplot(educ, aes(x = pressao_sistolica)) + 
  geom_density(fill = "limegreen") + 
  labs(title = "Densidade da pressão sistólica")
theme_classic()

#Boxplot
# Educação
ggplot(educ, aes(y = anos_estudo)) + 
  geom_boxplot(fill = "violet") + 
  labs(title = "Boxplot dos anos de estudo") 
theme_gray()

#Saúde
ggplot(educ, aes(y = pressao_sistolica)) + 
  geom_boxplot(fill = "darkorange") + 
  labs(title = "Boxplot da pressão sistólica")
theme_get()

#Dispersão: educação x saúde
ggplot(educ, aes(x = anos_estudo, y = pressao_sistolica)) + 
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "peru") + 
  labs(title = "Relação entre anos de estudo e pressão sistólica",
       x = "Anos de estudo",
       y = "Pressão Sistólica (mmHg)"
  ) 
theme_bw()
     
# Facetas
ggplot(educ, aes(x = anos_estudo, y = pressao_sistolica)) + 
  geom_point(alpha = 0.6) + 
  facet_wrap(~plano_saude) + 
  labs(title = "Relação entre educação e saúde por plano de saúde", 
        x = "Anos de estudo", 
        y = "Pressão sitólica (mmHg)")
theme_classic()

# Interpretação
# Histograma
 #Educação: através deste gráfico, é possível verificar que a maior parte dos indivíduos da amostra possuem entre 10 e 15 anos de estudo, sendo baixíssimo o número de pessoas que chegam aos 20 anos de estudo.
 #Saúde: a análise da amostra sugere que a maioria dos indivíduos apresentam pressão sistólica dentro dos parâmetros de normalidade, demonstrando que uma pequena parte tem hipotensão ou hipertensão.
# Densidade
 #Educação: demonstra uma maioria de indivíduos da amostra com anos entre 10 e 15, sendo o pico em 10 anos.
 #Saúde: por este gráfico, visualiza-se um pico onde a maior parte dos indivíduos da amostra possuem pressão sistólica dentro da normalidade (125 mmHg)
# Boxplot
 #Educação: nesta representação, os indivíduos encontram-se em sua maioria dentro do tempo de estudo entre 10 e 15 anos, estando a mediana localizada ligeiramente na parte de cima da caixa, com distribuição levemente assimétrica.Os dados acima da caixa apresentam maior dispersão.
 #Saúde: os indivíduos encontram-se em sua maioria dentro dos parâmetros de normalidade nos índices de pressão sistólica, estando a mediana localizada ligeiramente na parte de cima da caixa, com distribuição levemente assimétrica.Os dados acima da caixa apresentam maior dispersão.
# Dispersão: A linha de regressão está na posição horizontal, e mesmo como menos pontos de dispersão localizados nas posições acima de 15 anos de estudo, não é possível afirmar que 
#exista alguma relação entre os anos de estudo e os índices de pressão sistólica.Os outliers encontram-se bem abaixo de 100 mmHg e acima de 150 mmHg. Apesar do número de indivíduos com 20 anos de estudo ser baixo, nota-se que neste grupo, os índices de pressão
#sistólica não ultrapassaram os 150 mmHg, sugerindo uma tendência de que altos tempos de dedicação aos estudos produzem indivíduos que controlam melhor a sua saúde cardíaca.
# Facetas: é possível fazer comparação entre os grupos que possuem assistência saúde apenas privada, apenas pública, ambas ou nenhuma assistência. 

# 8. Educ_saude.csv

library(readr)

# A ideia principal é relacionar educação com saúde, buscando encontrar uma causalidade entre as variáveis ligadas à elas.

# Histograma
ggplot(educ, aes(x = tempo_estudo_h)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribuição do tempo de estudo (horas)",
       x = "Tempo de estudo (horas)",
       y = "Frequência escolar") +
  theme_minimal()

ggplot(educ, aes(x = pressao_sistolica)) +
  geom_histogram(binwidth = 5, fill = "darkorange", color = "brown") +
  labs(title = "Distribuição da pressão sistólica",
       x = "Pressão sistólica (mmHg)",
       y = "Frequência escolar") +
  theme_minimal()

ggplot(educ, aes(x = idade, fill = plano_saude)) +
  geom_histogram(binwidth = 2, color = "black", alpha = 0.7, position = "identity") +
  labs(title = "Distribuição das idades por plano de saúde",
       x = "Idade (anos)",
       y = "Frequência",
       fill = "Plano de saúde") +
  theme_minimal()


# Dispersão: idade vs tempo dedicado ao estudo
ggplot(educ, aes(x = idade, y = tempo_estudo_h)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre idade e tempo de estudo",
       x = "Idade (anos)",
       y = "Tempo de estudo (horas)") +
  theme_bw()

#            idade vs pressão 
ggplot(educ, aes(x = idade, y = pressao_sistolica)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relação entre idade e pressão sistólica",
       x = "Idade (anos)",
       y = "Pressão sistólica (mmHg)") +
  theme_bw()

# Facetas
  # Pressão vs plano de saúde
ggplot(educ, aes(x = pressao_sistolica, fill = plano_saude)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~ plano_saude) +
  labs(title = "Distribuição da pressão sistólica por plano de saúde",
       x = "Pressão sistólica (mmHg)",
       y = "Frequência") +
  theme_minimal()

  # Tempo dedicado aos estudos vs plano de saúde
ggplot(educ, aes(x = tempo_estudo_h, fill = plano_saude)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~ plano_saude) +
  labs(title = "Distribuição do tempo de estudo por plano de saúde",
       x = "Tempo de estudo (horas)",
       y = "Frequência") +
  theme_minimal()

# Ao fazer as análises dos gráficos obtidos, pode-se observar que quando se considera o aumento das idades dos indivíduos, 
#o tempo dedicado aos estudos diminui. E surgem mais alterações da saúde relacionadas ao aumento da pressão sistólica.
#Esperava-se que quanto mais horas de estudo levasse os indivíduos a acessarem os planos de saúde privados, mas o gráfico indica que 
#os acessos ao SUS são os mais utlizados, principalmente entre as pessoas com até 10 anos de estudo.
# Apesar de tentar buscar causalidade entre educação e saúde e as variáveis analisadas, podem ser feitas associações entre algumas variáveis,
#como indices de pressão com idade, acesso aos mecanismos de assistência em saúde com idade, e escolaridade com idade. Não há certezas de que 
#exista causa-efeitos entre as variáveis analisadas.


#10 (9)Desafio (pontos extras) – Eleições 2024 (Município de São Paulo) com TSE + electionsBR (realizado com auxílo da IA)

library(electionsBR)
library(scales)
packageVersion("electionsBR")

install.packages("electionsBR")

ls("package:electionsBR")

library(dplyr)

library(readr)
dados_cand_2024 <- fread("C:/Users/marci/Desktop/ELEICOESBR 2024/Planilha sem título - votacao_candidato_munzona_2024_SP.csv") 
dados_secao_2024 <- fread("C:/Users/marci/Desktop/ELEICOESBR 2024/Detalhe_votacao_secao_eleitoral_2024/detalhe_votacao_secao_2024_SP.csv")
dados_2024 <- fread("C:/Users/marci/Desktop/ELEICOESBR 2024/Detalhe_votacao_secao_eleitoral_2024/detalhe_votacao_secao_2024_SP.csv")

dados_2024_sp <- dados_2024
dados_secao_2024_sp <- dados_secao_2024
dados_cand_munzona_2024 <- dados_cand_2024
names(dados_2024)
names(dados_secao_2024_sp)
names(dados_cand_munzona_2024)
head(dados_secao_2024_sp)
head(dados_cand_munzona_2024)
head(dados_2024)

# Filtrando o município

  dados_secao_2024_sp <- readr::read_csv2(
    "C:/Users/marci/Desktop/ELEICOESBR 2024/Detalhe_votacao_secao_eleitoral_2024/detalhe_votacao_secao_2024_SP.csv", 
             locale = readr::locale(encoding = "Latin1") # ou "UTF-8" 
            ) 


"NM_MUNICIPIO" %in% names(dados_secao_2024_sp)

# Filtrando as linhas do município de São Paulo
dados_sp <- dados_secao_2024_sp |> 
  mutate(NM_MUNICIPIO = toupper(NM_MUNICIPIO)) |> 
  filter(.data[["NM_MUNICIPIO"]] == "SÃO PAULO")

# Agregando taxas por zona eleitoral
denom <- if ("QT_TOTAL_VOTOS" %in% names(dados_2024_sp)) "QT_TOTAL_VOTOS" else "
QT_COMPARECIMENTO"

tx_zona <- dados_secao_2024 |> 
  filter(CD_MUNICIPIO == 71072) |> 
  group_by(NR_ZONA) |> 
  summarise(
    aptos = sum(QT_APTOS, na.rm = TRUE),
    abst = sum(QT_ABSTENCOES, na.rm = TRUE),
    comp = sum(QT_COMPARECIMENTO, na.rm = TRUE),
    brancos = sum(QT_VOTOS_BRANCOS, na.rm = TRUE),
    nulos = sum(QT_VOTOS_NULOS, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(
    tx_abst = abst / aptos,
    tx_branco = brancos / comp,
    tx_nulo = nulos / comp,
    NR_ZONA = as.character(as.numeric(NR_ZONA))
  )
# Perfil do eleitor - escolaridade por zona

head(tx_zona$NR_ZONA, 3)
head(vp_sp_mun$NR_ZONA, 3)
# Pacotes essenciais
library(data.table)
library(dplyr)
library(stringi)

perfil_sp <- fread("C:/Users/marci/Desktop/ELEICOESBR 2024/Perfil_eleitorado_2024/perfil_eleitor_secao_2024_SP.csv")

# 3. Processar os dados 
# 3.1. Padronizando a base de votação 
tx_zona <- tx_zona |> 
  mutate(NR_ZONA = as.character(as.numeric(NR_ZONA)))

#  3.2. Criando vp_sp_mun com limpeza de acentos 
names(perfil_sp)

vp_sp_mun <- perfil_sp |> 
  filter(CD_MUNICIPIO == 71072) |> 
  mutate(
    DS_GRAU_ESCOLARIDADE = toupper(stri_trans_general(DS_GRAU_ESCOLARIDADE, "Latin-ASCII")),
    sup_ou_mais = DS_GRAU_ESCOLARIDADE %in% c("SUPERIOR COMPLETO", "SUPERIOR INCOMPLETO", "POS-GRADUACAO"),
    NR_ZONA = as.character(as.numeric(NR_ZONA))
  ) |> 
  group_by(NR_ZONA) |> 
  summarise(
    total_eleitores = sum(QT_ELEITORES_PERFIL, na.rm = TRUE),
    eleitores_sup = sum(QT_ELEITORES_PERFIL[sup_ou_mais], na.rm = TRUE),
    share_sup = eleitores_sup / total_eleitores,
    .groups = "drop"
  )

#  3.3. Unindo as bases 
df_final <- tx_zona |> 
  inner_join(vp_sp_mun, by = "NR_ZONA")

print(nrow(tx_zona))
print(nrow(vp_sp_mun))
print(nrow(df_final))

#  3.4. Verificação 

sum(is.na(df_final$share_sup))
 
 # Agora produza 3 dispersões: share_sup x tx_abst; share_sup x tx_branco;
 #share_sup x tx_nulo
 # Use eixos percentuais. Interprete em 8--12 linhas. Discuta limites.
 
# Gráficos
 
 library(ggplot2)
 
 ggplot(df_final, aes(x = share_sup, y = tx_abst)) +
   geom_point(color = "steelblue", alpha = 0.7) +
   geom_smooth(method = "lm", se = FALSE, color = "red") +
   scale_x_continuous(labels = scales::percent_format()) +
   scale_y_continuous(labels = scales::percent_format()) +
   labs(title = "Escolaridade nível superior (%) x Taxa de abstenção",
        x = "Proporção de eleitores com ensino superior",
        y = "Taxa de abstenção") +
   theme_minimal()
 
 # 1. CArregando bibliotecas gráficas 
 library(ggplot2)
 library(scales)
 
 # 2. Executa a união final 
 df_final <- tx_zona |> 
   inner_join(vp_sp_mun, by = "NR_ZONA")
 
 # 3. O GRÁFICO DE DISPERSÃO
 ggplot(df_final, aes(x = share_sup, y = tx_abst)) +
   geom_point(color = "steelblue", size = 3, alpha = 0.7) +
   geom_smooth(method = "lm", color = "red", se = FALSE) +
   scale_x_continuous(labels = percent_format()) +
   scale_y_continuous(labels = percent_format()) +
   labs(
     title = "Superior Completo vs. Abstenção (SP 2024)",
     x = "% de Eleitores com Ensino Superior",
     y = "Taxa de Abstenção"
   ) +
   theme_minimal()
 ggsave("D:/Documentos/Márcia/UFABC Mestrado/Métodos Quantitativos/Grafico_Abstencao.png", width = 8, height = 6)
 ggsave("D:/Documentos/Márcia/UFABC Mestrado/Métodos Quantitativos/Grafico_Abstencao.pdf", width = 8, height = 6)
 
 
 ggplot(df_final, aes(x = share_sup, y = tx_branco)) +
   geom_point(color = "darkorange", size = 3, alpha = 0.7) +
   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
   scale_x_continuous(labels = scales::percent_format()) +
   scale_y_continuous(labels = scales::percent_format()) +
   labs(title = "Escolaridade superior (%) x Taxa de votos em branco",
        subtitle = "Município de São Paulo - 2024",
        x = "Proporção de eleitores com ensino superior",
        y = "Taxa de votos em branco") +
   theme_minimal()
 ggsave("D:/Documentos/Márcia/UFABC Mestrado/Métodos Quantitativos/Grafico_Brancos.png", width = 8, height = 6)
 
 
 ggplot(df_final, aes(x = share_sup, y = tx_nulo)) +
   geom_point(color = "darkgreen", alpha = 0.7) +
   geom_smooth(method = "lm", se = FALSE, color = "red") +
   scale_x_continuous(labels = scales::percent_format()) +
   scale_y_continuous(labels = scales::percent_format()) +
   labs(title = "Escolaridade superior (%) x Taxa de votos nulos",
        x = "Proporção de eleitores com ensino superior",
        y = "Taxa de votos nulos") +
   theme_minimal()
 ggsave("D:/Documentos/Márcia/UFABC Mestrado/Métodos Quantitativos/Grafico_Nulos.png", width = 8, height = 6)
 
# Análise dos três gráficos:
 
 # Escolaridade superior versus Taxa de abstenção:
 # Ao analisar o gráfico que compara escolaridade superior com taxa de abstenção, nota-se que, à medida que a proporção de indivíduos
 # com formação acadêmica em nível superior, a taxa de abstenção se reduz, com pontos mais dispersos. Já a maior taxa de abstenção presente
 # entre pessoas com menor escolaridade, sugere que o conhecimento da importãncia de participar ativamente das decisões políticas naõ 
 # é baixa entre estes indivíduos. Em contrapartida, pode indicar que quanto mais alto o nível educacional, maior é a consciência da 
 # relevância de participar do pleito.
 # Escolaridade superior versus Taxa de votos em branco:
 # Na comparação entre a taxa de votos em branco com a proporção de eleitores com nível superior, há indicação de que a porcentagem de
 # de votos em branco é maior dentre os grupos com menores registros de indivíduos com formação superior, estando os pontos mais concentrados
 # na faixa de até 20% de graduados. 
 # Escolaridade superior versus Taxa de votos nulos:
 # Em relação à taxa de votos nulos, estão mais concentrados quando a porcentagem de indivíduos atinge apenas 20% da população.
 # Desta forma, sugere-se que quanto maior o proporção de eleitores com formação acadêmica superior, menor é a taxa de votos nulos.
 # Os três gráficos demonstraram uma correlação negativa forte percebida durante a análise das variáveis.
 # Conectando ao tema Participação Social, pode-se dizer que se a presença da população com melhor formação escolarntenderia a ser mais acentuada
 # na eleições, em um exercício mais consciente de cidadania constitucional.
 # Na busca de causalidade, é necessário observar outras variáveis que podem interferir no processo de participação nas eleições pelos indivíduos, 
 # considerando ainda as variáveis ocultas e informações que colhidas dos indivíduos que podem não estar atualizadas ou não ser corretas (inverídicas).
 