# Marcia de Lima Santos Leite
# Pos-graduacao em Politicas Publicas
# Métodos Quantitativos - UFABC
# Sao Caetano do Sul, 18 de dezembro de 2025.
# Lista 05

install.packages("tidyverse")
set.seed(123)
library(tidyverse)

# 1.1 Gerando a base de dados

# 2 Geracao da base de dados ficticia

n <- 400

dados <- tibble(
  id        = 1:n,
  idade     = round(rnorm(n, mean = 40, sd = 12)),
  sexo      = sample(c("F", "M"), n, replace = TRUE, prob = c(0.55, 0.45)),
  renda     = round(rlnorm(n, meanlog = log(2500), sdlog = 0.5), 0),
  escolarid = sample(c("fundamental", "medio", "superior"),
                     n, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
  ideologia = round(runif(n, 0, 10), 0),   # 0 = esquerda, 10 = direita
  apoio_gov = rbinom(n, 1, plogis(-1 + 0.015*(idade - 40) +
                                    0.4*(sexo == "F") +
                                    0.5*(renda > 3000))),
  satisf_gov = pmin(pmax(
    round(3 + 2*apoio_gov + 0.001*(renda - 2500) +
            rnorm(n, 0, 2), 0), 0), 10),
  
  # 0 = nao participou, 1 = participou de protestos
  
  protesto = rbinom(n, 1, plogis(-2 + 0.3*(ideologia <= 4) - 0.2*apoio_gov))
)

glimpse(dados)
## Rows: 400
## Columns: 9
## $ id         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, ...
## $ idade      <dbl> 33, 37, 59, 41, 42, 61, 46, 25, 32, 35, 55, 44, 45, 41, 33,...
## $ sexo       <chr> "F", "F", "F", "F", "F", "M", "F", "F", "M", "M", "F", "M",...
## $ renda      <dbl> 4277, 2466, 2459, 1171, 3712, 2250, 1800, 1234, 2152, 1635,...
## $ escolarid  <chr> "medio", "medio", "medio", "superior", "superior", "superio...
## $ ideologia  <dbl> 7, 6, 2, 9, 8, 3, 2, 7, 7, 5, 9, 7, 2, 0, 9, 1, 3, 4, 6, 3,...
## $ apoio_gov  <int> 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1,...
## $ satisf_gov <dbl> 4, 4, 2, 0, 3, 2, 7, 6, 6, 6, 7, 4, 9, 4, 6, 0, 2, 6, 4, 8,...
## $ protesto   <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0,...


# 3 - Exercicio 1 - Exploracao descritiva e graficos basicos

# Calcule, para a amostra:

# média, mediana, desvio-padrão e quartis de idade, renda e satisf_gov

meus_dados <- dados
resultado <- list(
  media = mean(meus_dados$idade, na.rm = TRUE), 
  mediana = median(meus_dados$idade, na.rm = TRUE),
  desvio_padrao = sd(meus_dados$idade, na.rm = TRUE),
  quartis = quantile(meus_dados$idade, probs=c(0.25, 0.75), na.rm=TRUE),
  resumo = summary(meus_dados$idade),  
  tabela = table(meus_dados$idade)
  )
print(resultado)

 
  resultado <- list(
    media = mean(meus_dados$renda, na.rm = TRUE), 
                 mediana = median(meus_dados$renda, na.rm = TRUE),
                 desvio_padrao = sd(meus_dados$renda, na.rm = TRUE),
                 quartis = quantile(meus_dados$renda, probs=c(0.25, 0.75), na.rm=TRUE),
                 resumo = summary(meus_dados$renda), 
                 tabela = table(meus_dados$renda)
    )  
print(resultado)    
    
    resultado <- list(
      media = mean(meus_dados$satisf_gov, na.rm = TRUE), 
                   mediana = median(meus_dados$satisf_gov, na.rm = TRUE),
                   desvio_padrao = sd(meus_dados$satisf_gov, na.rm = TRUE),
                   quartis = quantile(meus_dados$satisf_gov, probs=c(0.25, 0.75), na.rm=TRUE),
                   resumo = summary(meus_dados$satisf_gov), 
                   tabela = table(meus_dados$satisf_gov)
      )  
print(resultado)

# proporção de respondentes por categoria de sexo e escolarid

library(dplyr)

resultado <- meus_dados %>%
  count(sexo, escolarid) %>%
  mutate(proporcao = n/sum(n))

print(resultado)

# proporção de respondentes com apoio_gov = 1

resultado <- meus_dados %>%
  filter(apoio_gov == 1) %>%
  summarise(proporcao = n() / nrow(meus_dados))

print(resultado)

# 1.2 Construa os seguintes gráficos
library(ggplot2)

# um histograma de renda (use pelo menos 20 quebras)

ggplot(data = meus_dados, aes(x = renda)) +
  geom_histogram(
    bins = 20,
    fill = "green",
    color = "red"
  ) +
  labs(
    title = "Histograma de renda",
      x= "Renda", 
      y = "Frequência"
  )

# um histograma ou gráfico de barras da distribuição de satisf_gov

ggplot(data = meus_dados, aes(x = satisf_gov)) +
     geom_bar(
        fill = "purple",
         color = "navy"
       ) +
    labs(
         title = "Distribuição de Satisfação com o Governo",
         x= "Satisfação com o Governo", 
         y = "Frequência"
       )

# um gráfico de barras com a distribuição de escolarid

ggplot(data = meus_dados, aes(x = escolarid)) +
  geom_bar(
    fill = "orange",
    color = "pink"
  ) +
 labs(
    title = "Distribuição de Escolaridade",
    x = " Escolaridade",
    y =" Frequência"
  )


# A distribuição de renda parece assimétrica com maior concentração à esquerda de valores menores de 5000,00. 
# Quanto o nível de satisfação com o governo, o gráfico demonstra um equilíbrio entre as opinições, com menor votos em alta satisfação, ocasionando um menor registro de pessoas fortemente satisfeitas.

ggplot(data = meus_dados, aes(x = escolarid, fill = sexo)) +
  geom_bar(
    position = "dodge",
    color = "navy"
  ) +
  labs(
    title = "Comparação de Escolaridade x Sexo",
    x = " Escolaridade",
    y ="Sexo"
  )

# Para poder comparar os dados de sexo e escolaridade, precisei fazer outro gráfico. Nele está demonstrado que o sexo feminino está presente em maior número nos três níveis educacionais,
#fudamental, médio e superior.


# 7 Exercício 2 Escolha de testes estatísticos

  # São variáveis contínuas: satisf_gov, renda, ideologia; as categóricas são sexo, apoio_gov, escolaridade.
  # Teste indicados conforme a comparação sugerida:
  # - sexo(x) x apoio_gov(y) - tabela de contingência e  teste qui-quadrado
  # - escolarid (x) x apoio_gov(y) - tabela de contingência e teste qui-quadrado
  # - satist_gov(x) x apoio_gov(y) - Teste de diferença de médias / ANOVA
  # - satisf_gov(x) x renda(y) - correlação de Pearson ou Spearman
  # - satisf_gov(x) x ideologia(y) - correlação de Pearson ou Spearman

# 8 Exercício 3 Teste qui-quadrado: sexo e apoio ao governo

tab <- table(meus_dados$sexo, meus_dados$apoio_gov)
tab

prop.table(tab, margin = 1)

chisq.test(tab)

# 9 Tabela de contingência simples

tab_sexo_apoio <- table(dados$sexo, dados$apoio_gov)
tab_sexo_apoio
##    
##       0   1
##   F 142  78
##   M 108  72

# 10 Proporções por coluna 

prop.table(tab_sexo_apoio, margin = 2)

chisq.test(tab_sexo_apoio)

# o valor do qui-quadrado = 0.68956;
# os graus de liberdade df = 1 (tabela 2x2);
# o valor-p = 0,4063;
# se rejeita ou não \(H_0\) a 5%: não é possível rejeitar a H0;
# o significado dessa decisão em linguagem substantiva (sem jargão desnecessário): com este reultado, não há evidência suficiente para descartar a hipótese nula.
# Ou seja, pela estatística, não se encontrou a dependência entre o apoio do governo e o sexo na amostra utilizada no teste.

# Construa um gráfico de barras com a distribuição de apoio_gov dentro de cada categoria de sexo (use, por exemplo, position = "fill"):

ggplot(dados, aes(x = sexo, fill = factor(apoio_gov))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Apoio ao governo")

# Os resultados do qui-quadrado mostram que não existe diferença estatisticamente significativa com x-quadrado= 0,68956 e um p_valor = 0.4063. No gráfico as diferenças também são muito próximas visualmente entre os homens e mulheres que apoiam e os que não apoiam.
# Portanto, o padrão visual é coerente com o resultado do teste.


# 11 Exercício 4 – Diferença de médias: renda entre apoiadores e não apoiadores

# 4.1 

dados %>%
  group_by(apoio_gov) %>%
  summarise(
    media_renda = mean(renda, na.rm = TRUE),
    sd_renda    = sd(renda, na.rm = TRUE),
    n           = n()
  )
# \(H_0: \mu_{\text{apoia}} = \mu_{\text{não apoia}}\)
# \(H_1: \mu_{\text{apoia}} \neq \mu_{\text{não apoia}}\)

t.test(renda ~ apoio_gov, data = dados)

# A interpretação sugere que:
 # o intervalo de confiança se encontra entre -576,66 e +70,41
 # o p-valor = 0,1247
 # H_0 com o valor de p-valor >0,05 indica que não seja rejeitada a hipótese nula
 # a estimativa da diferença das médias entre o grupo dos apoiadores e o dos não apoiadores é de aproximadamente 253, que tem baixa significância estatística

# Construção de um gráfico boxplot

ggplot(dados, aes(x = factor(apoio_gov), y = renda)) +
  geom_boxplot( fill = "purple") +
  labs(title = "Boxplot de renda por apoio ao governo",
       x = "factor (apoio ao governo)", 
       y = "renda"
       )

# 12 Exercício 5 – Correlação: renda, ideologia e satisfação com o governo

# 5.1. Calcule a matriz de correlações de Pearson entre renda, ideologia e satisf_gov (use apenas casos completos):

dados %>%
  select(renda, ideologia, satisf_gov) %>%
  cor(use = "complete.obs")
##                 renda   ideologia  satisf_gov
## renda      1.00000000  0.02053989  0.59336969
## ideologia  0.02053989  1.00000000 -0.07964692
## satisf_gov 0.59336969 -0.07964692  1.00000000

dados %>% 
  select(where(is.numeric)) %>% 
  cor(use = "complete.obs")
# Esta sintaxe foi modificada com auxílio da IA, que explicou a mudança para garantir o uso de todas as variáveis numéricas disponíveis, não usando as que estiverem em texto.

dados %>% 
  select(renda, ideologia, satisf_gov) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  cor(use = "complete.obs")
# Também sugeriu de usar outra sintaxe, para tornar variáveis para numéricas, se houver possibilidade.

# 5.2. Para cada par de variáveis, com base na matriz:

# - indique o sinal da correlação (positivo/negativo);

# - comente, de forma qualitativa, a força da associação (fraca, moderada, forte), usando como referência aproximada:
  
 # |r| ≈ 0.1 (fraco),
 # |r| ≈ 0.3 (moderado),
 # |r| ≥ 0.5 (mais forte).

#            renda   ideologia  satisf_gov
# renda      1.00000000  0.02053989  0.59336969
# ideologia  0.02053989  1.00000000 -0.07964692
# satisf_gov 0.59336969 -0.07964692  1.00000000

# O sinal da correlação positivo indica que quando uma variável aumenta, outra também poderá aumentar; já o sinal negativo demonstra que ao aumentar uma váriavel, a outra poderá diminuir
# Renda com ideologia =  0.02053989 < 0,1 -> é associação fraca e positiva
# Renda com satisfação com o governo = 0.59336969 > 0,5 -> é uma associação forte e positiva
# Ideologia com satisfação com o governo = -0.07964692 < 0,1 -> é uma associação fraca e negativa

# 5.3 Gráfico de dispersão de renda (eixo x) por satisf_gov (eixo y), com linha de tendência linear

ggplot(dados, aes(x = renda, y = satisf_gov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE)

# Pelo gráfico, observa-se que quanto maior a renda, maior a satisfação com o governo. A dispersão dos pontos demonstra que quanto menor a renda, menor a satisfação, observando-se mais outiliers também nesta faixa de renda.

# 13 Exercício 6 – Regressão linear simples: satisfação e renda

# Considere o modelo:

# [ _i = _0 + _1 ,_i + _i]

# Estimar o modelo em R:

mod1 <- lm(satisf_gov ~ renda, data = dados)
summary(mod1)
## 
## Call:
 lm(formula = satisf_gov ~ renda, data = dados)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7149 -1.6354 -0.1648  1.5664  5.6727 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.346e+00  2.267e-01   5.938  6.3e-09 ***
## renda       1.017e-03  6.917e-05  14.706  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.195 on 398 degrees of freedom
## Multiple R-squared:  0.3521, Adjusted R-squared:  0.3505 
## F-statistic: 216.3 on 1 and 398 DF,  p-value: < 2.2e-16
 
# Interpretação dos coeficientes 
 # Intercepto: o ponto de intercessão da satisfação com o governo pode ser de 1.346 quando a renda = 0, funcionando como um ponto de partida. 
 # Coeficiente de renda: com um valor positivo, pode-se indicar que quanto maior a renda, maior o índice de satisfação com o governo na amostra.
 # p-valor com renda: 2.2e-16 << 0.05 (valor de Fisher), indica que há grande associação linear entre a renda e a satisfação com o governo, com grande significância estatística.

 install.packages("broom")
 library (broom)
  tidy(mod1)
  
# 6.3. Utilize o broom para obter uma tabela organizada:
  
#  Analise o output e conecte: 
    
 # estimativas;
 # erros-padrão;
 # estatísticas \(t\);
 # valores-p.  
  
summary(mod1)  

# Para responder, precisei rodar o 'summary(mod1) e visualizar quais eram as correspondências entre os dois tipos de tabelas construídas.
# Summary(mod1)                                Tidy(mod1)             Explicação
# Estimate                                     estimate               São os valores estimados dos coeficientes
# Std. Error                                   std. error             Indica a variação da estimativa
# t value                                      statistic              Comparação entre a estimativa e o erro, ajuda a saber se a evidência é forte ou fraca
# Pr(>|t|)                                     p.value                Demosntra o nível de confiança estatística do valor obtido

# 6.4. Refaça o gráfico de dispersão com reta de regressão (como no Exercício 5) e escreva um parágrafo relacionando:

# o gráfico;
# o sinal de \(\hat{\beta}_1\);
# o valor e significância de \(\hat{\beta}_1\).

ggplot(dados, aes(x = renda, y = satisf_gov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "red")

# O gráfico demosntra um reta ascendente que indica que quanto maior a renda maior o nível de satisfação, há menor número de pessoas com mais renda.
# O coeficiente de inclinação da reta é ascendente da esquerda para a direita, portanto é uma inclinação positiva (\(hat{\beta}_1\))
# O p-valor é 6.30e- 9, ou seja, ultra baixo, portanto a relação entre renda e satisfação com o governo indica uma associação válida estatisticamente.

# 14 Exercício 7 – Diagnóstico simples do modelo

# Usando o modelo do Exercício 6 (mod1):

# 7.1. Extraia resíduos e valores ajustados:

 dados_diag <- augment(mod1)
  glimpse(dados_diag)

  # 7.2. Construa:
  
  # um gráfico de resíduos vs valores ajustados (.resid vs .fitted);
  
  ggplot(dados_diag, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(x = "Valores ajustados", y = "Resíduos",
         title = "Resíduos vs Valores ajustados")
  
  # um gráfico QQ-plot dos resíduos.  
  
  ggplot(dados_diag, aes(sample = .resid)) +
    stat_qq(alpha = 0.4) +
    stat_qq_line(color = "red") +
    labs(title = "QQ-plot dos resíduos",
         x = "Quantis teóricos",
         y = "Quantis dos resíduos")
  
  # 7.3. Em texto, com base nos gráficos, comente:
  
  # a. se há indícios fortes de heterocedasticidade (padrão em funil, por exemplo): acredito que não há fortes indícios de heterocedasticidade, por estarem bem distribuídos próximos à lina zero;
  # b. se a distribuição dos resíduos parece muito distante de algo aproximadamente normal: os pontos estão bem próximos à linha vermelha, não parecendo distante de algo aproximadamente normal.
  
  # 7.4
  # O gráfico de resíduos vs valores ajustados ajuda a visualizar se os pontos estão dispersos aleatoriamente ou se formam algum desenho ou padrão. A aleatoriedade é o modo ideal.
  # O gráfico QQ-plot é imortante para comparar se os pontos estão mais próximos à linha reta, o que também indica que os erros dos modelos estão dentro do esperado.
  # Se estiverem dentro dos padrões esperados , pode-se dizer que o modelo é confiável.

# 15 Resíduos vs ajustados
  library(broom)
  mod1 <- lm(ideb ~ gasto_aluno, data = dados_educacao)
  dados_diag <- augment(mod1)
  
  ggplot(dados_diag, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  ggplot(dados_diag, aes(x = gasto_aluno, y = .resid)) + 
    geom_point(alpha = 0.4) + 
    geom_hline(yintercept = 0, linetype = "dashed")
  
# 16 QQ-plot
  ggplot(dados_diag, aes(sample = .resid)) +
    stat_qq() +
    stat_qq_line()
  # Heterocedasticidade: naõ há formação do padrão funil, sendo que os pontos se distribuem ao longo da linha de referência da normalidade.
  # Os pontos estão em sua grande maioria distribuídos sobre a linha de normalidade, indicando que os resíduos se encontram em nível aceitável.

# 17 Exercício 8 – Regressão com variável dummy e diferença de médias
  
  # Considere o modelo:
  
  #[ _i = _0 + _1 ,_i + _i]
  
  # onde apoio_gov é uma variável indicadora (0 = não apoia; 1 = apoia).
  
  # 8.1. Estime o modelo:
  
  mod2 <- lm(satisf_gov ~ apoio_gov, data = dados)
  summary(mod2)
  
  # 8.2. Calcule as médias de satisf_gov nos dois grupos (apoio_gov = 0 e apoio_gov = 1) e compare com os coeficientes de mod2.
  
  dados %>% 
    group_by(apoio_gov) %>% 
    summarise(media_satisf = mean(satisf_gov, na.rm = TRUE))
  
  
# 8.3. Mostre, com base nos resultados numéricos, que:
    
  # a.\(\hat{\beta}_0\) corresponde à média de satisf_gov para apoio_gov = 0;
  # b. \(\hat{\beta}_1\) corresponde à diferença entre as médias dos dois grupos.
  
  # Este exercício foi resolvido com auxílio da IA.
  
  # Coeficientes do modelo
  coefs <- coef(mod2)
  beta0 <- coefs[1]           # intercepto
  beta1 <- coefs[2]           # coeficiente da dummy apoio_gov
  
  # Médias por grupo
  medias <- dados %>%
    group_by(apoio_gov) %>%
    summarise(media_satisf = mean(satisf_gov, na.rm = TRUE), .groups = "drop")
  
  media_0 <- medias$media_satisf[medias$apoio_gov == 0]
  media_1 <- medias$media_satisf[medias$apoio_gov == 1]
  
  # Comparações
  c(beta0 = beta0,
    media_0 = media_0,
    beta0_mais_beta1 = beta0 + beta1,
    media_1 = media_1,
    beta1 = beta1,
    diferenca_medias = media_1 - media_0)
  
  # Opcional: verificar igualdade numérica (com tolerância)
  all.equal(beta0, media_0)
  all.equal(beta0 + beta1, media_1)
  all.equal(beta1, media_1 - media_0)
  
# 8.4. Aplique:
  
  t.test(satisf_gov ~ apoio_gov, data = dados)
  
# Compare o valor-p de apoio_gov no summary(mod2) com o valor-p do t.test. Em texto, discuta a relação entre os dois resultados.
  # Os resultados se repetiram no summary(mod2) e no t.test, indicando a mesma hipótese sobre a diferença entre as médias de satisfação dos dois grupos.Por tanto, o resultado estatístico é o mesmo.
  
# 18 Exercício 9 – Desafio final 
  
  # Estudo a relação entre ideologia e satisfação com o governo, sendo que:
  
    # pessoas com renda mais alta podem avaliar o governo de forma diferente;
    # apoiadores do governo tendem, em média, a ser mais satisfeitos.
  
  # Construa um gráfico de dispersão de ideologia (eixo x) por satisf_gov (eixo y):
  
  ggplot(dados, aes(x = ideologia, y = satisf_gov, color = factor(apoio_gov))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Ideologia",
         y = "Satisfação com o governo",
         color = "Apoio ao governo",
         title = "Dispersão de ideologia vs satisfação com o governo") +
   theme_classic() 
  
  # Este gráfico produziu uma imagem difícil de visualizar onde estão os apoiadores e os não apoiadores do governo segundo a ideologia, pois os pontos têm variações de cores, ou seja, azuis mais claros ou escuros e vermelhos mais claros ou escuros.
  # Assim, não é possível diferenciar os grupos e a análise fica prejudicada.
  
  # Estime os seguintes modelos:
  
  #  Modelo A (simples):
    # [ _i = _0 + _1 ,_i + u_i]
  
  modA <- lm(satisf_gov ~ ideologia, data = dados) 
    summary(modA)
    resid(modA)

    plot(modA)
    
# Ao observar o gráfico, não há variação significativa nos resíduos do modelo em relação aos valores ajustados.
# Desta forma, o modelo apresenta bom ajuste aos dados.
    
  # Modelo B (com mais estrutura):
    # [ _i = _0 + _1 ,_i + _2 ,_i + _3 ,_i + e_i]
    
  modB <- lm(satisf_gov ~ ideologia + apoio_gov + renda, data = dados) 
    summary(modB)
    resid(modB)
    plot(modB)
  
  #  Implemente em R, examine os e:
    
    # a. interprete sinal e significado substantivo do coeficiente de ideologia em cada modelo
      # No modelo A, o valor estimado em relação à ideologia ser igual à -0,07471, indica que quanto mais à direita, menor é a satisfação com o governo.
      #  o valor de 0,112 para o coeficiente de ideologia tem significância estatística baixa, tornando inconclusivo o fato da influência da ideologia ser agente da satisfação ou não com o governo.
      # No modelo B, tendo um coeficiente de ideologia no valor de 0.143204, com valor pouco significativo por ser bem próximo a zero, não havendo ação significativa sobre o índice de satisfação com o governo.
    
    # b. comente os valores-p de ideologia: há evidência de associação? Em que direção?
    # Para compreender a questão, utilizei a IA
 
    #  Há evidência de associação? → depende do valor‑p.
      #  Se < 0.05 → associação significativa.
      #  Se > 0.05 → não há evidência estatística suficiente.
    # No modelo A, o p_valor é -0,07471, ou seja, menor que 0,05, havendo associação significativa.
    # No modelo B, o p_valor é 0,143204, maior que 0.05, indicando que, avaliando por este índice, não há evidência estatística suficiente.
    
    # Em que direção? → depende do sinal do coeficiente (Estimate).
     # Negativo → mais à direita → menor satisfação.
     # Positivo → mais à direita → maior satisfação.
    # O valor do coeficiente estimado em ideologia do modelo B é -4.999e-02, valor negativo, com direção mais à direita com menor satisfação.
    # De modo semelhante, o coeficiente no modelo A também é negativo: mais à direita - menor satisfação com o governo.
    
    # c. discuta se a inclusão de apoio_gov e renda altera muito o coeficiente de ideologia (valor e significância).
    # Sobre o coeficiente de ideologia, quando comparado ao modelo A sem a adição das variáveis apoio_gov e renda e o modelo B com as duas inclusas, pode-se concluir que a inclusão de outras variáveis, como as citadas anteriormente interferem estatisticamente. Há uma evidente dependência da ação de outras variáveis.
    # Os valores de p_valor e valor estimado diminuem fortemente no modelo B, não influenciando na satisf_gov. 
    
# 18.3 9.3. Diagnóstico básico do Modelo B
    # Produza diagnósticos simples para o Modelo B:    
    
 # gráfico de resíduos vs valores ajustados
  
    dados$ajustados <- fitted(modB) 
    dados$residuos <- resid(modB) 
    
    ggplot(dados, aes(x = ajustados, y = residuos)) + 
      geom_point(color = "chocolate") + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "gold") + 
      
      labs(
        x = "Valores ajustados", 
        y = "Resíduos", 
         title = "Resíduos vs Valores Ajustados (Modelo B)") + 
      theme_minimal()
    
 # QQ-plot de resíduos(este gráfico foi construído com IA)
    
    ggplot(data.frame(residuos = dados$residuos), aes(sample = residuos)) +
      stat_qq(color = "springgreen4") + 
      stat_qq_line(color = "mediumorchid") + 
        labs(title = "QQ-plot dos resíduos (Modelo B)") + 
      theme_minimal()

  ?stat_qq  
  ?stat_qq_line 
    
    ggplot(data.frame(residuos = dados$residuos), aes(sample = residuos)) +
      geom_qq(color = "springgreen4") + 
      geom_qq_line(color = "mediumorchid") + 
      labs(title = "QQ-plot dos resíduos (Modelo B)") + 
      theme_minimal()
    
    #Perguntei se havia outra forma de montar a sintaxe, e a indicação da IA foi de um comando que eu já conhecia 'geom'. Ficou mais fácil assim.
    
    
 # algum gráfico que compare valores observados de satisf_gov e valores previstos (por exemplo, em função da ideologia, para apoio_gov fixo em 0 ou 1)
    
    # satif_gov vs apoio_gov, fixo em 0 ou 1 (com auxílio da IA)
    
    # 1. Criar grade de valores simulados; fixa renda na média
    novo <- crossing(
      ideologia = seq(min(dados$ideologia), max(dados$ideologia), length.out = 100),
      apoio_gov = c(0, 1),
      renda = mean(dados$renda, na.rm = TRUE)
    )
    
    # 2. Calcular valores previstos pelo modelo
    novo$previstos <- predict(modB, newdata = novo)
    
    # 3. Construir gráfico
    ggplot(dados, aes(x = ideologia, y = satisf_gov, color = factor(apoio_gov))) +
      geom_point(alpha = 0.6) +  # pontos observados
      geom_line(data = novo, aes(x = ideologia, y = previstos, color = factor(apoio_gov)),
                linewidth = 1) +      # linhas previstas
      labs(x = "Ideologia", y = "Satisfação observada / prevista",
           color = "Apoio ao governo",
           title = "Valores observados vs previstos (Modelo B)") +
      theme_minimal()
    
    
 # Comente, em texto, se há alguma evidência clara de problemas sérios de especificação ou de ajuste. Observações feitas para cada gráfico.
    # Gráfico de resíduos x valores ajustados: pela imagem formada, não nenhum formação de padrão, pois os pontos estão distribuídos aleatóriamente próximo à linha 0.
    # Gráfico QQ-plot dos resíduos: os pontos estão distribuídos proximos à linha de referência. Os resultados obtidos do p-valor e do intervalo de confiança se mostraram válidos e confiáveis.
    # Gráfico de valores observados x previstos: nesta relação, houve confirmação da dependência entre as variáveis, em uma visão estatística dos modelos.
    
  # 18.4 9.4. Síntese interpretativa (texto)
    # Escreva uma síntese (cerca de 10–15 linhas) respondendo:
      
      #O que aprendemos, com esses modelos e gráficos, sobre a relação entre ideologia e satisfação com o governo nesta amostra?
      
      #Em que medida essa análise é compatível com uma interpretação causal?
      
      #Quais são as principais limitações do exercício?
      
      #A pontuação extra será atribuída pela qualidade da argumentação substantiva, pela clareza da escrita e pelo uso consistente dos conceitos trabalhados ao longo do curso (estatística descritiva, testes de hipóteses, regressão e diagnóstico).  
    
    # A partir do modelo A, em que há uma comparação entre a ideologia política e a satisfação com o governo, pode-se dizer que, os indivíduos que aprovam o governo vigente apresentam maior concordância com a linha de pensamento dos líderes.
    # Já os indivíduos que discordam do modo de governar (ideologia contrária), mostram-se insatisfeitos com o governo.
    # Em se tratando da distribuição dos pontos no gráfico que compara resíduos versus valores ajustados, há uma aleatoriedade, que demonstra
    # que o modelo está adequado, sem apresentar problemas. Além disso, ao comparar os valores previstos com os observados,a relação entre as variáveis 
    # obedecem um padrão normal. Os erros que apareceram estão dentro da linha aceitável (há poucos outliers). 
    # Uma das conclusões que se pode tomar é que há uma dependência estatística entre as variáveis 'ideologia' e satisf_gov'.
    # No entanto, são necessárias mais evidências de que exista uma causalidade.
    # Para que seja possível levantar mais evidências da causa-efeito, torna-se necessário buscar outras variáveis, como as de controle, as omitidas, até as de confusão.
    # O pesquisador deve sempre compreender que não pode se limitar às causas mais superficiais. Os gráficos são instrumentos que podem nortear as análises, e consequentemente, 
    # direcionar os passos seguintes da investigação de fatores causadores de determinadas situações.
    # Além disso, os gráficos produzem elementos descritivos, mas não é possível tirar conclusões apenas com as informações
    # eles geraram. É preciso buscar mais elementos que comprovem (ou não) o que está sendo levantado.
    # Os dados com que se trabalham e produzem tabelas e gráficos são muito importantes para visualizar tendências, mas não produzem inferências causais.