if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(dplyr) == F) install.packages('dplyr'); require(tidyverse)
if(require(ggiraphExtra) == F) install.packages('ggiraphExtra'); require(ggiraphExtra)
if(require(modelr) == F) install.packages('modelr'); require(modelr)

setwd("C:/GitHub/Lista_7/Lista_7/Dados")

library(haven)

fair <- read_dta("fair.dta")

#1. A base de dados fair.dta (disponivel na pasta do OSF) possui variaveis adicionais ao banco de 
#dados sobre voto economico nos EUA. Com ela desenvolva as atividades abaixo e comente seu codigo:
  
  #a) Apresente uma analise descritiva de todas as variaveis da base de dados

summary(fair)


  #b) Apresente um modelo de regrecao linear bivariado no qual VOTE seja variavel dependente e 
#GROWTH variavel independente.
  
# Realizando regressão
reg1 <- lm(VOTE~GROWTH, data = fair)

**I. Descreva as variaveis utilizadas no modelo e sua relacao;**
  
  VOTE e a variavel dependente e GROWTH e a variavel independente. Ha relacao positiva entre as variaveis, ou seja, quanto maior o crescimento economico, maior a quantidade de votos. Analisando Pvalor, ve-se que a hipotese nula e descartada. 

#II. Apresente os resultados do modelo;

# Analisando os resultados da regressao

summary(reg1)

# Plotando grafico

ggplot(fair, aes(VOTE, GROWTH)) + 
  labs(x = "Voto", y ="Crescimento", title = "Gráfico de dispersão") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#III. Avalie o modelo quanto a seu ajuste e sua capacidade explicativa.
  
# Roots RMSE
rmse(reg1, fair)

# Intervalo de confianca
confint(reg1)

# A partir do $R^2$ se observa que a variavel `GROWTH` explica cerca de 36% do modelo. Sendo assim, 
#o modelo e satisfatorio. Alem disso, o RMSE tem um valor, para o modelo, relativamente baixo, o que
#nos mostra que a probabilidade de erro no modelo nao e grande.

# c) Apresente um modelo de regressao multivariada adicionando ao modelo do item a) a variavel 
# GOODNEWS como variavel independente.
  
# Realizando regressao

reg2 <- lm(VOTE~GROWTH + GOODNEWS, data = fair)


#I. Descreva a variavel adicionada ao modelo e sua relacao com as demais;
  
summary(fair$GOODNEWS)

#A variavel `GOODNEWS` no modelo e a variavel independente; Sua adicao ao modelo gera um melhor 
#ajuste e poder de explicacao.


#II. Apresente os resultados do modelo;
  
summary(reg2)

# Preparando dados para plotar

equation1=function(x){coef(reg2)[2]*x+coef(reg2)[1]}
equation2=function(x){coef(reg2)[2]*x+coef(reg2)[1]+coef(reg2)[3]}

# Plotando grafico

ggplot(fair,aes(y = VOTE, x = GROWTH, color = GOODNEWS)) + 
  geom_point() +
  stat_function(fun=equation1,geom="line",
                color=scales::hue_pal()(2)[1]) +
  stat_function(fun=equation2,geom="line",
                color=scales::hue_pal()(2)[2])


# Analisando como os dados se ajustam, percebemos que ha uma relacao positiva entre as variaveis.
# Além disso a variavel good news é valida ao modelo por ter um P valor menor que 0,05

#III. Avalie o modelo quanto a seu ajuste e sua capacidade explicativa. Comente e interprete os 
#coeficientes estimados, os intervalos de confianca do modelo e o RMSE e o $R^2$.**
  
# Intervalo de confianca
confint(reg2)

# Roots MSE
rmse(reg2, fair)

# A partir do $R^2$ se observa que o modelo responde aproximadamente 40.4%, sendo considerado 
# satisfatorio. O RMSE deste modelo e menor que o do modelo anterior, o que nos faz acreditar que a 
# probabilidade de erro e menor. O valor estimado e maior. Temos intervalos de confinça seguros 


# IV. Compare os resultados do item anterior com os adquiridos com o modelo do item b).
  
# O poder explicativo desse novo modelo e maior que o anterior e a probabilidade de erro tornou-se 
# menor. Porem, a variavel good news tem uma significancia menor para o modelo que a variavel 
# crescimento.


# V. Faca uma analise dos residuos ($u_i$), apresente sua media e indicios para concluir se ha ou 
# nao ha homocedasticidade no modelo.
  
mean(residuals(reg2))

plot(reg2)


# Observa-se pela manuntencao de padrao dos dados no grafico dos residuos pelos valores preditos, 
# que o modelo e homocedastico. Da mesma forma, a média dos residuos é baixa. 

# d) Apresente um modelo de regressão multivariada adicionando ao modelo do item b) a variavel WAR 
# como variavel independente.**
  
# Realizando regressao

reg3 <- lm(VOTE~GROWTH + WAR, data = fair)

# I. Descreva a variavel adicionada ao modelo e sua relacao com as demais;**
  
#A variavel guerra tem baixa influencia para o modelo. Estatisticamente e insignificante, com um 
#alto Pvalor de 0.19 e uma estimativa de erro de 2.98, bem maior que o erro do intercepto e da 
#variavel crescimento.

#II. Apresente os resultados do modelo;

summary(reg3)


# III. Avalie o modelo quanto a seu ajuste e sua capacidade explicativa. Comente e interprete os 
# coeficientes estimados, os intervalos de confianca do modelo e o RMSE e o R2.
  
rmse(reg3, fair)

confint(reg3)

# A partir dos resultados que foram pedidos, observamos que o modelo ajustado com a variavel war tem 
# um poder explicativo menor do que o modelo que se utiliza da variavel good news. O RMSE e maior 
# que o modelo anterior, enquanto o R² e menor. Alem disso, O valor estimado e negativo.

#IV. Compare os resultados do item anterior com os adquiridos com o modelo do item c).
  
ggPredict(reg3)

#Analisando os dois modelos, observa-se que, embora a variavel guerra tenha um pequeno poder 
#explicativo ao modelo a partir do R?, estatisticamente e insignificante sua relacao com as 
#outras variaveis. Com a elaboracao do gr?fico, percebe-se que h? dados relacionados a guerra apenas
#em tres casos, e um dos casos e outlier. 


#V. Faca uma analise dos residuos ($u_i$), apresente sua media e indicios para concluir se ha ou nao
#ha homocedasticidade no modelo.**
  
mean(residuals(reg3))

plot(reg3)


#Observa-se que os dados mantem um padrao no grafico que se analisa os residuos e os valores preditos.
#O modelo e homocedastico.

#VI. Qual variavel tem maior efeito descritivo sobre a variavel dependente? Apresente sua analise.
  
#Crescimento e a variavel de maior influencia. Seu poder de explicacao a partir do R² e o maior 
#sobre a questao dos votos em todos os modelos. Sua possibilidade de erro, por consequencia e menor. Alem disso, quando analisado o P valor, essa variavel tem o menor valor, nos mostrando que é significante.


#2. Numa das revistas listadas abaixo, identifique um artigo que apresente resultados de um modelo de
#regressao multivariada e realize as seguintes tarefas:**
  
  #a) Faca a citacao do artigo.
  
#Deutschmann, E., & Minkus, L. (2018). Balan?ando para a esquerda: Opini?o p?blica sobre integra??o
#econ?mica e pol?tica na Am?rica Latina, 1997-2010. Latin American Research Review , 53 (1), 38-56. 
#DOI: http://doi.org/10.25222/larr.250

  #b) Qual a variavel dependente do modelo? Apresente o modelo utilizado no artigo e a justificativa para tal verificando qual a relevancia do uso do modelo para a pergunta de pesquisa do artigo.**
  
#Sao realizadas duas regressoes multivariadas no artigo. A primeira tem como valor dependente apoio a
#integracao economica latino-americana e a segunda, apoio a integração politica latino-americana.
#Temos para a analise de ambos os casos 8 variáveis independentes. Segue trecho do artigo com a 
#explicacao: "De acordo com a pratica comum ( Rodriguez 2014, 66 ), nos o recodificamos em uma 
#variável categorica com as características “esquerda” [0-3], “centro” [4-6] e “direita” [7-10]. O 
#sexo e binario neste conjunto de dados com 1 = feminino e 0 = masculino. A educacao mede o nivel 
#educacional do respondente e consiste em três categorias fundidas: primaria ou analfabeta 
#(“primaria ou secundaria”), secundaria ou incompleta secundaria (“secundaria”) e educação terciaria
#terciaria ou incompleta (“terciaria”). Classe ocupacional e composto por onze categorias: executivo
#de alto nível, profissoes independentes, executivo de nível medio, profissoes assalariadas, 
#funcionario, empresário, trabalhador independente, aposentado, que nao trabalha, dona de casa e 
#estudante. 4 A situacao economica pessoal mede a percepcao de poder suprir as necessidades da 
#familia com a renda do entrevistado e da familia. Os participantes se autoconsideram em uma escala 
#de quatro pontos, de "pode economizar" a "grandes dificuldades". A situacao economica percebida do 
#pais e originalmente uma variavel de cinco pontos com categorias que variam de "muito boa" a "muito
#ruim". que recodificamos nas tres categorias “bom”, “nenhum” e “ruim”, ja que as categorias 
#extremas continham apenas alguns casos. Tambem registramos satisfcao com a democracia em uma 
#variavel ficticia (1 = satisfeito). A idade em anos e contínua e centrada na media." Este modelo 
#ajuda a compreender a piniao publica sobre a integracao economica e politica da America Latina a 
#partir de difentes campos: orientacao sexual, educacional e politica. 

#c) Interprete os resultados do modelo.
  
#Ambas as formas de integracao sao vistas favoravelmente pela maioria dos latino-americanos: os 
#valores variam de 90,7% (1997 Colombia) a 48,6% (2005 Equador) para integracao economica e de 84,6 
#(2002 Nicarágua) a 40,4% (2008 Honduras) para integracao politica com 72,6% e 63,1%, respectivamente
#Em todos os paises, a taxa media de apoio ao longo do tempo e superior a 60% para a integracao 
#economica e superior a 50% para a integracao politica. 