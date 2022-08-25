
# Banco de Dados

# O banco de dados escolhido pelo grupo foi o disponibilizado pela empresa **StatsBomb** no seu site através do seguinte endereço **https://statsbomb.com/what-we-do/hub/free-data/**.  Porém a própria empresa disponibiliza um pacote no R que quando baixado contém esses datasets para serem utilizados e dentre estes se encontram todas as partidas da Copa do Mundo de 2018.
# Assim através do pacote StatsBombR conseguimos obter um dataset com algumas informações a respeito das seleções da Copa do Mundo de 2018. Porém para o modelo fizemos uma seleção de variáveis e ainda escolhemos analisar somente os times que passaram para as Oitavas de final adiante.

library(ggplot2)
library(knitr)
library(ggfortify)
library(HistData)
library(plotly)
library(tidyr)
library(pastecs)
library(gridExtra)
library(lmtest)
library(nortest)
library(GGally)

# Carregando dataframe
copa_por_jogo_oitavas = read.csv('copa_por_jogo_oitavas.csv')

# Análise Descritiva

# Como sabemos a estatística descritiva é a etapa inicial da análise utilizada para descrever e resumir os dados, através dela podemos ter uma ideia geral sobre algumas conclusões a respeito dos nossos dados.
# Primeiramente, identificaremos se há presença de números ausentes.

# Percebemos que não temos a presença de nenhum NA.

# A seguir, temos uma tabela que contém algumas estatísticas sobre as nossas variáveis.

# A Copa do Mundo é um torneio de formato mata-mata a paritir das Oitavas-de-final e portanto sabemos que cada seleção jogaram pelo menos 4 partidas (3 da fase de grupos) e portanto junto das estatísticas acima, temos que algumas conclusões:

# A seleção que em média mais chutou em gol por partida foi a brasileira com uma média de 20,6 chutes por jogo e em compensação o Japão teve a menor média de apenas 11 chutes por jogo;

# O Brasil foi quem mais chutou, porém quem fez mais gols em média por partida foram os donos da casa, os russos tiveram uma média de 3,4 gols por jogo.

# Já sobre os passes temos uma outra seleção se destacando, a Espanha com seu estilo de jogo de posse de bola obteve uma média de 876,5 passes por jogo com uma outra média de 783,5 passes certos. Porém percebemos que com uma tática diferente, os suecos se destacam por trocarem poucos passes em comparação com as outras seleções, uma média de apenas 333,8 passes por jogo, menos da metade do que seleção espanhola efetua.

# Antes de analisarmos as outras variáveis do nosso banco, temos que falar sobre as faltas e dentre elas, temos que a seleção que mais ganhou faltas a seu favor foi a Argentina enquanto quem mais cometeram foram os russos.


# Percebemos que juntamente das mesmas seleções Espanha e Suécia que efetuam respectivamente, mais e menos passes, temos que elas são respectivamente, a que mais carrega a bola e a que menos carrega a bola. Porém, a equipe que mais perde o controle da bola é a colombiana dentre todas as outras seleções.

# Para entendermos um pouco melhor a tática da equipe sueca percebemos que junta dos russos, elas são as equipes que mais ocasionam situações de pressão aos seus adversários. Já o país que mais tem o drible como ferramenta é o México que consta com uma média de 27,25 dribles por partida.

# Novamente os espanhóis voltam a aparecer quando eles dominam os dados de passe para dentro da área e passes certos para dentro da área, ou seja, dentre os seus muitos passes vários são aqueles que se direcionam para a grande área. Porém, o que evidencia o estilo de jogo dessa seleção é quando comparamos essa média de passes para dentro da área com a média de passes totais por jogo, ou seja, temos que apenas 5% dos passes efetuadas pela equipe da Espanha são para a grande área.

# Em comparação, o Brasil que tem uma média de 48 passes para dentro da área, quase um passe a menos que a Espanha, tem uma porcentagem de cerca de 8,2% e troca quase 300 passes a menos que os espanhóis. A diferenças de estilo se evidencia quando claramente percebemos que a Suécia mesmo com a menor média de passes trocados apresenta uma porcentagem de 9,1% passes para a grande área.

# Para exemplificar melhor algumas dessas variáveis que comentamos acima, temos os seguintes gráficos:

a = ggplot(data = copa_por_jogo_oitavas,
       aes(x = reorder(team.name, shots), y = shots, fill = team.name)) +
  geom_bar(stat = "identity",width = 0.7) +
  labs(y="Chutes") +
  theme(axis.title.y = element_blank()) +  
  scale_y_continuous(expand = c(0,0)) + 
  theme(legend.position="none") + 
  coord_flip() 
b = ggplot(data = copa_por_jogo_oitavas,
       aes(x = reorder(team.name, goals), y = goals, fill = team.name)) +
  geom_bar(stat = "identity",width = 0.7) +
  labs(y="Gols") +
  theme(axis.title.y = element_blank()) +  
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.position="none") +
  coord_flip() 
c = ggplot(data = copa_por_jogo_oitavas,
       aes(x = reorder(team.name, passes), y = passes,  fill = team.name)) +
  geom_bar(stat = "identity",width = 0.7) +
  labs(y="Passes") +
  theme(axis.title.y = element_blank()) +  
  scale_y_continuous(expand = c(0,0)) + 
  theme(legend.position="none") + 
  coord_flip() 
d = ggplot(data = copa_por_jogo_oitavas,
       aes(x = reorder(team.name, pressure), y = pressure,  fill = team.name)) +
  geom_bar(stat = "identity",width = 0.7) +
  labs(y="Pressão") +
  theme(axis.title.y = element_blank()) +  
  scale_y_continuous(expand = c(0,0)) + 
  theme(legend.position="none") +
  coord_flip() 
f = ggplot(data = copa_por_jogo_oitavas,
       aes(x = reorder(team.name, passes_na_area), y = passes_na_area,  fill = team.name)) +
  geom_bar(stat = "identity",width = 0.7) +
  labs(y="Passes na Área") +
  theme(axis.title.y = element_blank()) +  
  scale_y_continuous(expand = c(0,0)) + 
  theme(legend.position="none") +
  coord_flip() 
e = ggplot(data = copa_por_jogo_oitavas,
       aes(x = reorder(team.name, dribble), y = dribble,  fill = team.name)) +
  geom_bar(stat = "identity",width = 0.7) +
  labs(y="Drible") +
  theme(axis.title.y = element_blank()) +  
  scale_y_continuous(expand = c(0,0)) + 
  theme(legend.position="none") +
  coord_flip() 
grid.arrange(a,b,c,d,e,f , ncol = 3)


# Abaixo, ainda encontramos os gráficos de densidades calculados para cada uma das variáveis, onde podemos perceber como elas se comportam e perceber que algumas variáveis como os passes e passes completos parecem ter os seus gráficos similares.

a = ggplot(copa_por_jogo_oitavas, mapping = aes(x = shots)) + 
  geom_density(alpha = 0.3) 
b = ggplot(copa_por_jogo_oitavas, mapping = aes(x = goals)) + 
  geom_density(alpha = 0.3) 
c = ggplot(copa_por_jogo_oitavas, mapping = aes(x = passes)) + 
  geom_density(alpha = 0.3) 
d = ggplot(copa_por_jogo_oitavas, mapping = aes(x = passes_completes)) + 
  geom_density(alpha = 0.3) 
e = ggplot(copa_por_jogo_oitavas, mapping = aes(x = foul_won)) + 
  geom_density(alpha = 0.3) 
f = ggplot(copa_por_jogo_oitavas, mapping = aes(x = foul_commited)) + 
  geom_density(alpha = 0.3) 
g = ggplot(copa_por_jogo_oitavas, mapping = aes(x = carry)) + 
  geom_density(alpha = 0.3) 
h = ggplot(copa_por_jogo_oitavas, mapping = aes(x = miscontrol)) + 
  geom_density(alpha = 0.3)
i = ggplot(copa_por_jogo_oitavas, mapping = aes(x = pressure)) + 
  geom_density(alpha = 0.3) 
j = ggplot(copa_por_jogo_oitavas, mapping = aes(x = dribble)) + 
  geom_density(alpha = 0.3) 
k = ggplot(copa_por_jogo_oitavas, mapping = aes(x = passes_na_area)) + 
  geom_density(alpha = 0.3) 
grid.arrange(a,b,c,d,e,f,g,h,i,j,k , ncol = 3)

# Selecionando variáveis

# Primeiro, fazemos um modelo com todas as variáveis, para observar como ele se comportaria. 

modelo_copa <- lm(goals~shots++passes+passes_completes+foul_won+
                    foul_commited+carry+miscontrol+pressure+dribble+passes_na_area
                    , data=copa_por_jogo_oitavas)
modelo_copa
summary(modelo_copa)

# A partir do resultado, podemos identificar que ele é, de certa forma, ineficaz, pois tem um Coeficiente de determinação de cerca de 58%. Para melhorar isso, utilizamos da estratégia de "seleção de variáveis", onde selecionamos apenas algumas variáveis que melhor se adequem ao nosso objetivo.

# Neste trabalho, optamos pelo método de seleção passo a passo (stepwise), na direção um passo a trás (backward), ou seja, o algoritmo vai testando as possibilidades uma por uma, e, a cada passo, uma variável é eliminada do modelo.

# Como critério de escolha das variáveis, é utilizado o Critério de Informação de Akaike (AIC), uma métrica que mensura a qualidade e a parcimonia do modelo, buscando a maior eficácia possível com o menor número de variáveis.

regressao <- step(modelo_copa, direction = 'backward')


# Após utilizar este método, percebemos que 4 variáveis são úteis para a melhor explicação do modelo, são elas:  Chutes, Pressão, Passes completos e Passes.

# Com a seleção de variáveis realizada, podemos, agora, com o modelo ajustado, ver se essa estratégia fez efeito na eficácia do modelo.

# Modelo
modelo_adj <- lm(goals~ pressure+ shots+
                 passes_completes+ passes, data=copa_por_jogo_oitavas)
summary(modelo_adj)


# Observando o novo modelo, percebemos que o Coeficiente de determinação saltou para 75%, uma melhora significativa na explicação de nossos dados.

# Além disso, podemos interpretar os nossos betas estimados do modelo, de forma que o intercepto não terá sentido prático, pois não existe a possibilidade de termos uma média de gols onde as outras varíaveis sejam nulas. Para os outros coeficientes temos que eles representam a variação esperada em Y (goals) quando a variável do coeficiente aumenta em uma unidade e as demais são mantidas fixas.

# Ainda, vale ressaltar que podemos obter um intervalo de confiança para os betas:

confint(modelo_adj)

# Análise de Resíduos

# Para a análise de resíduos, observamos a normalidade, a homoscedasticidade e os pontos influentes.

diagplot = autoplot(modelo_adj, which = 1:6, label.size = 3)
diagplot

# Normalidade

# Primeiramente, é feita a análise de normalidade.

i = diagplot[[2]]
ggplotly(i)

library(nortest)
library(lmtest)
norm.test.stat = c(shapiro.test(modelo_adj$residuals)$statistic, 
                   ks.test(modelo_adj$residuals, "pnorm")$statistic, 
                   ad.test(modelo_adj$residuals)$statistic, 
                   cvm.test(modelo_adj$residuals)$statistic)
norm.test.pvalue = c(shapiro.test(modelo_adj$residuals)$p.value, 
                     ks.test(modelo_adj$residuals, "pnorm")$p.value, 
                     ad.test(modelo_adj$residuals)$p.value, 
                     cvm.test(modelo_adj$residuals)$p.value)
norm.test = cbind(norm.test.stat, norm.test.pvalue)
rownames(norm.test) = c("Shapiro-Wilk", "Kolmogorov-Smirnov", 
                        "Anderson-Darling", "Cramer-Von-Mises")
colnames(norm.test) = c("Statistic", "P.value")
kable(norm.test, align = "c", caption = "Testes de normalidade")

# Após analisar a normalidade, pelo gráfico, é possível visualizar três pontos destacados: 

-Portugal(11);

-Rússia(12);

-Suécia(14).

# É importante destacá-los, porém, para a análise, esses pontos não foram relevantes para rejeitar a normalidade. Rússia e Suécia, os pontos nos extremos, estão distantes do restante das seleções, mas seguem a reta de normalidade.
# Para os testes, com 5% de confiança, todos aceitaram a normalidade. Kolmogorov-Smirnov apresentou um p-valor baixo, porém não suficiente para rejeitar o teste.
# Portanto, considero que os resíduos seguem uma distribuição normal.

# Homocedasticidade

# É feita a análise da Homocedasticidade.

h = ggplot(mapping = aes(x = modelo_adj$residuals)) + geom_histogram() + 
  xlab("Residuals") + ylab(" ")
ggplotly(h)


j = diagplot[[1]]
ggplotly(j)

homo.test = cbind(bptest(modelo_adj, studentize = FALSE)$statistic, 
                  bptest(modelo_adj, studentize = FALSE)$p.value)
rownames(homo.test) = c("Breusch-Pagan")
colnames(homo.test) = c("Statistic", "P.value")
kable(homo.test, align = "c", caption = "Teste de homocedasticidade")

# Para a homocedasticidade, é possível visualizar, alguns pontos destacados. Novamente, eles não influenciam no resultado. Tais pontos são os mesmos que foram destacados na normalidade.
# Com o teste de Breusch-Pagan, não rejeito a homocedasticidade.
# Portanto, considero que os resíduos são homocedasticos.

# Pontos destacados no modelo

# É feita a análise dos pontos.

k = diagplot[[4]]
ggplotly(k)

# Para os pontos influentes, observados pela distância de Cook, posso dizer que são eles: 

- 14 (Suécia);
- 12 (Rússia); 
- 9 (Japão).

m = diagplot[[3]]
m

# Para os outliers, observados pelo gráfico acima, posso dizer que são eles: 

- 14 (Suécia);

- 12 (Rússia);

- 11 (Portugal).

i = diagplot[[5]]
ggplotly(i)


# Para os pontos de alavanca, observados pelo gráfico acima, posso dizer que são eles: 

- 14 (Suécia);

- 12 (Rússia).

# Predição

# A nossa equação nos dá o valor predito da média de gols por jogo da equipe dado o número médio de pressão, chutes, passes completos e passes da equipe por jogo. Abaixo, podemos ver como ela ficou:

# Y=-10.01072 + 0.06917X_1 + 0.04376X_2 - 0.04056X_3 + 0.02831X_4

# Como exemplo, vamos tentar predizer o número médio de 4 das seleções que jogaram as oitavas de final: Argentina, Brasil, Croácia e França. Foram selecionadas essas 4 pelos seguintes critérios: as duas melhores seleções do continente em que vivemos, conforme julgam os autores do trabalho, e as duas seleções finalistas da copa.

beta_0=modelo_adj$coefficients[1]
beta_1=modelo_adj$coefficients[3]
beta_2=modelo_adj$coefficients[5]
beta_3=modelo_adj$coefficients[4]
beta_4=modelo_adj$coefficients[2]
Y_1=beta_0+beta_1*14.5+beta_2*615.25+beta_3*508.5+beta_4*158.5
Y_2=beta_0+beta_1*20.6+beta_2*580+beta_3*495.2+beta_4*173.6
Y_3=beta_0+beta_1*17.71429+beta_2*580.8571+beta_3*454+beta_4*164.4286
Y_4=beta_0+beta_1*11.71429+beta_2*465.5714+beta_3*371.4286+beta_4*187.7143

# Para a Argentina, ficamos com esses valores:

# Y=-10.01072 + 0.06917\cdot14.5 + 0.04376*615.25 - 0.04056\cdot508.5 + 0.02831\cdot158.5=1.780012

# Para o Brasil, ficamos com esses valores:

# Y_2=-10.01072 + 0.06917\cdot20.6 + 0.04376*580 - 0.04056\cdot495.2 + 0.02831\cdot173.6=1.626136

# Para a Croácia, ficamos com esses valores:

# Y_2=-10.01072 + 0.06917\cdot17.71429 + 0.04376*580.8571 - 0.04056\cdot454 + 0.02831\cdot164.4286=2.875522

# Para a França, ficamos com esses valores:

# Y_2=-10.01072 + 0.06917\cdot11.71429 + 0.04376*465.5714 - 0.04056\cdot371.4286 + 0.02831\cdot187.7143=1.423269

# Conclusão

# Concluimos que nosso objetivo foi atingido, pois as suposições necessárias para um modelo de regressão linear múltipla foram todas seguidas (normalidade, homocedasticidade). Também vimos que ajustamos nosso modelo para ter o maior valor possível do R^2 ajustado, que foi 75%.

# Vale salientar que, mesmo nosso modelo seguindo todas as suposições necessárias e tendo um R^2 ajustado "bom", não conseguimos utilizar nossa equação e nosso modelo em outros campeonatos, apenas no nosso banco de dados, já que tem várias variáveis invisíveis que não conseguimos enxergar no futebol, e que não nos permitem utilizar a mesma equação para outro banco de dados.

# Por fim, podemos analisar os valores da predição de acordo com os valores disponíveis no nosso banco de dados de número médio de gols por jogo:

# Para seleção Argentina, temos um valor predito na equação de aproximadamente $1.78$, e o valor disponível no nosso banco de dados tinha como valor médio de gols por jogo igual a $1.5$. Para seleção brasileira, nosso valor predito foi aproximadamente $1.63$, e o valor do banco de dados era $1.6$. Para as seleções da final, o valor predito para a Croácia foi $2.88$ e o valor do banco de dados era aproximadamente $2.86$, já para a França tínhamos um valor predito de $1.42$ e um valor médio de gols por jogo de $1.71$.

# Como podemos ver, nossa predição para esse banco de dados foi boa, dado que temos valores bem parecidos para Brasil e Croácia, e para Argentina e França temos valores próximos a realidade.



