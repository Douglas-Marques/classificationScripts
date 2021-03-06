install.packages('class', dependencies = T) #Instala o pacote e suas dependencias
library(class) #Carrega o pacote 'e1071' no nosso workspace

summary(iris) #Mostrando o dataset que iremos 'clusterizar', leia-se agrupar

amostra = sample(2, 150, replace = T, prob = c(0.7, 0.3)) #Cria uma variavel que ser� auxiliar na dispers�o aleat�ria dos dados. Utilizando a m�todologia 'hold out' onde utilizamos 70% dos dados para cria��o do modelo e os outros 30% para o teste
iristreino = iris[amostra==1,] #Dividimos 70% dos dados para treino
dim(iristreino) #Mostra o tamanho da nossa amostra de treino
iristeste = iris[amostra==2,] #Dividimos 30% dos dados para teste
dim(iristeste) #Mostra o tamanho da nossa amostra de teste

previsao = knn(iristreino[,1:4], iristeste[,1:4], iristreino[,5], k = 3) #Dado   um   objeto   x   sem   classifica��o (iristeste[,1:4]),   verifica-se os k vizinhos treinados mais pr�ximos a  ele (iristreino[,1:4]).  A  categoria  atribu�da  ao  objeto  x  �  a  que  possui   o   maior   n�mero   de   ocorr�ncias   (k-ocorr�ncias) pr�ximas a ele. 
previsao #Mostra na tela a esp�cie de cad item de iristeste

confusao = table(iristeste[,5], previsao) #Cria a tabela de confus�o para termos acesso a quantidade bruta de acertos e erros da predicao
confusao #Mostra a tabela no console

taxaacerto = (confusao[1] + confusao[5] + confusao[9]) / sum(confusao) #Metrica para calcular a % de acerto da predicao
taxaacerto #Mostra a taxa de acerto obtida depois de executar o modelo no dataset de teste
taxaerro = (confusao[2] + confusao[3] + confusao[4] + confusao[6] + confusao[7] + confusao[8])/ sum(confusao) #Metrica para calcular a % de erro da predicao
taxaerro #Mostra a taxa de erro obtida depois de executar o modelo no dataset de teste