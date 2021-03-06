install.packages('randomForest', dependencies = T) # Instala a biblioteca e suas dependencias
library(randomForest) #Importa a biblioteca 'randomForest' para o uso do modelo

credito = read.csv(file.choose(), sep = ',', header = T) #Carrega o dataset 'credito.csv' contendo os dados de usuarios de um banco alem�o
fix(credito) #Imprime uma janela com o dataset

amostra = sample(2, 1000, replace=T, prob=c(0.7,0.3)) #Cria uma variavel que ser� auxiliar na dispers�o aleat�ria dos dados. Utilizando a m�todologia 'hold out' onde utilizamos 70% dos dados para cria��o do modelo e os outros 30% para o teste
creditotreino = credito[amostra == 1,] #Dividimos 70% dos dados para treino
dim(creditotreino) #Mostra o tamanho da nossa amostra de treino
creditoteste = credito[amostra == 2,] #Dividimos 30% dos dados para teste
dim(creditoteste) #Mostra o tamanho da nossa amostra de teste

floresta = randomForest(class ~ ., data = creditotreino, ntree = 100, importance = T) #Cria o modelo baseado no algoritmo random forest, que por sua vez utiliza diversas �rvores de decis�o, para formar o modelo, da� o nome random forest. nTree � o n�mero de �rvores, quanto mais melhor. E importance = T serve para o algoritmo utilizar apenas as informa��es mais re levantes.
varImpPlot(floresta) #Imprime na tela as m�dias mais significativas para o modelo

previsao = predict(floresta, creditoteste) #Usa o metodo 'predict()' passando o nosso modelo e os dados de teste, que n�o foram analisados antes, para prever o comportamento dos clientes
previsao #Retorna uma instancia para cada parametro, prevendo com base nos seus dados o seu comportamento
 
confusao = table(previsao, creditoteste$class)
confusao #Mostra a tabela no console

taxaacerto = (confusao[1] + confusao[4]) / sum(confusao) #Metrica para calcular a % de acerto da predicao
taxaacerto #Mostra a taxa de acerto obtida depois de executar o modelo no dataset de teste
taxaerro = (confusao[2] + confusao[3]) / sum(confusao) #Metrica para calcular a % de erro da predicao
taxaerro #Mostra a taxa de erro obtida depois de executar o modelo no dataset de teste