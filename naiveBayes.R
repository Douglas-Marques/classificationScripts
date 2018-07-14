#Meu primeiro código Machine Learning:

install.packages('e1071', dependencies = T) # Instala a biblioteca e suas dependencias
library('e1071') #Importa a biblioteca 'e1071' para o uso do modelo de Naive Bayes

credito = read.csv(file.choose(), sep = ',', header = T) #Carrega o dataset 'credito.csv' contendo os dados de usuarios de um banco alemão
fix(credito) #Imprime uma janela com o dataset

amostra = sample(2, 1000, replace=T, prob=c(0.7,0.3)) #Cria uma variavel que será auxiliar na dispersão aleatória dos dados. Utilizando a métodologia 'hold out' onde utilizamos 70% dos dados para criação do modelo e os outros 30% para o teste
creditotreino = credito[amostra == 1,] #Dividimos 70% dos dados para treino
dim(creditotreino) #Mostra o tamanho da nossa amostra de treino
creditoteste = credito[amostra == 2,] #Dividimos 30% dos dados para teste
dim(creditoteste) #Mostra o tamanho da nossa amostra de teste

modelo = naiveBayes(class ~ ., creditotreino) #Criacao do modelo Naive Bayes. Comparando 'class' a nossa classe principal, com todas as outras classes do dataset e assim calculando as caracteristicas que formam um 'bom pagador' e um 'mal pagador'
modelo #Mostra os parametros calculados para a criacao do modelo de predicao

predicao = predict(modelo, creditoteste) #Usa o metodo 'predict()' passando o nosso modelo e os dados de teste, que não foram analisados antes, para prever o comportamento dos clientes
predicao #Retorna uma instancia para cada parametro, prevendo com base nos seus dados o seu comportamento

confusao = table(creditoteste$class, predicao) #Cria a tabela de confusão para termos acesso a quantidade bruta de acertos e erros da predicao
confusao #Mostra a tabela no console

taxaacerto = (confusao[1] + confusao[4]) / sum(confusao) #Metrica para calcular a % de acerto da predicao
taxaacerto #Mostra a taxa de acerto obtida depois de executar o modelo no dataset de teste
taxaerro = (confusao[2] + confusao[3]) / sum(confusao) #Metrica para calcular a % de erro da predicao
taxaerro #Mostra a taxa de erro obtida depois de executar o modelo no dataset de teste

novocredito = read.csv(file.choose(), sep = ',', header = T) #Carregamos um novo dataset para aplicarmos a predicao
fix(novocredito) #Imprime uma janela com o novo dataset
predict(modelo, novocredito) #Preve o comportamento do cliente