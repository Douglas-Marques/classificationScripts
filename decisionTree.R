install.packages('rpart', dependencies = T) #Instala a biblioteca e suas dependencias
library(rpart)#Importa a biblioteca 'rpart' para o uso do modelo decision tree neste workspace

credito = read.csv(file.choose(), sep = ',', header = T) #Carrega o dataset 'credito.csv' contendo os dados de usuarios de um banco alemão
fix(credito) #Imprime uma janela com o dataset

amostra = sample(2, 1000, replace=T, prob=c(0.7,0.3)) #Cria uma variavel que será auxiliar na dispersão aleatória dos dados. Utilizando a métodologia 'hold out' onde utilizamos 70% dos dados para criação do modelo e os outros 30% para o teste
creditotreino = credito[amostra == 1,] #Dividimos 70% dos dados para treino
dim(creditotreino) #Mostra o tamanho da nossa amostra de treino
creditoteste = credito[amostra == 2,] #Dividimos 30% dos dados para teste
dim(creditoteste) #Mostra o tamanho da nossa amostra de teste

arvore = rpart(class ~ ., data = creditotreino, method = 'class') #Montamos o modelo de classificação, criando uma decision tree
arvore #Mostra a árvore no console
plot(arvore) #Plota a arvore criada
text(arvore, use.n = T, all = T, cex = .8) #Plota o conteudo da arvore

teste = predict(arvore, newdata = creditoteste) #Prediz, com base no nosso modelo o comportamento dos clientes
teste #Imprime na tela a % dos clientes serem bons ou maus pagadores

cred = cbind(creditoteste, teste) #Adiciona uma nova coluna no dataset, com a predição feita anteriormente
fix(cred) #Imprime em uma janela o resultado

cred['Result'] = ifelse(cred$bad >= 0.5, "bad", "good") #Cria uma nova coluna e utiliza uma lógica simples útiliza para classificar os clientes
fix(cred)

confusao = table(cred$class, cred$Result) #Cria a tabela de confusão para termos acesso a quantidade bruta de acertos e erros da predicao
confusao #Mostra a tabela no console

taxaacerto = (confusao[1] + confusao[4]) / sum(confusao) #Metrica para calcular a % de acerto da predicao
taxaacerto #Mostra a taxa de acerto obtida depois de executar o modelo no dataset de teste
taxaerro = (confusao[2] + confusao[3]) / sum(confusao) #Metrica para calcular a % de erro da predicao
taxaerro #Mostra a taxa de erro obtida depois de executar o modelo no dataset de teste

novocredito = read.csv(file.choose(), sep = ',', header = T) ##Carregamos um novo dataset para aplicarmos a predicao
fix(novocredito) #Imprime uma janela com o novo dataset
novoteste = predict(arvore, newdata = novocredito) #Preve o comportamento do cliente
novoteste #Imprime a % do cliente se comportar como bom ou mau pagador
