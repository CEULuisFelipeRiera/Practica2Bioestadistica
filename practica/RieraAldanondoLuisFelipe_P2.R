#PRACTICA 2 
#La variable aleatoria X representa los posibles resultados que se pueden obtener cuandose lanza al aire un dado de seis caras y la variable aleatoria Y representa la suma de losresultados obtenidos al lanzar el mismo dado dos veces. Asumiendo que el dado no estátrucado, y que al lanzarlo, todos los resultados son equiprobables, obtenga: la funciónde masa de probabilidad de la variable aleatoria X y de la variable aleatoria Y, los valoresesperados (o la esperanza matemática) en ambos casos E[X] y E[Y], los valores esperadosde cada una de las dos variables aleatorias consideradas al cuadrado E[X2] y E[Y2] y,finalmente, las varianzas V[X] y V[Y]

#1. Función de masa de probabilidad de la variable aleatoria X
# Define the outcomes of the die roll
resultadosposibles_X <- 1:6

# Since the die is fair, each outcome has equal probability
prob_X <- rep(1/6, 6)

# Name the probabilities with their corresponding outcomes
names(prob_X) <- resultadosposibles_X
prob_X



#2. Función de masa de probabilidad de la variable aleatoria Y
# Define the possible outcomes of the sum of two die rolls
outcomes_Y <- 2:12

# Calculate the number of ways to get each outcome
ways_Y <- c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)

# Calculate the probabilities for each outcome
prob_Y <- ways_Y / 36

# Name the probabilities with their corresponding outcomes
names(prob_Y) <- outcomes_Y

# Print the PMF of Y
prob_Y



#2.1 Obtenga la función masa de probabilidad de la variable aleatoria Y. Para ello,cree en primer lugar un vector (llámelo X) que contenga todos los posibles resultados asociados a la variable aleatoria X y posteriormente, a partir de dichovector, cree una tabla cuyas entradas sean las sumas de todos los posiblesresultados que se pueden obtener al lanzar el dado dos veces, es decir, una tablaque represente todos los posibles resultados de la variable aleatoria Y. Utilicepara ello la función outer () que tiene la siguiente sintaxis: outer (X, X, FUN = ‘+’).A partir de la tabla anterior cree una tabla de frecuencias y finalmente, a partirde ésta, obtenga la función de masa de probabilidad de Y pedida
X <- 1:6

# Create a table of all possible sums of two die rolls
sums <- outer(X, X, FUN = '+')

# Create a frequency table of the sums
freq_table <- table(sums)

# Convert the frequency table to a probability mass function
prob_Y <- freq_table / sum(freq_table)

# Print the PMF of Y
prob_Y



#3. Valores esperados (o la esperanza matemática) en ambos casos E[X] y E[Y]
# Calculate the expected value of X
E_X <- sum(as.numeric(names(prob_X)) * prob_X)

# Calculate the expected value of Y
E_Y <- sum(as.numeric(names(prob_Y)) * prob_Y)

# Print the expected values
E_X
E_Y



#4. Valores esperados de cada una de las dos variables aleatorias consideradas al cuadrado E[X2] y E[Y2]
# Calculate the expected value of X^2
E_X2 <- sum((as.numeric(names(prob_X))^2) * prob_X)

# Calculate the expected value of Y^2
E_Y2 <- sum((as.numeric(names(prob_Y))^2) * prob_Y)

# Print the expected values
E_X2
E_Y2



#5. Varianzas V[X] y V[Y]
# Calculate the variance of X
V_X <- E_X2 - E_X^2

# Calculate the variance of Y
V_Y <- E_Y2 - E_Y^2

# Print the variances
V_X
V_Y