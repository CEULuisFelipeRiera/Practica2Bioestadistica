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




#ejercicio1.2
#En cierta tribu del sur del Amazonas, las familias suelen tener de 3 a 6 hijos. La funciónmasa de probabilidad estimada de la variable aleatoria asociada X, es la siguiente:P(X = 3) = 0.35, P(X = 4) = 0.45, P(X = 5) = 0.16, P(X = 6) = 0.04. A partir de estainformación, calcule E [X], V [X], y obtenga además la función de distribución deprobabilidad acumulada correspondiente, F(x). Posteriormente, determine cuál es, paraestas familias, la probabilidad de tener no más de 4 hijos y cuál es la probabilidad detener más de 3 pero menos de 6 hijos
#1. Calcule E [X], V [X]
# Define the PMF of X
prob_X <- c(3 = 0.35, 4 = 0.45, 5 = 0.16, 6 = 0.04)

# Calculate the expected value of X
E_X <- sum(as.numeric(names(prob_X)) * prob_X)

# Calculate the expected value of X^2
E_X2 <- sum((as.numeric(names(prob_X))^2) * prob_X)

# Calculate the variance of X
V_X <- E_X2 - E_X^2

# Print the expected value and variance
E_X
V_X



#2. Obtenga la función de distribución de probabilidad acumulada utilizando paraello la función cumsum ()
# Calculate the cumulative distribution function of X
F_X <- cumsum(prob_X)

# Print the CDF of X
F_X



#3. Determine cuál es, para estas familias, la probabilidad de tener no más de 4 hijos
# Calculate the probability of having no more than 4 children
prob_no_more_than_4 <- F_X["4"]

# Print the probability
prob_no_more_than_4


#4. Determine cuál es la probabilidad de tener más de 3 pero menos de 6 hijos
# Calculate the probability of having more than 3 but less than 6 children
prob_more_than_3_less_than_6 <- F_X["5"] - F_X["3"]

# Print the probability
prob_more_than_3_less_than_6




#ejercicio2.1
#Asumiendo que la determinación del género un bebé durante su gestación sigue unadistribución binomial, obtenga la función de masa de probabilidad y la función dedensidad de probabilidad cuando la variable de interés es el número de hijas en unafamilia que tenga en total 5 hij@s, y represéntelas gráficamente.
#1. Calcule en primer lugar la función de masa de probabilidad utilizando la funcióndbinom () cuya sintaxis es la siguiente: dbinom (x, n, p = probability) dondex = número de éxitos, n = número de experimentos y p = probabilidad de éxito
# Define the number of trials and the probability of success
n <- 5
p <- 0.5

# Define the possible outcomes
x <- 0:n

# Calculate the PMF of X
prob_X <- dbinom(x, n, p)

# Name the probabilities with their corresponding outcomes
names(prob_X) <- x

# Print the PMF of X
prob_X



#2. Calcule posteriormente la función de distribución de probabilidad acumuladautilizando la función pbinom ().
# Define the possible outcomes
x <- 0:n

# Calculate the CDF of X
F_X <- pbinom(x, n, p)

# Name the probabilities with their corresponding outcomes
names(F_X) <- x

# Print the CDF of X
F_X



#3. Represente ambas funciones gráficamente utilizando la función plot ().
# Plot the PMF of X
plot(as.numeric(names(prob_X)), prob_X, type = "h", main = "PMF of X", xlab = "Number of daughters", ylab = "Probability")

# Plot the CDF of X
plot(as.numeric(names(F_X)), F_X, type = "s", main = "CDF of X", xlab = "Number of daughters", ylab = "Cumulative probability")




#ejercicio2.2
#Una determinada especie de ratón desarrolla una forma particular de distrofia muscularque tiene una clara base genética. Se sabe que, en este grupo, la probabilidad deaparición de la citada distrofia muscular es de ¼. Si se analizase una muestra de un total de 20 ratones de esta especie, calcule las probabilidades de que: (a) menos de cincotuviesen distrofia muscular, (b) justo cinco tuviesen distrofia muscular, (c) más de dospero menos de 8 tuvieran distrofia muscular.
#1. Determine los parámetros del modelo de variable aleatoria a utilizar.
# Define the number of trials and the probability of success
n <- 20
p <- 1/4



#2. Calcule las probabilidades pedidas (a), (b) y (c) utilizando las funciones pbinom ()y dbinom ().
# Calculate the probability of less than five mice having muscular dystrophy
prob_less_than_5 <- pbinom(4, n, p)

# Calculate the probability of exactly five mice having muscular dystrophy
prob_exactly_5 <- dbinom(5, n, p)

# Calculate the probability of more than two but less than eight mice having muscular dystrophy
prob_more_than_2_less_than_8 <- pbinom(7, n, p) - pbinom(2, n, p)

# Print the probabilities
prob_less_than_5
prob_exactly_5
prob_more_than_2_less_than_8




#ejercicio2.3
#Suponga que la probabilidad de que un paciente que ha contraído ébola se recupere esde 0.1. Si 16 personas (sin relación de parentesco entre ellas) han contraído el virus,calcule: (a) el número esperado de pacientes que se recuperarán, (b) la probabilidad deque como mucho 5 se recuperen, (c) la probabilidad de que al menos 5 se recuperen,(d) la probabilidad de que exactamente 5 se recuperen
#1. Determine los parámetros del modelo de variable aleatoria a utilizar.
# Define the number of trials and the probability of success
n <- 16
p <- 0.1



#2. Determine el número esperado de pacientes que se recuperarán (a) utilizando lafórmula adecuado según el modelo utilizado.
# Calculate the probability of exactly two patients recovering
prob_exactly_2 <- dbinom(2, n, p)

# Calculate the probability of at least three patients recovering
prob_at_least_3 <- 1 - pbinom(2, n, p)

# Print the probabilities
prob_exactly_2
prob_at_least_3



#3. Calcule las probabilidades pedidas (b), (c) y (d) utilizando las funciones pbinom ()y dbinom ().
# Calculate the probability of at most five patients recovering
prob_at_most_5 <- pbinom(5, n, p)

# Calculate the probability of at least five patients recovering
prob_at_least_5 <- 1 - pbinom(4, n, p)

# Calculate the probability of exactly five patients recovering
prob_exactly_5 <- dbinom(5, n, p)

# Print the probabilities
prob_at_most_5
prob_at_least_5
prob_exactly_5





#ejercicio2.4
#Imagine que una de las máquinas expendedoras de tickets del aparcamiento de ciertohospital está averiada y no siempre genera el ticket correspondiente. Asumiendon = 181 y p = 0.15 (siendo p la probabilidad de éxito, es decir, la probabilidad de que lamáquina genere el ticket), obtenga la gráfica de la función masa de probabilidad de lavariable binomial X~B(n,p) asociada a este problema.
#1. Defina n, p, y un vector que contenga todos los posibles valores de X.
# Define the number of trials and the probability of success
n <- 181
p <- 0.15

# Define the possible outcomes
x <- 0:n



#2. Utilice dbinom () para generar una variable que represente el valor de la funciónde masa de probabilidad en función de X.
# Calculate the PMF of X
prob_X <- dbinom(x, n, p)

# Name the probabilities with their corresponding outcomes
names(prob_X) <- x

# Print the PMF of X
prob_X



#3. Represente en R dicha función de masa de probabilidad utilizando plot ()
# Plot the PMF of X
plot(as.numeric(names(prob_X)), prob_X, type = "h", main = "PMF of X", xlab = "Number of tickets", ylab = "Probability")





#ejercicio3.1
#Una máquina que fabrica piezas para prótesis de rodilla tiene una probabilidad de fallodel 3% (p = 0.03) Asumiendo que esta probabilidad de fallo se puede considerarconstante, se pide calcular una serie de probabilidades según las instrucciones que seadjuntan a continuación.


