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
#1. Calcule la probabilidad de que la máquina falle en la quinta pieza fabricadautilizando dgeom ()
# Define the probability of failure
p <- 0.03

# Calculate the probability that the machine fails on the fifth piece
prob_fail_on_5 <- dgeom(4, p)

# Print the probability
prob_fail_on_5



#2. Calcule la probabilidad de que el primer fallo ocurra antes de fabricar la sextapieza.
# Define the probability of failure
p <- 0.03

# Calculate the probability that the first failure occurs before the sixth piece
prob_fail_before_6 <- pgeom(5, p)

# Print the probability
prob_fail_before_6



#3. Represente en R gráficamente la función de masa de probabilidad de la variablealeatoria geométrica asociada a este problema.
# Define the probability of failure and the possible outcomes
p <- 0.03
x <- 0:20

# Calculate the PMF of the geometric distribution
prob_X <- dgeom(x, p)

# Plot the PMF
plot(x, prob_X, type = "h", main = "PMF of Geometric Distribution", xlab = "Number of trials before first failure", ylab = "Probability")



#4. ¿Qué ocurre en la gráfica de la función de masa de probabilidad si se modifica elvalor de p? ¿Y con la función de distribución de probabilidad? Represente ambasgráficas para tres valores de p distintos y comente las diferencias. Utilicepar (mfrow = c (number_of_rows, number_of_columns)) si quiere representarvarias gráficas a la vez
# Define the probability of failure and the possible outcomes
p <- 0.03
x <- 0:20

# Calculate the PMF of the geometric distribution
prob_X <- dgeom(x, p)

# Plot the PMF
plot(x, prob_X, type = "h", main = "PMF of Geometric Distribution", xlab = "Number of trials before first failure", ylab = "Probability")




#ejercicio3.2
#Se está probando un nuevo medicamento contra la alergia primaveral que, demomento, tiene una probabilidad de no funcionar del 20%. ¿Cuál sería el númeromáximo de pacientes con alergia que deberían tomar el medicamento para que laprobabilidad de encontrar a uno en el que el medicamento no haya funcionado seadel 75%?
#2. Busque qué función relacionada con la distribución geométrica puede utilizar enR para resolver el ejercicio y obtenga con ella el resultado pedido
# Define the probability of the medication not working
p <- 0.2

# Calculate the maximum number of patients
n <- qgeom(0.75, p)

# Print the maximum number of patients





#ejercicio3.3
#En el servicio de cirugía de un hospital, están probando nuevos procedimientos pararealizar implantes cocleares en recién nacidos. Utilizando una distribución geométrica,resuelva los apartados siguientes.
#1. Calcule la probabilidad de obtener dos fallos antes de obtener un éxito, si laprobabilidad de éxito de uno de los nuevos procedimientos es del 70%
# Define the number of failures and the probability of success
r <- 2
p <- 0.7

# Calculate the probability of obtaining two failures before obtaining a success
prob_two_failures_before_success <- dnbinom(r - 1, r, p)

# Print the probability
prob_two_failures_before_success


#2. Calcule la probabilidad de obtener como mucho 7 fallos antes de obtener unéxito, si en este caso, el procedimiento utilizado tiene una probabilidad de éxitodel 30%.
# Define the number of successes and the probability of success
r <- 2
p <- 0.7

# Calculate the probability of obtaining three failures before obtaining two successes
prob_three_failures_before_two_successes <- pnbinom(2, r, 1 - p)

# Print the probability
prob_three_failures_before_two_successes



#3. Obtenga el número máximo de fallos que habría antes de un éxito en el 95% delos casos, si la probabilidad de éxito en esta ocasión es del 40%
# Define the number of successes and the probability of success
r <- 1
p <- 0.4

# Calculate the maximum number of failures before a success in 95% of the cases
max_failures_before_success <- qnbinom(0.95, r, 1 - p)

# Print the maximum number of failures
max_failures_before_success





#ejercicio4.1
# Para optar a una beca universitaria, los alumnos de ingeniería biomédica de ciertauniversidad deben haber aprobado al menos 10 asignaturas con al menos unacalificación de 8.0. Si la probabilidad p de sacar al menos un 8.0 en cada una de estasasignaturas (bajo condición de independencia) es de un 40%, calcule la probabilidad quetiene un estudiante de poder optar a esta beca cuando lleve cursadas 15 asignaturas.
#2. Resuelva el ejercicio utilizando dnbinom () y teniendo en cuenta que en R ladistribución binomial negativa cuenta el número de fallos hasta conseguir réxitos
# Define the number of successes, the number of trials, and the probability of success
r <- 10
n <- 15
p <- 0.4

# Calculate the probability that a student can apply for the scholarship
prob_scholarship <- sum(dnbinom(r:n, r, p))

# Print the probability
prob_scholarship


#3. Represente en R gráficamente la evolución de la función de masa de probabilidadde este modelo binomial negativo en función de los valores de X. ¿Cómocambiaría esta gráfica para distintos valores de p?
# Define the number of successes, the number of trials, and the probability of success
r <- 10
n <- 15
p <- 0.4

# Define the possible outcomes
x <- r:n

# Calculate the PMF of the negative binomial distribution
prob_X <- dnbinom(x, r, p)

# Plot the PMF
plot(x, prob_X, type = "h", main = "PMF of Negative Binomial Distribution", xlab = "Number of trials before 10th success", ylab = "Probability")





#ejercicio5.1
#Imagine que en un colegio de educación especial estudian un total de 46 niñas y49 niños. Una vez al año, se escoge al azar a 20 de est@s niñ@s para que participen en una obra de teatro que luego es representada para sus padres. ¿Cuál es la probabilidadde que entre l@s 20 niñ@s escogidos haya exactamente 5 niñas y 15 niños? ¿Y laprobabilidad de que el número de niñas elegidas sea de cómo mucho 5?
#2. Conteste a las dos preguntas planteadas resolviendo en este caso el ejercicio enR utilizando dhyper () o phyper () según corresponda.
# Define the number of girls, boys, total children chosen, and the number of girls chosen
girls <- 46
boys <- 49
total_chosen <- 20
girls_chosen <- 5

# Calculate the probability that exactly 5 girls and 15 boys are chosen
prob_exact <- dhyper(girls_chosen, girls, boys, total_chosen)

# Calculate the probability that at most 5 girls are chosen
prob_at_most <- phyper(girls_chosen, girls, boys, total_chosen)

# Print the probabilities
prob_exact
prob_at_most



#3. Represente en R gráficamente tanto la función de masa de probabilidad como lafunción de distribución del modelo hipergeométrico utilizado en ese ejerciciocuando éste cuenta el número de niñas seleccionadas
# Define the number of girls, boys, and total children chosen
girls <- 46
boys <- 49
total_chosen <- 20

# Define the possible outcomes
x <- 0:total_chosen

# Calculate the PMF and CDF of the hypergeometric distribution
pmf <- dhyper(x, girls, boys, total_chosen)
cdf <- phyper(x, girls, boys, total_chosen)

# Plot the PMF
plot(x, pmf, type = "h", main = "PMF and CDF of Hypergeometric Distribution", xlab = "Number of girls chosen", ylab = "Probability")
# Add the CDF to the plot
lines(x, cdf, col = "red")

# Add a legend
legend("topright", legend = c("PMF", "CDF"), col = c("black", "red"), lty = 1)





#ejercicio5.2
#En cierto hospital madrileño, hay 5 vacantes en el servicio de cirugía. De entre l@scandidat@s, 500 mujeres y 450 hombres cumplen los requisitos necesarios para elpuesto, por lo que la elección final se va a realizar al azar. Bajo estas premisas, calculelas probabilidades pedidas según las instrucciones que se adjuntan a continuación.
#1. Calcule la probabilidad de que de las 5 vacantes exactamente 4 sean ocupadaspor mujeres utilizando un modelo hipergeométrico
# Define the number of women, men, total vacancies, and the number of women chosen
women <- 500
men <- 450
total_vacancies <- 5
women_chosen <- 4

# Calculate the probability that exactly 4 vacancies are filled by women
prob_exact <- dhyper(women_chosen, women, men, total_vacancies)

# Print the probability
prob_exact



#2. Calcule, con el mismo modelo, la probabilidad de que al menos 3 vacantes seanocupadas por hombres.
# Define the number of women, men, total vacancies, and the number of men chosen
women <- 500
men <- 450
total_vacancies <- 5
men_chosen <- 3

# Calculate the probability that at least 3 vacancies are filled by men
prob_at_least <- 1 - phyper(men_chosen - 1, men, women, total_vacancies)

# Print the probability
prob_at_least



#3. Según las condiciones del problema, ¿es posible realizar una aproximaciónutilizando un modelo binomial? Si es así, utilícelo para calcular de nuevo lasprobabilidades pedidas en los dos apartados anteriores y compare los resultadosobtenidos en ambos casos
# Define the number of trials, and the probability of success
n <- 5
p <- 500 / (500 + 450) # Probability of choosing a woman
q <- 1 - p # Probability of choosing a man

# Calculate the probability that exactly 4 vacancies are filled by women using the binomial distribution
prob_exact_binom <- dbinom(4, n, p)

# Calculate the probability that at least 3 vacancies are filled by men using the binomial distribution
prob_at_least_binom <- sum(dbinom(3:n, n, q))

# Print the probabilities
prob_exact_binom
prob_at_least_binom



#4 Represente gráficamente las funciones de masa de probabilidad tanto delmodelo binomial como del modelo geométrico utilizados en este problema.
# Define the number of trials, and the probability of success
n <- 5
p <- 500 / (500 + 450) # Probability of choosing a woman
q <- 1 - p # Probability of choosing a man

# Calculate the probability that exactly 4 vacancies are filled by women using the binomial distribution
prob_exact_binom <- dbinom(4, n, p)

# Calculate the probability that at least 3 vacancies are filled by men using the binomial distribution
prob_at_least_binom <- sum(dbinom(3:n, n, q))

# Print the probabilities
prob_exact_binom
prob_at_least_binom





#ejercicio6.1
#Se sabe que, en promedio, la llegada de pacientes al servicio de urgencias de ciertocentro de salud, en un día, es de 2. Bajo estas premisas, siga las instrucciones pararesponder a las preguntas que se plantean a continuación
#1. Calcule la probabilidad de que en un día concreto no acuda ningún paciente alservicio de urgencias de dicho centro de salud. Utilice para ello la funcióndpois ().
# Define the average arrival of patients
lambda <- 2

# Calculate the probability that no patients arrive
prob_no_patients <- dpois(0, lambda)

# Print the probability
prob_no_patients



#2. Calcule la probabilidad de que en un día concreto lleguen menos de 6 pacientesal servicio de urgencias de este centro de salud. Ayúdese en R de la funciónppois ().
# Define the average arrival of patients
lambda <- 2

# Calculate the probability that fewer than 6 patients arrive
prob_less_than_6 <- ppois(5, lambda)

# Print the probability
prob_less_than_6



#3. Finalmente, obtenga la probabilidad de que en un día concreto la llegada depacientes sea mayor que 3 pero menor que 6
# Define the average arrival of patients
lambda <- 2

# Calculate the probability that more than 3 but fewer than 6 patients arrive
prob_between_3_and_6 <- ppois(5, lambda) - ppois(3, lambda)

# Print the probability
prob_between_3_and_6





#ejercicio6.2
#Un grupo de estudiantes de ingeniería forestal, están realizando un estudio en áreas de10 m x 10 m de bosque subtropical sobre la probabilidad de encontrar un númerodeterminado de árboles. Bajo la premisa de que los árboles se encuentran distribuidosde manera aleatoria y teniendo en cuenta que la media de árboles en áreas de estetamaño es de 30, se pide calcular una serie de probabilidades que se describen acontinuación.
#1. Calcule la probabilidad de encontrar no más de 3 árboles en un área de 1 m2ajustando en primer lugar el valor de la media a la nueva área bajo estudio yutilizando posteriormente la función ppois ()
# Define the adjusted average number of trees
lambda_adjusted <- 30 / 100

# Calculate the probability of finding no more than 3 trees
prob_no_more_than_3 <- ppois(3, lambda_adjusted)

# Print the probability
prob_no_more_than_3



#2. Calcule la probabilidad de encontrar exactamente 1 árbol en un área de 5 m2utilizando la función dpois ()
# Define the adjusted average number of trees
lambda_adjusted <- 30 * 5 / 100

# Calculate the probability of finding exactly 1 tree
prob_exactly_1 <- dpois(1, lambda_adjusted)

# Print the probability
prob_exactly_1



#3. Finalmente, calcule la probabilidad de encontrar al menos 2 árboles en un áreade 2 m2. Intente realizar este último cálculo en R de dos maneras distintas.Explore para ello las posibilidades de la función ppois ().
# Define the adjusted average number of trees
lambda_adjusted <- 30 * 2 / 100

# Calculate the probability of finding at least 2 trees using the cumulative distribution function
prob_at_least_2 <- 1 - ppois(1, lambda_adjusted)

# Calculate the probability of finding at least 2 trees using the probability mass function
prob_at_least_2_alternative <- 1 - (dpois(0, lambda_adjusted) + dpois(1, lambda_adjusted))

# Print the probabilities
prob_at_least_2
prob_at_least_2_alternative




#ejercicio6.3
#Cierta enfermedad, que suele ser detectada justo al nacer, tiene una prevalencia (p) del0.01%. Si se realiza un estudio acerca de esta enfermedad en un hospital en el que sesabe que han nacido n = 5000 niños en el último año, calcule las probabilidades pedidassegún las instrucciones que se adjuntan a continuación
#1. Calcule cuál es la probabilidad de que, de entre esos 5000 niños, haya al menosuno con la citada enfermedad utilizando en primer lugar un modelo binomial
# Define the number of trials, and the probability of success
n <- 5000
p <- 1 / 10000 # Probability of a child having the disease

# Calculate the probability that at least one child has the disease
prob_at_least_one <- 1 - dbinom(0, n, p)

# Print the probability
prob_at_least_one



#2. Justifique si es posible o no utilizar como aproximación un modelo de Poisson, yen caso afirmativo, resuelva de nuevo el apartado anterior y compare losresultados obtenidos.
# Define the average number of successes
lambda <- n * p

# Calculate the probability that at least one child has the disease using the Poisson distribution
prob_at_least_one_poisson <- 1 - dpois(0, lambda)

# Print the probability
prob_at_least_one_poisson



#3. Calcule la probabilidad de que no haya más de dos niños con la enfermedad deentre los 5000 considerados utilizando un modelo binomial.
# Define the number of trials, and the probability of success
n <- 5000
p <- 1 / 10000 # Probability of a child having the disease

# Calculate the probability that exactly 3 children have the disease
prob_exactly_3 <- dbinom(3, n, p)

# Print the probability
prob_exactly_3



#4. De nuevo, si es posible utilizar un modelo de Poisson, resuelva de nuevo elapartado anterior y compare los resultados obtenidos
# Define the average number of successes
lambda <- n * p

# Calculate the probability that exactly 3 children have the disease using the Poisson distribution
prob_exactly_3_poisson <- dpois(3, lambda)

# Print the probability
prob_exactly_3_poisson



#5. Represente sobre la misma gráfica, la función de masa de probabilidad de los dosmodelos utilizados en el ejercicio y comente las similitudes y/o diferencias entreambas
# Define the number of trials, and the probability of success
n <- 5000
p <- 1 / 10000 # Probability of a child having the disease

# Define the average number of successes
lambda <- n * p

# Define the range of outcomes
x <- 0:10

# Calculate the PMFs
pmf_binomial <- dbinom(x, n, p)
pmf_poisson <- dpois(x, lambda)

# Plot the PMFs
plot(x, pmf_binomial, type = "h", lty = 2, xlab = "Number of children with the disease", ylab = "Probability", main = "PMF of Binomial and Poisson Distributions")
lines(x, pmf_poisson, type = "h", lty = 1, col = "red")

# Add a legend
legend("topright", legend = c("Binomial", "Poisson"), lty = c(2, 1), col = c("black", "red"))



#6. ¿Qué ocurre en la comparativa del modelo binomial con el modelo de Poisson si,dejando constante el producto n·p, se disminuye n y se aumenta p? Representegráficamente las funciones de masa de probabilidad para distintas parejas (n,p)y comente el resultado.
# Define the original number of trials and probability of success
n_original <- 5000
p_original <- 1 / 10000 # Probability of a child having the disease

# Define the new pairs of (n, p)
np_pairs <- list(c(500, 1/1000), c(50, 1/100), c(5, 1/10))

# Define the range of outcomes
x <- 0:10

# Plot the original PMFs
plot(x, dbinom(x, n_original, p_original), type = "h", lty = 2, xlab = "Number of children with the disease", ylab = "Probability", main = "PMF of Binomial and Poisson Distributions")
lines(x, dpois(x, n_original * p_original), type = "h", lty = 1, col = "red")

# Plot the new PMFs
colors <- c("blue", "green", "purple")
for (i in 1:length(np_pairs)) {
  n <- np_pairs[[i]][1]
  p <- np_pairs[[i]][2]
  lines(x, dbinom(x, n, p), type = "h", lty = 2, col = colors[i])
  lines(x, dpois(x, n * p), type = "h", lty = 1, col = colors[i])
}

# Add a legend
legend("topright", legend = c("Original Binomial", "Original Poisson", "New Binomial 1", "New Poisson 1", "New Binomial 2", "New Poisson 2", "New Binomial 3", "New Poisson 3"), lty = c(2, 1, 2, 1, 2, 1, 2, 1), col = c("black", "red", colors, colors))

