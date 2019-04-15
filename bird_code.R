### BIRD CODE
set.seed(21)

total_years = 100
curr_time = 0

n = 15    # start with how many birds
init_id = 1:n
init_allele_1 = c(rep("green", 5), rep("yellow", 5), rep("blue", 5))
init_allele_2 = sample(c("green", "yellow", "blue"), size=n, replace=TRUE)
init_colour = c(rep(NULL, n))
for (i in 1:n) {
  init_colour[i] = sample(c(init_allele_1[i], init_allele_2[i]), size=1)
}
init_birth = rep(0, n)
init_death = rexp(n, 1/10)
init_parent_id = rep(0, n)
init_w = c(rnorm(5, 1, 1), rnorm(5, 0, 1), rnorm(5, -1, 1))    # initial L values drawn from N(25,3)
init_mu = 8/(1+exp(-2*init_w))

pop = data.frame(init_id, init_allele_1, init_allele_2, init_colour, init_birth, init_parent_id, init_w, init_mu)
colnames(pop) = c("ID", "Allele1", "Allele2", "Colour", "BirthTime", "DeathTime", "ParentID", "w", "mean")

bird_reproduce - function(popln, id, time_birth) {
  child_id = dim(popln)[1] + 1
  child_allele_1 = sample(popln$Allele1[id], popln$Allele2[id], size=1)
  child_allele_2 = sample(c(popln$Allele1[(popln$BirthTime <= time_birth) + (popln$DeathTime >= time_birth) == 2], popln$Allele2[(popln$BirthTime <= time_birth) + (popln$DeathTime >= time_birth) == 2]), size=1)
  child_colour = sample(c(child_allele_1, child_allele_2), size=1)
  child_birth = time_birth
  child_death = time_birth + rexp(1, 1/10)
  child_parent_id = id
  if (child_colour == "green") {
    child_w = rnorm(1, 1, 1)
  }
  else if (child_colour = "yellow") {
    
  }
}



