### BASE MODEL FOR POPULATION
set.seed(21)

total_years = 700   # total years, make it a multiple of 100
curr_time = 0

n = 10    # start with 100 females
init_id = 1:n
init_sex = rep(0, n)
init_birth = rep(0, n)
init_mum_id = rep(0, n)
init_l = rnorm(n, 25, 3)    # initial L values drawn from N(25,3)
init_death = rexp(n, 1/25)   # very high variance! ask peter about this?

population = data.frame(init_id, init_sex, init_birth, init_mum_id, init_l, init_death)
colnames(population) = c("ID", "Sex", "BirthTime", "MumID", "L", "DeathTime")

have_child = function(pop, id, time_birth) {
  child_id = dim(pop)[1] + 1
  child_sex = rbinom(1, 1, 0.5)
  child_birth = time_birth
  child_mum_id = id
  current_l_in_pop = pop$L[(pop$BirthTime <= time_birth) + (pop$DeathTime >= time_birth) == 2]
  child_l = (pop$L[id] + sample(current_l_in_pop, 1))/2 #   child L is half mum, half dad
  if (rbinom(1, 1, 0.02) == 1) {
    child_l = rnorm(1, child_l, 0.02*child_l)   #   with 2% chance of 2%-SD mutation
  }
  if (child_sex == 0) {
    child_death = 2*child_l*rbeta(1, 2, 2) + time_birth   # exponential was such high variance, so turned into beta
  }
  else {
    child_death = child_l/5 + time_birth
  }
  new_row = data.frame(child_id, child_sex, child_birth, child_mum_id, child_l, child_death)
  colnames(new_row) = c("ID", "Sex", "BirthTime", "MumID", "L", "DeathTime")
  pop = rbind(pop, new_row)
  return(pop)
}

alive = function(id, time_rn) {
  if ((population$BirthTime[id] <= time_rn) + (population$DeathTime[id] >= time_rn) == 2) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

sexually_mature = function(id, time_rn) {
  if (time_rn >= (population$L[id]/2 + population$BirthTime[id])) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

looking_after_child = function(id, time_rn) {
  if (any((population$BirthTime[population$MumID == id] + population$L[population$MumID == id]/5) > time_rn)) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

ready_for_baby = function(id, time_rn) {
  if (alive(id, time_rn) && sexually_mature(id, time_rn) && !(looking_after_child(id, time_rn))) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

ready_for_baby_vect = function(id_vec, time_rn) {
  return_vec = c(NULL)
  return_vec[1] = ready_for_baby(id_vec[1], time_rn)
  if (length(id_vec) == 1) {
    return(return_vec)
  }
  else {
    for (i in 2:length(id_vec)) {
      return_vec = append(return_vec, ready_for_baby(id_vec[i], time_rn))
    }
    return(return_vec)
  }
}


while(curr_time <= total_years) {
  new_mum_logical_vector = ready_for_baby_vect(population$ID, curr_time)
  ids_of_new_mums = population$ID[new_mum_logical_vector]
  for (i in ids_of_new_mums) {
    population = have_child(population, i, curr_time)
  }
  curr_time = curr_time + 1
}

smoothingSpline = smooth.spline(population$BirthTime, population$L)
plot(population$BirthTime, population$L, xlab = "Time (years)", ylab = "L", pch=20)
lines(smoothingSpline, col="blue", lwd=3)







