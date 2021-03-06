csv.echo <- read.csv("./echo.csv")
csv.comorb <- read.csv("./comorbidities.csv")

names(csv.echo) <- paste("S1",names(csv.echo),sep=".")
names(csv.comorb) <- paste("S2",names(csv.comorb),sep=".")

data <- cbind(csv.echo, csv.comorb)

ix0 <- which(data$S1.Aortic.Stenosis == 0
  & data$S1.Bicuspid.AV == 0 
  & data$S1.Rheumatic.AV == 0)
pid0 <- data$S1.Patient[ix0]

ix1 <- which(data$S1.Aortic.Stenosis == 1 
  & data$S1.Bicuspid.AV == 0 
  & data$S1.Rheumatic.AV == 0
  & data$S2.CPPD.Pseudogout.Chondrocalcinosis == 0  # S2.C
  & data$S2.SLE == 0                                # S2.E
  & data$S2.RA == 0                                 # S2.D
  & data$S2.Psoriatic.Arthritis == 0                # S2.F
  & data$S2.Rheumatic.Fever == 0)                   # S2.J
pid1 <- data$S1.Patient[ix1]

write("group 0 patients", stdout())
print(pid0)
write("group 1 patients", stdout())
print(pid1)

# Set random number generator seed for reproducibility
set.seed(001)

age_lookup <- list()
for (i in c(1:nrow(data))) {
  pidstr <- as.character(data$S1.Patient[i])
  age <- data$S1.Age[i]
  age_lookup[[pidstr]] <- age
}

find_age <- function(in_ix, out_ix, filename, nmatches = 2) {
  sampled_pid <- c()
  lines <- c()
  for (ix in in_ix) {
    pid <- data$S1.Patient[ix]
    age <- data$S1.Age[ix]
    matched_pid <- data$S1.Patient[out_ix][which(data$S1.Age[out_ix] == age)]
    matched_pid <- matched_pid[!(matched_pid %in% sampled_pid)]

    # Randomly sample
    selected_pid <- sample(matched_pid)
    if (length(matched_pid) > 2) {
      selected_pid <- matched_pid[1:2]
    }
    sampled_pid <- unique(c(sampled_pid, selected_pid))
    line <- paste(c(pid,":",selected_pid),collapse=" ")
    lines <- c(lines, line)
  }
  f <- file(filename)
  writeLines(lines, f)
  close(f)
}

find_age_even <- function(in_ix, out_ix, filename, nmatches = 2) {
  sampled_pid <- c()
  sampled_dict <- list()
  for (i in c(1:nmatches)) {
    for (ix in in_ix) {
      pid <- data$S1.Patient[ix]
      age <- data$S1.Age[ix]
      ix_age <- which(data$S1.Age[out_ix] == age)
      if (length(ix_age) > 0) {
        matched_pid <- data$S1.Patient[out_ix][ix_age]
        matched_pid <- matched_pid[!(matched_pid %in% sampled_pid)]
        if (length(matched_pid) > 0) {
          matched_pid <- matched_pid[sample(1:length(matched_pid))]
          #for (p in matched_pid) {
          #  if (age_lookup[[as.character(p)]] != age) {
          #    print(paste("wtf", p))
          #  }
          #}
          selected_pid <- matched_pid[1]
          pidstr <- as.character(pid)
          if (!is.null(sampled_dict[[pidstr]])) {
            #print(sampled_dict[[pidstr]])
            sampled_dict[[pidstr]] <- c(sampled_dict[[pidstr]], selected_pid)
          } else {
            sampled_dict[[pidstr]] <- selected_pid
          }
          sampled_pid <- unique(c(sampled_pid, selected_pid))
        }
      }
    }
  }

  # Check age differences
  check_age_diff(age_lookup, sampled_dict)

  for (age_diff_limit in c(1:10)) {
    for (ix in in_ix) {
      pid <- data$S1.Patient[ix]
      pidstr <- as.character(pid)
      if (is.null(sampled_dict[[pidstr]]) || length(sampled_dict[[pidstr]]) < 2) {
        age <- data$S1.Age[ix]
        #print(age)
        age_diff <- abs(data$S1.Age[out_ix] - age)
        ix_age_diff <- which(age_diff == age_diff_limit)
        if (length(ix_age_diff) > 0) {
          matched_pid <- data$S1.Patient[out_ix][ix_age_diff]
          matched_pid <- matched_pid[!(matched_pid %in% sampled_pid)]
          if (length(matched_pid) > 0) {
            matched_pid <- matched_pid[sample(1:length(matched_pid))]
            selected_pid <- matched_pid[1]
            if (!is.null(sampled_dict[[pidstr]])) {
              sampled_dict[[pidstr]] <- c(sampled_dict[[pidstr]], selected_pid)
            } else {
              sampled_dict[[pidstr]] <- selected_pid
            }
            sampled_pid <- unique(c(sampled_pid, selected_pid))
          }
        }
      }
    }
  }

  # Check age differences
  check_age_diff(age_lookup, sampled_dict)
  
  headers <- paste(c("Aortic Stenosis","Non-aortic Stenosis Match 1","Non-aortic Stenosis Match 2"),collapse=",")
  lines <- c(headers)
  for (ix in in_ix) {
    pid <- data$S1.Patient[ix]
    selected_pid <- c()
    pidstr <- as.character(pid)
    rows <- rep("",2)
    if (!is.null(sampled_dict[[pidstr]])) {
      selected_pid <- sampled_dict[[pidstr]]
      rows[1:length(selected_pid)] <- selected_pid
    }
    #line <- paste(c(pid,":",selected_pid),collapse=" ")
    line <- paste(c(pid,rows),collapse=",")
    lines <- c(lines, line)
  }
  f <- file(filename)
  writeLines(lines, f)
  close(f)
}

check_age_diff <- function(age_lookup, sampled_dict) {
  max_age_diff <- 0
  for (ps in names(sampled_dict)) {
    age <- age_lookup[[ps]]
    sampled_age <- sapply(sampled_dict[[ps]], function(x) return(age_lookup[[as.character(x)]]))
    #print(sampled_age)
    max_age_diff <- max(c(max_age_diff, abs(sampled_age - age)))
  }
  msg <- paste(c("Max age difference in samples = ",max_age_diff),collapse="")
  write(msg,stdout())
}

#find_age(in_ix = ix0, out_ix = ix1, "patient_group0_samples.txt")
#find_age_even(in_ix = ix0, out_ix = ix1, "patient_group0_even_samples.csv")

find_age(in_ix = ix1, out_ix = ix0, "patient_group1_samples.txt")
find_age_even(in_ix = ix1, out_ix = ix0, "patient_group1_even_samples.csv")
