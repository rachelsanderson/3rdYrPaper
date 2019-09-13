gen_fake_names <- function(fName, lName, pFirst, pLast){
  fNameBad <- perturb_name(fName, pFirst)
  lNameBad <- perturb_name(lName, pLast)
  return(list(fNew = fNameBad, lNew = lNameBad))
}

perturb_name <- function(names, pError){
  vowels<- c("a","e","i","o","u","y")
  n <- length(names)
  badNames <- as.character(names)
  
  # half of errors are typos
  ind <- rbinom(n,1, prob = pError/2)
  badNames[ind == 1] <- str_replace(as.character(names)[ind == 1], "[aeiou]", sample(vowels,1))
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "ll", "l")
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "rr", "r")
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "C", "K")
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "K", "C")
  
  # half of errors are missing letters
  ind <- rbinom(n,1, prob = pError/2)
  charNames <- strsplit(badNames[ind==1], "")
  letters_to_drop <- lapply(charNames, sample, 1)
  badNames[ind == 1] <- str_replace(badNames[ind == 1], unlist(letters_to_drop), "")
  return(badNames)
}
  

