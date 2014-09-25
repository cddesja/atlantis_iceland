fg <- read.csv("~/Dropbox/hi/atlantis/iceland_atlantis/GroupsIceland.csv", sep = ";")

flagrec <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH"){
    x <- paste("flagrecruit",fg$Code[i], " 3 3\n", sep = "")
    flagrec <- append(flagrec,x)
  }
  if(fg$InvertType[i] == "BIRD" | fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK"){
    x <- paste("flagrecruit",fg$Code[i], " 12 3\n", sep = "")
    flagrec <- append(flagrec,x)
  }
}
cat(flagrec, sep ="")

flagrepo <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH" | fg$InvertType[i] == "BIRD" | fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK"){
    x <- paste("flagext_reprod",fg$Code[i], " 0 0\n", sep = "")
    flagrepo <- append(flagrepo,x)
  }
}
cat(flagrepo, sep ="")

flagsp <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH"){
    x <- paste("flagrecpeak",fg$Code[i], " 1 1\n", sep = "")
    flagsp <- append(flagsp,x)
  }
  if(fg$InvertType[i] == "BIRD" | fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK"){
    x <- paste("flagrecpeak",fg$Code[i], " 0 1\n", sep = "")
    flagsp <- append(flagsp,x)
  }
}
cat(flagsp, sep ="")

flagbearlive <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH" | fg$InvertType[i] == "BIRD"){
    x <- paste("flagbearlive",fg$Code[i], " 0 0\n", sep = "")
    flagbearlive <- append(flagbearlive,x)
  }
  if(fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK"){
    x <- paste("flagbearlive",fg$Code[i], " 1 1\n", sep = "")
    flagbearlive <- append(flagbearlive,x)
  }
}
cat(flagbearlive, sep ="")

flaglocalrecruit <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH"){
    x <- paste("flaglocalrecruit",fg$Code[i], " 0 0\n", sep = "")
    flaglocalrecruit <- append(flaglocalrecruit,x)
  }
  if(fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK" | fg$InvertType[i] == "BIRD"){
    x <- paste("flaglocalrecruit",fg$Code[i], " 1 1\n", sep = "")
    flaglocalrecruit <- append(flaglocalrecruit,x)
  }
}
cat(flaglocalrecruit, sep ="")

flagmother <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH"  | fg$InvertType[i] == "SHARK" | fg$InvertType[i] == "BIRD"){
    x <- paste("flagmother",fg$Code[i], " 0 0\n", sep = "")
    flagmother <- append(flagmother,x)
  }
  if(fg$InvertType[i] == "MAMMAL"){
    x <- paste("flagmother",fg$Code[i], " 1 1\n", sep = "")
    flagmother <- append(flagmother,x)
  }
}
cat(flagmother, sep ="")

feed_while_spawn <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH" | fg$InvertType[i] == "BIRD" | fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK"){
    x <- paste("feed_while_spawn",fg$Code[i], " 1 0\n", sep = "")
    feed_while_spawn <- append(feed_while_spawn,x)
  }
}
cat(feed_while_spawn, sep ="")

flagtempsensitive <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH" | fg$InvertType[i] == "BIRD" | fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK"){
    x <- paste("flagtempsensitive",fg$Code[i], " 0 1\n", sep = "")
    flagtempsensitive <- append(flagtempsensitive,x)
  }
}
cat(flagtempsensitive, sep ="")

habdepend <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH" | fg$InvertType[i] == "BIRD" | fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK"){
    x <- paste(fg$Code[i],"_habdepend 0 ", fg$Long.Name[i]," dependent on demersal habitat: 0 = no, 1 = yes 0  1\n", sep = "")
    habdepend <- append(habdepend,x)
  }
}
cat(habdepend, sep ="")

habdepend <- NULL
for(i in 1:nrow(fg)){
  if(fg$InvertType[i] == "FISH" | fg$InvertType[i] == "BIRD" | fg$InvertType[i] == "MAMMAL" | fg$InvertType[i] == "SHARK"){
    x <- paste(fg$Code[i],"_ddepend_move 0  0 = off, 1 = sedentary, 2 = on, 3 = sticky 0\n", sep = "")
    habdepend <- append(habdepend,x)
  }
}
cat(habdepend, sep ="")








