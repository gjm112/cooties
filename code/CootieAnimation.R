library(ggplot2)
library(gganimate)
set.seed(1234)
all <- list()
for (j in 1:30){
  dat <- data.frame(x = rep(NA, 100), y = rep(NA, 100), frame = 1:100, sex = "B", id = j, cooties  = 0)
  dat$x[1] <- runif(1,-1,1)
  dat$y[1] <- runif(1,-5,5)
  for (i in 2:100){
    dat$x[i] <- runif(1,-1,1)
    dat$x[i] <- ifelse(dat$x[i] < -1, -1,dat$x[i])
    dat$x[i] <- ifelse(dat$x[i] > 1, 1,dat$x[i])
    dat$y[i] <- dat$y[i-1] + runif(1,-0.5,0.5)
  }
  all[[j]] <- dat
}

greg <- do.call(rbind,all)

set.seed(12345)
all <- list()
for (j in 1:30){
  dat <- data.frame(x = rep(NA, 100), y = rep(NA, 100), frame = 1:100, sex = "G", id = j, cooties  = 0)
  dat$x[1] <- runif(1,-1,1)
  dat$y[1] <- runif(1,-5,5)
  for (i in 2:100){
    dat$x[i] <-  runif(1,-1,1)
    dat$x[i] <- ifelse(dat$x[i] < -1, -1,dat$x[i])
    dat$x[i] <- ifelse(dat$x[i] > 1, 1,dat$x[i])
    dat$y[i] <- dat$y[i-1] + runif(1,-0.5,0.5)
  }
  all[[j]] <- dat
}

sarah <- do.call(rbind,all)

both <- rbind(sarah,greg)
both$new <- 0


#Split into cooties and non cooties 
#Now add cooties info
library(tidyverse)
both$cooties[both$sex == "B" & both$id == 1 ] <- 1
both$cooties[both$sex == "G" & both$id == 1 ] <- 1


#Frame 1
fff <- 1
for (fff in 1:100){print(fff)
  #Check boys
  coot <- both %>% filter(sex == "G" & frame == fff & cooties == 1)
  
  ids <- both %>% filter(sex == "B" & frame == fff) %>% inner_join(coot, by = "frame") %>% 
    mutate(dist = sqrt((x.x-x.y)^2 + (y.x-y.y)^2)) %>% filter(dist < 0.25) %>% pull(id.x)
  
  #New cooties cases
  both$cooties[both$sex == "B" & both$frame > fff & both$id %in% ids] <- 1
  
  
  #Now check girls
  coot <- both %>% filter(sex == "B" & frame == fff & cooties == 1)
  ids <- both %>% filter(sex == "G" & frame == fff) %>% inner_join(coot, by = "frame") %>% 
    mutate(dist = sqrt((x.x-x.y)^2 + (y.x-y.y)^2)) %>% filter(dist < 0.25) %>% pull(id.x)
  
  #New cooties cases
  both$cooties[both$sex == "G" & both$frame > fff & both$id %in% ids] <- 1
  
  
}

both


#Merge on time 
tim <- paste0(floor(seq(6,10,length = 100)),":",floor((seq(6,10,length = 100) - floor(seq(6,10,length = 100)))*60)," pm")
tim[nchar(tim) == 6] <- paste0(substring(tim[nchar(tim) == 6],1,2),"0",substring(tim[nchar(tim) == 6],3,6))
times <- data.frame(frame = 1:100, time = tim)
both <- both %>% left_join(times, by = c("frame" = "frame"))

g <- ggplot(aes(x = x, y = y, col = sex), data = both) + geom_point(size = 5, alpha = 0.2) + geom_point(aes(x = x, y = y, col = sex), data = both %>% filter(cooties == 1), size = 10) + transition_time(frame) + scale_color_manual(values = c("blue","pink")) 
animate(g, nframes = 100, fps=2.5)
anim_save(filename = "/Users/gregorymatthews/test_first.png",animate(g, nframes = 100, fps=2.5))     

both %>% group_by(frame,sex) %>% summarize(cooties = sum(cooties)) %>% ggplot(aes(x = frame, y = cooties, col = sex)) + geom_line(lwd = 3) + scale_colour_manual(values = c("blue","pink")) + scale_x_continuous(breaks=c(0,25,50,75,100), labels = c("6pm","6:30pm","7pm","7:30pm","8pm")) + ylim(0,30)


set.seed(1234)
all <- list()
for (j in 1:30){
dat <- data.frame(x = rep(NA, 100), y = rep(NA, 100), frame = 1:100, sex = "B", id = j, cooties  = 0)
dat$x[1] <- -1 
dat$y[1] <- runif(1,-5,5)
for (i in 2:100){
  dat$x[i] <- dat$x[i-1] + rexp(1) - 2
  dat$x[i] <- ifelse(dat$x[i] < -1, -1,dat$x[i])
  dat$x[i] <- ifelse(dat$x[i] > 1, 1,dat$x[i])
  dat$y[i] <- dat$y[i-1] + runif(1,-0.5,0.5)
}
all[[j]] <- dat
}

greg <- do.call(rbind,all)

set.seed(12345)
all <- list()
for (j in 1:30){
  dat <- data.frame(x = rep(NA, 100), y = rep(NA, 100), frame = 1:100, sex = "G", id = j, cooties  = 0)
  dat$x[1] <- 1 
  dat$y[1] <- runif(1,-5,5)
  for (i in 2:100){
    dat$x[i] <- dat$x[i-1] - rexp(1) + 2
    dat$x[i] <- ifelse(dat$x[i] < -1, -1,dat$x[i])
    dat$x[i] <- ifelse(dat$x[i] > 1, 1,dat$x[i])
    dat$y[i] <- dat$y[i-1] + runif(1,-0.5,0.5)
  }
  all[[j]] <- dat
}

sarah <- do.call(rbind,all)

both <- rbind(sarah,greg)
both$new <- 0


#Split into cooties and non cooties 
#Now add cooties info
library(tidyverse)
both$cooties[both$sex == "B" & both$id == 1 ] <- 1
both$cooties[both$sex == "G" & both$id == 1 ] <- 1


#Frame 1
fff <- 1
for (fff in 1:100){print(fff)
#Check boys
coot <- both %>% filter(sex == "G" & frame == fff & cooties == 1)

ids <- both %>% filter(sex == "B" & frame == fff) %>% inner_join(coot, by = "frame") %>% 
  mutate(dist = sqrt((x.x-x.y)^2 + (y.x-y.y)^2)) %>% filter(dist < 0.25) %>% pull(id.x)

#New cooties cases
both$cooties[both$sex == "B" & both$frame > fff & both$id %in% ids] <- 1


#Now check girls
coot <- both %>% filter(sex == "B" & frame == fff & cooties == 1)
ids <- both %>% filter(sex == "G" & frame == fff) %>% inner_join(coot, by = "frame") %>% 
  mutate(dist = sqrt((x.x-x.y)^2 + (y.x-y.y)^2)) %>% filter(dist < 0.25) %>% pull(id.x)

#New cooties cases
both$cooties[both$sex == "G" & both$frame > fff & both$id %in% ids] <- 1


}

both


#Merge on time 
tim <- paste0(floor(seq(6,10,length = 100)),":",floor((seq(6,10,length = 100) - floor(seq(6,10,length = 100)))*60)," pm")
tim[nchar(tim) == 6] <- paste0(substring(tim[nchar(tim) == 6],1,2),"0",substring(tim[nchar(tim) == 6],3,6))
times <- data.frame(frame = 1:100, time = tim)
both <- both %>% left_join(times, by = c("frame" = "frame"))

g <- ggplot(aes(x = x, y = y, col = sex), data = both) + geom_point(size = 5, alpha = 0.2) + geom_point(aes(x = x, y = y, col = sex), data = both %>% filter(cooties == 1), size = 10) + transition_time(frame) + scale_color_manual(values = c("blue","pink")) 
animate(g, nframes = 100, fps=2.5)
anim_save(filename = "/Users/gregorymatthews/test.png",animate(g, nframes = 100, fps=2.5))     

both %>% group_by(frame,sex) %>% summarize(cooties = sum(cooties)) %>% ggplot(aes(x = frame, y = cooties, col = sex)) + geom_line(lwd = 3) + scale_colour_manual(values = c("blue","pink")) + scale_x_continuous(breaks=c(0,25,50,75,100), labels = c("6pm","6:30pm","7pm","7:30pm","8pm")) + ylim(0,30)



#Repeat with a vaccine 
library(ggplot2)
library(gganimate)
set.seed(1234)
all <- list()
for (j in 1:30){
  dat <- data.frame(x = rep(NA, 100), y = rep(NA, 100), frame = 1:100, sex = "B", id = j, cooties  = 0)
  dat$x[1] <- -1 
  dat$y[1] <- runif(1,-5,5)
  for (i in 2:100){
    dat$x[i] <- dat$x[i-1] + rexp(1) - 2
    dat$x[i] <- ifelse(dat$x[i] < -1, -1,dat$x[i])
    dat$x[i] <- ifelse(dat$x[i] > 1, 1,dat$x[i])
    dat$y[i] <- dat$y[i-1] + runif(1,-0.5,0.5)
  }
  all[[j]] <- dat
}

greg <- do.call(rbind,all)

set.seed(12345)
all <- list()
for (j in 1:30){
  dat <- data.frame(x = rep(NA, 100), y = rep(NA, 100), frame = 1:100, sex = "G", id = j, cooties  = 0)
  dat$x[1] <- 1 
  dat$y[1] <- runif(1,-5,5)
  for (i in 2:100){
    dat$x[i] <- dat$x[i-1] - rexp(1) + 2
    dat$x[i] <- ifelse(dat$x[i] < -1, -1,dat$x[i])
    dat$x[i] <- ifelse(dat$x[i] > 1, 1,dat$x[i])
    dat$y[i] <- dat$y[i-1] + runif(1,-0.5,0.5)
  }
  all[[j]] <- dat
}

sarah <- do.call(rbind,all)

both <- rbind(sarah,greg)
both$vax <- 0
both$vax[both$id > 20] <- 1



#Split into cooties and non cooties 
#Now add cooties info
library(tidyverse)
both$cooties[both$sex == "B" & both$id == 1 ] <- 1
both$cooties[both$sex == "G" & both$id == 1 ] <- 1


#Frame 1
fff <- 1
for (fff in 1:100){print(fff)
  #Check boys
  coot <- both %>% filter(sex == "G" & frame == fff & cooties == 1)
  
  ids <- both %>% filter(sex == "B" & frame == fff) %>% inner_join(coot, by = "frame") %>% 
    mutate(dist = sqrt((x.x-x.y)^2 + (y.x-y.y)^2)) %>% filter(dist < 0.25) %>% pull(id.x)
  
  #New cooties cases
  both$cooties[both$sex == "B" & both$frame > fff & both$id %in% ids & both$vax == 0] <- 1
  ids <- ids[runif(length(ids)) < 0.1]
  both$cooties[both$sex == "B" & both$frame > fff & both$id %in% ids & both$vax == 1] <- 1
  
  #Now check girls
  coot <- both %>% filter(sex == "B" & frame == fff & cooties == 1)
  ids <- both %>% filter(sex == "G" & frame == fff) %>% inner_join(coot, by = "frame") %>% 
    mutate(dist = sqrt((x.x-x.y)^2 + (y.x-y.y)^2)) %>% filter(dist < 0.25) %>% pull(id.x)
  
  #New cooties cases
  both$cooties[both$sex == "G" & both$frame > fff & both$id %in% ids & both$vax == 0] <- 1
  ids <- ids[runif(length(ids)) < 0.1]
  both$cooties[both$sex == "G" & both$frame > fff & both$id %in% ids & both$vax == 1] <- 1
  
  
}

both


#Merge on time 
tim <- paste0(floor(seq(6,10,length = 100)),":",floor((seq(6,10,length = 100) - floor(seq(6,10,length = 100)))*60)," pm")
tim[nchar(tim) == 6] <- paste0(substring(tim[nchar(tim) == 6],1,2),"0",substring(tim[nchar(tim) == 6],3,6))
times <- data.frame(frame = 1:100, time = tim)
both <- both %>% left_join(times, by = c("frame" = "frame"))

g <- ggplot(aes(x = x, y = y, col = sex), data = both) + geom_point(size = 5, alpha = 0.2) + geom_point(aes(x = x, y = y, col = sex), data = both %>% filter(cooties == 1), size = 10) + transition_time(frame) + scale_color_manual(values = c("blue","pink")) 
animate(g, nframes = 100, fps=2.5)
anim_save(filename = "/Users/gregorymatthews/test_withvax.png",animate(g, nframes = 100, fps=2.5))     

both %>% group_by(frame,sex) %>% summarize(cooties = sum(cooties)) %>% ggplot(aes(x = frame, y = cooties, col = sex)) + geom_line(lwd = 3) + scale_colour_manual(values = c("blue","pink")) + scale_x_continuous(breaks=c(0,25,50,75,100), labels = c("6pm","6:30pm","7pm","7:30pm","8pm")) + ylim(0,30)
















#Now add cooties info
library(tidyverse)
both$cooties[both$sex == "B" & both$id == 1 ] <- 1

#Calculate the distance 
for (fff in 1:100){print(fff)
temp <- both %>% filter(frame == fff) 
cooties <- both %>% filter(frame == fff & cooties == 1) 

mindist <- rep(Inf,10)
#check girls 
if (any(cooties$sex == "B")){
  #x <- temp[temp$sex == "G",][1,]
  ggg <- temp[temp$sex == "G",]
  findmindist <- function(x){
    vec <- rep(NA, nrow(cooties))
    for (q in 1:nrow(cooties)){
    vec[q] <- sqrt(sum((x[,c("x","y")] - cooties[cooties$sex == "B",c("x","y")][q,])^2))
    }
    min(vec)
  }
  
mindist <- rep(NA,nrow(ggg))
 for (q in 1:nrow(ggg)){
  mindist[q] <- (findmindist(ggg[q,]))
 }

both$mindist[both$frame == fff & both$sex == "G"] <- mindist
  }

mindist <- rep(Inf,10)
#check boys
if (any(cooties$sex == "G")){
  #x <- temp[temp$sex == "G",][1,]
  bbb <- temp[temp$sex == "B",]
  findmindist <- function(x){
    vec <- rep(NA, nrow(cooties))
    for (q in 1:nrow(cooties)){
      vec[q] <- sqrt(sum((x[,c("x","y")] - cooties[cooties$sex == "G",c("x","y")][q,])^2))
    }
    min(vec)
  }
  
  mindist <- rep(NA,nrow(bbb))
  for (q in 1:nrow(bbb)){
    mindist[q] <- (findmindist(bbb[q,]))
  }
  
  both$mindist[both$frame == fff & both$sex == "B"] <- mindist
}

#Update who got cooties


}



both %>% filter(id == 1 & sex == "G")


g <- ggplot(aes(x = x, y = y, col = sex), data = both) + geom_point(size = 3) + transition_time(frame) 
animate(g, nframes = 100, fps=1)




#Create 10 girls and 10 boys 




ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')




