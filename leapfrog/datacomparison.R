#data.full <- SA_testing.data2
#data.full <- cbind(data.full,NA,NA,NA,NA)
#names(data.full) <- c("map","iteration","time","SA","LF","optimal","RE_SA","RE_LF")

#data.full$LF <- LF_testing.data$distance
#data.full$optimal<- course.list$distance[match(course.list$name[data.full$map],course.list$name)]
#data.full$RE_LF <- (data.full$LF-data.full$optimal)/data.full$optimal
#data.full$RE_SA <- (data.full$SA-data.full$optimal)/data.full$optimal
#data.full$RE_LF <- data.full$RE_LF*100
#data.full$RE_SA <- data.full$RE_SA*100
#data.full <- cbind(NA,data.full)
#names(data.full)[1] <- "nodes"
# eil51, ts225, pr1002, gr120, rat195, Bays29, Berlin52, Cho130, KroA100, pcb442, pr76, gr48, pma343
data.full[1:20,1] <- 51
data.full[21:40,1] <- 225
data.full[41:60,1] <- 1002
data.full[61:80,1] <- 120
data.full[81:100,1] <- 195
data.full[101:120,1] <- 29
data.full[121:140,1] <- 52
data.full[141:160,1] <- 130
data.full[161:180,1] <- 100
data.full[181:200,1] <- 442
data.full[201:220,1] <- 76
data.full[221:240,1] <- 48
data.full[241:260,1] <- 343
data.full <- data.full[1:260,]
data.full <- data.full[order(data.full$nodes),]
data.full.backup <- data.full
#save(data.full, file="data_full.RData")
boxplot(time~nodes,
        data=data.full,
        main="Runtimes vs Node Count",
        xlab="Node Count",
        ylab="Runtime",
        col="orange",
        border="brown"
)
boxplot(RE_LF~nodes,
        data=data.full,
        main="Relative Error vs Node Count",
        xlab="Node Count",
        ylab="Relative Error",
        col="green",
        border="blue",
        ylim = c(0, 35)
)
boxplot(RE_SA~nodes,
        data=data.full,
        add = TRUE,
        col = "red")
legend(0.5, 35, c("SA", "LF"),
       fill = c("red", "blue"))


