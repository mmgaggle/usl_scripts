#!/usr/bin/env Rscript

library(usl)
library(ggplot2)

args <- commandArgs(trailingOnly=TRUE)
csv <- args[1]
data <- read.csv(csv)

usl.model <- usl(iops ~ clients, data = data, method = 'nlxb')
i <- with(data, expand.grid(
 clients = seq(min(clients), max(clients), length=100)
))
c <- predict(usl.model, newdata = i, interval = "confidence")
c <- cbind(i, c)
p <- ggplot(data = data)
p <- p + geom_point(aes(x = clients, y = iops))
p <- p + geom_smooth(aes(x = clients, y = fit,
 ymin = lwr, ymax = upr),
 data = c, stat = "identity")
p <- p + ggtitle("USL Analysis of Ceph Storage Performance")
p <- p + scale_x_continuous("Clients")
p <- p + scale_y_continuous("Benchmark throughput (iop/s)")

ggsave("out.png", plot = last_plot())

