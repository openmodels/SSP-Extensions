## setwd("~/research/iamup/SSP-Extensions/data/")

library(reshape2)
library(dplyr)
library(ggplot2)

pop0 <- read.csv("../data/pop_0.csv", skip=2)

all.ssp.pop <- data.frame()
for (ssp in 1:5) {
    df <- read.csv(paste0("../results/ssp", ssp, "-pop.csv"))

    ## Add 2020 from original data
    orig <- read.csv(paste0("../data/ssp", ssp, "_pop_rate.csv"), skip=2)
    orig2 <- melt(orig, 'year', variable.name='region', value.name='rate')
    orig2$rate.lb <- orig2$rate
    orig2$rate.ub <- orig2$rate

    df2 <- rbind(subset(orig2, year == 2020), df)

    years <- c(2015, unique(df2$year))
    last.pop <- pop0
    last.pop$pop_0.lb <- pop0$pop_0
    last.pop$pop_0.ub <- pop0$pop_0
    all.pop <- cbind(year=2015, last.pop)
    for (tt in 2:length(years)) {
        workdf <- last.pop %>% left_join(subset(df2, year == years[tt]))
        pop1 <- workdf$pop_0 * (1 + workdf$rate / 100)^(years[tt] - years[tt-1])
        pop1.lb <- workdf$pop_0.lb * (1 + workdf$rate.lb / 100)^(years[tt] - years[tt-1])
        pop1.ub <- workdf$pop_0.ub * (1 + workdf$rate.ub / 100)^(years[tt] - years[tt-1])
        last.pop <- data.frame(region=workdf$region, pop_0=pop1, pop_0.lb=pop1.lb, pop_0.ub=pop1.ub)
        all.pop <- rbind(all.pop, cbind(year=years[tt], last.pop))
    }

    names(all.pop)[3:5] <- c('pop', 'pop.lb', 'pop.ub')

    ## ggplot(all.pop, aes(year, pop, colour=region, fill=region)) +
    ##     geom_line() + geom_ribbon(aes(ymin=pop.lb, ymax=pop.ub), alpha=.5)

    write.csv(all.pop, paste0("../results/ssp", ssp, "-pop-million.csv"), row.names=F)

    all.ssp.pop <- rbind(all.ssp.pop, cbind(ssp, all.pop))
}

gloall.ssp.pop <- all.ssp.pop %>% group_by(ssp, year) %>% summarize(pop=sum(pop), pop.lb=sum(pop.lb), pop.ub=sum(pop.ub))

ggplot(gloall.ssp.pop, aes(year, pop / 1000, colour=factor(ssp), fill=factor(ssp))) +
    geom_line() + geom_ribbon(aes(ymin=pop.lb / 1000, ymax=pop.ub / 1000), alpha=.5) +
    coord_cartesian(ylim=c(0, 30)) + theme_bw() + scale_colour_discrete(name="SSP") + scale_fill_discrete(name="SSP") +
    scale_x_continuous(name=NULL, expand=c(0, 0)) + scale_y_continuous(name="Global population (billion)", expand=c(0, 0))
