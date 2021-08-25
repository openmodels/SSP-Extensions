## setwd("~/research/iamup/SSP-Extensions")

library(reshape2)
library(dplyr)
library(ggplot2)

pop0 <- read.csv("data/pop_0.csv", skip=2)
gdp0 <- read.csv("data/gdp_0.csv", skip=2)

for (variable in c('pop', 'gdppc')) {
    if (variable == 'pop') {
        baseline <- pop0
        label <- "Population (millions)"
        toplimit <- 30000
    } else {
        baseline <- data.frame(region=gdp0$Region, gdppc0=gdp0$gdp_0 / pop0$pop_0)
        label <- "GDP per capita (USD)"
        toplimit <- 30000000
    }
    names(baseline)[2] <- 'value0'

    all.ssp.df <- data.frame()
    for (ssp in 1:5) {
        ## Add 2020 from original data
        if (variable == 'pop') {
            orig <- read.csv(paste0("data/ssp", ssp, "_pop_rate.csv"), skip=2)
            orig2 <- melt(orig, 'year', variable.name='region', value.name='rate')
            orig2$rate.lb <- orig2$rate
            orig2$rate.ub <- orig2$rate
        } else {
            orig.gdp <- read.csv(paste0("data/ssp", ssp, "_gdp_rate.csv"), skip=2)
            orig <- orig.gdp - read.csv(paste0("data/ssp", ssp, "_pop_rate.csv"), skip=2)
            orig$year <- orig.gdp$year
            orig2 <- melt(orig, 'year', variable.name='region', value.name='rate')
            orig2$rate.lb <- orig2$rate
            orig2$rate.ub <- orig2$rate
        }

        df <- read.csv(paste0("results/ssp", ssp, "-", variable, ".csv"))
        df2 <- rbind(subset(orig2, year == 2020), df)

        years <- c(2015, unique(df2$year))
        last.df <- baseline
        last.df$value0.lb <- baseline$value0
        last.df$value0.ub <- baseline$value0
        all.df <- cbind(year=2015, last.df)
        for (tt in 2:length(years)) {
            workdf <- last.df %>% left_join(subset(df2, year == years[tt]))
            value1 <- workdf$value0 * (1 + workdf$rate / 100)^(years[tt] - years[tt-1])
            value1.lb <- workdf$value0.lb * (1 + workdf$rate.lb / 100)^(years[tt] - years[tt-1])
            value1.ub <- workdf$value0.ub * (1 + workdf$rate.ub / 100)^(years[tt] - years[tt-1])
            last.df <- data.frame(region=workdf$region, value0=value1, value0.lb=value1.lb, value0.ub=value1.ub)
            all.df <- rbind(all.df, cbind(year=years[tt], last.df))
        }

        gp <- ggplot(all.df, aes(year, value0, colour=region, fill=region)) +
            geom_line() + geom_ribbon(aes(ymin=value0.lb, ymax=value0.ub), alpha=.5) +
            theme_bw() + scale_colour_discrete(name="Region") + scale_fill_discrete(name="Region") +
            scale_x_continuous(name=NULL, expand=c(0, 0)) + scale_y_continuous(name=label, expand=c(0, 0))
        ggsave(paste0("figures/ssp", ssp, "-", variable, ".pdf"), gp, width=5, height=5)

        all.ssp.df <- rbind(all.ssp.df, cbind(ssp, all.df))

        names(all.df)[3:5] <- paste0(variable, c('', '.lb', '.ub'))
        if (variable == 'pop')
            write.csv(all.df, paste0("results/ssp", ssp, "-", variable, "-million.csv"), row.names=F)
        else
            write.csv(all.df, paste0("results/ssp", ssp, "-", variable, "-usd.csv"), row.names=F)
    }

    gloall.ssp.df <- all.ssp.df %>% group_by(ssp, year) %>% summarize(value=sum(value0), value.lb=sum(value0.lb), value.ub=sum(value0.ub))

    if (variable == 'pop') {
        gp <- ggplot(gloall.ssp.df, aes(year, value, colour=factor(ssp), fill=factor(ssp))) +
            geom_line() + geom_ribbon(aes(ymin=value.lb, ymax=value.ub), alpha=.5) +
            coord_cartesian(ylim=c(0, toplimit)) + theme_bw() +
            scale_colour_discrete(name="SSP") + scale_fill_discrete(name="SSP") +
            scale_x_continuous(name=NULL, expand=c(0, 0)) + scale_y_continuous(name=label, expand=c(0, 0))
        ggsave(paste0("figures/", variable, ".pdf"), gp, width=5, height=5)
    }
}
