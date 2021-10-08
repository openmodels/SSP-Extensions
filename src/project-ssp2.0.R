## setwd("~/research/SSP-Extensions/src")

library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)

df.pop <- read_excel("../data/ssp2.0-pop.xlsx")
df.gdp <- read_excel("../data/ssp2.0-gdp.xlsx")

df.pop.long <- melt(df.pop, names(df.pop)[1:5], variable.name='year')
df.gdp.long <- melt(df.gdp, names(df.gdp)[1:5], variable.name='year')

df <- df.pop.long %>% left_join(df.gdp.long, by=c('year', names(df.pop)[1:3]), suffix=c('.pop', '.gdp'))
df <- subset(df, year != 'Notes' & !is.na(Scenario))
df$year <- as.numeric(as.character(df$year))

for (model in c("IIASA GDP", "OECD Env-Growth")) {
    print(model)
    for (ssp in 1:5) {
        for (variable in c('pop', 'gdppc')) {
            subdf <- subset(df, Model == model & Scenario == paste0('SSP', ssp))

            if (variable == 'pop') {
                knowns <- subdf[, c('Region', 'year', 'value.pop')]
                label <- "Population (millions)"
                toplimit <- 30000
            } else {
                knowns <- cbind(subdf[, c('Region', 'year')], value.gdppc=1000 * subdf$value.gdp / subdf$value.pop)
                label <- "GDP per capita (USD)"
                toplimit <- 30000000
            }
            names(knowns)[3] <- 'value'

            results <- read.csv(paste0("../results/ssp2.0-", model, "-SSP", ssp, "-", variable, ".csv"))

            years <- 2015:max(results$year)

            last.df <- NULL
            all.df <- data.frame()
            for (tt in 1:length(years)) {
                if (years[tt] %in% knowns$year) {
                    last.df <- subset(knowns, year == years[tt]) %>% left_join(subset(results, year == years[tt]))
                    last.df$value.lb <- last.df$value
                    last.df$value.ub <- last.df$value
                } else {
                    value1 <- last.df$value * (1 + last.df$rate / 100)
                    value1.lb <- last.df$value.lb * (1 + last.df$rate.lb / 100)
                    value1.ub <- last.df$value.ub * (1 + last.df$rate.ub / 100)
                    last.df$value <- value1
                    last.df$value.lb <- value1.lb
                    last.df$value.ub <- value1.ub
                    last.df$year <- years[tt]
                    if (years[tt] %in% results$year) {
                        last.df.tmp <- last.df %>% left_join(subset(results, year == years[tt]), by=c('Region', 'year'), suffix=c('.old', ''))
                        last.df <- last.df.tmp[, c('Region', 'year', 'value', 'rate', 'rate.lb', 'rate.ub', 'value.lb', 'value.ub')]
                    }
                }
                all.df <- rbind(all.df, last.df[, c('Region', 'year', 'value', 'value.lb', 'value.ub')])
            }

            gp <- ggplot(all.df, aes(year, value, colour=Region, fill=Region)) +
                geom_line() + geom_ribbon(aes(ymin=value.lb, ymax=value.ub), alpha=.5) +
                theme_bw() + scale_colour_discrete(name="Region") + scale_fill_discrete(name="Region") +
                scale_x_continuous(name=NULL, expand=c(0, 0)) + scale_y_continuous(name=label, expand=c(0, 0)) +
                guides(fill="none", colour="none")

            if (variable == 'pop') {
                ggsave(paste0("../figures/ssp2.0-", model, "-SSP", ssp, "-", variable, "-million.pdf"), gp, width=5, height=5)
                write.csv(all.df, paste0("../results/ssp2.0-", model, "-SSP", ssp, "-", variable, "-million.csv"), row.names=F)
            } else {
                ggsave(paste0("../figures/ssp2.0-", model, "-SSP", ssp, "-", variable, "-usd.pdf"), gp, width=5, height=5)
                write.csv(all.df, paste0("../results/ssp2.0-", model, "-SSP", ssp, "-", variable, "-usd.csv"), row.names=F)
            }
        }
    }
}
