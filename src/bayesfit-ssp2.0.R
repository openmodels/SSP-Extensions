## setwd("~/research/SSP-Extensions/src")

library(readxl)
library(reshape2)
library(dplyr)
library(ggplot2)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

results <- data.frame(ssp=c(), variable=c(), converge=c(), decay=c())

## Read in SSP data
df.pop <- read_excel("../data/ssp2.0-pop.xlsx")
df.gdp.rate <- read_excel("../data/ssp2.0-gdp-rate.xlsx")
df.pop.rate <- read_excel("../data/ssp2.0-pop-rate.xlsx")

## Combine
df.pop.long <- melt(df.pop, names(df.pop)[1:5], variable.name='year')
df.gdp.rate.long <- melt(df.gdp.rate, names(df.gdp.rate)[1:5], variable.name='year')
df.pop.rate.long <- melt(df.pop.rate, names(df.pop.rate)[1:5], variable.name='year')

df <- df.gdp.rate.long %>% left_join(df.pop.rate.long, by=c('year', names(df.pop)[1:3]), suffix=c('.gdp.rate', '')) %>% left_join(df.pop.long, by=c('year', names(df.pop)[1:3]), suffix=c('.pop.rate', '.pop'))
df <- subset(df, year != 'Notes' & !is.na(Scenario))
df$year <- as.numeric(as.character(df$year))

for (model in c("IIASA GDP", "OECD Env-Growth")) {
    print(model)
    for (ssp in 1:5) {
        for (variable in c('gdppc', 'pop')) {
            subdf <- subset(df, Model == model & Scenario == paste0('SSP', ssp))
            if (variable == 'gdppc') {
                subdf$rate <- subdf$value.gdp.rate - subdf$value.pop.rate
            } else
                subdf$rate <- subdf$value.pop.rate

            ## Fill in extra columns
            subdf$rate.lag <- NA
            subdf$meanrate.lag <- NA
            subdf$ydiff <- NA

            years <- unique(subdf$year)
            weights <- subdf$value.pop[subdf$year == years[1]]
            weights[is.na(weights)] <- 0

            for (yi in 2:length(years)) {
                subdf$ydiff[subdf$year == years[yi]] <- years[yi] - years[yi-1]
                subdf$rate.lag[subdf$year == years[yi]] <- subdf$rate[subdf$year == years[yi-1]]
                subdf$meanrate.lag[subdf$year == years[yi]] <- sum(subdf$rate[subdf$year == years[yi-1]] * weights, na.rm=T) / sum(!is.na(subdf$rate[subdf$year == years[yi-1]]) * weights)
            }

            weights <- weights[(!is.na(subdf$ydiff) & !is.na(subdf$rate))[subdf$year == years[2]]]
            subdf <- subdf[!is.na(subdf$ydiff) & !is.na(subdf$rate),]

            ## Simple Bayesian model
            stan.code <- "
data {
  int<lower=0> N; // observations
  int<lower=0> M; // regions

  int<lower=1, upper=M> region[N];

  vector[N] rate; // growth rate
  vector[N] rate_lag; // previous period growth rate
  vector[N] meanrate_lag; // previous period average growth rate
  vector[N] ydiff; // years between periods
}
parameters {
  real<lower=0, upper=0.5> converge; // rate of convergence
  real<lower=0, upper=0.5> decay; // rate of decay

  real<lower=0> sigma[M];
}
model {
  // Assume that every growth observation derives from this model
  for (ii in 1:N) {
    rate[ii] ~ normal(rate_lag[ii] * (1 - ydiff[ii] * (converge + decay)) + meanrate_lag[ii] * ydiff[ii] * converge, sigma[region[ii]]);
  }
}"

            ## Setup data
            subdf$Region <- factor(subdf$Region)
            stan.data <- list(N=nrow(subdf), M=length(unique(subdf$Region)),
                              region=as.numeric(subdf$Region), rate=subdf$rate,
                              rate_lag=subdf$rate.lag, meanrate_lag=subdf$meanrate.lag, ydiff=subdf$ydiff)

            ## Fit model
            fit <- stan(model_code=stan.code, data=stan.data, iter=1000, chains=4)
            la0 <- extract(fit, permute=T)

            ## Full Bayesian model
            stan.code.iterative <- "
data {
  int<lower=0> N; // periods
  int<lower=0> M; // regions
  int<lower=0> Tmaxp1; // max ydiff years plus 1

  vector[M] rate[N]; // growth rate
  vector[M] rate_lag[N]; // previous period growth rate
  int<lower=0> ydiff[N]; // years between periods
  vector[M] weights; // weights for weight averaging

  real converge_mu;
  real converge_sigma;
  real decay_mu;
  real decay_sigma;
  real logsigma_mu[M];
  real logsigma_sigma[M];
}
parameters {
  real<lower=0, upper=0.5> converge; // rate of convergence
  real<lower=0, upper=0.5> decay; // rate of decay

  real<lower=0> sigma[M];
}
transformed parameters {
  vector[M] rate_pred[N,Tmaxp1]; // predicted rate

  for (ii in 1:N) {
    rate_pred[ii, 1] = rate_lag[ii];
    for (tt in 1:ydiff[ii]) {
       rate_pred[ii, tt+1] = rate_pred[ii, tt] * (1 - converge - decay) + converge * dot_product(rate_pred[ii, tt], weights);
    }
    if (ydiff[ii] + 1 < Tmaxp1) {
      for (tt in (ydiff[ii]+1):Tmaxp1)
        rate_pred[ii, tt] = 0 * weights;
    }
  }
}
model {
  // Assume that every growth observation derives from this model
  for (ii in 1:N) {
    rate[ii] ~ normal(rate_pred[ii, ydiff[ii] + 1], sigma);
  }
  converge ~ normal(converge_mu, converge_sigma);
  decay ~ normal(decay_mu, decay_sigma);
  sigma ~ lognormal(logsigma_mu, logsigma_sigma);
}"

            # remove two periods because don't have lag rate for 2010 or rate for 2100
            stan.data <- list(N=length(years) - 2, M=length(unique(subdf$Region)), Tmaxp1=6,
                              rate=dcast(subdf, year ~ Region, value.var='rate')[, -1],
                              rate_lag=dcast(subdf, year ~ Region, value.var='rate.lag')[, -1],
                              ydiff=diff(years)[-1], weights=weights / sum(weights),
                              converge_mu=mean(la0$converge), converge_sigma=sd(la0$converge),
                              decay_mu=mean(la0$decay), decay_sigma=sd(la0$decay),
                              logsigma_mu=apply(log(la0$sigma), 2, mean), logsigma_sigma=apply(log(la0$sigma), 2, sd))

            ## Fit model
            fit <- stan(model_code=stan.code.iterative, data=stan.data, iter=1000, chains=4,
                        init=function() {
                            list(converge=sample(la0$converge, 1),
                                 decay=sample(la0$decay, 1),
                                 sigma=la0$sigma[sample(1:nrow(la0$sigma), 1),])
                        }, pars=c('converge', 'decay', 'sigma'))
            la <- extract(fit, permute=T)

            ## Check results
            mean(la$converge)
            quantile(la$converge, probs=c(.025, .975))

            mean(la$decay)
            quantile(la$decay, probs=c(.025, .975))

            results <- rbind(results, data.frame(ssp, variable, converge=mean(la$converge), decay=mean(la$decay)))

            ## Predict it into the future
            dfpred <- subdf[, c('year', 'Region', 'rate')]
            dfpred$rate.lb <- subdf$rate
            dfpred$rate.ub <- subdf$rate

            rate_lags <- t(matrix(dfpred$rate[dfpred$year == 2095], length(unique(subdf$Region)), 2000))
            for (year in 2100:2300) {
                meanrate_lag <- as.numeric(rate_lags %*% weights / sum(weights))
                rates <- matrix(NA, 2000, length(unique(subdf$Region)))
                for (rr in 1:length(unique(subdf$Region))) {
                    if (year == 2100)
                        rates[, rr] <- rate_lags[, rr] * (1 - 5 * (la$converge + la$decay)) + 5 * meanrate_lag * la$converge
                    else
                        rates[, rr] <- rate_lags[, rr] * (1 - la$converge - la$decay) + meanrate_lag * la$converge

                    rate.lb <- quantile(rates[, rr], probs=.025)
                    rate.ub <- quantile(rates[, rr], probs=.975)
                    dfpred <- rbind(dfpred, data.frame(year, Region=unique(dfpred$Region)[rr], rate=mean(rates[, rr]),
                                                       rate.lb, rate.ub))
                }

                rate_lags = rates
            }

            ## Plot the results
            gp <- ggplot(dfpred, aes(year, rate)) +
                geom_ribbon(aes(ymin=rate.lb, ymax=rate.ub, fill=Region), alpha=.5) +
                geom_line(aes(colour=Region)) +
                theme_bw() + xlab(NULL) +
                guides(fill="none", colour="none")
            if (variable == 'gdppc')
                gp <- gp + ylab("GDP per capita growth")
            else
                gp <- gp + ylab("Population growth")

            ggsave(paste0("../figures/ssp2.0-", model, "-SSP", ssp, "-", variable, ".pdf"), gp, width=6, height=4)

            write.csv(dfpred, paste0("../results/ssp2.0-", model, "-SSP", ssp, "-", variable, ".csv"), row.names=F)
        }
    }
}
