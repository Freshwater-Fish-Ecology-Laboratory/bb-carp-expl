# Create two m-arrays for Carp Lake. one with high-reward tags and one with standard tags. 
# should have pre-season and post-season as release occasions, year of recovery as recovery season
# Tag type 1 is standard.
# Tag type 2 is high-reward.
###############################################
library(tidyverse) 
library(jagsUI)
library(MCMCvis)
library(RColorBrewer)
library(HDInterval)

# Read in data: tag returns in m-array format with both tag types, identified by t
m_array_carp <- read_csv("carp_lk/data/raw/m_array_carp_v2.csv")

# Split into 2 df's so we have the format of the 3 dimensional m-array. Convert from a list 
# to an array. 
m_array_carp <- split(m_array_carp, m_array_carp$t)
m_array_carp[[1]] <- m_array_carp[[1]][, 3:7]
m_array_carp[[2]] <- m_array_carp[[2]][, 3:7]

m_array_carp <- array(unlist(list(rbind(m_array_carp[[1]], rep(0, 4), rep(0, 4), rep(0, 4), rep(0, 4)),
                      rbind(m_array_carp[[2]], rep(0, 4), rep(0, 4), rep(0, 4), rep(0, 4)))),
                      dim = c(7, 5, 2))



# Number of years of tag-releases:
yrs <- 4

# Adjust rel_yrs to match rows in rel
rel_yrs <- rep(1:yrs, each = 2)

# Number of tags released for each occasion (pre and post; for tag type 1 and 2):
ntags_pre_1 <- c(47, 59, 0, 0)
ntags_post_1 <- c(11, 0, 0, NA)
ntags_pre_2 <- c(44, 63, 0, 0)
ntags_post_2 <- c(12, 0, 0, NA)

rel_pre <- cbind(ntags_pre_1, ntags_pre_2)
rel_pos <- cbind(ntags_post_1, ntags_post_2)

rel <- matrix(NA, nrow = 2 * nrow(rel_pre), ncol = 2)

for (t in 1:2) {
  rel[, t] <- as.vector(rbind(rel_pre[, t], rel_pos[, t]))
}

rel <- na.exclude(rel)

pre_idx <- seq(1, nrow(rel), 2)
pos_idx <- seq(2, nrow(rel), 2)


###############################################
# Define model:
model_path <- "carp_lk/codes/R/model_carp.R"
###############################################

# Running model:

# Bundle data
jags_data <- list(m_array_carp = m_array_carp, rel = rel, Y = yrs, R = nrow(rel), 
                  pre_idx = pre_idx, pos_idx = pos_idx,
                  rel_yrs = rel_yrs)

# Initial values
inits <- function(){list(mean_s = runif(1, 0.5, 1), mean_h = runif(1, 0, 0.3),
                         mean_l = runif(1, 0.5, 1))}

# Parameters monitored
parameters <- c("mean_s", "mean_h", "mean_l")

# MCMC settings
ni <- 100000
nb <- ni / 2
nt <- (ni - nb) / 500
nc <- 4

# Call JAGS from R --------------------------------------------------------

fit <- jagsUI(jags_data, inits = inits, parallel=TRUE, 
              parameters.to.save=parameters,  model_path, 
              n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, DIC=FALSE)

mdn_s <- fit$q50$mean_s
mdn_h <- fit$q50$mean_h
mdn_l <- fit$q50$mean_l
Rhat <- all(unlist(fit[[1]]$Rhat) < 1.1)


out <- list(s = mdn_s, h = mdn_h, l = mdn_l, fit = fit, 
            Rhat = Rhat)

# Save RDS file for output ------------------------------------------------

saveRDS(out, paste("carp_lk/data/output_2tag_", ".rds", sep = ""))  

MCMCsummary(fit, params = c("mean_s", "mean_h", "mean_l"))
MCMCtrace(fit, params = c("mean_s", "mean_h", "mean_l"),
          pdf = FALSE)

hdi(fit)
# Summarizing and reporting results! ---------------------------------------

# plots for presentation 3/7/25
library(grid)
library(gridExtra)

tagdata <- read.csv("carp_lk/data/raw/tag_type.csv") %>%
  rename("Tag type" = type)

repdata <- read.csv("carp_lk/data/raw/rep_method.csv") %>%
  rename("Reporting method" = rec.method)

tagplt <- ggplot(tagdata, aes(fill=`Tag type`, y=number, x=rel.rep, label = number)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "GnBu")+
  theme_minimal() +
  labs(y = "Total tagged burbot", x = "") +
  theme(legend.position = "top")



repplt <- ggplot(repdata, aes(fill=`Reporting method`, y=number, x=rel.rep, label = number)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "GnBu", direction = -1)+
  theme_minimal() +
  labs(y = "", x = "") +
  theme(legend.position = "top")


grid.arrange(tagplt, repplt, nrow = 1)
