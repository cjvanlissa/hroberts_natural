40.2	2.9	12	40.9	3.8	12	40.2	2.4	12	41.6	5.1	12

m_pret <- 40.2
sd_pret <- 2.9	
n_pret <- 12
m_prec <- 40.9
sd_prec <- 3.8
n_prec <- 12
m_postt <- 40.2
sd_post <- 2.4
n_post <- 12
m_postc <- 41.6
sd_posc <- 5.1
n_posc <- 12
assumed_pre_post_cor <- .5

datT <- escalc(
      measure = "SMCR",
      m1i = m_postt,
      m2i = m_pret,
      sd1i = sd_pret,
      ni = n_pret,
      ri = rep(assumed_pre_post_cor, length(n_pret))
    )

datC <-
    escalc(
      measure = "SMCR",
      m1i = m_postc,
      m2i = m_prec,
      sd1i = sd_prec,
      ni = n_prec,
      ri = rep(assumed_pre_post_cor, length(n_prec))
    )
yi = datT$yi - datC$yi
vi = datT$vi + datC$vi