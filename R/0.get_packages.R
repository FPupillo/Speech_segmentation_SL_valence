options(repos = c(CRAN = "https://cloud.r-project.org"))


required_packages <- c("ggplot2",
                       "dplyr", 
                       "lme4",
                       "lmerTest", 
                       "car", 
                       "Rmisc", 
                       "reshape2",
                       "knitr",
                       "gt",  
                       "sjPlot", # to use "tab_model"
                       "effectsize", # to  get the effect sizes
                       "emmeans" # estimated margina.l means

)

# Check and install missing packages
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg)
  } else {
    message(paste(pkg, "is already installed."))
  }
}

# Optionally load all packages
lapply(required_packages, library, character.only = TRUE)