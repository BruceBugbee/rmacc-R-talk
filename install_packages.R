#Install all packages used in the presentation
demo_pkgs <- c("dplyr", "tidyr", "nycflights13", "ggplot2")

for (i in 1:length(demo_pkgs)) {
  install.packages(demo_pkgs[i])
}