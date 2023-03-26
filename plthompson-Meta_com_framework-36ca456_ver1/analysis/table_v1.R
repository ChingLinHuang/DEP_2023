library(gridExtra)
library(grid)
library(tidyverse)
library(gt)
find_cell <- function(table, row, col, name="core-bg"){
  l <- table$layout
  return(which((l$t==row) & (l$l==col) & (l$name==name)))
}

add_title <- function(g, title, padding = unit(2,"mm"), lpos=1, ...){
  tg <- textGrob(title, ...)
  g %>%
    gtable::gtable_add_rows(heights = grobHeight(tg) + padding, pos = 0L) %>% 
    gtable::gtable_add_grob(tg, t=1,b=1,l=lpos,r=ncol(g))
}

add_label <- function(g, xlab = "", ylab = "", padding = unit(2,"mm"), lpos = 1, tpos = 1, ...){
  tg_x <- textGrob(xlab, ...)
  tg_y <- textGrob(ylab, rot = 90, ...)
  g %>%
    gtable::gtable_add_rows(heights = grobHeight(tg_x) + padding, pos = 0) %>% 
    gtable::gtable_add_grob(tg_x, t=1,b=1,l=lpos,r=ncol(g)) %>%
    gtable::gtable_add_cols(widths = grobWidth(tg_y) + padding, pos = 0) %>% 
    gtable::gtable_add_grob(tg_y, t=tpos,b=nrow(g),l=1,r=1)
}


######
######  Summary of statistics
######
setwd("C:\\Users\\andy\\Downloads\\analysis\\res")
load(".\\VP_4\\res_VP.rdata")
load(".\\DNCI_4\\res_DNCI.rdata")
load(".\\Stegen_4\\res_Stegen.rdata")
load(".\\RF\\dat.rdata")

setwd("C:\\Users\\andy\\Downloads\\analysis\\tables")
dat_raw <- dat_VP %>% 
  full_join(dat_Stegen) %>% 
  full_join(dat_DNCI) 

summary_statistics <- dat %>% 
  left_join(dat_raw) %>% 
  select(Selection, DispLimit, HomoDisp, Drift, Env, `Env and Spatial`, Resid, Spatial, DNCI, CI.DNCI) %>% 
  mutate(DNCI = as.numeric(DNCI), CI.DNCI = as.numeric(CI.DNCI)) %>%
  apply(2, function(x) c(range(x), mean(x), sd(x))) %>%
  round(digit = 2)
row.names(summary_statistics) <- c("Min", "Max", "Mean", "Sd")

tg <- tableGrob(summary_statistics) %>% 
  add_title("Summary of statistics")

h <- convertHeight(sum(tg$heights), "in", TRUE)
w <- convertWidth(sum(tg$widths), "in", TRUE)
ggsave(file = "summary_statistics.pdf", tg, width = w, height = h)
ggsave(file = "summary_statistics.jpg", tg, width = w, height = h)


#######
####### results of RF
#######

Method <- c("Stegen", "VP", "DNCI", "all")
Time <- c(1, 4, 20)
for(m in Method){
  for(t in Time){
    setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")
    load(paste0("RF_t", t, "_", m, ".rdata"))
    dat <- get(paste0("RF_t", t, "_", m))
    
    setwd("C:\\Users\\andy\\Downloads\\analysis\\tables")
    
    ### importance
    #####
    tb <- t(round(dat$importance, digits = 3))
    tb_1 <- t(dat$importance)
    colnames(tb) <- c("Biotic", "Dispersal", "Abiotic")
    tg <- tableGrob(tb)
    
    for(ind_col in 1:ncol(tb)){
      ind_max <- which(tb_1[ ,ind_col] == max(tb_1[ ,ind_col]))
      ind <- find_cell(tg, ind_max + 1, ind_col + 1, "core-bg")
      tg$grobs[ind][[1]][["gp"]] <- gpar(fill="gray", col = "gray")
    }
    tg <- tg %>% add_title(paste0("importance_t", t, "_", m))
    
    h <- convertHeight(sum(tg$heights), "in", TRUE)
    w <- convertWidth(sum(tg$widths), "in", TRUE)
    ggsave(file = paste0("importance_t", t, "_", m, ".pdf"), tg, width = w, height = h)
    ggsave(file = paste0("importance_t", t, "_", m, ".jpg"), tg, width = w, height = h)
    #####
    
    ### Prediction
    #####
    ## i
    tb <- dat$prediction$i
    tg <- tableGrob(tb)
    for(ind_col in 1:ncol(tb)){
      ind_row <- ind_col
      ind <- find_cell(tg, ind_row + 1, ind_col + 1, "core-bg")
      tg$grobs[ind][[1]][["gp"]] <- gpar(fill="gray", col = "gray")
    }
    tg <- tg %>% 
      add_label(xlab = "Prediction", ylab = "True values") %>%
      add_title(paste0("Prediction_t", t, "_", m, "_Disp"))
     
    
    h <- convertHeight(sum(tg$heights), "in", TRUE)
    w <- convertWidth(sum(tg$widths), "in", TRUE)
    ggsave(file = paste0("Prediction_t", t, "_", m, "_Disp", ".pdf"), tg, width = w, height = h)
    ggsave(file = paste0("Prediction_t", t, "_", m, "_Disp", ".jpg"), tg, width = w, height = h)
    
    ## j
    tb <- dat$prediction$j
    tg <- tableGrob(tb)
    for(ind_col in 1:ncol(tb)){
      ind_row <- ind_col
      ind <- find_cell(tg, ind_row + 1, ind_col + 1, "core-bg")
      tg$grobs[ind][[1]][["gp"]] <- gpar(fill="gray", col = "gray")
    }
    tg <- tg %>% 
      add_label(xlab = "Prediction", ylab = "True values") %>%
      add_title(paste0("Prediction_t", t, "_", m, "_Abiotic"))
    
    
    h <- convertHeight(sum(tg$heights), "in", TRUE)
    w <- convertWidth(sum(tg$widths), "in", TRUE)
    ggsave(file = paste0("Prediction_t", t, "_", m, "_Abiotic", ".pdf"), tg, width = w, height = h)
    ggsave(file = paste0("Prediction_t", t, "_", m, "_Abiotic", ".jpg"), tg, width = w, height = h)
    
    ## k
    tb <- dat$prediction$k
    tg <- tableGrob(tb)
    for(ind_col in 1:ncol(tb)){
      ind_row <- ind_col
      ind <- find_cell(tg, ind_row + 1, ind_col + 1, "core-bg")
      tg$grobs[ind][[1]][["gp"]] <- gpar(fill="gray", col = "gray")
    }
    tg <- tg %>% 
      add_label(xlab = "Prediction", ylab = "True values") %>%
      add_title(paste0("Prediction_t", t, "_", m, "_Biotic"))
    
    
    h <- convertHeight(sum(tg$heights), "in", TRUE)
    w <- convertWidth(sum(tg$widths), "in", TRUE)
    ggsave(file = paste0("Prediction_t", t, "_", m, "_Biotic", ".pdf"), tg, width = w, height = h)
    ggsave(file = paste0("Prediction_t", t, "_", m, "_Biotic", ".jpg"), tg, width = w, height = h)
    
    #####
  }
}

### Performance
#####
ind <- 1
tb <- c()
for(m in Method){
  for(t in Time){
    setwd("C:\\Users\\andy\\Downloads\\analysis\\res\\RF")
    load(paste0("RF_t", t, "_", m, ".rdata"))
    dat <- get(paste0("RF_t", t, "_", m))
    
    tb <- rbind(tb, dat$performance)
    row.names(tb)[ind] <- paste0("t", t, "_", m)
    ind <- ind + 1
  }
}
colnames(tb) <- c("Dispersal", "Abiotic", "Biotic")
tb <- round(tb, digits = 4)

setwd("C:\\Users\\andy\\Downloads\\analysis\\tables")
tg <- tableGrob(tb)
for(ind_col in 1:ncol(tb)){
  ind_max <- which(tb[ ,ind_col] == max(tb[ ,ind_col]))
  ind <- find_cell(tg, ind_max + 1, ind_col + 1, "core-bg")
  tg$grobs[ind][[1]][["gp"]] <- gpar(fill="gray", col = "gray")
}
tg <- tg %>% 
  add_label(ylab = "Model") %>%
  add_title("Prediction performance")


h <- convertHeight(sum(tg$heights), "in", TRUE)
w <- convertWidth(sum(tg$widths), "in", TRUE)
ggsave(file = paste0("Performance.pdf"), tg, width = w, height = h)
ggsave(file = paste0("Performance.jpg"), tg, width = w, height = h)
#####
