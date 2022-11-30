#### load libraries and prep ####
library(gridExtra)
library(RColorBrewer)

# uncomment to view available brewer schemes
#RColorBrewer::display.brewer.all()

# define custom theme 
theme_np <- function() {
  theme(
      line = element_line(color = "black"),
      rect = element_rect(fill = "#FFFFFF",
                          linetype = 0, colour = NA),
      text = element_text(color = "#333333"),
      axis.title = element_blank(),
      axis.text = element_text(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      axis.text.x=element_text(size=11),
      axis.text.y=element_text(size=10),
      legend.background = element_rect(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      panel.background = element_blank(),
      panel.grid = element_line(colour = NULL),
      panel.grid.major = element_line(colour = "#BFBFBF"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0, size = rel(1.4), face = "bold"),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      strip.background = element_rect())
}

# hist of MR severity
MRbar <- DF %>% 
  ggplot(mapping = aes(Severity)) + 
  geom_bar(aes(y = ..prop.., group = 1), 
           fill =  c("#f4ba55", "#d26257", "#bc2354", "#77025e"), 
           show.legend = F) +
  theme_np() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribution of Mitral Regurgitation", y = "", x = "")
MRbar

####--- density plot _variable_ by MR severity ---####
# create function to quickly create density plots
create_density <- function(x, var1, var2, title) {
  var1 <- enquo(var1);
  var2 <- enquo(var2);
  x %>%
    pivot_longer(cols = !!var1) %>%
    ggplot() +
    geom_density(mapping = aes(x = !!var2, fill = value, color = value), alpha = 0.4) +
    scale_color_manual(values = c("#f4ba55", "#d26257", "#bc2354")) +
    scale_fill_manual(values = c("#f4ba55", "#d26257", "#bc2354")) +
    theme_np() +
    theme(panel.grid.major = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank()) +
    labs(x = "", y = "", 
         title = paste0(title))
    
}

# density plots
TimePCIdens <- create_density(DF, Severity2, TimeToPCI, "Time to PCI by MR Severity")
TROPdens <- create_density(DF, Severity2, PeakTroponin, "Peak Troponin by MR Severity")
AGEdens <- create_density(DF, Severity2, AGE, "Age by MR Severity")
LVEFdens <- create_density(DF, Severity2, LVEF..., "LVEF by MR Severity")
grid.arrange(TimePCIdens, TROPdens, AGEdens, ncol = 1)

TimePCIdens
AGEdens
LVEFdens

####--- tile plots to show counts of blockage location by MR severity ---####
# create function
create_tiles2 <- function(x, var1, title) {
  var1 <- enquo(var1);
  x %>%
    filter(!is.na(!!var1)) %>%
    group_by(Severity2, !!var1,) %>%
    count() %>%
    ggplot(aes(y = !!var1, x = Severity2, fill = n)) + 
    geom_tile(show.legend = F) +
    geom_text(aes(label = paste0(round((n/sum(n))*100, 1), '%'))) +
    scale_fill_gradient(low = "#f4ba55", high = "#bc2354") +
    theme_np() + 
    theme(panel.grid.major = element_blank()) +
    labs(title = paste0(title))
}

# tile plots
LMStile2 <- create_tiles2(DF, LMS, "LMS")
LADproxtile2 <- create_tiles2(DF, LADProx, "Proximal LAD")
LADothertile2 <- create_tiles2(DF, LADOther, "Other LAD")
RCAtile2 <- create_tiles2(DF, RCA, "RCA")
LCxtile2 <- create_tiles2(DF, LCx, "LCx")
Multiple <- create_tiles2(DF, MultiBlock, "Multiple")
# arrange tile plots in one image
grid.arrange(LMStile2, LADproxtile2, LADothertile2, RCAtile2, LCxtile2, ncol = 5)

####---- factor plots ----####
# create factor count table
fct_table <- DF %>%
  select(where(is.factor))
fct_list <- lapply(fct_table, fct_count)

# set more readable names for plots
fct_list_names <- c("Gender", "Smoker", "Family History of CAD", "Hypercholesterolemia", "Hypertension", 
                    "CVA", "Renal Function", "Previous MI", "Previous CABG", "Previous PTCA", "Diabetes",
                    "ECG", "MI Type", "OOHCA", "LMS", "LAD Proximal","LAD Other","RCA", "LCx", "Graft",
                    "LMS Intervention", "LAD Proximal Intervention", "LAD Other Intervention", "LCx Intervention",
                    "RCA Intervention", "Clopidogrel", "Ticagrelor", "Prasugrel", "Eptifibatide", "Clinical HF", 
                    "History Of HF", "Known LV Impairment", "MR Severity", "Pre-existing MR", "30 Day Death", 
                    "12 Month Death","MR Severity")
names(fct_list) <- fct_list_names

# for loop to generate factor plots
fct_plot_list <- list()
for (i in 1:length(fct_list)) {
  title <- fct_list_names[i]
  p <- ggplot() +
    geom_col(data = fct_list[[i]], aes(x = f, y = n, fill = f), show.legend = F)+ 
    theme_np() +
    scale_fill_grey(start = 0.6, end = 0.3, na.value = "#f2353b") +
    theme(panel.grid.major = element_blank(),
          legend.title = element_blank(),
          axis.text.x=element_text(size=10),
          plot.title = element_text(hjust = 0, size = rel(1.2), face = "bold"),) +
    labs(title = paste0(title))
  fct_plot_list[[i]] <- p
}
names(fct_plot_list) <- fct_list_names

# select best plots
fct_plot_list_best <- fct_plot_list[c(1,2,8,9,13,14,21:27,31,35)]

# create grouped factor plot with grid.arrange
fct_plots_grid1 <- do.call("grid.arrange", c(fct_plot_list_best, ncol = 3))
fct_plots_grid1

#### ---- Continuous Variables ----####
library(GGally)
DF %>% 
  select(AGE, CreatinineClearance, IndexTroponin, PeakTroponin, TimeToPCI, DTBforPPCI.mins., LVEF..., Severity3) %>%
  ggpairs(columns = c(1,2,4,5), aes(color = Severity3, alpha = 1)) +
  scale_color_manual(values = c("#f4ba55", "#bc2354")) +
  scale_fill_manual(values = c("#f4ba55", "#bc2354")) +
  theme_np() +
  theme(panel.grid.major = element_blank())
