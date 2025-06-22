################################################################################
############################ DESCRIPTIVE ANALYSIS ##############################
################################################################################

# ==============================================================================
# GDP COMMUNALITIES IN EA 
# ==============================================================================
GDP_communality <- function(country_codes) {
  
  library(readxl)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(zoo)
  
  # Create folder for saving graphs
  dir.create("Figures", showWarnings = FALSE)
  
  # Define recession periods
  recession_df <- data.frame(
    start = as.Date(c("2008-01-01", "2011-07-01", "2020-01-01")),
    end   = as.Date(c("2009-06-30", "2013-03-31", "2020-06-30"))
  )
  
  all_gdp <- list()
  
  for (cc in country_codes) {
    data_path <- paste0("Data/", cc, "/Original/", cc, "DataQ_LT.xlsx")
    gdp_label <- paste0("GDP_", cc)
    
    df <- read_excel(data_path)
    
    if (cc == "EA") {
      EA_corr_data <- dplyr::select(df, -Time)
      correlation_matrix <- cor(EA_corr_data, use = "pairwise.complete.obs")
      melted_corr <- melt(correlation_matrix)
      
      # Crop long labels for better readability
      label_crop <- function(x) {
        sapply(x, function(v) substr(v, 1, 15))
      }
      
      p_heat <- ggplot(melted_corr, aes(x = label_crop(Var1), y = label_crop(Var2), fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
        theme_bw(base_size = 12) +
        labs(title = "Correlation Heatmap of EA Variables", x = NULL, y = NULL) +
        theme(
          axis.text.x = element_text(angle = 90, size = 6, hjust = 1),
          axis.text.y = element_text(size = 6),
          plot.title = element_text(hjust = 0.5)
        )
      
      ggsave("Figures/EA_Correlation_Heatmap.png", p_heat, width = 10, height = 8, bg = "white")
    }
    
    gdp_idx <- which(tolower(colnames(df)) == tolower(gdp_label))
    gdp_series <- df[[gdp_idx]] * 100
    date_series <- df[[1]]
    
    gdp_df <- data.frame(
      date = as.Date(date_series),
      gdp = gdp_series,
      country = cc
    )
    
    all_gdp[[cc]] <- gdp_df
  }
  
  full_gdp <- bind_rows(all_gdp)
  
  # Filter out EA for plots and set country order
  gdp_filtered <- full_gdp %>% filter(country != "EA")
  gdp_filtered$country <- factor(gdp_filtered$country, levels = c("DE", "FR", "IT", "ES"))
  
  # Line plot with recession shading
  p1 <- ggplot(gdp_filtered, aes(x = date, y = gdp, color = country)) +
    geom_rect(data = recession_df, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    geom_line(linewidth = 1) +
    labs(title = "GDP Time Series By Country", x = "Date", y = "GDP") +
    theme_bw(base_size = 14) +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  
  ggsave("Figures/GDP_TimeSeries_SelectedCountries.png", p1, width = 10, height = 6, bg = "white")
  
  # Faceted plot for DE, FR, IT, ES with fixed layout
  target_countries <- c("DE", "FR", "IT", "ES")
  gdp_faceted <- gdp_filtered %>% filter(country %in% target_countries)
  
  p2 <- ggplot(gdp_faceted, aes(x = date, y = gdp)) +
    geom_rect(data = recession_df, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey", alpha = 0.2) +
    geom_line(color = "steelblue", linewidth = 1) +
    facet_wrap(~ country, ncol = 2, scales = "free_y") +
    labs(title = "GDP Time Series By Country", x = "Date", y = "GDP (Index)") +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave("Figures/GDP_TimeSeries_Facet_Selected.png", p2, width = 10, height = 7, bg = "white")
  
  # Comovement heatmap
  wide_gdp <- gdp_filtered %>%
    filter(!is.na(gdp)) %>%
    pivot_wider(names_from = country, values_from = gdp)
  
  gdp_matrix <- as.matrix(wide_gdp[,-1])
  rownames(gdp_matrix) <- wide_gdp$date
  
  comov_matrix <- cor(gdp_matrix, use = "pairwise.complete.obs")
  comov_melt <- melt(comov_matrix)
  colnames(comov_melt) <- c("Country1", "Country2", "Correlation")
  
  # Set factor levels for ordering in heatmap
  comov_melt$Country1 <- factor(comov_melt$Country1, levels = c("DE", "FR", "IT", "ES"))
  comov_melt$Country2 <- factor(comov_melt$Country2, levels = c("DE", "FR", "IT", "ES"))
  
  p3 <- ggplot(comov_melt, aes(x = Country1, y = Country2, fill = Correlation)) +
    geom_tile(color = "black", linewidth = 0.3) +
    geom_text(aes(label = round(Correlation, 2)), size = 3, color = "black") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                         limits = c(-1, 1), name = "Correlation") +
    labs(title = "GDP Correlation Across Countries", x = NULL, y = NULL) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(hjust = 0.5))
  
  ggsave("Figures/GDP_Correlation_Heatmap.png", p3, width = 8, height = 6, bg = "white")
  
  # Export for inspection
  assign("gdp_results", list(
    data = all_gdp,
    combined = full_gdp,
    comovement = comov_matrix
  ), envir = .GlobalEnv)
}

# ==============================================================================
# GDP BY COUNTRY CONTRIBUTION
# ==============================================================================
plot_euro_area_gdp <- function(file_path = "Data/GDP_millionsEA.xlsx",
                               sheet_name = "GDP",
                               quarter = "2024-Q4",
                               special_countries = c("Italy", "Germany", "France", "Spain", "Ireland",
                                                     "Greece", "Netherlands", "Belgium", "Austria"),
                               show_table = TRUE) {
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(knitr)
  library(kableExtra)
  
  # 1. Importa e pulisci dati
  gdp_data_raw <- read_excel(file_path, sheet = sheet_name)
  
  gdp_data <- gdp_data_raw %>%
    dplyr::select(Country = 1, value = !!sym(quarter)) %>%
    mutate(
      Country = case_when(
        Country == "Germany (until 1990 former territory of the FRG)" ~ "Germany",
        Country == "Euro area" ~ NA_character_,
        TRUE ~ Country
      ),
      value = suppressWarnings(as.numeric(value))
    ) %>%
    filter(!is.na(Country), !Country %in% c(
      "European Union - 27 countries (from 2020)",
      "Euro area – 20 countries (from 2023)")
    )
  
  # 2. Mappa base
  world <- ne_countries(scale = "large", returnclass = "sf")
  europe <- world %>% filter(region_un == "Europe")
  
  euro_area_countries <- c(
    "Austria", "Belgium", "Croatia", "Cyprus", "Estonia", "Finland",
    "France", "Germany", "Greece", "Ireland", "Italy", "Latvia",
    "Lithuania", "Luxembourg", "Malta", "Netherlands", "Portugal",
    "Slovakia", "Slovenia", "Spain"
  )
  
  # 3. Calcolo share
  gdp_total_ea <- gdp_data %>%
    filter(Country %in% euro_area_countries) %>%
    summarise(total = sum(value, na.rm = TRUE)) %>%
    pull(total)
  
  gdp_data <- gdp_data %>%
    mutate(GDP_Share_EA = (value / gdp_total_ea) * 100)
  
  # 4. Join mappa + dati
  europe_gdp <- europe %>%
    left_join(gdp_data, by = c("name" = "Country")) %>%
    mutate(
      EA_Member = name %in% euro_area_countries,
      SpecialCountry = name %in% special_countries,
      FillColor = case_when(
        EA_Member ~ GDP_Share_EA,
        TRUE ~ NA_real_
      )
    )
  
  # 5. Plot
  gdp_plot <- ggplot() +
    # Sfondo acqua
    theme_void() +
    theme(
      panel.background = element_rect(fill = "aliceblue", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),
      legend.position = "right",
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "serif"),
      plot.caption = element_text(size = 9, hjust = 1, family = "serif")
    ) +
    
    # Tutti i paesi in grigio chiaro
    geom_sf(data = europe, fill = "grey95", color = "black", size = 0.1) +
    
    # Paesi non EA grigio medio
    geom_sf(data = europe_gdp %>% filter(!EA_Member), fill = "grey85", color = "black", size = 0.1) +
    
    # Paesi EA con sfumatura blu
    geom_sf(data = europe_gdp %>% filter(EA_Member),
            aes(fill = FillColor), color = "black", size = 0.2) +
    
    # Evidenziazione rossa dei paesi speciali
    geom_sf(data = europe_gdp %>% filter(SpecialCountry),
            fill = NA, color = "red", size = 0.9) +
    
    # Etichette
    geom_sf_text(data = europe_gdp %>% filter(SpecialCountry & EA_Member),
                 aes(label = paste0(round(GDP_Share_EA, 1), "%")),
                 size = 3.5, color = "black", fontface = "bold", family = "serif") +
    
    # Colori scala blu
    scale_fill_gradientn(
      colours = c("white", "#cce5f6", "#99ccee", "#66aadf", "#3388cc", "#005f99"),
      name = paste0("Share of EA GDP (%) – ", quarter),
      limits = c(0, max(europe_gdp$GDP_Share_EA, na.rm = TRUE)),
      guide = guide_colorbar(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        barwidth = unit(6, "cm"),
        barheight = unit(0.4, "cm")
      )
    ) +
    theme(
      panel.background = element_rect(fill = "aliceblue", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold", family = "serif"),
      legend.text = element_text(size = 9, family = "serif"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "serif"),
      plot.caption = element_text(size = 9, hjust = 1, family = "serif")
    ) + 
    
    coord_sf(xlim = c(-25, 45), ylim = c(33, 72), expand = FALSE) +
    labs(
      title = "Euro Area GDP Composition"
    )
  
  print(gdp_plot)
  
  ggsave("Figures/euro_area_gdp_map.pdf", plot = last_plot(), width = 9, height = 7)
  
  # 6. Tabella
  gdp_table <- gdp_data %>%
    filter(Country %in% euro_area_countries) %>%
    arrange(desc(GDP_Share_EA)) %>%
    dplyr::select(Country, `GDP (Million €)` = value, `Contribution (%)` = GDP_Share_EA)
  
  if (show_table) {
    return(kable(gdp_table, format = "latex", booktabs = TRUE, digits = 2,
                 caption = paste("GDP Contribution by Country in the Euro Area –", quarter)) %>%
             kable_styling(latex_options = c("hold_position", "striped", "scale_down")))
  } else {
    return(list(map = gdp_plot, table = gdp_table))
  }
}


################################################################################
############################## DATSET PREPARATION ##############################
################################################################################

# ==============================================================================
# STANDARDIZATION AND DESTANDARDIZATION
# ==============================================================================

standardize_Y <- function(Y) {
  T <- dim(Y)[1]
  p1 <- dim(Y)[2]
  p2 <- dim(Y)[3]
  
  Y_std <- array(NA, dim = dim(Y), dimnames = dimnames(Y))
  mean_Y <- matrix(0, p1, p2)
  sd_Y <- matrix(1, p1, p2)
  
  for (i in 1:p1) {
    for (j in 1:p2) {
      y_ij <- Y[, i, j]
      mu <- mean(y_ij, na.rm = TRUE)
      sigma <- sd(y_ij, na.rm = TRUE)
      if (is.na(sigma) || sigma == 0) sigma <- 1
      Y_std[, i, j] <- (y_ij - mu) / sigma
      mean_Y[i, j] <- mu
      sd_Y[i, j] <- sigma
    }
  }
  
  return(list(Y_scaled = Y_std, mean = mean_Y, sd = sd_Y))
}

inverse_standardize_Y <- function(Y_scaled, mean_Y, sd_Y) {
  T <- dim(Y_scaled)[1]
  p1 <- dim(Y_scaled)[2]
  p2 <- dim(Y_scaled)[3]
  
  Y_original <- array(NA, dim = dim(Y_scaled), dimnames = dimnames(Y_scaled))
  
  for (t in 1:T) {
    Y_original[t,,] <- Y_scaled[t,,] * sd_Y + mean_Y
  }
  
  return(Y_original)
}

center_Y <- function(Y) {
  T <- dim(Y)[1]
  p1 <- dim(Y)[2]
  p2 <- dim(Y)[3]
  
  Y_centered <- array(NA, dim = dim(Y))
  dimnames(Y_centered) <- dimnames(Y) 
  mean_Y <- matrix(0, p1, p2)
  
  for (i in 1:p1) {
    for (j in 1:p2) {
      y_ij <- Y[, i, j]
      mu <- mean(y_ij, na.rm = TRUE)
      Y_centered[, i, j] <- y_ij - mu
      mean_Y[i, j] <- mu
    }
  }
  
  return(list(Y_centered = Y_centered, mean = mean_Y))
}

decenter_Y <- function(Y_centered, mean_Y) {
  T <- dim(Y_centered)[1]
  p1 <- dim(Y_centered)[2]
  p2 <- dim(Y_centered)[3]
  
  Y_original <- array(NA, dim = dim(Y_centered))
  dimnames(Y_original) <- dimnames(Y_centered)
  
  for (t in 1:T) {
    Y_original[t,,] <- Y_centered[t,,] + mean_Y
  }
  
  return(Y_original)
}

inverse_standardize_nowcasts <- function(nowcast_list, mean_Y, sd_Y) {
  lapply(nowcast_list, function(mat) {
    mat * sd_Y + mean_Y
  })
}


# ==============================================================================
# NOT AVAILABLE PERCENTAGE
# ==============================================================================

nan_percent_Y <- function(Y) {
  total_values <- length(Y)
  total_NAs <- sum(is.na(Y))
  percent <- total_NAs / total_values * 100
  return(percent)
}


# ==============================================================================
# GENERAL PREPARATION OF DATA 
# ==============================================================================

select_base_variable_names <- function(countries, P) {
  modelM <- P$modelM
  modelQ <- P$modelQ
  
  model_sizes <- list(
    small = list(Q = 1, M = 5),  # small: solo GDP tra le trimestrali
    medium = list(Q = 5, M = 10),
    large = list(Q = 10, M = 50)
  )
  
  n_m <- model_sizes[[tolower(modelM)]]$M
  n_q <- model_sizes[[tolower(modelQ)]]$Q
  
  selected_m_names <- c()
  selected_q_names <- c()
  
  for (cc in countries) {
    DataFile <- paste0("Data/", cc, "/Processed/Data_", cc, ".xlsx")
    monthly_data <- readxl::read_excel(DataFile, sheet = "MonthlyLong")
    quarterly_data <- readxl::read_excel(DataFile, sheet = "QuarterlyLong")
    
    DataM <- monthly_data[, -1]
    DataQ <- quarterly_data[, -1]
    
    gdp_q_idx <- which(grepl("^GDP_", colnames(DataQ), ignore.case = TRUE))[1]
    gdp_q <- DataQ[[gdp_q_idx]]
    gdp_rep <- rep(gdp_q, each = 3)[1:nrow(DataM)]
    
    # Correlazioni mensili
    cors_m <- apply(DataM, 2, function(x) suppressWarnings(cor(x, gdp_rep, use = "pairwise.complete.obs")))
    top_m <- names(cors_m)[order(abs(cors_m), decreasing = TRUE)[1:min(n_m, length(cors_m))]]
    
    # Correlazioni trimestrali (solo se medium/large)
    top_q <- NULL
    if (n_q > 1) {
      cors_q <- apply(DataQ[, -gdp_q_idx], 2, function(x) suppressWarnings(cor(x, gdp_q, use = "pairwise.complete.obs")))
      top_q <- names(DataQ)[-gdp_q_idx][order(abs(cors_q), decreasing = TRUE)[1:min(n_q, length(cors_q))]]
    }
    
    # Rimozione suffisso paese
    base_m <- gsub(paste0("_", cc, "$"), "", top_m)
    base_q <- if (!is.null(top_q)) gsub(paste0("_", cc, "$"), "", top_q) else character()
    
    selected_m_names <- union(selected_m_names, base_m)
    selected_q_names <- union(selected_q_names, base_q)
  }
  
  # Per modello small: trimestrali = solo GDP
  if (n_q == 1) {
    selected_q_names <- "GDP"
  }
  
  return(list(monthly = selected_m_names, quarterly = selected_q_names))
}

filter_common_variables_across_countries <- function(countries, selected_base_m, selected_base_q) {
  valid_m_vars <- list()
  valid_q_vars <- list()
  
  for (cc in countries) {
    DataFile <- paste0("Data/", cc, "/Processed/Data_", cc, ".xlsx")
    monthly_data <- readxl::read_excel(DataFile, sheet = "MonthlyLong")
    quarterly_data <- readxl::read_excel(DataFile, sheet = "QuarterlyLong")
    
    DataM <- monthly_data[, -1]
    DataQ <- quarterly_data[, -1]
    
    col_m <- colnames(DataM)
    col_q <- colnames(DataQ)
    
    # Rimuove suffisso country e costruisce mappa
    base_m <- gsub(paste0("_", cc, "$"), "", col_m)
    base_q <- gsub(paste0("_", cc, "$"), "", col_q)
    
    matched_m <- selected_base_m[selected_base_m %in% base_m]
    matched_q <- selected_base_q[selected_base_q %in% base_q]
    
    # Mapping: base → full
    matched_full_m <- setNames(paste0(matched_m, "_", cc), matched_m)
    matched_full_q <- setNames(paste0(matched_q, "_", cc), matched_q)
    
    valid_m_vars[[cc]] <- matched_full_m
    valid_q_vars[[cc]] <- matched_full_q
  }
  
  # Trova solo i base_name presenti in tutti i paesi
  common_base_m <- Reduce(intersect, lapply(valid_m_vars, names))
  common_base_q <- Reduce(intersect, lapply(valid_q_vars, names))
  
  # Per ogni paese, ritorna solo le variabili comuni
  filtered_vars_m <- lapply(valid_m_vars, function(x) x[common_base_m])
  filtered_vars_q <- lapply(valid_q_vars, function(x) x[common_base_q])
  
  return(list(monthly = filtered_vars_m, quarterly = filtered_vars_q))
}

prepare_country_data <- function(country_code, P, selected_vars_m, selected_vars_q) {
  # ---- PATHS ----
  DataFile <- paste0("Data/", country_code, "/Processed/Data_", country_code, ".xlsx")
  LegendFile <- paste0("Data/", country_code, "/Original/Legend_", country_code, ".xlsx")
  
  # ---- MONTHLY DATA ----
  monthly_data <- readxl::read_excel(DataFile, sheet = "MonthlyLong", col_names = TRUE)
  DatesM <- as.Date(monthly_data[[1]])
  DataM <- monthly_data[, -1]
  vars_m_country <- selected_vars_m[[country_code]]
  DataM <- DataM[, vars_m_country]
  
  # Monthly Legend: recupera la classe delle variabili selezionate
  legend_m <- readxl::read_excel(LegendFile, sheet = "MDescriptionFull")
  idx_m <- match(vars_m_country, legend_m[[1]])  # prima colonna = nomi variabili
  ClassM <- legend_m$Class[idx_m]
  TransfM <- floor(legend_m[idx_m, 8])
  GroupM <- legend_m$M1[idx_m]
  
  # Annualization for monthly
  DataMTrf <- DataM
  annualize_idx <- which(TransfM[[1]] %in% c(2, 4))
  DataMTrf[, annualize_idx] <- DataMTrf[, annualize_idx] * 100

  # ---- COVID MASK SOLO SU VARIABILI REALI ----
  CovidNaN <- function(data, dates, class, start_covid, end_covid) {
    real_idx <- which(grepl("r", tolower(class)))
    mask <- dates >= as.Date(as.yearmon(start_covid)) & dates <= as.Date(as.yearmon(end_covid))
    covid_matrix <- matrix(FALSE, nrow = nrow(data), ncol = ncol(data))
    covid_matrix[mask, real_idx] <- TRUE
    data[covid_matrix] <- NA
    list(data, covid_matrix)
  }
  resM <- CovidNaN(DataMTrf, DatesM, ClassM, P$covid_start, P$covid_end)
  DataMTrf <- resM[[1]]
  Covid_obsM <- resM[[2]]
  T_M <- nrow(DataMTrf)
  
  # ---- QUARTERLY DATA ----
  quarterly_data <- readxl::read_excel(DataFile, sheet = "QuarterlyLong", col_names = TRUE)
  DatesQ <- as.Date(quarterly_data[[1]])
  DataQ <- quarterly_data[, -1]
  vars_q_country <- selected_vars_q[[country_code]]
  DataQ <- DataQ[, vars_q_country]
  
  # Quarterly Legend
  legend_q <- readxl::read_excel(LegendFile, sheet = "QDescriptionFull")
  idx_q <- match(vars_q_country, legend_q[[1]])  # prima colonna = nomi variabili
  ClassQ <- legend_q$Class[idx_q]
  TransfQ <- floor(legend_q[idx_q, 8])
  GroupQ <- legend_q$M1[idx_q]
  
  
  DataQTrf <- DataQ
  annualize_idx <- which(TransfQ[[1]] %in% c(2, 4))
  DataQTrf[, annualize_idx] <- DataQTrf[, annualize_idx] * 100
  
  resQ <- CovidNaN(DataQTrf, DatesQ, ClassQ, P$covid_start, P$covid_end)
  DataQTrf <- resQ[[1]]
  Covid_obsQ <- resQ[[2]]
  
  # ---- ESPANSIONE TRIMESTRALE A MENSILE ----
  DataQMTrf <- do.call(rbind, lapply(1:nrow(DataQTrf), function(i) {
    rbind(rep(NA, ncol(DataQTrf)),
          rep(NA, ncol(DataQTrf)),
          DataQTrf[i, ])
  }))
  T_Q <- nrow(DataQMTrf)
  
  # ---- UNIFICA LUNGHEZZA ----
  T_final <- max(T_M, T_Q)
  if (T_M < T_final) {
    paddingM <- data.frame(matrix(NA, T_final - T_M, ncol(DataMTrf)))
    colnames(paddingM) <- colnames(DataMTrf)
    DataMTrf <- rbind(DataMTrf, paddingM)
  }
  if (T_Q < T_final) {
    paddingQ <- data.frame(matrix(NA, T_final - T_Q, ncol(DataQMTrf)))
    colnames(paddingQ) <- colnames(DataQMTrf)
    DataQMTrf <- rbind(DataQMTrf, paddingQ)
  }
  
  # ---- OUTPUT ----
  Data <- cbind(DataMTrf, DataQMTrf)
  Series <- c(vars_m_country, vars_q_country)
  Group <- c(GroupM, GroupQ)
  
  result <- list(
    Data = Data,
    DatesM = DatesM,
    DatesQ = DatesQ,
    Series = Series,
    UnbalancedPattern = NULL,
    Covid_obsM = Covid_obsM,
    Covid_obsQ = Covid_obsQ,
    Group = Group,
    Name = paste0("Data_", country_code),
    quarterly_start_idx = ncol(DataMTrf) + 1
  )
  
  assign(result$Name, result$Data, envir = .GlobalEnv)
  return(result)
}

# ==============================================================================
# REMOVE COUNTRY CODES FROM VARIABLES NAME
# ==============================================================================

# Funzione per rimuovere suffissi paese
remove_country_code <- function(names, ccodes) {
  pattern <- paste0("_(", paste(ccodes, collapse = "|"), ")$")
  gsub(pattern, "", names)
}

# ==============================================================================
# BUILD METRIX-VARIATE DATA
# ==============================================================================

tensor_data <- function(countries, P) {
  T_max <- P$Tmax
  
  # Carica i dataset per ciascun paese
  data_list <- lapply(countries, function(cc) get(paste0("Data_", cc)))
  names(data_list) <- countries
  
  # Rimuove i codici paese dalle colonne
  cleaned_data_list <- list()
  for (i in seq_along(countries)) {
    cc <- countries[i]
    data <- data_list[[cc]]
    
    # Tronca a Tmax osservazioni se serve
    if (nrow(data) > T_max) {
      data <- data[1:T_max, ]
    }
    
    colnames(data) <- remove_country_code(colnames(data), countries)
    cleaned_data_list[[cc]] <- data
  }
  
  # Trova tutte le variabili comuni
  all_vars <- unique(unlist(lapply(cleaned_data_list, colnames)))
  
  # Crea tensor Y
  Y <- array(NA, dim = c(T_max, length(countries), length(all_vars)),
             dimnames = list(
               format(res$DatesM[1:T_max], "%Y-%m"),
               countries,
               all_vars
             ))
  
  # Allinea le variabili per ciascun paese
  for (i in seq_along(countries)) {
    cc <- countries[i]
    data <- cleaned_data_list[[cc]]
    
    # Nuova matrice con tutte le variabili, NA dove mancano
    aligned_data <- matrix(NA, nrow = T_max, ncol = length(all_vars))
    colnames(aligned_data) <- all_vars
    matched_vars <- intersect(colnames(data), all_vars)
    aligned_data[1:nrow(data), matched_vars] <- as.matrix(data[, matched_vars])
    
    Y[, i, ] <- aligned_data
  }
  
  # Crea matrice W (1 se osservato, 0 se NA)
  W <- ifelse(is.na(Y), 0, 1)
  
  return(list(Y = Y, W = W))
}
