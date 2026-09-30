# =============================================================================
# SCM Course — Reusable R Functions
# Prof. Dr. Thomas Kirschstein | HS RheinMain
# Bachelor Supply Chain Management
# =============================================================================
# Functions are grouped into four domains:
#   1. Location Planning (CoG, Weiszfeld, Haversine, AHP)
#   2. Transportation Problem (Vogel's Approximation)
#   3. Inventory Models (Newsvendor, EOQ, Pooling)
#   4. Warehouse Location Problem (Add / Drop Heuristics)
# =============================================================================

# Farbdefinitionen

cols <- list(
  top    = "#0B6E4F",
  strat  = "#2D6A4F",
  mid    = "#52B788",
  low    = "#FFFFFF",
  atp    = "#D97B29",
  border = "#2F3E46",
  text   = "#1F2933",
  muted  = "#6B7280",
  dash   = "#7C8792",
  bg     = "#FFFFFF"
)

# Zeichenfunktionen Blockdiagramme

mid <- function(a, b) (a + b) / 2

chevron_path <- function(x0, x1, y0, y1, head = 0.28, notch = 0.24) {
  xm <- x1 - head
  paste0(
    "M ", x0, ",", y0, " ",
    "L ", xm, ",", y0, " ",
    "L ", x1, ",", mid(y0, y1), " ",
    "L ", xm, ",", y1, " ",
    "L ", x0, ",", y1, " ",
    "L ", x0 + notch, ",", mid(y0, y1), " Z"
  )
}

shape_rect <- function(x0, x1, y0, y1, fill, line = cols$border, width = 1.2) {
  list(
    type = "rect", x0 = x0, x1 = x1, y0 = y0, y1 = y1,
    xref = "x", yref = "y",
    line = list(color = line, width = width),
    fillcolor = fill,
    layer = "below"
  )
}

shape_path <- function(path, fill, line = cols$border, width = 1.2) {
  list(
    type = "path", path = path, xref = "x", yref = "y",
    line = list(color = line, width = width),
    fillcolor = fill,
    layer = "below"
  )
}

shape_line <- function(x0, x1, y0, y1, color, width = 2) {
  list(
    type = "line", x0 = x0, x1 = x1, y0 = y0, y1 = y1,
    xref = "x", yref = "y",
    line = list(color = color, width = width)
  )
}

text_trace <- function(data, x, y, text, color, size = 16) {
  add_text(
    fig, data = data,
    x = x, y = y, text = text,
    textfont = list(size = size, color = color, family = "Arial"),
    hoverinfo = "skip", showlegend = FALSE
  )
}

arrow_ann <- function(x, y, ax, ay, dash = FALSE,
                      color = cols$border, width = 1.5) {
  list(
    x = x, y = y, ax = ax, ay = ay,
    xref = "x", yref = "y", axref = "x", ayref = "y",
    text = "", showarrow = TRUE,
    arrowhead = 2, arrowsize = 1,
    arrowwidth = width, arrowcolor = color,
    arrowdash = if (dash) "dash" else "solid"
  )
}
kable_zeilenweise <- function(data,
                              fragment_typ = "fade-up",
                              start_index = 1,
                              ...) {
  
  tab_html <- knitr::kable(
    data,
    format = "html",
    escape = FALSE,
    ...
  )
  
  doc <- xml2::read_html(as.character(tab_html))
  
  zeilen <- xml2::xml_find_all(
    doc,
    ".//tbody/tr"
  )
  
  for (i in seq_along(zeilen)) {
    
    xml2::xml_attr(
      zeilen[i],
      "class"
    ) <- paste(
      "fragment",
      fragment_typ
    )
    
    xml2::xml_attr(
      zeilen[i],
      "data-fragment-index"
    ) <- start_index + i - 1
  }
  
  knitr::asis_output(
    as.character(
      xml2::xml_find_first(
        doc,
        ".//table"
      )
    )
  )
}

# 0. Inventory management ─────────────────────────────────────────────────────

#' Simulation of a periodic-review (t, S) inventory policy
#'
#' Sequence of events within a period k:
#'   1. Receipt of the order placed in period k - wbz
#'   2. Realisation of demand
#'   3. Update of net inventory and physical inventory
#'   4. Determination of outstanding orders
#'   5. Order decision in every t-th period
#'
#' @param bedarf Numeric vector of period demands d[k] (German: Bedarf = demand).
#' @param t Positive integer order (review) interval.
#' @param S Order-up-to level (target inventory).
#' @param anfangsbestand Initial physical inventory before period 1.
#' @param wbz Fixed replenishment lead time in whole periods.
#' @param backorders Logical. If TRUE, shortages are carried forward as
#'   backorders. If FALSE, unmet demand is lost (lost sales).
#'
#' @return A tibble with inventory, order and delivery information
#'   (column names are in German, e.g. Periode = period, Bedarf = demand).
#'
#' @examples
#' simuliere_tS(
#'   bedarf = c(8, 9, 13, 16, 12, 8, 9, 11),
#'   t = 2,
#'   S = 30,
#'   anfangsbestand = 20,
#'   wbz = 2
#' )
simuliere_tS <- function(bedarf,
                         t,
                         S,
                         anfangsbestand,
                         wbz,
                         backorders = TRUE) {
  
  if (!is.numeric(bedarf) || length(bedarf) == 0L) {
    stop("'bedarf' must be a non-empty numeric vector.")
  }
  
  if (any(is.na(bedarf)) || any(bedarf < 0)) {
    stop("'bedarf' must not contain missing or negative values.")
  }
  
  if (length(t) != 1L || is.na(t) || t < 1 || t != as.integer(t)) {
    stop("'t' must be a positive integer.")
  }
  
  if (length(S) != 1L || is.na(S) || S < 0) {
    stop("'S' must be a non-negative number.")
  }
  
  if (length(anfangsbestand) != 1L ||
      is.na(anfangsbestand) ||
      anfangsbestand < 0) {
    stop("'anfangsbestand' must be a non-negative number.")
  }
  
  if (length(wbz) != 1L ||
      is.na(wbz) ||
      wbz < 0 ||
      wbz != as.integer(wbz)) {
    stop("'wbz' must be a non-negative integer.")
  }
  
  n_perioden <- length(bedarf)
  
  periode <- seq_len(n_perioden)
  
  liefermenge <- numeric(n_perioden)
  bestellmenge <- numeric(n_perioden)
  offene_bestellungen <- numeric(n_perioden)
  
  physischer_bestand_vor_bedarf <- numeric(n_perioden)
  physischer_bestand <- numeric(n_perioden)
  
  nettobestand_vor_bestellung <- numeric(n_perioden)
  nettobestand <- numeric(n_perioden)
  
  fehlmenge <- numeric(n_perioden)
  disponibler_bestand_vor_bestellung <- numeric(n_perioden)
  disponibler_bestand_nach_bestellung <- numeric(n_perioden)
  
  # With backorders, n_0 equals the initial physical inventory.
  netto_vorperiode <- anfangsbestand
  
  for (k in periode) {
    
    # 1. Delivery at the start of the period:
    # order o_(k-wbz) arrives in period k.
    if (wbz == 0L) {
      liefermenge[k] <- 0
    } else if (k > wbz) {
      liefermenge[k] <- bestellmenge[k - wbz]
    }
    
    # 2. Net inventory after delivery and realisation of demand.
    nettobestand_vor_bestellung[k] <-
      netto_vorperiode + liefermenge[k] - bedarf[k]
    
    # With backordering, net inventory is carried forward including
    # shortages. With lost sales, unmet demand is lost.
    if (backorders) {
      nettobestand[k] <- nettobestand_vor_bestellung[k]
    } else {
      nettobestand[k] <- max(
        0,
        nettobestand_vor_bestellung[k]
      )
    }
    
    # Physical inventory is never negative.
    physischer_bestand[k] <- max(
      0,
      nettobestand[k]
    )
    
    # Inventory immediately before demand is realised equals the
    # previous period's net inventory plus the quantity received.
    physischer_bestand_vor_bedarf[k] <- max(
      0,
      netto_vorperiode + liefermenge[k]
    )
    
    # Shortage quantity is only relevant with backordering.
    fehlmenge[k] <- if (backorders) {
      max(0, -netto_vorperiode - liefermenge[k] + bedarf[k])
    } else {
      max(0, -netto_vorperiode - liefermenge[k] + bedarf[k])
    }
    
    # 3. Outstanding orders before the new order:
    # all orders of the last wbz periods that have not yet
    # arrived.
    if (wbz == 0L) {
      offene_bestellungen[k] <- 0
    } else {
      start_offen <- max(1L, k - wbz + 1L)
      ende_offen <- k - 1L
      
      offene_bestellungen[k] <- if (start_offen <= ende_offen) {
        sum(bestellmenge[start_offen:ende_offen])
      } else {
        0
      }
    }
    
    # 4. Inventory position before the new order.
    disponibler_bestand_vor_bestellung[k] <-
      nettobestand[k] + offene_bestellungen[k]
    
    # 5. Order decision:
    # in periods 1, 1+t, 1+2t, ... inventory is raised to S.
    if ((k - 1L) %% t == 0L) {
      bestellmenge[k] <- max(
        0,
        S - disponibler_bestand_vor_bestellung[k]
      )
    }
    
    # Inventory position immediately after ordering.
    disponibler_bestand_nach_bestellung[k] <-
      disponibler_bestand_vor_bestellung[k] + bestellmenge[k]
    
    # Carry forward to the next period.
    netto_vorperiode <- nettobestand[k]
  }
  
  tibble::tibble(
    Periode = periode,
    Bedarf = bedarf,
    Lieferung = liefermenge,
    Physischer_Bestand_vor_Bedarf = physischer_bestand_vor_bedarf,
    Physischer_Bestand = physischer_bestand,
    Nettobestand = nettobestand,
    Fehlmenge = fehlmenge,
    Offene_Bestellungen = offene_bestellungen,
    Disponibler_Bestand_vor_Bestellung =
      disponibler_bestand_vor_bestellung,
    Bestellmenge = bestellmenge,
    Disponibler_Bestand_nach_Bestellung =
      disponibler_bestand_nach_bestellung,
    Bestellperiode = (periode - 1L) %% t == 0L
  )
}






#' (t,S) Inventory control function
#'
#' calculates inventory stocks for a (t,S)-controlled storage
#'
#' @param d  numeric vector with period demands
#' @param t  numeric scalar with order interval
#' @param S  numeric scalar with order-up-to-level
#' @param l.ini  numeric initial inventory level
#' @param wbz  integer number of  order lead time
#' @return        Named matrix with demand, stock levels, orders and deliveries
#' @examples
#' demand <- rpois(25, lambda = 25)
#' tS.lager(d = demand, t =3, S = 65, l.ini = 50, wbz = 2)

tS.lager <- function(d, t, S, l.ini,  wbz){
  n <- length(d)
  tmp.mat <- matrix(0, nrow= n+1+wbz, ncol= 5 )
  colnames(tmp.mat) <- c("Bedarf","Lagerbestand", "Bestellung", "Lieferung", "dis.LB")
  
  tmp.mat[,"Bedarf"] <- c(0,d, rep(0,wbz))
  tmp.mat[1,"Lagerbestand"] <- l.ini
  tmp.mat[1,"dis.LB"] <- l.ini
  
  for(i in 2:(n+1)){
    if((i-1) %% t == 0){
      tmp.mat[i, "Bestellung"] <- S - tmp.mat[i-1, "dis.LB"]
      tmp.mat[i+wbz, "Lieferung"] <- tmp.mat[i, "Bestellung"]
    }
    
    tmp.mat[i, "dis.LB"] <- tmp.mat[i-1, "dis.LB"] - tmp.mat[i, "Bedarf"] + tmp.mat[i, "Bestellung"]
    tmp.mat[i, "Lagerbestand"] <- tmp.mat[i-1, "Lagerbestand"] - tmp.mat[i, "Bedarf"] + tmp.mat[i, "Lieferung"]
    
  }
  return(tmp.mat[2:(n+1),]) 
}

tS.lager.new <- function(d, t, S, l.ini,  wbz){
  n <- length(d)
  tmp.mat <- matrix(0, nrow= n+1+wbz, ncol= 7 )
  colnames(tmp.mat) <- c("Bedarf","Lagerbestand","Nettobestand", "offene_Bestellungen", "Bestellmenge", "Lieferung", "disp_LB", "disp_LB_vor_Nachfrage")
  
  tmp.mat[,"Bedarf"] <- c(0,d, rep(0,wbz))
  tmp.mat[1,"Lagerbestand"] <- tmp.mat[1,"dis.LB"] <- tmp.mat[1,"Nettobestand"] <- l.ini
  tmp.mat[1,"offene_Bestellungen"] <- 0
  
  for(i in 2:(n+1)){
    
    tmp.mat[i, "Nettobestand"] <- tmp.mat[i-1, "Nettobestand"] - tmp.mat[i, "Bedarf"] + tmp.mat[i, "Lieferung"]
    
    tmp.mat[i, "offene_Bestellungen"] <- tmp.mat[i-1, "offene_Bestellungen"] - tmp.mat[i, "Lieferung"]
    
    
    if((i-1) %% t == 0){
      tmp.mat[i, "Bestellung"] <- S - tmp.mat[i-1, "dis.LB"] #+ tmp.mat[i, "Bedarf"] + tmp.mat[i, "Lieferung"]
      tmp.mat[i+wbz, "Lieferung"] <- tmp.mat[i, "Bestellung"]
    }
    
    tmp.mat[i, "dis.LB"] <- tmp.mat[i-1, "dis.LB"] - tmp.mat[i, "Bedarf"] + tmp.mat[i, "Bestellung"]
    
    
    tmp.mat[i, "Lagerbestand"] <- max(0,tmp.mat[i, "Nettobestand"])
    
  }
  return(tmp.mat[2:(n+1),]) 
}



# ─── 1. LOCATION PLANNING ─────────────────────────────────────────────────────

#' Center of Gravity (CoG) Heuristic
#'
#' Minimises the sum of weighted squared Euclidean distances.
#' Fast closed-form solution — a reasonable starting point for Weiszfeld.
#'
#' @param coords  data.frame with columns \code{x}, \code{y}, \code{demand}
#' @return        Named numeric vector c(x = ..., y = ...)
#' @examples
#' sites <- data.frame(x = c(0,-13,5,-18,8), y = c(0,3,-25,-5,-5),
#'                     demand = c(800,450,600,350,500))
#' center_of_gravity(sites)
center_of_gravity <- function(coords) {
  stopifnot(all(c("x", "y", "demand") %in% names(coords)))
  if (any(coords$demand < 0)) stop("Demand values must be non-negative.")
  total_demand <- sum(coords$demand)
  if (total_demand == 0) stop("Total demand must be positive.")
  c(
    x = sum(coords$demand * coords$x) / total_demand,
    y = sum(coords$demand * coords$y) / total_demand
  )
}


#' Weiszfeld Algorithm for the Euclidean Steiner-Weber Problem
#'
#' Minimises the sum of weighted Euclidean distances (not squared) by
#' iteratively re-weighted least-squares updates.
#'
#' @param coords    data.frame with columns \code{x}, \code{y}, \code{demand}
#' @param max_iter  Maximum number of iterations (default 100)
#' @param tol       Convergence tolerance — stop when step length < tol (default 1e-6)
#' @param start     Optional starting point c(x, y). Defaults to CoG.
#' @return          List with fields:
#'   \describe{
#'     \item{x}{Optimal x-coordinate}
#'     \item{y}{Optimal y-coordinate}
#'     \item{iterations}{Number of iterations performed}
#'     \item{history}{data.frame of (iter, x, y, twd) per iteration}
#'     \item{converged}{Logical — did the algorithm converge?}
#'   }
#' @examples
#' sites <- data.frame(x = c(0,-13,5,-18,8), y = c(0,3,-25,-5,-5),
#'                     demand = c(800,450,600,350,500))
#' weiszfeld(sites)
weiszfeld <- function(coords, max_iter = 100, tol = 1e-6, start = NULL) {
  stopifnot(all(c("x", "y", "demand") %in% names(coords)))

  # Internal helper: total weighted Euclidean distance
  twd <- function(fx, fy) sum(coords$demand * sqrt((coords$x - fx)^2 +
                                                      (coords$y - fy)^2))

  # Starting point
  if (is.null(start)) {
    cog   <- center_of_gravity(coords)
    x <- cog["x"]; y <- cog["y"]
  } else {
    x <- start[1]; y <- start[2]
  }

  history   <- data.frame(iter = 0L, x = x, y = y, twd = twd(x, y))
  converged <- FALSE

  for (k in seq_len(max_iter)) {
    # Euclidean distances from current estimate
    d <- sqrt((coords$x - x)^2 + (coords$y - y)^2)

    # Guard against coincidence with a demand site (avoids division by zero)
    d[d < 1e-10] <- 1e-10

    w_d   <- coords$demand / d
    x_new <- sum(w_d * coords$x) / sum(w_d)
    y_new <- sum(w_d * coords$y) / sum(w_d)

    history <- rbind(history,
                     data.frame(iter = k, x = x_new, y = y_new,
                                twd  = twd(x_new, y_new)))

    step_len <- sqrt((x_new - x)^2 + (y_new - y)^2)
    x <- x_new; y <- y_new

    if (step_len < tol) {
      converged <- TRUE
      break
    }
  }

  list(x = x, y = y, iterations = nrow(history) - 1L,
       history = history, converged = converged)
}


#' Haversine Distance Between Two Geographic Points
#'
#' Returns the great-circle distance in kilometres.
#'
#' @param lat1  Latitude  of point 1 (decimal degrees)
#' @param lon1  Longitude of point 1 (decimal degrees)
#' @param lat2  Latitude  of point 2 (decimal degrees)
#' @param lon2  Longitude of point 2 (decimal degrees)
#' @return      Distance in km (scalar or vector)
#' @examples
#' haversine(51.5, 0, 48.8, 2.35)  # London to Paris ≈ 341 km
haversine <- function(lat1, lon1, lat2, lon2) {
  R     <- 6371.0                  # Earth's mean radius (km)
  phi1  <- lat1 * pi / 180
  phi2  <- lat2 * pi / 180
  dphi  <- (lat2 - lat1) * pi / 180
  dlam  <- (lon2 - lon1) * pi / 180
  a     <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  R * 2 * atan2(sqrt(a), sqrt(1 - a))
}


#' AHP Priority Vector via Eigenvector Method
#'
#' Computes the normalised principal eigenvector of a pairwise comparison
#' matrix — the standard AHP priority derivation.
#'
#' @param mat  n×n pairwise comparison matrix (must be positive, square).
#'             Entry [i,j] = "how much more important is criterion i than j?"
#' @return     Normalised priority vector (sums to 1)
#' @examples
#' mat <- matrix(c(1, 3, 5,
#'                 1/3, 1, 2,
#'                 1/5, 1/2, 1), nrow = 3, byrow = TRUE)
#' ahp_priority_vector(mat)
ahp_priority_vector <- function(mat) {
  if (!is.matrix(mat) || nrow(mat) != ncol(mat))
    stop("'mat' must be a square matrix.")
  if (any(mat <= 0))
    stop("All entries must be strictly positive.")

  n      <- nrow(mat)
  # Geometric mean method as a stable approximation to the eigenvector method
  geo_means <- apply(mat, 1, function(row) prod(row)^(1/n))
  weights   <- geo_means / sum(geo_means)
  names(weights) <- rownames(mat)
  weights
}


#' AHP Consistency Check
#'
#' Computes lambda_max, CI, and CR for a pairwise comparison matrix.
#'
#' @param mat      n×n pairwise comparison matrix
#' @param weights  Priority vector from \code{ahp_priority_vector()} — if NULL,
#'                 computed internally.
#' @return         List with lambda_max, CI (Consistency Index), and CR
#'                 (Consistency Ratio). CR < 0.10 is acceptable.
#' @examples
#' mat <- matrix(c(1, 3, 5, 1/3, 1, 2, 1/5, 1/2, 1), 3, byrow = TRUE)
#' ahp_consistency(mat)
ahp_consistency <- function(mat, weights = NULL) {
  # Random Index table (Saaty, n = 1..10)
  RI <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49)

  if (is.null(weights)) weights <- ahp_priority_vector(mat)
  n <- nrow(mat)

  # Weighted sum vector
  Aw        <- as.numeric(mat %*% weights)
  lambda_max <- mean(Aw / weights)
  CI        <- (lambda_max - n) / (n - 1)
  ri        <- if (n <= 10) RI[n] else 1.49
  CR        <- CI / ri

  list(lambda_max = lambda_max, CI = CI, CR = CR,
       consistent = CR < 0.10)
}


# ─── 2. TRANSPORTATION PROBLEM ────────────────────────────────────────────────

#' Vogel's Approximation Method (VAM) for the Transportation Problem
#'
#' Finds an initial feasible allocation for the classical transportation
#' problem. VAM typically yields solutions within 0–5% of optimum.
#'
#' The problem may be unbalanced: if sum(supply) != sum(demand), a dummy
#' source or destination with zero cost is added automatically.
#'
#' @param cost_mat  m×n cost matrix (supply nodes as rows, demand nodes as cols)
#' @param supply    Numeric vector of supply quantities (length m)
#' @param demand    Numeric vector of demand quantities (length n)
#' @param verbose   Print iteration details (default FALSE)
#' @return          List with:
#'   \describe{
#'     \item{allocation}{m×n allocation matrix}
#'     \item{total_cost}{Total transportation cost}
#'     \item{balanced}{Was the problem balanced? (logical)}
#'   }
#' @examples
#' cost <- matrix(c(2,3,1,7, 5,2,4,3, 4,6,3,5), 3, byrow = TRUE)
#' vogels_approximation(cost, supply = c(120,80,100), demand = c(70,90,60,80))
vogels_approximation <- function(cost_mat, supply, demand, verbose = FALSE) {
  # ── Balance the problem ──────────────────────────────────────────────────
  balanced <- TRUE
  total_s  <- sum(supply); total_d <- sum(demand)
  if (total_s > total_d) {
    # Excess supply → add dummy destination with zero cost
    cost_mat <- cbind(cost_mat, Dummy = 0)
    demand   <- c(demand, Dummy = total_s - total_d)
    balanced <- FALSE
  } else if (total_d > total_s) {
    # Excess demand → add dummy source with zero cost
    cost_mat <- rbind(cost_mat, Dummy = 0)
    supply   <- c(supply, Dummy = total_d - total_s)
    balanced <- FALSE
  }

  m <- nrow(cost_mat); n <- ncol(cost_mat)
  alloc       <- matrix(0, m, n,
                        dimnames = list(rownames(cost_mat), colnames(cost_mat)))
  sup         <- supply
  dem         <- demand
  active_rows <- seq_len(m)
  active_cols <- seq_len(n)
  iter        <- 0L

  while (length(active_rows) > 0 && length(active_cols) > 0) {
    iter <- iter + 1L

    # Penalty = difference between two smallest costs (0 if only one active)
    row_pen <- sapply(active_rows, function(i) {
      v <- sort(cost_mat[i, active_cols]); if (length(v) >= 2) v[2] - v[1] else 0
    })
    col_pen <- sapply(active_cols, function(j) {
      v <- sort(cost_mat[active_rows, j]); if (length(v) >= 2) v[2] - v[1] else 0
    })

    if (verbose) {
      cat(sprintf("\n[VAM iter %d]\n", iter))
      cat("Row penalties:", setNames(row_pen, rownames(cost_mat)[active_rows]), "\n")
      cat("Col penalties:", setNames(col_pen, colnames(cost_mat)[active_cols]), "\n")
    }

    if (max(row_pen, na.rm = TRUE) >= max(col_pen, na.rm = TRUE)) {
      sel_r_idx <- which.max(row_pen)
      i <- active_rows[sel_r_idx]
      j <- active_cols[which.min(cost_mat[i, active_cols])]
    } else {
      sel_c_idx <- which.max(col_pen)
      j <- active_cols[sel_c_idx]
      i <- active_rows[which.min(cost_mat[active_rows, j])]
    }

    qty        <- min(sup[i], dem[j])
    alloc[i,j] <- alloc[i,j] + qty
    sup[i]     <- sup[i] - qty
    dem[j]     <- dem[j] - qty

    if (verbose) cat(sprintf("  Allocate %g: [%s] → [%s]\n",
                             qty, rownames(cost_mat)[i], colnames(cost_mat)[j]))

    if (sup[i] <= 0) active_rows <- setdiff(active_rows, i)
    if (dem[j] <= 0) active_cols <- setdiff(active_cols, j)
  }

  list(
    allocation = alloc,
    total_cost = sum(alloc * cost_mat),
    balanced   = balanced
  )
}


# ─── 3. INVENTORY MODELS ──────────────────────────────────────────────────────

#' Newsvendor Model with Normal Demand
#'
#' Solves the single-period newsvendor problem assuming normally distributed demand.
#'
#' @param mu    Mean demand
#' @param sigma Standard deviation of demand (must be > 0)
#' @param p     Selling price per unit
#' @param c     Purchase / production cost per unit
#' @param s     Salvage value per unit (must satisfy s < c < p)
#' @return      Named list with CR, Q_star, expected profit, and service level.
#' @examples
#' newsvendor_normal(mu = 500, sigma = 100, p = 200, c = 120, s = 60)
newsvendor_normal <- function(mu, sigma, p, c, s) {
  if (sigma <= 0)  stop("sigma must be positive.")
  if (!(s < c && c < p)) stop("Must satisfy s < c < p.")

  cu     <- p - c          # underage cost (margin lost per unit of stockout)
  co     <- c - s          # overage cost  (loss per unsold unit)
  cr     <- cu / (cu + co) # critical ratio = (p-c)/(p-s)

  z_star <- qnorm(cr)
  q_star <- mu + sigma * z_star

  # Loss function: L(z) = E[max(Z-z,0)] for standard normal Z
  L <- function(z) dnorm(z) - z * (1 - pnorm(z))

  z_q          <- (q_star - mu) / sigma
  exp_leftover <- sigma * L(-z_q)          # E[max(Q-D,0)]
  exp_stockout <- sigma * L(z_q)           # E[max(D-Q,0)]
  exp_sales    <- mu - exp_stockout

  exp_revenue  <- p * exp_sales + s * exp_leftover
  exp_cost     <- c * q_star
  exp_profit   <- exp_revenue - exp_cost

  list(
    mu = mu, sigma = sigma, p = p, c = c, s = s,
    cu = cu, co = co, cr = cr,
    z_star       = z_star,
    q_star       = q_star,
    exp_sales    = exp_sales,
    exp_leftover = exp_leftover,
    exp_stockout = exp_stockout,
    exp_revenue  = exp_revenue,
    exp_profit   = exp_profit,
    service_level = pnorm(z_star)
  )
}


#' EOQ — Economic Order Quantity
#'
#' Classic Wilson formula for the economic order quantity.
#'
#' @param K  Fixed ordering (setup) cost per order (€)
#' @param D  Demand rate (units per time period, same period as h)
#' @param h  Holding cost per unit per time period (€)
#' @return   Named list with Q_star (EOQ), cycle time T_star, and total cost TC.
#' @examples
#' eoq(K = 200, D = 1000, h = 5)
eoq <- function(K, D, h) {
  if (any(c(K, D, h) <= 0)) stop("K, D, h must all be strictly positive.")
  q_star <- sqrt(2 * K * D / h)
  t_star <- q_star / D           # cycle length (same time unit as D)
  tc     <- sqrt(2 * K * D * h) # total annual cost (holding + ordering)
  list(
    K = K, D = D, h = h,
    q_star = q_star,
    t_star = t_star,
    orders_per_period = D / q_star,
    total_cost = tc
  )
}


#' Inventory Pooling — Safety Stock Comparison
#'
#' Compares the safety stock required under (a) independent ordering at
#' n decentralised locations vs. (b) a single centralised pool.
#' Assumes independent, identically distributed demand at each location.
#'
#' @param n      Number of locations
#' @param sigma  Standard deviation of demand at each individual location
#'               (same period as lead time)
#' @param z      Service level quantile (e.g. \code{qnorm(0.95)} for 95\%)
#' @return       Named list with:
#'   \describe{
#'     \item{ss_individual}{Safety stock per location (individual policy)}
#'     \item{ss_total_individual}{Total safety stock across all n locations}
#'     \item{ss_central}{Safety stock at the central warehouse}
#'     \item{savings}{Absolute reduction in safety stock units}
#'     \item{savings_pct}{Percentage reduction}
#'     \item{pooling_factor}{sqrt(n) — the theoretical pooling factor}
#'   }
#' @examples
#' pooling_safety_stock(n = 10, sigma = 40, z = qnorm(0.95))
pooling_safety_stock <- function(n, sigma, z) {
  if (n < 1 || sigma <= 0 || z < 0)
    stop("n >= 1, sigma > 0, and z >= 0 required.")

  ss_individual       <- z * sigma
  ss_total_individual <- n * ss_individual
  sigma_central       <- sqrt(n) * sigma    # combined std under independence
  ss_central          <- z * sigma_central

  savings     <- ss_total_individual - ss_central
  savings_pct <- savings / ss_total_individual * 100

  list(
    n                   = n,
    sigma               = sigma,
    z                   = z,
    ss_individual       = ss_individual,
    ss_total_individual = ss_total_individual,
    sigma_central       = sigma_central,
    ss_central          = ss_central,
    savings             = savings,
    savings_pct         = savings_pct,
    pooling_factor      = sqrt(n)
  )
}


# ─── 4. WAREHOUSE LOCATION PROBLEM ────────────────────────────────────────────

#' Internal Helper — Total Cost for a Given Set of Open Warehouses
#'
#' Each customer is served by the cheapest open warehouse.
#'
#' @param open_wh           Integer indices of open warehouses
#' @param fixed_costs       Named vector of annual fixed costs per warehouse
#' @param transport_cost_mat (warehouses × customers) transport cost per unit
#' @param demand            Customer demand vector
#' @return                  List with fixed, transport, and total cost, plus assignment
.wlp_cost <- function(open_wh, fixed_costs, transport_cost_mat, demand) {
  if (length(open_wh) == 0)
    return(list(fixed = 0, transport = Inf, total = Inf, assignment = integer(0)))

  sub         <- transport_cost_mat[open_wh, , drop = FALSE]
  min_tc      <- apply(sub, 2, min)
  assignment  <- apply(sub, 2, which.min)   # index within open_wh
  fc          <- sum(fixed_costs[open_wh])
  tc          <- sum(min_tc * demand)
  list(fixed = fc, transport = tc, total = fc + tc, assignment = open_wh[assignment])
}


#' Add Heuristic for the Warehouse Location Problem (WLP)
#'
#' Starting from no open warehouses, iteratively opens the warehouse that
#' achieves the greatest reduction in total cost. Stops when no further
#' improvement is possible.
#'
#' @param fixed_costs         Named numeric vector of fixed costs per warehouse
#' @param transport_cost_mat  (warehouses × customers) matrix of per-unit transport costs
#' @param demand              Numeric vector of customer demands
#' @param verbose             Print iteration details (default TRUE)
#' @return                    List with open_warehouses (indices), fixed_cost,
#'                            transport_cost, total_cost, and assignment.
#' @examples
#' fc  <- c(5000,7000,5000,6000,4000)
#' tc  <- matrix(c(2,3,1,5,4,6,8, 3,2,4,3,5,7,1, 1,4,2,6,3,5,9,
#'                 5,3,7,2,6,4,3, 4,1,3,4,2,8,5), 5, byrow=TRUE)
#' d   <- c(100,80,120,60,90,70,110)
#' add_heuristic_wlp(fc, tc, d)
add_heuristic_wlp <- function(fixed_costs, transport_cost_mat, demand,
                               verbose = TRUE) {
  nWH      <- length(fixed_costs)
  open     <- integer(0)
  all_wh   <- seq_len(nWH)
  wh_names <- if (!is.null(names(fixed_costs))) names(fixed_costs) else paste0("WH", all_wh)
  iter     <- 0L

  repeat {
    iter       <- iter + 1L
    candidates <- setdiff(all_wh, open)
    if (length(candidates) == 0) break

    # Current cost (Inf if no warehouse open yet)
    cost_cur <- if (length(open) > 0)
      .wlp_cost(open, fixed_costs, transport_cost_mat, demand)$total
    else Inf

    # Resulting total cost for each candidate addition
    cand_cost <- sapply(candidates, function(wh) {
      .wlp_cost(c(open, wh), fixed_costs, transport_cost_mat, demand)$total
    })
    names(cand_cost) <- wh_names[candidates]
    savings <- cost_cur - cand_cost

    # Select by resulting total cost (robust even when
    # cost_cur = Inf: otherwise all savings equal "Inf" in the first
    # step and which.max() would merely pick by order)
    best_idx    <- which.min(cand_cost)
    best_saving <- savings[best_idx]
    if (best_saving <= 0 && length(open) > 0) break   # no improvement

    best_wh <- candidates[best_idx]
    open    <- c(open, best_wh)

    if (verbose) {
      res <- .wlp_cost(open, fixed_costs, transport_cost_mat, demand)
      cat(sprintf("[Add iter %d] Open %s | Total cost: %.0f\n",
                  iter, wh_names[best_wh], res$total))
    }
  }

  res <- .wlp_cost(open, fixed_costs, transport_cost_mat, demand)
  list(
    open_warehouses = open,
    open_names      = wh_names[open],
    fixed_cost      = res$fixed,
    transport_cost  = res$transport,
    total_cost      = res$total,
    assignment      = res$assignment
  )
}


#' Drop Heuristic for the Warehouse Location Problem (WLP)
#'
#' Starting from all warehouses open, iteratively closes the warehouse whose
#' removal yields the smallest cost increase (or the largest decrease). Stops
#' when closing any remaining warehouse would increase total cost.
#'
#' @param fixed_costs         Named numeric vector of fixed costs per warehouse
#' @param transport_cost_mat  (warehouses × customers) matrix of per-unit transport costs
#' @param demand              Numeric vector of customer demands
#' @param verbose             Print iteration details (default TRUE)
#' @return                    Same structure as \code{add_heuristic_wlp}
#' @examples
#' fc  <- c(5000,7000,5000,6000,4000)
#' tc  <- matrix(c(2,3,1,5,4,6,8, 3,2,4,3,5,7,1, 1,4,2,6,3,5,9,
#'                 5,3,7,2,6,4,3, 4,1,3,4,2,8,5), 5, byrow=TRUE)
#' d   <- c(100,80,120,60,90,70,110)
#' drop_heuristic_wlp(fc, tc, d)
drop_heuristic_wlp <- function(fixed_costs, transport_cost_mat, demand,
                                verbose = TRUE) {
  nWH      <- length(fixed_costs)
  open     <- seq_len(nWH)                   # start with all open
  wh_names <- if (!is.null(names(fixed_costs))) names(fixed_costs) else paste0("WH", seq_len(nWH))
  iter     <- 0L

  repeat {
    if (length(open) <= 1) break
    iter     <- iter + 1L
    cost_cur <- .wlp_cost(open, fixed_costs, transport_cost_mat, demand)$total

    # Cost after dropping each open warehouse
    costs_after_drop <- sapply(open, function(wh) {
      remaining <- setdiff(open, wh)
      if (length(remaining) == 0) return(Inf)
      .wlp_cost(remaining, fixed_costs, transport_cost_mat, demand)$total
    })
    names(costs_after_drop) <- wh_names[open]

    best_drop_cost <- min(costs_after_drop)
    if (best_drop_cost >= cost_cur) break    # no improvement from dropping

    wh_to_drop <- open[which.min(costs_after_drop)]
    open       <- setdiff(open, wh_to_drop)

    if (verbose) {
      cat(sprintf("[Drop iter %d] Close %s | New total cost: %.0f\n",
                  iter, wh_names[wh_to_drop], best_drop_cost))
    }
  }

  res <- .wlp_cost(open, fixed_costs, transport_cost_mat, demand)
  list(
    open_warehouses = open,
    open_names      = wh_names[open],
    fixed_cost      = res$fixed,
    transport_cost  = res$transport,
    total_cost      = res$total,
    assignment      = res$assignment
  )
}


# ─── UTILITY ──────────────────────────────────────────────────────────────────

#' DuPont ROI Decomposition
#'
#' @param revenue       Total revenue
#' @param mat_cost      Material costs
#' @param pers_cost     Personnel costs
#' @param fixed_assets  Net fixed assets
#' @param cash          Cash and equivalents
#' @param inventory     Inventory
#' @param receivables   Trade receivables
#' @return Named list with all DuPont components (profit, ROS, asset turnover, ROI)
#' @examples
#' dupont_roi(11000, 5500, 4950, 2000, 500, 3300, 1075)
dupont_roi <- function(revenue, mat_cost, pers_cost,
                       fixed_assets, cash, inventory, receivables) {
  profit         <- revenue - mat_cost - pers_cost
  total_assets   <- fixed_assets + cash + inventory + receivables
  ros            <- profit / revenue
  asset_turnover <- revenue / total_assets
  roi            <- ros * asset_turnover

  list(
    revenue        = revenue,
    mat_cost       = mat_cost,
    pers_cost      = pers_cost,
    profit         = profit,
    total_assets   = total_assets,
    fixed_assets   = fixed_assets,
    cash           = cash,
    inventory      = inventory,
    receivables    = receivables,
    ros            = ros,
    asset_turnover = asset_turnover,
    roi            = roi
  )
}


# ─── 6. FIGURES ───────────────────────────────────────────────────────────────

#' Open system model of the firm (after Kummer et al. 2018)
#'
#' Draws the company as an open system between suppliers and customers with
#' the functional areas procurement, production and sales, the cross-sectional
#' function logistics and the three flow levels (managerial, financial, goods).
#'
#' @param base_size Base font size.
#' @return A ggplot object.
#' @examples
#' plot_open_firm_model()
plot_open_firm_model <- function(base_size = 12) {
  red    <- "#C8102E"
  pink   <- "#F6C5B8"
  salmon <- "#E9967A"
  grey   <- "#D9D9D9"
  line   <- "#4D4D4D"
  fs     <- base_size / ggplot2::.pt

  boxes <- data.frame(
    xmin = c(0.0, 7.4, 1.3, 3.1, 4.9),
    xmax = c(1.1, 8.5, 3.1, 4.9, 6.7),
    ymin = c(1.2, 1.2, 0.4, 0.4, 0.4),
    ymax = c(4.0, 4.0, 4.6, 4.6, 4.6),
    fill = c(grey, grey, pink, pink, pink)
  )
  labels <- data.frame(
    x = c(0.55, 7.95, 2.2, 4.0, 5.8, 4.0),
    y = c(2.6, 2.6, 4.25, 4.25, 4.25, 4.95),
    label = c("Suppliers", "Customers", "Procurement", "Production", "Sales", "Management")
  )
  # arrow segments: 4 pieces between suppliers and customers
  brk  <- seq(1.15, 7.35, length.out = 5)
  segs <- data.frame(x = head(brk, -1) + 0.08, xend = tail(brk, -1) - 0.08)
  mk <- function(y, type) transform(segs, y = y, yend = y, type = type)
  flows <- rbind(mk(3.65, "info"), mk(3.10, "fin"), mk(2.45, "goods"))

  legend_df <- data.frame(
    x = c(0.3, 3.2, 6.0), xend = c(1.0, 3.9, 6.7), y = -0.45, yend = -0.45,
    type = c("info", "fin", "goods"),
    label = c("Managerial level", "Financial level", "Goods level")
  )
  lt <- c(info = "42", fin = "15", goods = "solid")
  lw <- c(info = 2.2, fin = 2.6, goods = 2.2)

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = boxes,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(fill)),
      colour = line, linewidth = 0.4) +
    ggplot2::annotate("rect", xmin = 1.3, xmax = 6.7, ymin = 4.6, ymax = 5.3,
      fill = "white", colour = line, linetype = "dotted", linewidth = 0.5) +
    ggplot2::annotate("rect", xmin = 0.6, xmax = 7.2, ymin = 0.9, ymax = 1.65,
      fill = salmon, colour = line, linewidth = 0.4) +
    ggplot2::annotate("text", x = 4.0, y = 1.45, label = "Logistics",
      fontface = "bold", size = fs, colour = "grey20") +
    ggplot2::annotate("text", x = c(2.2, 4.0, 5.8), y = 1.1,
      label = c("Procurement logistics", "Production logistics", "Distribution logistics"),
      fontface = "bold", size = fs * 0.85, colour = "grey20") +
    ggplot2::geom_text(data = labels, ggplot2::aes(x = x, y = y, label = label),
      fontface = "bold", size = fs, colour = "grey20")

  arr <- grid::arrow(length = grid::unit(0.14, "inches"), type = "closed")
  for (tp in names(lt)) {
    d <- flows[flows$type == tp, ]
    both <- tp != "fin"                    # payments flow upstream only
    # dashed/dotted shaft without arrow heads (heads are drawn solid below)
    p <- p + ggplot2::geom_segment(data = d,
      ggplot2::aes(x = x + 0.18, xend = xend - if (both) 0.18 else 0.02, y = y, yend = yend),
      colour = red, linewidth = lw[[tp]], linetype = lt[[tp]],
      lineend = if (tp == "fin") "round" else "butt")
    # solid arrow heads
    p <- p + ggplot2::geom_segment(data = d,
      ggplot2::aes(x = x + 0.2, xend = x, y = y, yend = yend),
      colour = red, linewidth = lw[[tp]] * 0.6, arrow = arr, linejoin = "mitre")
    if (both) p <- p + ggplot2::geom_segment(data = d,
      ggplot2::aes(x = xend - 0.2, xend = xend, y = y, yend = yend),
      colour = red, linewidth = lw[[tp]] * 0.6, arrow = arr, linejoin = "mitre")
    l <- legend_df[legend_df$type == tp, ]
    p <- p + ggplot2::geom_segment(data = l,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      colour = red, linewidth = lw[[tp]], linetype = lt[[tp]],
      lineend = if (tp == "fin") "round" else "butt") +
      ggplot2::annotate("text", x = l$xend + 0.12, y = l$y, label = l$label,
        hjust = 0, size = fs * 0.9, colour = "grey20")
  }
  p + ggplot2::coord_fixed(xlim = c(0, 8.5), ylim = c(-0.7, 5.35), expand = FALSE) +
    ggplot2::theme_void(base_size = base_size)
}


#' Managerial (order-processing) activities of the firm (after Kummer et al. 2018, p. 45)
#'
#' Draws the external managerial activities with suppliers and customers
#' (inquiry, offer, order, order confirmation, invoice) and the internal
#' managerial activities between sales, production and procurement
#' (requirement, queries, reports). Numbers 1-4 mark the sequence in which a
#' customer inquiry propagates upstream through the firm.
#'
#' @param base_size Base font size.
#' @return A ggplot object.
#' @examples
#' plot_order_cycles_model()
plot_order_cycles_model <- function(base_size = 12) {
  red    <- "#C8102E"
  pink   <- "#F6C5B8"
  salmon <- "#E9967A"
  grey   <- "#D9D9D9"
  line   <- "#4D4D4D"
  fs     <- base_size / ggplot2::.pt

  # block-arrow polygon: from x0 to x1 (direction given by order), centre yc, height h
  block_arrow <- function(x0, x1, yc, h, id, head = 0.35, double = FALSE) {
    s <- sign(x1 - x0); hd <- min(head, abs(x1 - x0) / 2)
    if (!double) {
      xs <- c(x0, x1 - s * hd, x1 - s * hd, x1, x1 - s * hd, x1 - s * hd, x0)
      ys <- yc + c(-h / 2, -h / 2, -h * 0.75, 0, h * 0.75, h / 2, h / 2)
    } else {
      xs <- c(x0, x0 + s * hd, x0 + s * hd, x1 - s * hd, x1 - s * hd, x1,
              x1 - s * hd, x1 - s * hd, x0 + s * hd, x0 + s * hd)
      ys <- yc + c(0, -h * 0.75, -h / 2, -h / 2, -h * 0.75, 0, h * 0.75, h / 2, h / 2, h * 0.75)
    }
    data.frame(x = xs, y = ys, id = id)
  }

  # external activities (white arrows); dir = -1 points left, +1 points right
  ext_rows <- data.frame(
    label = c("Inquiry", "Offer", "Order", "Order\nconfirmation", "Invoice"),
    y     = c(5.75, 5.15, 4.55, 3.95, 3.35),
    dir   = c(-1, 1, -1, 1, 1)
  )
  ext <- do.call(rbind, lapply(seq_len(nrow(ext_rows)), function(i) {
    r <- ext_rows[i, ]
    rbind(
      transform(block_arrow(if (r$dir > 0) 0.85 else 2.25, if (r$dir > 0) 2.25 else 0.85,
                            r$y, 0.38, paste0("L", i)), side = "L", label = r$label, dir = r$dir),
      transform(block_arrow(if (r$dir > 0) 7.75 else 9.15, if (r$dir > 0) 9.15 else 7.75,
                            r$y, 0.38, paste0("R", i)), side = "R", label = r$label, dir = r$dir)
    )
  }))
  ext_lab <- unique(ext[, c("id", "side", "label", "dir")])
  ext_lab$x <- ifelse(ext_lab$side == "L", 1.55, 8.45) - ext_lab$dir * 0.12
  ext_lab$y <- ext_rows$y[as.integer(sub("[LR]", "", ext_lab$id))]

  # internal activities (salmon arrows) between sales-production and production-procurement
  int_rows <- data.frame(label = c("Requirement", "Queries", "Reports"),
                         y = c(5.15, 4.25, 3.35), dir = c(-1, 1, 1))
  int <- do.call(rbind, lapply(seq_len(nrow(int_rows)), function(i) {
    r <- int_rows[i, ]
    do.call(rbind, lapply(list(c(2.75, 4.85, "A"), c(5.15, 7.25, "B")), function(sp) {
      a <- as.numeric(sp[1]); b <- as.numeric(sp[2])
      transform(block_arrow(if (r$dir > 0) a else b, if (r$dir > 0) b else a, r$y, 0.55,
                            paste0(sp[3], i)),
                label = r$label, xm = (a + b) / 2 - r$dir * 0.1, ym = r$y)
    }))
  }))
  int_lab <- unique(int[, c("id", "label", "xm", "ym")])

  both <- rbind(block_arrow(0.75, 3.95, 2.7, 0.3, "P1", head = 0.3, double = TRUE),
                block_arrow(6.05, 9.25, 2.7, 0.3, "P2", head = 0.3, double = TRUE))

  legend_ext <- rbind(block_arrow(1.3, 0.3, 0.75, 0.3, "LE1", 0.3),
                      block_arrow(0.3, 1.3, 0.25, 0.3, "LE2", 0.3))
  legend_int <- rbind(block_arrow(6.6, 5.6, 0.75, 0.3, "LI1", 0.3),
                      block_arrow(5.6, 6.6, 0.25, 0.3, "LI2", 0.3))

  nums <- data.frame(x = c(8.3, 5.35, 2.95, 1.7), y = c(6.3, 5.75, 5.75, 6.3),
                     label = c("1", "2", "3", "4"))

  poly <- function(d, fill, col = line) ggplot2::geom_polygon(data = d,
    ggplot2::aes(x = x, y = y, group = id), fill = fill, colour = col, linewidth = 0.35)

  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = c(0, 8.5), xmax = c(1.5, 10), ymin = 1.05, ymax = 6.6,
      fill = grey, colour = line, linewidth = 0.4) +
    ggplot2::annotate("rect", xmin = c(1.5, 3.8, 6.2), xmax = c(3.8, 6.2, 8.5),
      ymin = 1.05, ymax = 6.6, fill = pink, colour = line, linewidth = 0.4) +
    ggplot2::annotate("rect", xmin = 1.5, xmax = 8.5, ymin = 6.6, ymax = 7.3,
      fill = "white", colour = line, linetype = "dotted", linewidth = 0.5) +
    ggplot2::annotate("text", x = c(5, 2.65, 5, 7.35), y = c(6.95, 6.3, 6.3, 6.3),
      label = c("Management", "Procurement", "Production", "Sales"),
      fontface = "bold", size = fs, colour = "grey20") +
    ggplot2::annotate("text", x = c(0.42, 9.58), y = 4.55, label = c("Suppliers", "Customers"),
      fontface = "bold", size = fs * 0.9, colour = "grey20", angle = 90) +
    ggplot2::annotate("rect", xmin = 1.1, xmax = 8.9, ymin = 1.3, ymax = 2.2,
      fill = salmon, colour = line, linewidth = 0.4) +
    ggplot2::annotate("text", x = 5, y = 1.98, label = "Logistics",
      fontface = "bold", size = fs * 0.9, colour = "grey20") +
    ggplot2::annotate("text", x = c(2.65, 5, 7.35), y = 1.55,
      label = c("Procurement logistics", "Production logistics", "Distribution logistics"),
      fontface = "bold", size = fs * 0.75, colour = "grey20") +
    poly(ext, "white") +
    ggplot2::geom_text(data = ext_lab, ggplot2::aes(x = x, y = y, label = label),
      size = fs * 0.72, lineheight = 0.8, colour = "grey15") +
    poly(int, salmon, col = "#B84A3A") +
    ggplot2::geom_text(data = int_lab, ggplot2::aes(x = xm, y = ym, label = label),
      size = fs * 0.85, fontface = "bold", colour = "white") +
    poly(both, "white") +
    ggplot2::annotate("text", x = c(2.35, 7.65), y = 2.7,
      label = "Product information/queries", size = fs * 0.7, colour = "grey15") +
    ggplot2::geom_point(data = nums, ggplot2::aes(x = x, y = y), shape = 21, size = fs * 2.2,
      fill = red, colour = "white", stroke = 0.8) +
    ggplot2::geom_text(data = nums, ggplot2::aes(x = x, y = y, label = label),
      size = fs * 0.85, fontface = "bold", colour = "white") +
    poly(legend_ext, "white") + poly(legend_int, salmon, col = "#B84A3A") +
    ggplot2::annotate("text", x = c(1.5, 6.8), y = 0.5, hjust = 0, lineheight = 0.9,
      label = c("External managerial\nactivities", "Internal managerial\nactivities"),
      size = fs * 0.8, colour = "grey20") +
    ggplot2::coord_fixed(xlim = c(0, 10), ylim = c(0, 7.35), expand = FALSE) +
    ggplot2::theme_void(base_size = base_size)
}


#' Levels of the management system (after Dyckhoff 1998)
#'
#' Pyramid with the normative, strategic, tactical and operational levels of
#' the management system above the performance system, and the value,
#' information and material levels on the right.
#'
#' @param base_size Base font size.
#' @return A ggplot object.
#' @examples
#' plot_management_pyramid()
plot_management_pyramid <- function(base_size = 12) {
  fs  <- base_size / ggplot2::.pt
  ink <- "grey10"
  apex <- c(5, 10); bl <- c(0.6, 0); br <- c(9.4, 0)
  # x-coordinates of the triangle edges at height y
  xl <- function(y) apex[1] - (apex[1] - bl[1]) * (apex[2] - y) / apex[2]
  xr <- function(y) apex[1] + (br[1] - apex[1]) * (apex[2] - y) / apex[2]
  lev_y <- c(7.3, 5.6, 3.9, 2.2)                    # level boundaries
  hlines <- data.frame(x = xl(lev_y), xend = xr(lev_y), y = lev_y, yend = lev_y,
                       lw = c(0.5, 0.5, 0.5, 0.9))
  # "fan" lines (operating units) from the tactical level downwards
  k <- -4:4
  fan <- data.frame(x = 5 + k * 0.33, y = 3.9, xend = 5 + k * 0.95, yend = 0)
  fan <- fan[fan$x > xl(3.9) + 0.1 & fan$x < xr(3.9) - 0.1, ]
  fan_top <- data.frame(x = 5 + k * 0.33, y = 5.6, xend = 5 + k * 0.33, yend = 3.9)
  fan_top <- fan_top[abs(fan_top$x - 5) < 0.8 & fan_top$x != 5, ]
  lab <- data.frame(
    x = 5, y = c(7.9, 6.45, 4.75, 3.05, 1.1),
    label = c("normative", "strategic", "tactical", "operational", "performance system"))
  side <- data.frame(x = 11.8, y = c(8.6, 4.75, 1.1),
                     label = c("value level", "information level", "material level"))
  # bracket along the left edge (management system)
  off <- -0.6
  br_df <- data.frame(x = xl(c(9.9, 2.2)) + off * c(1, 1), y = c(9.9, 2.2) + c(0.05, -0.05))
  ggplot2::ggplot() +
    ggplot2::annotate("polygon", x = c(bl[1], apex[1], br[1]), y = c(bl[2], apex[2], br[2]),
      fill = "white", colour = ink, linewidth = 1.1) +
    ggplot2::geom_segment(data = hlines, ggplot2::aes(x = x, xend = xend, y = y, yend = yend,
      linewidth = I(lw)), colour = ink) +
    ggplot2::geom_segment(data = fan, ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      colour = ink, linewidth = 0.35) +
    ggplot2::geom_segment(data = fan_top, ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      colour = ink, linewidth = 0.35) +
    ggplot2::annotate("segment", x = -0.5, xend = 13.2, y = c(7.3, 2.2), yend = c(7.3, 2.2),
      linetype = "dashed", colour = ink, linewidth = 0.5) +
    ggplot2::annotate("label", x = 5, y = lab$y, label = lab$label, size = fs,
      label.size = 0, fill = "white", colour = ink) +
    ggplot2::annotate("text", x = side$x, y = side$y, label = side$label, size = fs, colour = ink) +
    ggplot2::annotate("segment", x = br_df$x[1], xend = br_df$x[2], y = br_df$y[1], yend = br_df$y[2],
      colour = ink, linewidth = 0.5) +
    ggplot2::annotate("segment", x = br_df$x, xend = br_df$x + 0.28, y = br_df$y, yend = br_df$y,
      colour = ink, linewidth = 0.5) +
    ggplot2::annotate("text", x = mean(br_df$x) - 0.45, y = mean(br_df$y) + 0.2,
      label = "management system", size = fs, colour = ink,
      angle = atan2(br_df$y[1] - br_df$y[2], br_df$x[1] - br_df$x[2]) * 180 / pi) +
    ggplot2::coord_fixed(xlim = c(-0.6, 13.3), ylim = c(-0.2, 10.2), expand = FALSE) +
    ggplot2::theme_void(base_size = base_size)
}

#' Substantive and formal objectives of the company (after Buscher et al. 2013, p. 12)
#'
#' Classification of corporate objectives into substantive and formal
#' objectives (monetary / non-monetary, quantifiable / non-quantifiable) and
#' their transformation into operational target metrics.
#'
#' @param base_size Base font size.
#' @return A ggplot object.
#' @examples
#' plot_objective_types()
plot_objective_types <- function(base_size = 12) {
  fs  <- base_size / ggplot2::.pt
  ink <- "grey10"
  xs  <- c(0, 2.3, 4.6, 6.9, 9.2)                  # column borders
  cell <- function(x0, x1, y0, y1) data.frame(xmin = x0, xmax = x1, ymin = y0, ymax = y1)
  grid_rects <- rbind(
    cell(xs[1], xs[2], 5.1, 7.3),                    # substantive objectives header
    cell(xs[2], xs[5], 6.6, 7.3),                    # formal objectives
    cell(xs[2], xs[3], 5.85, 6.6), cell(xs[3], xs[5], 5.85, 6.6),
    cell(xs[2], xs[4], 5.1, 5.85), cell(xs[4], xs[5], 5.1, 5.85),
    cell(xs[1], xs[2], 3.3, 5.1), cell(xs[2], xs[3], 3.3, 5.1),
    cell(xs[3], xs[4], 3.3, 5.1), cell(xs[4], xs[5], 3.3, 5.1),
    cell(2.3, 6.9, 0.35, 1.75))                      # target metrics box
  heads <- data.frame(
    x = c(1.15, 5.75, 3.45, 6.9, 4.6, 8.05), y = c(6.2, 6.95, 6.22, 6.22, 5.47, 5.47),
    label = c("Substantive\nobjectives", "Formal objectives", "Monetary objectives",
              "Non-monetary objectives", "Quantifiable objectives", "NQ objectives"))
  body <- data.frame(
    x = xs[1:4] + 0.1, y = 4.2,
    label = c("Types of products\nto be produced in\nterms of quantity\nand quality",
              "Profit objectives\nRevenue objectives\nCost objectives\nLiquidity objectives",
              "Growth objectives\nMarket share\nobjectives\nEnvironmental\nobjectives",
              "Social objectives\nAutonomy, flexibility\nand prestige\nEnvironmental\nobjectives"))
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = grid_rects, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", colour = ink, linewidth = 0.5) +
    ggplot2::geom_text(data = heads, ggplot2::aes(x = x, y = y, label = label),
      fontface = "bold", size = fs * 0.95, lineheight = 0.9, colour = ink) +
    ggplot2::geom_text(data = body, ggplot2::aes(x = x, y = y, label = label),
      hjust = 0, size = fs * 0.85, lineheight = 0.9, colour = ink) +
    ggplot2::annotate("segment", x = 4.6, xend = 4.6, y = c(3.25, 2.25), yend = c(2.8, 1.8),
      arrow = grid::arrow(length = grid::unit(0.1, "inches")), linewidth = 0.5, colour = ink) +
    ggplot2::annotate("text", x = 4.6, y = 2.53, label = "Objective transformation",
      fontface = "bold", size = fs, colour = ink) +
    ggplot2::annotate("text", x = 4.6, y = 1.05, lineheight = 1,
      label = "max. period contribution margin\nmin. inventory levels\nmax. capacity utilisation\nmin. order lead times",
      size = fs * 0.9, colour = ink) +
    ggplot2::coord_fixed(xlim = c(-0.05, 9.25), ylim = c(0.25, 7.35), expand = FALSE) +
    ggplot2::theme_void(base_size = base_size)
}
