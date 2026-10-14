#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# Estimating the stream of the River Cam from rowing GPS traces
#
# Model:   v_ground = k_session * s^p  +  d * u_session
#   v_ground  GPS speed (m/s)
#   s         stroke rate (cadence, strokes/min)
#   p         effort exponent (expected 1/3; fixed first, then freed as a check)
#   d         +1 downstream, -1 upstream
#   u_session stream speed (m/s, positive = flowing downstream) for each
#             session/day; it acts equally on both directions within a session
#   k_session crew-/day-specific "fitness" constant
#
# Only points with ground speed >= MIN_SPEED are used.
#
# Usage:  Rscript river_stream.R [norwich_json.gz] [output_dir]
# ---------------------------------------------------------------------------

pkgs <- c("jsonlite", "ggplot2")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
  library(p, character.only = TRUE)
}

args      <- commandArgs(trailingOnly = TRUE)
in_file   <- if (length(args) >= 1) args[1] else "norwich_json.gz"
out_dir   <- if (length(args) >= 2) args[2] else "."
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

MIN_SPEED   <- 3      # m/s, ignore slower points (turns, starts, paddling)
EXPONENT    <- 1/3    # v ~ s^(1/3)
SMOOTH_SECS <- 15     # window used to decide direction of travel
# The Cam flows north (Cambridge -> Ely), so increasing position along the
# river axis (below, oriented to point north) is "downstream".
DOWNSTREAM_IS_NORTH <- TRUE

theme_set(theme_minimal(base_size = 12))
dir_cols <- c(downstream = "#d95f02", upstream = "#1b9e77")

# ---- 1. Load ---------------------------------------------------------------
raw <- fromJSON(gzfile(in_file), simplifyVector = TRUE)
sessions <- lapply(seq_len(nrow(raw)), function(i) {
  r <- raw$records[[i]]
  r$session <- sub("^cnn-norwich-[0-9]+-", "", sub("\\.fit$", "", raw$file[i]))
  r
})
d <- do.call(rbind, lapply(sessions, function(r)
  r[, c("session", "timestamp", "latitude", "longitude", "cadence", "speed")]))
d <- d[complete.cases(d), ]
d$session <- factor(d$session)
d <- d[order(d$session, d$timestamp), ]
d$t <- d$timestamp - ave(d$timestamp, d$session, FUN = min)

# ---- 2. Position along the river and direction of travel -------------------
# Local metric coordinates (m) and the principal axis of the whole track,
# which for this stretch of the Cam is effectively the river axis.
lat0 <- mean(d$latitude); lon0 <- mean(d$longitude)
d$x <- (d$longitude - lon0) * 111320 * cos(lat0 * pi / 180)
d$y <- (d$latitude  - lat0) * 110540
pc   <- prcomp(d[, c("x", "y")])
axis <- pc$rotation[, 1]
if (axis["y"] < 0) axis <- -axis                 # orient the axis northwards
if (!DOWNSTREAM_IS_NORTH) axis <- -axis
d$pos <- as.numeric(as.matrix(d[, c("x", "y")]) %*% axis)   # m along river, + = downstream

# Direction: sign of the smoothed rate of change of the along-river position,
# computed per session over a +/- SMOOTH_SECS window (robust to GPS jitter and
# to uneven sampling).
direction_of <- function(df) {
  n <- nrow(df); vel <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    w <- which(abs(df$t - df$t[i]) <= SMOOTH_SECS)
    if (length(w) >= 3) vel[i] <- coef(lm(pos ~ t, data = df[w, ]))[2]
  }
  df$pos_vel <- vel
  df
}
d <- do.call(rbind, lapply(split(d, d$session), direction_of))
d$dir <- factor(ifelse(d$pos_vel > 0, "downstream", "upstream"),
                levels = c("downstream", "upstream"))
d$d   <- ifelse(d$dir == "downstream", 1, -1)

# ---- 3. Filter -------------------------------------------------------------
dat <- subset(d, !is.na(dir) & speed >= MIN_SPEED & cadence > 0)
dat$e <- dat$cadence^EXPONENT                    # effort proxy
cat(sprintf("Points: %d total, %d used (speed >= %.1f m/s)\n",
            nrow(d), nrow(dat), MIN_SPEED))
print(table(dat$session, dat$dir))

# ---- 4. Fit ----------------------------------------------------------------
# (a) MAIN: linear in (k, u) for fixed exponent; one crew constant AND one
#     stream speed per session (days differ, so the stream is not pooled).
fit_main <- lm(speed ~ 0 + session:e + session:d, data = dat)
# (b) For comparison: a single stream shared by both sessions.
fit_shared <- lm(speed ~ 0 + session:e + d, data = dat)
# (c) Free exponent (check on the 1/3 assumption), nls, stream per session.
lv <- levels(dat$session)
dat$s1 <- as.numeric(dat$session == lv[1]); dat$s2 <- as.numeric(dat$session == lv[2])
fit_p <- nls(speed ~ (k1 * s1 + k2 * s2) * cadence^p + d * (u1 * s1 + u2 * s2),
             data = dat, start = c(k1 = 1, k2 = 1, u1 = 0.1, u2 = 0.1, p = 1/3))

cat("\n=== Main model: speed = k_session * s^(1/3) + d * u_session ===\n")
print(summary(fit_main)$coefficients)
cat("\n=== Free exponent (stream per session) ===\n"); print(summary(fit_p)$coefficients)
cat("\n=== Shared-stream model (comparison) ===\n"); print(summary(fit_shared)$coefficients)
cat(sprintf("AIC per-session stream: %.1f   shared stream: %.1f\n", AIC(fit_main), AIC(fit_shared)))

# Block bootstrap (resample 60 s blocks within each session) -> honest CI,
# since neighbouring points are strongly autocorrelated.
set.seed(1)
dat$block <- paste(dat$session, floor(dat$t / 60))
blocks_by_session <- split(unique(dat$block), sapply(strsplit(unique(dat$block), " "), `[`, 1))
u_names <- paste0("session", lv, ":d")
boot <- t(replicate(500, {
  b  <- unlist(lapply(blocks_by_session, function(bl) sample(bl, replace = TRUE)))
  db <- do.call(rbind, lapply(b, function(x) dat[dat$block == x, ]))
  tryCatch(coef(lm(speed ~ 0 + session:e + session:d, data = db))[u_names],
           error = function(e) rep(NA_real_, length(lv)))
}))
colnames(boot) <- lv
u_hat <- setNames(coef(fit_main)[u_names], lv)
ci <- apply(boot, 2, quantile, probs = c(.025, .975), na.rm = TRUE)
u_tab <- data.frame(session = lv, u_ms = u_hat, lo = ci[1, ], hi = ci[2, ],
                    u_kmh = u_hat * 3.6, row.names = NULL)
cat("\nStream estimate per session (m/s, + = downstream; block-bootstrap 95% CI):\n")
print(u_tab, digits = 3)
dif <- boot[, 1] - boot[, 2]
cat(sprintf("Difference between sessions: %.3f m/s (95%% CI %.3f .. %.3f)\n",
            u_hat[1] - u_hat[2], quantile(dif, .025, na.rm = TRUE), quantile(dif, .975, na.rm = TRUE)))
cat(sprintf("Residual SD = %.3f m/s; free exponent p = %.3f\n", sigma(fit_main), coef(fit_p)["p"]))

dat$u_s   <- u_hat[as.character(dat$session)]
dat$fit   <- fitted(fit_main)
dat$resid <- resid(fit_main)
dat$still <- dat$speed - dat$d * dat$u_s        # speed corrected to still water
lab <- setNames(sprintf("%s\nu = %.2f m/s (%.2f..%.2f)", lv, u_hat, ci[1, ], ci[2, ]), lv)
dat$panel <- factor(lab[as.character(dat$session)], levels = lab)

# ---- 5. Plots --------------------------------------------------------------
save_plot <- function(p, name, w = 9, h = 5.5)
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 150)

# 5.1 Track coloured by direction
p1 <- ggplot(subset(d, !is.na(dir)), aes(x, y, colour = dir)) +
  geom_path(aes(group = interaction(session, cumsum(c(TRUE, diff(as.integer(dir)) != 0)))),
            alpha = .6) +
  facet_wrap(~session) + coord_equal() +
  scale_colour_manual(values = dir_cols, name = NULL) +
  labs(title = "Direction assigned to each GPS point", x = "East (m)", y = "North (m)")
save_plot(p1, "01_track_direction.png", 10, 6)

# 5.2 Along-river position over time with direction and used points
p2 <- ggplot(subset(d, !is.na(dir)), aes(t / 60, pos, colour = dir)) +
  geom_point(size = .6, alpha = .4) +
  geom_point(data = dat, size = .9) +
  facet_wrap(~session, ncol = 1) +
  scale_colour_manual(values = dir_cols, name = NULL) +
  labs(title = "Position along the river axis (points used in fit emphasised)",
       x = "Time (min)", y = "Along-river position (m, + = downstream)")
save_plot(p2, "02_position_time.png", 9, 7)

# 5.3 Speed against effort, with fitted lines per direction
grid <- expand.grid(cadence = seq(min(dat$cadence), max(dat$cadence), length.out = 100),
                    session = levels(dat$session), d = c(1, -1))
grid$e <- grid$cadence^EXPONENT
grid$speed <- predict(fit_main, newdata = grid)
grid$panel <- factor(lab[as.character(grid$session)], levels = lab)
grid$dir <- factor(ifelse(grid$d > 0, "downstream", "upstream"), levels = levels(dat$dir))
p3 <- ggplot(dat, aes(cadence, speed, colour = dir)) +
  geom_point(alpha = .45, size = 1) +
  geom_line(data = grid, linewidth = 1) +
  facet_wrap(~panel) +
  scale_colour_manual(values = dir_cols, name = NULL) +
  labs(title = "Speed vs stroke rate with per-session stream",
       x = "Stroke rate (spm)", y = "Ground speed (m/s)")
save_plot(p3, "03_speed_vs_cadence.png", 10, 5.5)

# 5.4 Same, on the s^(1/3) axis, speed corrected to still water: the two
#     directions should collapse onto a single line.
p4 <- ggplot(dat, aes(e, still, colour = dir)) +
  geom_point(alpha = .45, size = 1) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, colour = "black", linewidth = .7) +
  facet_wrap(~panel) +
  scale_colour_manual(values = dir_cols, name = NULL) +
  labs(title = "Still-water speed (ground speed - d*u_session) vs s^(1/3)",
       x = expression(s^{1/3}), y = "Still-water speed (m/s)")
save_plot(p4, "04_still_water_collapse.png", 10, 5.5)

# 5.5 Profile of the fit over candidate stream speeds, per session
u_grid <- seq(-0.5, 0.8, by = 0.01)
prof <- do.call(rbind, lapply(lv, function(l) {
  dl <- dat[dat$session == l, ]
  do.call(rbind, lapply(u_grid, function(u) {
    f <- lm(I(speed - d * u) ~ 0 + e, data = dl)
    data.frame(session = l, u = u, rmse = sqrt(mean(resid(f)^2)))
  }))
}))
p5 <- ggplot(prof, aes(u, rmse)) + geom_line() +
  geom_vline(data = u_tab, aes(xintercept = u_ms), linetype = 2) +
  facet_wrap(~session) +
  labs(title = "Misfit as a function of assumed stream speed",
       x = "Stream speed u (m/s, + downstream)", y = "RMSE (m/s)")
save_plot(p5, "05_profile_stream.png", 10, 4.5)

# 5.6 Residuals along the river: does the stream vary with location?
p6 <- ggplot(dat, aes(pos, resid, colour = dir)) +
  geom_point(alpha = .4, size = 1) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, linewidth = .8) +
  geom_hline(yintercept = 0) +
  facet_wrap(~panel) +
  scale_colour_manual(values = dir_cols, name = NULL) +
  labs(title = "Residuals along the river", x = "Along-river position (m)",
       y = "Residual (m/s)")
save_plot(p6, "06_residuals_position.png", 10, 5.5)

# 5.7 Bootstrap distributions
bl <- data.frame(session = rep(lv, each = nrow(boot)), u = as.vector(boot))
p7 <- ggplot(bl, aes(u, fill = session)) +
  geom_histogram(bins = 30, alpha = .6, position = "identity") +
  geom_vline(data = u_tab, aes(xintercept = u_ms, colour = session), linetype = 2) +
  labs(title = "Block bootstrap of the stream estimate, per session",
       x = "u (m/s)", y = "count")
save_plot(p7, "07_bootstrap.png", 8, 4.5)

cat("\nPlots written to", normalizePath(out_dir), "\n")
