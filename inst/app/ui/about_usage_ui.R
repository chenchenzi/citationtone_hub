###############################################
# About tab → "Community & reach"
#
# Public figures collected once a day by .github/workflows/usage-stats.yaml
# (GitHub stars with their dates; Zenodo views and downloads) and committed as
# usage.json to the `usage-data` branch. The app's server reads that file, so
# visitors' browsers never contact GitHub or Zenodo, and new figures appear
# without redeploying. The cards and charts are plain HTML + SVG.
###############################################

# SHINYTONE_USAGE_URL (a URL or a local file) overrides it, e.g. for testing.
ABOUT_USAGE_URL <- Sys.getenv("SHINYTONE_USAGE_URL",
  "https://raw.githubusercontent.com/chenchenzi/citationtone_hub/usage-data/usage.json")

# One cached copy per R process: refreshed after 6 h, or after 10 min if the
# last attempt failed, so at most one session in a while waits for the fetch.
.about_usage_cache <- new.env()

about_usage_get <- function(url = ABOUT_USAGE_URL, ttl = 6 * 3600, retry = 600) {
  cache <- .about_usage_cache
  if (!is.null(cache$at)) {
    age <- as.numeric(difftime(Sys.time(), cache$at, units = "secs"))
    if (age < if (isTRUE(cache$res$ok)) ttl else retry) return(cache$res)
  }
  res <- tryCatch({
    old <- options(timeout = 5)
    on.exit(options(old), add = TRUE)
    txt <- suppressWarnings(readLines(url, warn = FALSE, encoding = "UTF-8"))
    d <- jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = TRUE)
    if (!is.list(d$github) || !is.list(d$zenodo)) stop("unexpected usage.json")
    list(ok = TRUE, data = d)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  # A failed refresh keeps showing the last good figures and retries sooner.
  if (!res$ok && isTRUE(cache$res$ok)) {
    cache$at <- Sys.time() - ttl + retry
    return(cache$res)
  }
  cache$res <- res
  cache$at  <- Sys.time()
  res
}

usage_time <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
usage_day  <- function(t) paste(as.integer(format(t, "%d")), format(t, "%b %Y"))
usage_num  <- function(n) format(n, big.mark = ",", scientific = FALSE, trim = TRUE)

# Step line(s) of running totals over a shared time window, as an SVG string.
# Each series: list(t = POSIXct of the changes, y = running total after each,
# col = colour, tip = hover text per change, fill = TRUE for a shaded area).
usage_svg <- function(series, t0, t1, ymax, w = 320, h = 112) {
  pl <- 8; pr <- 8; pt <- 10; pb <- 10
  span <- max(as.numeric(difftime(t1, t0, units = "secs")), 1)
  X <- function(t) pl + as.numeric(difftime(t, t0, units = "secs")) / span * (w - pl - pr)
  Y <- function(y) h - pb - y / max(ymax, 1) * (h - pt - pb)
  f <- function(v) sprintf("%.1f", v)
  grid <- sprintf('<line x1="%s" x2="%s" y1="%s" y2="%s" stroke="#d8dcda" stroke-width="1"/>',
                  f(pl), f(w - pr), f(c(Y(0), Y(max(ymax, 1) / 2))), f(c(Y(0), Y(max(ymax, 1) / 2))))
  body <- vapply(series, function(s) {
    n <- length(s$t)
    if (n == 0) return("")
    # Steps: flat until each change, then up; the last level runs to t1.
    xs <- c(X(s$t[1]), as.vector(rbind(X(s$t), c(X(s$t[-1]), X(t1)))))
    ys <- c(Y(if (isTRUE(s$from0)) 0 else s$y[1]), as.vector(rbind(Y(s$y), Y(s$y))))
    pts <- paste(f(xs), f(ys), sep = ",", collapse = " ")
    area <- if (isTRUE(s$fill))
      sprintf('<polygon points="%s %s,%s %s,%s" fill="%s" fill-opacity="0.12"/>',
              pts, f(X(t1)), f(Y(0)), f(xs[1]), f(Y(0)), s$col) else ""
    tips <- paste(sprintf('<circle cx="%s" cy="%s" r="7" fill="transparent"><title>%s</title></circle>',
                          f(X(s$t)), f(Y(s$y)), htmltools::htmlEscape(s$tip)), collapse = "")
    paste0(area,
           sprintf('<polyline points="%s" fill="none" stroke="%s" stroke-width="2" stroke-linejoin="round"/>',
                   pts, s$col),
           sprintf('<circle cx="%s" cy="%s" r="3.5" fill="%s" stroke="#fff" stroke-width="1.5"/>',
                   f(X(t1)), f(Y(s$y[n])), s$col),
           tips)
  }, character(1))
  sprintf('<svg viewBox="0 0 %d %d" class="usage-svg" role="img">%s%s</svg>',
          w, h, paste(grid, collapse = ""), paste(body, collapse = ""))
}

about_usage_cards <- function(d) {
  updated <- usage_time(d$updated)
  st      <- unlist(d$github$starred_at)   # [] arrives as an empty list
  star_t  <- sort(usage_time(if (length(st)) st else character(0)))
  hist    <- as.data.frame(d$zenodo$history)
  zen_t   <- as.POSIXct(paste(hist$date, "00:00:00"), tz = "UTC")
  vis     <- if (length(d$visits$daily)) as.data.frame(d$visits$daily) else NULL
  vis_t   <- if (!is.null(vis)) as.POSIXct(paste(vis$date, "00:00:00"), tz = "UTC")
  t0 <- min(c(vis_t, star_t, zen_t, updated))
  if (difftime(updated, t0, units = "days") < 1) t0 <- updated - 86400

  star_col <- "#a8821f"; view_col <- "#2f8f74"; dl_col <- "#c4673a"; vis_col <- "#f3969a"   # the theme pink (Minty secondary)
  card <- function(bg, dot, title, kind, number, unit, legend, svg)
    tags$div(class = "usage-card", style = paste0("background:", bg, ";"),
      tags$div(class = "usage-head",
        tags$span(class = "usage-dot", style = paste0("background:", dot, ";")),
        tags$span(class = "usage-title", title),
        tags$span(class = "usage-kind", kind)),
      tags$div(class = "usage-num", number, tags$span(class = "usage-unit", unit)),
      legend,
      HTML(svg))

  n_star <- d$github$stars
  stars_svg <- usage_svg(list(list(
      t = star_t, y = seq_along(star_t), col = star_col, fill = TRUE, from0 = TRUE,
      tip = sprintf("%s: star %d", usage_day(star_t), seq_along(star_t)))),
    t0, updated, max(n_star, length(star_t)))

  zen_svg <- usage_svg(list(
      list(t = zen_t, y = hist$views, col = view_col, fill = TRUE,
           tip = sprintf("%s: %s views", usage_day(zen_t), usage_num(hist$views))),
      list(t = zen_t, y = hist$downloads, col = dl_col,
           tip = sprintf("%s: %s downloads", usage_day(zen_t), usage_num(hist$downloads)))),
    t0, updated, max(hist$views, hist$downloads))
  vis_card <- if (!is.null(vis)) {
    cum <- cumsum(vis$sessions)
    card("#fdf1f1", vis_col, "App visits", "total", usage_num(d$visits$total), "visits", NULL,
         usage_svg(list(list(
             t = vis_t, y = cum, col = vis_col, fill = TRUE, from0 = TRUE,
             tip = sprintf("%s: %s visits (%s in all)", usage_day(vis_t),
                           usage_num(vis$sessions), usage_num(cum)))),
           t0, updated, max(cum)))
  }

  legend_item <- function(col, label, n)
    tags$span(class = "usage-leg", tags$span(class = "usage-dot sm", style = paste0("background:", col, ";")),
              label, " ", tags$strong(usage_num(n)))

  tagList(
    tags$style(HTML("
      .usage-grid{display:flex;flex-wrap:wrap;gap:18px;margin-top:6px;}
      .usage-card{flex:1 1 250px;border-radius:16px;padding:18px 22px 12px;text-align:left;
               display:flex;flex-direction:column;}
      .usage-head{display:flex;align-items:center;gap:8px;margin-bottom:4px;}
      .usage-title{font-weight:700;color:#2c3e3a;}
      .usage-kind{margin-left:auto;font-size:0.75rem;color:#8a8f8c;}
      .usage-dot{width:9px;height:9px;border-radius:50%;display:inline-block;}
      .usage-dot.sm{width:7px;height:7px;margin-right:4px;}
      .usage-num{font-size:2.6rem;font-weight:800;color:#2c3e3a;line-height:1.15;}
      .usage-unit{font-size:0.85rem;font-weight:400;color:#777;margin-left:6px;}
      .usage-legs{font-size:0.8rem;color:#666;display:flex;gap:14px;margin:2px 0 4px;}
      .usage-svg{width:100%;height:auto;display:block;margin-top:auto;padding-top:4px;}
      .usage-axis{display:flex;justify-content:space-between;font-size:0.75rem;color:#999;margin:8px 4px 0;}
      .usage-notes{font-size:0.8rem;color:#777;margin-top:10px;text-align:left;}
      .usage-notes summary{cursor:pointer;}
      .usage-notes ul{margin:6px 0 0;padding-left:18px;}
    ")),
    tags$p(style = "color: #777; font-size: 0.95rem; margin: 0 0 16px 0;",
           if (!is.null(vis)) "App visits, GitHub stars and the Zenodo archive, updated daily."
           else "GitHub stars and the Zenodo archive, updated daily."),
    tags$div(class = "usage-grid",
      vis_card,
      card("#fbf6e7", star_col, "GitHub stars", "current", usage_num(n_star),
           if (n_star == 1) "star" else "stars", NULL, stars_svg),
      card("#eef6f3", view_col, "Zenodo archive", "total", usage_num(d$zenodo$views), "views",
           tags$div(class = "usage-legs",
             legend_item(view_col, "views", d$zenodo$views),
             legend_item(dl_col, "downloads", d$zenodo$downloads)),
           zen_svg)),
    tags$div(class = "usage-axis",
      tags$span(usage_day(t0)),
      tags$span(paste0("updated ", usage_day(updated), ", ", format(updated, "%H:%M"), " UTC"))),
    tags$details(class = "usage-notes",
      tags$summary("About these numbers"),
      tags$ul(
        if (!is.null(vis))
          tags$li(nows("App visits: visits (sessions) to the online app counted by Google ",
                       "Analytics since ", usage_day(as.POSIXct(paste(d$visits$since, "00:00:00"), tz = "UTC")),
                       ", when it was added, up to the day before the update. Ad blockers and some ",
                       "privacy browsers stop it, so the real number is higher.")),
        tags$li(nows("GitHub stars: when each current stargazer starred the ",
                tags$a(href = paste0("https://github.com/", d$github$repo), target = "_blank",
                       rel = "noopener noreferrer", "repository"),
                "; someone who removes a star drops out of the line.")),
        tags$li(nows("Zenodo: unique views and downloads of the ",
                tags$a(href = paste0("https://doi.org/", d$zenodo$concept_doi), target = "_blank",
                       rel = "noopener noreferrer", "archived release"),
                " across all versions, as its Zenodo page shows them. Zenodo gives only ",
                "running totals, so this line starts when daily recording began (",
                usage_day(min(zen_t)), ").")),
        tags$li("Collected once a day and read by the app's server; your browser does not ",
                "contact GitHub or Zenodo. Hover over a line to see its points."))))
}

about_usage_ui <- function(input, output, session) {
  usage <- reactiveVal(NULL)   # NULL until the first fetch returns
  # After the first render, so the landing page never waits on the network.
  # Local runs with options(shinytone.check_updates = FALSE) make no request.
  session$onFlushed(function() {
    if (!isTRUE(getOption("shinytone.check_updates", TRUE))) {
      usage(list(ok = FALSE, skipped = TRUE))
      return(invisible())
    }
    later::later(function() usage(about_usage_get()), delay = 0.5)
  }, once = TRUE)

  output$about_usage <- renderUI({
    res <- usage()
    note <- function(...) tags$p(style = "color:#999; font-size:0.85rem; font-style:italic;", ...)
    if (is.null(res)) return(note("Loading figures…"))
    if (isTRUE(res$skipped))
      return(note(nows("Not fetched, as network checks are turned off (",
                       tags$code("shinytone.check_updates = FALSE"), ").")))
    if (!isTRUE(res$ok))
      return(note("The figures are not available right now. They are collected once a day ",
                  "from GitHub and Zenodo."))
    tryCatch(about_usage_cards(res$data),
             error = function(e) note("The figures could not be displayed."))
  })
}
