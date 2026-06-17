library(bslib)

# Define UI for data upload----
ui <- fluidPage(
  # Include the custom CSS file
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  # ),
  # Syntax highlighting for R code blocks
  tags$head(
    # Favicons
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "favicon-192.png"),
    tags$link(rel = "apple-touch-icon", sizes = "192x192", href = "favicon-192.png"),
    tags$link(rel = "apple-touch-icon", sizes = "512x512", href = "favicon-512.png"),
    # Google Analytics (GA4) — replace G-5RG08QWPPG with your measurement ID
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-5RG08QWPPG"),
    tags$script(HTML("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-5RG08QWPPG');
    ")),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;700&display=swap"),
    # IBM Plex Serif powers the animated wordmark in the landing-page logo.
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Serif:ital,wght@0,400;0,500;1,400&display=swap"),
    # Brand-styled mobile menu toggle. The top navbar collapses into this
    # button on narrow screens; recolor to the Shinytone teal, add a "Menu"
    # label for discoverability, and fold the bars into an X when open.
    # Markup: button.navbar-toggle > span.sr-only + three span.icon-bar.
    tags$style(HTML("
      .navbar-default .navbar-toggle {
        position: relative;
        border: 1px solid #cfe3db !important;
        border-radius: 8px !important;
        padding: 9px 56px 9px 12px !important;
        background-color: #ffffff !important;
        transition: background-color .15s ease, border-color .15s ease, box-shadow .15s ease;
      }
      .navbar-default .navbar-toggle:hover,
      .navbar-default .navbar-toggle:focus {
        background-color: #e8f5f0 !important;
        border-color: #78c2ad !important;
        box-shadow: 0 2px 7px rgba(44, 95, 79, .12);
      }
      .navbar-default .navbar-toggle:focus-visible {
        outline: 2px solid #78c2ad; outline-offset: 2px;
      }
      .navbar-default .navbar-toggle .icon-bar {
        display: block;
        width: 22px; height: 2.5px;
        border-radius: 2px;
        background-color: #2c5f4f !important;
        transition: transform .22s ease, opacity .18s ease, background-color .15s ease;
      }
      .navbar-default .navbar-toggle .icon-bar + .icon-bar { margin-top: 5px; }
      .navbar-default .navbar-toggle::after {
        content: 'Menu';
        position: absolute; right: 13px; top: 50%;
        transform: translateY(-50%);
        color: #2c5f4f; font-weight: 500; font-size: 0.82rem; line-height: 1;
      }
      /* BootStrap removes .collapsed on the toggle while the menu is open. */
      .navbar-default .navbar-toggle:not(.collapsed) .icon-bar:nth-child(2) { transform: translateY(7.5px) rotate(45deg); }
      .navbar-default .navbar-toggle:not(.collapsed) .icon-bar:nth-child(3) { opacity: 0; }
      .navbar-default .navbar-toggle:not(.collapsed) .icon-bar:nth-child(4) { transform: translateY(-7.5px) rotate(-45deg); }
    ")),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/r.min.js"),
    tags$style("
      #plot_button, #show_code_button { display: block; }
      #save_plot_button { display: block; margin-top: 4px; width: fit-content; }
      #visualise_guide strong, #normalise_guide strong, #view_guide strong, #inspect_guide strong, #model_guide strong, #gca_guide strong, #gamm_guide strong, #summarise_guide strong { font-weight: 900; color: #3a3a3a; }
      /* Foldable guide boxes (native <details>, open by default) */
      details.guide-box {
        background-color: #f0faf7; border-left: 4px solid #78c2ad;
        padding: 8px 14px; margin-bottom: 12px; border-radius: 4px;
        font-size: 0.88rem; color: #555;
      }
      details.guide-box > summary.guide-summary {
        cursor: pointer; font-weight: 900; color: #3a3a3a;
        list-style: none; outline: none; user-select: none;
        display: flex; align-items: center; gap: 6px; padding: 2px 0;
      }
      details.guide-box > summary.guide-summary::-webkit-details-marker { display: none; }
      details.guide-box > summary.guide-summary::before {
        content: '▸'; display: inline-block; color: #78c2ad;
        font-size: 0.9em; transition: transform 0.15s ease;
      }
      details.guide-box[open] > summary.guide-summary::before { transform: rotate(90deg); }
      details.guide-box > .guide-body { margin-top: 8px; }
      details.guide-box > .guide-body > *:last-child { margin-bottom: 0; }
      /* Keyboard-shortcut reference: keys on the left, description on the right */
      .kbd-ref { padding: 6px 0 2px 2px; font-size: 0.84rem; color: #555; }
      .kbd-ref .kbd-set-title {
        font-weight: 700; color: #2c5f4f; font-size: 0.82rem;
        margin: 9px 0 3px 0;
      }
      .kbd-ref .kbd-set-title:first-child { margin-top: 0; }
      .kbd-ref .kbd-row {
        display: grid; grid-template-columns: 150px 1fr;
        gap: 10px; align-items: baseline; padding: 2px 0;
      }
      .kbd-ref .kbd-keys { white-space: nowrap; }
      .kbd-ref .kbd-keys .sep { color: #b0b0b0; margin: 0 3px; }
      .kbd-ref .kbd-desc { color: #555; line-height: 1.45; }
      .plot-spinner-wrap { position: relative; }
      .plot-spinner-wrap .recalculating { opacity: 0.3 !important; }
      .plot-spinner-wrap .recalculating ~ .plot-spinner,
      .plot-spinner-wrap .recalculating + .plot-spinner { display: block !important; }
      .plot-spinner {
        display: none; position: absolute; top: 50%; left: 50%;
        transform: translate(-50%, -50%); z-index: 10;
      }
      .plot-spinner::after {
        content: ''; display: block; width: 40px; height: 40px;
        border: 4px solid #e0e0e0; border-top-color: #78c2ad;
        border-radius: 50%; animation: spin 0.8s linear infinite;
      }
      @keyframes spin { to { transform: rotate(360deg); } }
      /* Startup loading screen */
      #loading-screen {
        position: fixed; top: 0; left: 0; width: 100%; height: 100%;
        background: #fff; z-index: 9999;
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        transition: opacity 0.4s ease;
      }
      #loading-screen.fade-out { opacity: 0; pointer-events: none; }
      #loading-screen .loader {
        width: 48px; height: 48px;
        border: 5px solid #e0e0e0; border-top-color: #78c2ad;
        border-radius: 50%; animation: spin 0.8s linear infinite;
      }
      #loading-screen p {
        margin-top: 16px; font-family: 'Open Sans', sans-serif;
        color: #888; font-size: 0.95rem;
      }
      /* Feature cards on the About tab */
      .feature-section { margin-bottom: 28px; }
      .feature-section h3 {
        margin: 16px 0 4px 0; color: #2c5f4f;
      }
      .feature-section p.lead {
        color: #666; font-size: 0.95rem; margin-bottom: 12px;
      }
      .feature-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(230px, 1fr));
        gap: 12px;
      }
      .feature-card {
        background: #ffffff;
        border: 1px solid #d6e7df;
        border-left: 3px solid #78c2ad;
        border-radius: 4px;
        padding: 12px 14px;
        display: flex;
        flex-direction: column;
        transition: box-shadow 0.15s, transform 0.15s;
      }
      .feature-card:hover {
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        transform: translateY(-1px);
      }
      .feature-card-title {
        font-weight: 700; font-size: 0.97rem;
        color: #2c5f4f; margin-bottom: 4px;
      }
      .feature-card-icon { font-size: 1.15rem; margin-right: 6px; }
      .feature-card-desc {
        font-size: 0.84rem; color: #555; flex-grow: 1; line-height: 1.4;
      }
      .feature-section-cta {
        display: inline-block;
        margin-top: 14px;
        padding: 7px 16px;
        background: #e8f5f0;
        color: #2c5f4f;
        border-radius: 4px;
        font-size: 0.92rem;
        font-weight: 600;
        text-decoration: none;
        transition: background 0.15s, color 0.15s;
      }
      .feature-section-cta:hover {
        background: #78c2ad;
        color: #ffffff;
        text-decoration: none;
      }
      /* Showcase (Option B): image on the left, clickable feature list on the right */
      .showcase-row {
        display: grid;
        grid-template-columns: 1.55fr 1fr;
        gap: 24px;
        align-items: stretch;
        margin-top: 8px;
      }
      @media (max-width: 900px) {
        .showcase-row { grid-template-columns: 1fr; }
      }
      .showcase-image {
        background: #f7faf9;
        border: 1px solid #d6e7df;
        border-radius: 6px;
        padding: 10px;
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 280px;
      }
      .showcase-image img {
        width: 100%;
        max-height: 460px;
        object-fit: contain;
        border-radius: 4px;
        box-shadow: 0 1px 6px rgba(0,0,0,0.08);
      }
      .showcase-cards {
        display: flex; flex-direction: column; gap: 8px;
      }
      .showcase-card {
        background: #ffffff;
        border: 1px solid #d6e7df;
        border-left: 3px solid transparent;
        border-radius: 4px;
        padding: 11px 14px;
        cursor: pointer;
        transition: border-left-color 0.15s, background 0.15s, transform 0.1s;
      }
      .showcase-card:hover {
        background: #f0faf7;
        border-left-color: #78c2ad;
        transform: translateY(-1px);
      }
      .showcase-card.active {
        background: #e8f5f0;
        border-left-color: #2c5f4f;
      }
      .showcase-card-title {
        font-weight: 700; color: #2c5f4f;
        font-size: 0.98rem; margin-bottom: 4px;
      }
      .showcase-card-icon { font-size: 1.1rem; margin-right: 6px; }
      .showcase-card-desc {
        font-size: 0.83rem; color: #555; line-height: 1.45;
      }
      /* Inline chip showing the corresponding tab name next to a friendly section heading */
      .tab-ref-chip {
        font-family: 'SFMono-Regular', Menlo, Consolas, monospace;
        font-size: 0.7rem;
        font-weight: 500;
        color: #6b7d75;
        background: #eef5f1;
        border: 1px solid #d6e7df;
        padding: 2px 8px;
        border-radius: 10px;
        letter-spacing: 0.02em;
        white-space: nowrap;
      }
      .tab-ref-chip::before { content: '\\21AA  '; opacity: 0.6; margin-right: 2px; }
      /* FAQ subsection cards (Privacy & Data / Hosting / Usage) */
      .faq-section {
        background: #f0faf7;
        border-radius: 8px;
        padding: 18px 24px 6px 24px;
        margin: 20px 0;
      }
      .faq-section > h4 {
        margin-top: 0 !important;
        margin-bottom: 12px;
        color: #2c5f4f !important;
        font-weight: 700;
      }
      .faq-section .faq-item {
        background: #ffffff;
        border-radius: 4px;
        border-left: 3px solid #78c2ad;
        padding: 10px 14px 4px 14px;
        margin-bottom: 10px !important;
      }
      .faq-section .faq-item p { margin-bottom: 6px; }
    "),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        function watchForCode(id) {
          var target = document.getElementById(id);
          if (!target) return;
          var observer = new MutationObserver(function() {
            var codeEls = target.querySelectorAll('pre code');
            codeEls.forEach(function(el) {
              if (!el.classList.contains('hljs')) {
                el.classList.add('language-r');
                hljs.highlightElement(el);
              }
            });
          });
          observer.observe(target, { childList: true, subtree: true });
        }
        watchForCode('r_code_output');
        watchForCode('model_r_code');
        watchForCode('gca_r_code');
        watchForCode('gamm_r_code');
        watchForCode('summarise_r_code');
      });
      // Dismiss loading screen once Shiny connects
      $(document).on('shiny:connected', function() {
        var el = document.getElementById('loading-screen');
        if (el) { el.classList.add('fade-out'); setTimeout(function(){ el.remove(); }, 500); }
      });
      // Scroll smoothly to a target element by id (used by server via
      // session$sendCustomMessage('scroll_to_id', 'some_id')).
      Shiny.addCustomMessageHandler('scroll_to_id', function(id) {
        var el = document.getElementById(id);
        if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'});
      });
      // Programmatically populate the #uploadfile <input type=file> with a
      // file fetched from a URL relative to www/. We construct a File +
      // DataTransfer, assign to .files, and dispatch the change event so
      // Shiny's fileInput binding picks it up exactly like a manual upload.
      Shiny.addCustomMessageHandler('load_sample_csv', function(args) {
        fetch(args.url)
          .then(function(r) { return r.blob(); })
          .then(function(blob) {
            var file = new File([blob], args.filename, {type: 'text/csv'});
            var dt = new DataTransfer();
            dt.items.add(file);
            var input = document.getElementById('uploadfile');
            if (!input) return;
            input.files = dt.files;
            input.dispatchEvent(new Event('change', {bubbles: true}));
          })
          .catch(function(err) { console.error('Sample load failed:', err); });
      });
    "))
  ),
  # Startup loading screen
  tags$div(id = "loading-screen",
    tags$div(class = "loader"),
    tags$p("Loading app...")
  ),
  #Navbar structure for UI
  navbarPage(title = tags$img(src = "shinytone.svg", height = "60px", alt = "Shinytone",
                              style = "margin-top: -16px; margin-bottom: -16px;"),
             id = "main_nav",
             windowTitle = "Shinytone",
             # Collapse the top nav into a hamburger toggle on narrow screens
             # instead of letting the items stack onto multiple rows.
             collapsible = TRUE,
             theme = bs_theme(version = 5, bootswatch = "minty",
                              heading_font = font_google("Open Sans"),
                              base_font = font_google("Open Sans"),
                              "font-size-base" = "0.9rem"),
             # About & Privacy panel (landing page)
             tabPanel("About",
                      icon = icon("house"),
                      # --- Hero: logo + tagline + author/source ---
                      tags$div(style = "text-align: center; padding: 32px 15px 16px 15px;",
                        # Animated logo (v3: rising/falling draw-on entrance,
                        # eye pop, wordmark rise, then a bold idle swim).
                        # CSS is scoped to #hero-logo and all classes/keyframes
                        # are htl-* prefixed so nothing leaks into the rest of
                        # the UI. The navbar logo stays the static shinytone.svg.
                        tags$style(HTML("
                          #hero-logo { height: 110px; width: auto; overflow: visible;
                                       margin-bottom: 16px; display: inline-block; }
                          #hero-logo .htl-draw-teal, #hero-logo .htl-draw-coral {
                            stroke-dasharray: 100; stroke-dashoffset: 0; }
                          #hero-logo .htl-draw-teal  { animation: htl-draw .75s cubic-bezier(.25,.6,.2,1) 0s   both; }
                          #hero-logo .htl-draw-coral { animation: htl-draw .78s cubic-bezier(.25,.6,.2,1) .12s both; }
                          @keyframes htl-draw { from { stroke-dashoffset: 100; } to { stroke-dashoffset: 0; } }
                          #hero-logo .htl-mark { transform-box: fill-box; transform-origin: 18% 50%;
                            animation: htl-swim 1.85s ease-in-out 1.45s infinite; }
                          @keyframes htl-swim {
                            0%   { transform: translateX(0)   translateY(0)   rotate(0deg);   }
                            15%  { transform: translateX(4px) translateY(-2px) rotate(11deg);  }
                            40%  { transform: translateX(1px) translateY(1px)  rotate(-12deg); }
                            65%  { transform: translateX(5px) translateY(-2px) rotate(9deg);   }
                            85%  { transform: translateX(2px) translateY(0)    rotate(-6deg);  }
                            100% { transform: translateX(0)   translateY(0)    rotate(0deg);   } }
                          #hero-logo .htl-eye-pop { transform-box: fill-box; transform-origin: center;
                            animation: htl-pop .42s cubic-bezier(.2,1.5,.4,1) .95s both; }
                          @keyframes htl-pop { from { transform: scale(0); } to { transform: scale(1); } }
                          #hero-logo .htl-eye { transform-box: fill-box; transform-origin: center;
                            animation: htl-blink 5s ease-in-out 2.9s infinite; }
                          @keyframes htl-blink { 0%,92%,100% { transform: scaleY(1); } 96% { transform: scaleY(.12); } }
                          #hero-logo .htl-wordmark { animation: htl-rise .6s ease-out 1s both; }
                          @keyframes htl-rise { from { transform: translateY(8px); opacity: 0; }
                                                to   { transform: translateY(0);   opacity: 1; } }
                          #hero-logo .htl-glint { fill: #ffffff; animation: htl-sweep 5s ease-in-out 2.1s infinite; }
                          @keyframes htl-sweep {
                            0%   { transform: translateX(58px)  skewX(-18deg); opacity: 0;   }
                            6%   { opacity: .55; }
                            15%  { transform: translateX(300px) skewX(-18deg); opacity: .55; }
                            18%  { opacity: 0; }
                            100% { transform: translateX(300px) skewX(-18deg); opacity: 0;   } }
                          @media (prefers-reduced-motion: reduce) {
                            #hero-logo .htl-draw-teal, #hero-logo .htl-draw-coral, #hero-logo .htl-mark,
                            #hero-logo .htl-eye-pop, #hero-logo .htl-eye, #hero-logo .htl-wordmark {
                              animation: none !important; }
                            #hero-logo .htl-glint { display: none; } }
                        ")),
                        HTML(r"[<svg id="hero-logo" viewBox="0 0 320 90" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Shinytone">
  <title>Shinytone</title>
  <desc>Animated Shinytone logo: the two strokes draw on along their rising and falling curves, the eye appears, the wordmark rises, and the fish gently swims.</desc>
  <defs>
    <clipPath id="htlWordclip">
      <text x="82" y="56" font-family="'IBM Plex Serif', Georgia, serif" font-size="38" font-weight="500" letter-spacing="-0.5">Shiny<tspan font-style="italic" font-weight="400">tone</tspan></text>
    </clipPath>
    <mask id="htlDrawTeal" maskUnits="userSpaceOnUse" x="-45" y="-40" width="90" height="80">
      <path class="htl-draw-teal" d="M -22 12.5 Q 0 14 22 -10" fill="none" stroke="#fff" stroke-width="12" stroke-linecap="round" pathLength="100"/>
    </mask>
    <mask id="htlDrawCoral" maskUnits="userSpaceOnUse" x="-45" y="-40" width="90" height="80">
      <path class="htl-draw-coral" d="M -22 -10.5 Q 0 -14 22 12" fill="none" stroke="#fff" stroke-width="12" stroke-linecap="round" pathLength="100"/>
    </mask>
  </defs>
  <g transform="translate(42,45)">
    <g class="htl-mark">
      <path d="M -22 12 Q 0 10 22 -10 Q 0 18 -22 13 Z" fill="#5cb89a" mask="url(#htlDrawTeal)"/>
      <path d="M -22 -10 Q 0 -10 22 12 Q 0 -18 -22 -11 Z" fill="#e8927d" mask="url(#htlDrawCoral)"/>
      <g class="htl-eye-pop"><circle class="htl-eye" cx="-14" cy="0" r="2.6" fill="#2c5f4f"/></g>
    </g>
  </g>
  <text class="htl-wordmark" x="82" y="56" font-family="'IBM Plex Serif', Georgia, serif" font-size="38" font-weight="500" fill="#2c5f4f" letter-spacing="-0.5">Shiny<tspan font-style="italic" font-weight="400" fill="#c66b56">tone</tspan></text>
  <g clip-path="url(#htlWordclip)">
    <rect class="htl-glint" x="0" y="16" width="22" height="58"/>
  </g>
</svg>]"),
                        tags$p(style = "color: #555; font-size: 1.02rem; max-width: 820px; margin: 0 auto 14px auto; line-height: 1.55;",
                          "An open-source tool for citation tone research across tone languages. ",
                          "It integrates the full citation tone analysis workflow into an interactive tool. ",
                          "It is designed for phoneticians, typologists, fieldworkers, and students working on lexical tone production."),
                        tags$p(style = "color: #777; font-size: 0.88rem; margin: 0;",
                          icon("laptop-code"), " Developer: ",
                          tags$a(href = "https://chenzixu.rbind.io/", target = "_blank", "Chenzi Xu"),
                          tags$span(style = "margin: 0 10px; color: #ccc;", "·"),
                          icon("github"), " ",
                          tags$a(href = "https://github.com/chenchenzi/citationtone_hub", target = "_blank",
                                 "github.com/chenchenzi/citationtone_hub")
                        ),
                        tags$p(style = "color: #777; font-size: 0.88rem; margin: 6px 0 0 0;",
                          tags$span(style = "color: #78c2ad;", icon("seedling")), " ",
                          "Shinytone creators: ",
                          tags$a(href = "https://chenzixu.rbind.io/", target = "_blank", "Chenzi Xu"),
                          " & ",
                          tags$a(href = "https://congzhang-linguist.github.io", target = "_blank", "Cong Zhang")
                        )
                      ),

                      # --- Sticky in-page navigation + sticky top navbar ---
                      tags$style(HTML("
                        /* Make the Shinytone top navbar sticky so tabs stay
                           accessible while scrolling long pages. */
                        .navbar {
                          position: sticky !important;
                          top: 0; z-index: 1030;
                        }
                        .about-toc {
                          position: sticky;
                          top: 56px;   /* sit right under the Shinytone navbar */
                          z-index: 100;
                          background: rgba(255, 255, 255, 0.92);
                          backdrop-filter: blur(6px);
                          -webkit-backdrop-filter: blur(6px);
                          padding: 16px 16px;
                          text-align: center;
                          font-size: 0.9rem;
                          color: #6b7d75;
                          margin-bottom: 12px;
                        }
                        .about-toc a {
                          color: #2c5f4f; font-weight: 600;
                          text-decoration: none;
                          padding: 4px 10px; border-radius: 4px;
                          transition: background 0.12s, color 0.12s;
                        }
                        .about-toc a:hover {
                          background: #e8f5f0; color: #2c5f4f;
                          text-decoration: none;
                        }
                        .about-toc .sep { color: #c8d4cf; padding: 0 2px; }
                        /* Offset scroll target so headings don't slide under
                           the Shinytone navbar + sticky TOC when jumped to. */
                        #about-sec-terminology, #about-sec-features,
                        #about-sec-workflows, #about-sec-faq,
                        #about-sec-rpackage {
                          scroll-margin-top: 130px;
                        }
                        html { scroll-behavior: smooth; }

                        /* =================================================
                           R package showcase block
                           ================================================= */
                        .rpkg-card {
                          max-width: 1080px;
                          margin: 28px auto 0 auto;
                          padding: 30px 28px;
                          background:
                            radial-gradient(circle at top right,
                                            rgba(120, 194, 173, 0.10) 0%,
                                            rgba(120, 194, 173, 0)   55%),
                            #ffffff;
                          border: 1px solid #e3ece8;
                          border-radius: 14px;
                          box-shadow: 0 4px 18px rgba(44, 95, 79, 0.07);
                          display: grid;
                          grid-template-columns: 220px 1fr;
                          gap: 32px;
                          align-items: center;
                        }
                        .rpkg-card .rpkg-logo {
                          display: flex;
                          justify-content: center;
                          align-items: center;
                        }
                        .rpkg-card .rpkg-logo img {
                          width: 200px;
                          max-width: 100%;
                          height: auto;
                          filter: drop-shadow(0 6px 14px rgba(44, 95, 79, 0.18));
                          transition: transform 0.25s ease,
                                      filter    0.25s ease;
                        }
                        .rpkg-card .rpkg-logo img:hover {
                          transform: translateY(-3px) rotate(-2deg);
                          filter: drop-shadow(0 10px 22px rgba(44, 95, 79, 0.28));
                        }
                        .rpkg-card .rpkg-body h3 {
                          margin: 0 0 6px 0;
                          color: #2c5f4f;
                          font-weight: 700;
                          font-size: 1.25rem;
                        }
                        .rpkg-card .rpkg-body .rpkg-sub {
                          color: #6b7d75;
                          font-size: 0.92rem;
                          margin: 0 0 14px 0;
                        }
                        .rpkg-card .rpkg-body p {
                          color: #444;
                          font-size: 0.93rem;
                          line-height: 1.55;
                          margin: 0 0 12px 0;
                        }
                        .rpkg-install {
                          background: #14342b;
                          color: #e4f3ed;
                          padding: 12px 14px;
                          border-radius: 8px;
                          font-family: 'SFMono-Regular', Consolas,
                                       'Liberation Mono', Menlo, monospace;
                          font-size: 0.86rem;
                          line-height: 1.55;
                          overflow-x: auto;
                          margin: 6px 0 14px 0;
                          box-shadow: inset 0 1px 0 rgba(255,255,255,0.04);
                        }
                        .rpkg-install .rpkg-comment {
                          color: #8eb5a8;
                        }
                        .rpkg-install .rpkg-fn {
                          color: #ffd49a;
                        }
                        .rpkg-install .rpkg-str {
                          color: #c5e6d9;
                        }
                        .rpkg-highlight {
                          display: flex;
                          align-items: flex-start;
                          gap: 10px;
                          background: #f0f9f5;
                          border: 1px solid #cde8dc;
                          border-left: 3px solid #78c2ad;
                          border-radius: 0 8px 8px 0;
                          padding: 11px 14px;
                          margin: 0 0 14px 0;
                          font-size: 0.88rem;
                          line-height: 1.5;
                          color: #2c5f4f;
                        }
                        .rpkg-highlight .rpkg-highlight-icon {
                          color: #78c2ad;
                          font-size: 1.05rem;
                          line-height: 1.45;
                          flex: 0 0 auto;
                        }
                        .rpkg-highlight code {
                          background: #e0f0e9;
                          color: #2c5f4f;
                          padding: 1px 5px;
                          border-radius: 3px;
                          font-size: 0.85em;
                        }
                        .rpkg-badges {
                          display: flex;
                          flex-wrap: wrap;
                          gap: 8px;
                          margin-top: 6px;
                        }
                        .rpkg-badges a {
                          display: inline-flex;
                          align-items: center;
                          gap: 6px;
                          padding: 6px 12px;
                          border-radius: 999px;
                          font-size: 0.82rem;
                          font-weight: 600;
                          text-decoration: none;
                          border: 1px solid #d2e4dd;
                          background: #f7fbfa;
                          color: #2c5f4f;
                          transition: background 0.15s ease,
                                      transform  0.15s ease,
                                      border-color 0.15s ease;
                        }
                        .rpkg-badges a:hover {
                          background: #e8f5f0;
                          border-color: #78c2ad;
                          transform: translateY(-1px);
                          text-decoration: none;
                        }
                        .rpkg-badges a.primary {
                          background: #78c2ad;
                          color: #ffffff;
                          border-color: #78c2ad;
                        }
                        .rpkg-badges a.primary:hover {
                          background: #5fa993;
                          border-color: #5fa993;
                        }
                        /* Function-highlight tiles under the main card */
                        .rpkg-fn-grid {
                          max-width: 1080px;
                          margin: 18px auto 0 auto;
                          display: grid;
                          grid-template-columns: repeat(4, 1fr);
                          gap: 14px;
                        }
                        .rpkg-fn-tile {
                          background: #ffffff;
                          border: 1px solid #e3ece8;
                          border-radius: 10px;
                          padding: 14px 16px;
                          box-shadow: 0 2px 6px rgba(44, 95, 79, 0.05);
                          transition: box-shadow 0.18s ease,
                                      transform  0.18s ease;
                        }
                        .rpkg-fn-tile:hover {
                          box-shadow: 0 6px 16px rgba(44, 95, 79, 0.10);
                          transform: translateY(-2px);
                        }
                        .rpkg-fn-tile .rpkg-fn-name {
                          font-family: 'SFMono-Regular', Consolas,
                                       'Liberation Mono', Menlo, monospace;
                          font-size: 0.9rem;
                          font-weight: 700;
                          color: #2c5f4f;
                          margin: 0 0 4px 0;
                        }
                        .rpkg-fn-tile .rpkg-fn-desc {
                          font-size: 0.82rem;
                          color: #555;
                          line-height: 1.45;
                          margin: 0;
                        }
                        /* Narrow screens: stack the hero, two columns of tiles */
                        @media (max-width: 760px) {
                          .rpkg-card {
                            grid-template-columns: 1fr;
                            text-align: center;
                            padding: 24px 18px;
                          }
                          .rpkg-card .rpkg-logo img {
                            width: 160px;
                          }
                          .rpkg-badges { justify-content: center; }
                          .rpkg-fn-grid {
                            grid-template-columns: repeat(2, 1fr);
                          }
                        }

                        /* Tone & terminology: horizontal scrollable cards.
                           On wide screens all three cards fit; on narrow
                           screens the row scrolls left/right with snap. */
                        .term-row {
                          display: flex;
                          gap: 18px;
                          max-width: 1200px;
                          margin: 24px auto 0 auto;
                          padding: 4px 20px 18px 20px;
                          align-items: stretch;
                          overflow-x: auto;
                          scroll-snap-type: x mandatory;
                          -webkit-overflow-scrolling: touch;
                          scrollbar-width: thin;
                          scrollbar-color: #c8d4cf transparent;
                          /* Soft right-edge fade hints that there is more
                             content off-screen than the visible viewport. */
                          mask-image: linear-gradient(to right,
                                       black 0,
                                       black calc(100% - 40px),
                                       transparent 100%);
                          -webkit-mask-image: linear-gradient(to right,
                                              black 0,
                                              black calc(100% - 40px),
                                              transparent 100%);
                        }
                        .term-row::-webkit-scrollbar {
                          height: 8px;
                        }
                        .term-row::-webkit-scrollbar-thumb {
                          background: #c8d4cf;
                          border-radius: 4px;
                        }
                        .term-card {
                          flex: 0 0 400px;
                          scroll-snap-align: start;
                          background: #ffffff;
                          border: 1px solid #e3ece8;
                          border-radius: 10px;
                          padding: 22px 24px;
                          box-shadow: 0 2px 6px rgba(44, 95, 79, 0.06);
                          transition: box-shadow 0.18s ease, transform 0.18s ease;
                          display: flex;
                          flex-direction: column;
                        }
                        .term-card:hover {
                          box-shadow: 0 6px 18px rgba(44, 95, 79, 0.10);
                          transform: translateY(-2px);
                        }
                        /* Coloured accent bar at the top of each card */
                        .term-card::before {
                          content: '';
                          display: block;
                          height: 4px;
                          width: 42px;
                          background: #78c2ad;
                          border-radius: 2px;
                          margin: -6px 0 14px 0;
                        }
                        .term-card h4 {
                          margin: 0 0 12px 0;
                          color: #2c5f4f;
                          font-weight: 700;
                          font-size: 1.08rem;
                          display: flex;
                          align-items: center;
                          gap: 8px;
                        }
                        .term-card h4 .term-icon {
                          color: #78c2ad;
                          font-size: 0.95em;
                        }
                        .term-card p {
                          font-size: 0.92rem;
                          line-height: 1.55;
                          color: #444;
                        }
                        /* Compact quote box for narrow card columns */
                        .tone-quote {
                          border-left: 3px solid #78c2ad;
                          background: #f7fbfa;
                          padding: 10px 14px;
                          margin: 10px 0;
                          border-radius: 0 5px 5px 0;
                        }
                        .tone-quote p {
                          font-style: italic;
                          color: #2c5f4f;
                          margin: 0 0 4px 0;
                          font-size: 0.85rem;
                          line-height: 1.5;
                        }
                        .tone-quote .attrib {
                          font-style: normal;
                          color: #666;
                          font-size: 0.76rem;
                          margin: 0;
                        }
                        /* On wide screens (3 cards x ~360px + gaps ~ 1140px
                           fit fine). On narrow screens, the row scrolls
                           horizontally rather than stacking, so all three
                           remain peers and the user can swipe to compare. */
                      ")),
                      tags$div(class = "about-toc",
                        tags$a(href = "#about-sec-features",     "What you can do"),
                        tags$span(class = "sep", " · "),
                        tags$a(href = "#about-sec-workflows",    "Recommended workflows"),
                        tags$span(class = "sep", " · "),
                        tags$a(href = "#about-sec-terminology",  "Tone & terminology"),
                        tags$span(class = "sep", " · "),
                        tags$a(href = "#about-sec-rpackage",     "R package"),
                        tags$span(class = "sep", " · "),
                        tags$a(href = "#about-sec-faq",          "FAQ")
                      ),

                      # --- Section break + landing heading ---
                      tags$div(id = "about-sec-features",
                        style = "max-width: 900px; margin: 40px auto 8px auto; text-align: center; padding: 0 15px;",
                        # Short teal accent bar (visual divider)
                        tags$div(style = "width: 56px; height: 3px; background: #78c2ad; margin: 0 auto 24px auto; border-radius: 2px;"),
                        tags$h2(style = "color: #2c5f4f; margin: 0 0 8px 0; font-weight: 700;",
                                "What you can do"),
                        tags$p(style = "color: #777; font-size: 0.95rem; margin: 0;",
                          "Two complementary workspaces, from audio recordings to pitch contour analysis.")
                      ),

                      # --- What you can do (interactive showcase: image + clickable cards) ---
                      tags$div(style = "max-width: 1080px; margin: 0 auto; padding: 8px 15px 0 15px;",
                        # Inline JS handler — swaps the image and highlights the active card
                        tags$script(HTML("
                          window.shinytoneShowcase = function(section, file) {
                            var img = document.getElementById('showcase-img-' + section);
                            if (img) img.src = 'screenshots/' + file;
                            document.querySelectorAll('.showcase-card[data-section=\"' + section + '\"]').forEach(function(c){
                              c.classList.toggle('active', c.dataset.file === file);
                            });
                          };
                        ")),
                        local({
                          showcase_card <- function(section, emoji, name, file, desc, active = FALSE) {
                            cls <- if (active) "showcase-card active" else "showcase-card"
                            js  <- sprintf("shinytoneShowcase('%s', '%s'); return false;", section, file)
                            tags$div(class = cls,
                              `data-section` = section, `data-file` = file,
                              onclick = js,
                              tags$div(class = "showcase-card-title",
                                HTML(paste0("<span class='showcase-card-icon'>", emoji, "</span>", name))),
                              tags$div(class = "showcase-card-desc", desc)
                            )
                          }
                          section_cta <- function(main, label) {
                            js <- sprintf(
                              "Shiny.setInputValue('about_nav_target', '%s|Start', {priority:'event'}); return false;",
                              main)
                            tags$div(style = "text-align: right; margin-top: 12px;",
                              tags$a(class = "feature-section-cta",
                                     href = "#", onclick = js, label)
                            )
                          }
                          tagList(
                            # --- F0 Analysis section ---
                            tags$div(class = "feature-section",
                              h4(style = "color: #78c2ad; margin-top: 20px; display: flex; align-items: baseline; flex-wrap: wrap; gap: 10px;",
                                 "Pitch Contour Analysis",
                                 tags$span(class = "tab-ref-chip", "F0 Analysis tab")
                              ),
                              tags$p(class = "lead",
                                "Run the full citation-tone analysis pipeline on f0 measurements (CSV)."),
                              tags$div(class = "showcase-row",
                                tags$div(class = "showcase-image",
                                  tags$img(id = "showcase-img-analysis",
                                           src = "screenshots/visualise.png",
                                           alt = "F0 Analysis screenshot")
                                ),
                                tags$div(class = "showcase-cards",
                                  showcase_card("analysis", "\U0001F3A8", "Visualise", "visualise.png",
                                    "Plot f0 contours coloured by tone, with optional speaker faceting and normalisation (z-score or semitone). Export the plots and the underlying R code.",
                                    active = TRUE),
                                  showcase_card("analysis", "\U0001F50E", "Inspect", "inspect.png",
                                    "Flag likely f0 artefacts such as octave jumps, out-of-range outliers, and tracking errors, using by-speaker z-scores and sample-to-sample jump detection."),
                                  showcase_card("analysis", "\U0001F4CA", "Model", "model.png",
                                    "Three modelling approaches: per-token Legendre polynomials, Growth Curve Analysis (GCA) with mixed effects, and Generalised Additive Mixed Models (GAMM)."),
                                  showcase_card("analysis", "\U0001F4CB", "Summarise", "summarise.png",
                                    "Convert tone contours into Chao tone numerals (1–5 scale) for cross-language comparison. Compare reference-line and interval-based methods.")
                                )
                              ),
                              section_cta("F0 Analysis", "Get started →")
                            ),
                            # --- F0 Processing section ---
                            tags$div(class = "feature-section",
                              h4(style = "color: #78c2ad; margin-top: 20px; display: flex; align-items: baseline; flex-wrap: wrap; gap: 10px;",
                                 "Pitch Measurement",
                                 tags$span(class = "tab-ref-chip", "F0 Processing tab")
                              ),
                              tags$p(class = "lead",
                                "Go from raw .wav files to clean f0 contours, ready for the analysis pipeline above."),
                              tags$div(class = "showcase-row",
                                tags$div(class = "showcase-image",
                                  tags$img(id = "showcase-img-processing",
                                           src = "screenshots/extraction.png",
                                           alt = "F0 Processing screenshot")
                                ),
                                tags$div(class = "showcase-cards",
                                  showcase_card("processing", "\U0001F30A", "Extract", "extraction.png",
                                    "Extract f0 contours from .wav files using the wrassp R package, or import pre-computed Praat .Pitch and .PitchTier files.",
                                    active = TRUE),
                                  showcase_card("processing", "\U0001F527", "Correct", "correction.png",
                                    "Review and correct extraction artefacts by listening to the audio, inspecting the waveform, and editing individual frames (halve, double, interpolate, smooth, manual entry, etc.).")
                                )
                              ),
                              section_cta("F0 Processing", "Get started →")
                            )
                          )
                        })
                      ),

                      # --- Typical workflows (visual data-journey diagram) ---
                      tags$div(id = "about-sec-workflows",
                        style = "max-width: 1080px; margin: 56px auto 8px auto; text-align: center; padding: 0 15px;",
                        tags$div(style = "width: 56px; height: 3px; background: #78c2ad; margin: 0 auto 24px auto; border-radius: 2px;"),
                        tags$h2(style = "color: #2c5f4f; margin: 0 0 8px 0; font-weight: 700;",
                                "Recommended workflows"),
                        tags$p(style = "color: #777; font-size: 0.95rem; margin: 0;",
                          "Four major pipelines through Shinytone. The output of each pipeline chains into the input of the next.")
                      ),
                      tags$style(HTML("
                        .workflows-section { max-width: 1080px; margin: 0 auto; padding: 20px 15px 8px 15px; }
                        .workflow-row {
                          margin-bottom: 20px; padding: 14px 18px;
                          background: #ffffff;
                          border: 1px solid #e0e8e3; border-radius: 8px;
                          box-shadow: 0 1px 3px rgba(0,0,0,0.03);
                        }
                        .workflow-head {
                          display: flex; align-items: baseline; gap: 10px;
                          margin-bottom: 4px;
                        }
                        .workflow-num {
                          display: inline-flex; align-items: center; justify-content: center;
                          width: 24px; height: 24px; border-radius: 50%;
                          background: #78c2ad; color: #ffffff;
                          font-size: 0.82rem; font-weight: 700;
                          flex-shrink: 0;
                        }
                        .workflow-title {
                          font-weight: 700; color: #2c5f4f; font-size: 1.0rem;
                        }
                        .workflow-desc {
                          color: #777; font-size: 0.85rem; margin: 0 0 12px 34px;
                        }
                        .workflow-flow {
                          display: flex; align-items: center; flex-wrap: wrap;
                          gap: 6px; margin-left: 34px;
                        }
                        .wf-step {
                          display: inline-flex; align-items: center;
                          padding: 5px 12px; border-radius: 6px;
                          font-size: 0.82rem; font-weight: 500;
                          white-space: nowrap;
                        }
                        .wf-step.wf-data {
                          background: #e3f2fd; border: 1.5px solid #90caf9;
                          color: #1565c0; font-weight: 600;
                        }
                        .wf-step.wf-optional {
                          background: #f4faff; border: 1.5px dashed #90caf9;
                          color: #1565c0; font-weight: 500;
                        }
                        .workflow-chain-arrow {
                          text-align: center; color: #b6c4be;
                          font-size: 1.5rem; line-height: 1;
                          margin: -4px 0;
                        }
                        .wf-step.wf-app {
                          background: #e8f5f0; border: 1.5px solid #78c2ad;
                          color: #2a7a5a;
                        }
                        .wf-step.wf-app.wf-opt {
                          border-style: dashed;
                        }
                        .wf-step.wf-external {
                          background: #fff8e1; border: 1.5px dashed #e0a800;
                          color: #8a6d00; font-style: italic;
                        }
                        .wf-step.wf-result {
                          background: #fde6e7; border: 1.5px solid #f3969a;
                          color: #a85a5d; font-weight: 700;
                        }
                        .wf-or {
                          display: inline-flex; align-items: center;
                          color: #888; font-size: 0.75rem; font-style: italic;
                          padding: 0 2px;
                        }
                        .wf-arrow { color: #aaa; font-size: 1.2rem; margin: 0 2px; display: inline-flex; align-items: center; }
                        .wf-legend {
                          display: flex; flex-wrap: wrap; gap: 16px;
                          margin-top: 18px; font-size: 0.78rem; color: #777;
                          justify-content: center;
                        }
                        .wf-legend > div { display: inline-flex; align-items: center; gap: 6px; }
                        .wf-swatch { display: inline-block; width: 14px; height: 14px; border-radius: 3px; }
                      ")),
                      tags$div(class = "workflows-section", id = "workflows-section",
                        style = "position: relative;",

                        # SVG overlay for dynamic output->input curves (paths injected by JS).
                        # Sits ABOVE the workflow rows (z-index 5) so the curves are not
                        # clipped by the rows' white backgrounds at start/end points.
                        HTML("<svg id='workflow-connectors' xmlns='http://www.w3.org/2000/svg' style='position:absolute; top:0; left:0; width:100%; height:100%; pointer-events:none; z-index:5; overflow:visible;'></svg>"),

                        # --- Workflow 1 ---
                        tags$div(class = "workflow-row", style = "position: relative; z-index: 1;",
                          tags$div(class = "workflow-head",
                            tags$span(class = "workflow-num", "1"),
                            tags$span(class = "workflow-title", "Pitch extraction"),
                            tags$span(class = "tab-ref-chip", "F0 Processing tab")
                          ),
                          tags$div(class = "workflow-desc",
                            "The F0 Extraction tab is a hub: pick a source, attach metadata, and download a tidy CSV."),
                          tags$div(class = "workflow-flow",
                            tags$span(class = "wf-step wf-data", HTML("&#127908; .wav")),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-external", HTML("&#128202; .Pitch (Praat)")),
                            tags$span(class = "wf-or", "or"),
                            tags$span(class = "wf-step wf-optional", HTML("&#128202; pre-extracted f0 .csv (optional)")),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-app", "F0 Extraction (+ metadata)"),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-result", `data-flow-out` = "extracted-f0", HTML("&#128202; f0 + metadata .csv"))
                          )
                        ),

                        # --- Workflow 2 ---
                        tags$div(class = "workflow-row", style = "position: relative; z-index: 1;",
                          tags$div(class = "workflow-head",
                            tags$span(class = "workflow-num", "2"),
                            tags$span(class = "workflow-title", "Pitch-tracking quality check"),
                            tags$span(class = "tab-ref-chip", "F0 Analysis tab")
                          ),
                          tags$div(class = "workflow-desc",
                            "Optionally normalise, then visualise and inspect to flag pitch-tracking errors. Inspection runs on raw f0, so Normalise is not required here."),
                          tags$div(class = "workflow-flow",
                            tags$span(class = "wf-step wf-data", `data-flow-in` = "extracted-f0", HTML("&#128202; f0 .csv")),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-app wf-opt", "Normalise (optional)"),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-app", "Visualise"),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-app", "Inspect"),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-result", `data-flow-out` = "flagged-f0", HTML("&#128202; .csv with flags"))
                          )
                        ),

                        # --- Workflow 3 ---
                        tags$div(class = "workflow-row", style = "position: relative; z-index: 1;",
                          tags$div(class = "workflow-head",
                            tags$span(class = "workflow-num", "3"),
                            tags$span(class = "workflow-title", "Pitch-tracking correction"),
                            tags$span(class = "tab-ref-chip", "F0 Processing tab")
                          ),
                          tags$div(class = "workflow-desc",
                            "Bring the audio and the flagged tokens back together to fix problematic frames by ear."),
                          tags$div(class = "workflow-flow",
                            tags$span(class = "wf-step wf-data", HTML("&#127908; .wav")),
                            tags$span(class = "wf-or", "+"),
                            tags$span(class = "wf-step wf-data", `data-flow-in` = "flagged-f0", HTML("&#128202; .csv with flags")),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-app", "F0 Correction"),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-result", `data-flow-out` = "cleaned-f0", HTML("&#128202; cleaned f0 .csv"))
                          )
                        ),

                        # --- Workflow 4 ---
                        tags$div(class = "workflow-row", style = "position: relative; z-index: 1;",
                          tags$div(class = "workflow-head",
                            tags$span(class = "workflow-num", "4"),
                            tags$span(class = "workflow-title", "Pitch contour modelling and summary"),
                            tags$span(class = "tab-ref-chip", "F0 Analysis tab")
                          ),
                          tags$div(class = "workflow-desc",
                            "Fit polynomial / GCA / GAMM models and convert contours into Chao tone numerals."),
                          tags$div(class = "workflow-flow",
                            tags$span(class = "wf-step wf-data", `data-flow-in` = "cleaned-f0", HTML("&#128202; clean f0 .csv")),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-app", "Normalise"),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-app", "Model"),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-app", "Summarise"),
                            tags$span(class = "wf-arrow", HTML("&#10132;")),
                            tags$span(class = "wf-step wf-result", HTML("&#128203; coefficients / Chao numerals"))
                          )
                        ),

                        # --- Legend ---
                        tags$div(class = "wf-legend",
                          tags$div(tags$span(class = "wf-swatch", style = "background:#e3f2fd; border:1px solid #90caf9;"), "Data file"),
                          tags$div(tags$span(class = "wf-swatch", style = "background:#f4faff; border:1px dashed #90caf9;"), "Optional data"),
                          tags$div(tags$span(class = "wf-swatch", style = "background:#e8f5f0; border:1px solid #78c2ad;"), "In-app step"),
                          tags$div(tags$span(class = "wf-swatch", style = "background:#e8f5f0; border:1px dashed #78c2ad;"), "Optional step"),
                          tags$div(tags$span(class = "wf-swatch", style = "background:#fff8e1; border:1px dashed #e0a800;"), "External step (Praat)"),
                          tags$div(tags$span(class = "wf-swatch", style = "background:#fde6e7; border:1px solid #f3969a;"), "Output")
                        ),

                        # --- JS: draw curved SVG paths from each output chip to its
                        # matching input chip on the next row. Re-runs on resize so
                        # paths stay correct when chips reflow.
                        tags$script(HTML("
                          (function() {
                            // Build a single filled brush-stroke arrow as one closed path.
                            // Both edges are cubic beziers offset perpendicular to the
                            // endpoint tangents. For laterally-offset arrows the bezier
                            // can pinch in the middle, so we use a generous ctrlScale and
                            // a wide body width to keep the rendered shape uniform.
                            function buildArrowPath(x1, y1, x2, y2) {
                              var dy_total = y2 - y1;
                              var dx_total = Math.abs(x2 - x1);
                              var bend = Math.max(24, dy_total / 2);
                              var c1x = x1, c1y = y1 + bend;
                              var c2x = x2, c2y = y2 - bend;

                              // Tangent at start (toward c1)
                              var sx = c1x - x1, sy = c1y - y1;
                              var slen = Math.sqrt(sx * sx + sy * sy) || 1;
                              sx /= slen; sy /= slen;
                              var spx = -sy, spy = sx;

                              // Tangent at end (from c2 toward end)
                              var tx = x2 - c2x, ty = y2 - c2y;
                              var tlen = Math.sqrt(tx * tx + ty * ty) || 1;
                              tx /= tlen; ty /= tlen;
                              var px = -ty, py = tx;

                              var bodyW   = 11;   // body half-width (slim)
                              var tailW   = bodyW;
                              var neckW   = bodyW;
                              var headW   = 22;   // arrowhead flare (2x body — clearly an arrow)
                              var headLen = 18;

                              // Adaptive ctrlScale: laterally-heavier curves need bigger
                              // control-point offsets to keep the middle from pinching.
                              var ratio = dx_total / Math.max(40, dy_total);
                              var ctrlScale = 1.7 + 0.9 * Math.min(1.4, ratio);

                              // Neck point (where the head starts)
                              var nx = x2 - tx * headLen;
                              var ny = y2 - ty * headLen;

                              // Outer / inner neck edges
                              var noX = nx + px * neckW, noY = ny + py * neckW;
                              var niX = nx - px * neckW, niY = ny - py * neckW;

                              // Arrowhead flare points
                              var ahLX = nx + px * headW, ahLY = ny + py * headW;
                              var ahRX = nx - px * headW, ahRY = ny - py * headW;

                              // Tail edges
                              var taoX = x1 + spx * tailW, taoY = y1 + spy * tailW;
                              var taiX = x1 - spx * tailW, taiY = y1 - spy * tailW;

                              // Bezier control points for outer / inner edges
                              var oc1X = c1x + spx * tailW * ctrlScale, oc1Y = c1y + spy * tailW * ctrlScale;
                              var oc2X = c2x + px  * neckW * ctrlScale, oc2Y = c2y + py  * neckW * ctrlScale;
                              var ic1X = c1x - spx * tailW * ctrlScale, ic1Y = c1y - spy * tailW * ctrlScale;
                              var ic2X = c2x - px  * neckW * ctrlScale, ic2Y = c2y - py  * neckW * ctrlScale;

                              return 'M ' + taoX + ' ' + taoY +
                                     ' C ' + oc1X + ' ' + oc1Y + ', ' +
                                              oc2X + ' ' + oc2Y + ', ' +
                                              noX + ' ' + noY +
                                     ' L ' + ahLX + ' ' + ahLY +
                                     ' L ' + x2 + ' ' + y2 +
                                     ' L ' + ahRX + ' ' + ahRY +
                                     ' L ' + niX + ' ' + niY +
                                     ' C ' + ic2X + ' ' + ic2Y + ', ' +
                                              ic1X + ' ' + ic1Y + ', ' +
                                              taiX + ' ' + taiY +
                                     ' Z';
                            }

                            function drawWorkflowConnectors() {
                              var section = document.getElementById('workflows-section');
                              var svg     = document.getElementById('workflow-connectors');
                              if (!section || !svg) return;
                              // Clear previously drawn arrows.
                              Array.prototype.slice.call(svg.children).forEach(function(node){
                                if (!node.tagName) return;
                                var t = node.tagName.toLowerCase();
                                if (t === 'g' || t === 'path') node.remove();
                              });

                              var sRect = section.getBoundingClientRect();
                              svg.setAttribute('width',  section.offsetWidth);
                              svg.setAttribute('height', section.offsetHeight);

                              var outs = section.querySelectorAll('[data-flow-out]');
                              outs.forEach(function(out) {
                                var id  = out.getAttribute('data-flow-out');
                                var ins = section.querySelectorAll('[data-flow-in=\"' + id + '\"]');
                                ins.forEach(function(inEl) {
                                  var oR = out.getBoundingClientRect();
                                  var iR = inEl.getBoundingClientRect();

                                  var x1 = oR.left - sRect.left + oR.width / 2;
                                  var y1 = oR.bottom - sRect.top;
                                  var x2 = iR.left - sRect.left + iR.width / 2;
                                  var y2 = iR.top - sRect.top - 2;

                                  var d = buildArrowPath(x1, y1, x2, y2);
                                  var path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
                                  path.setAttribute('d', d);
                                  path.setAttribute('fill', '#f3969a');
                                  path.setAttribute('fill-opacity', '0.18');
                                  path.setAttribute('stroke', 'none');
                                  svg.appendChild(path);
                                });
                              });
                            }

                            // Draw after Shiny is ready (the About tab is visible by default)
                            $(document).on('shiny:connected', function() {
                              setTimeout(drawWorkflowConnectors, 100);
                            });
                            window.addEventListener('resize', function() {
                              clearTimeout(window.__wfRedraw);
                              window.__wfRedraw = setTimeout(drawWorkflowConnectors, 80);
                            });
                            // Also redraw when navigating back to the About tab
                            $(document).on('shown.bs.tab', function(){ setTimeout(drawWorkflowConnectors, 80); });
                          })();
                        "))
                      ),

                      # ============ Tone & terminology ============
                      tags$div(id = "about-sec-terminology",
                        style = "max-width: 900px; margin: 56px auto 0 auto; text-align: center; padding: 0 15px;",
                        tags$div(style = "width: 56px; height: 3px; background: #78c2ad; margin: 0 auto 24px auto; border-radius: 2px;"),
                        tags$h2(style = "color: #2c5f4f; margin: 0 0 8px 0; font-weight: 700;",
                                "Tone & terminology"),
                        tags$p(style = "color: #777; font-size: 0.95rem; margin: 0;",
                          "A brief orientation to the linguistic notions Shinytone is built around.")
                      ),
                      tags$div(class = "term-row",

                        # --- What is tone? ---
                        tags$div(class = "term-card",
                          h4(tags$span(class = "term-icon", icon("book-open")),
                             "What is tone?"),
                          tags$p("A ", tags$em("tone language"),
                            " uses pitch contrastively at the lexical level. ",
                            "The same string of consonants and vowels can map ",
                            "to different words depending on the pitch pattern ",
                            "with which it is produced. Definitions span ",
                            "seventy years of typology and theory:"
                          ),
                          tags$div(class = "tone-quote",
                            tags$p("A tone language may be defined provisionally as a language having lexically significant, contrastive, but relative pitch on each syllable."),
                            tags$p(class = "attrib", "Pike (1948), ", tags$em("Tone Languages"), ", p. 3.")
                          ),
                          tags$div(class = "tone-quote",
                            tags$p("A language with tone is one in which an indication of pitch enters into the lexical realisation of at least some morphemes."),
                            tags$p(class = "attrib", "Hyman (2006), ", tags$em("Word-prosodic typology"), ", ", tags$em("Phonology"), ", 23(2), p. 229.")
                          ),
                          tags$div(class = "tone-quote",
                            tags$p("A language is a tone language if the pitch of a word can change the meaning of the word."),
                            tags$p(class = "attrib", "Yip (2002), ", tags$em("Tone"), ", p. 1.")
                          )
                        ),

                        # --- Citation tones ---
                        tags$div(class = "term-card",
                          h4(tags$span(class = "term-icon", icon("microphone")),
                             "Citation tones"),
                          tags$p(
                            "A ", tags$em("citation tone"),
                            " is the pitch shape a word takes when it is ",
                            "pronounced in isolation, away from the influence ",
                            "of neighbouring words, sentence intonation, or ",
                            "discourse context. Citation forms hold ",
                            "co-articulatory and prosodic variation as constant ",
                            "as possible, making them the cleanest starting ",
                            "point for studying a language's tonal contrasts. ",
                            "Most reference grammars, fieldwork elicitation ",
                            "protocols, and tone-inventory descriptions begin ",
                            "from citation data."
                          ),
                          tags$p(
                            "Shinytone is designed for this use case: ",
                            "controlled recordings of single words or short ",
                            "syllables, one tone category per token, multiple ",
                            "speakers and repetitions. The models and Chao ",
                            "numeral conversion routines all assume a ",
                            "citation-style design."
                          )
                        ),

                        # --- Pitch vs f0 ---
                        tags$div(class = "term-card",
                          h4(tags$span(class = "term-icon", icon("wave-square")),
                             "Pitch vs. f0"),
                          tags$p(
                            "Strictly speaking, Shinytone analyses ",
                            tags$strong("f0"),
                            " (fundamental frequency): the ", tags$em("physical"),
                            " property of a voiced sound, equal to the lowest ",
                            "frequency component of its waveform and measured ",
                            "in Hertz."
                          ),
                          tags$p(
                            tags$strong("Pitch"),
                            " is the ", tags$em("perceptual"),
                            " correlate: how high or low a listener hears the ",
                            "sound. The two are closely related but not ",
                            "identical, since pitch perception also depends on ",
                            "intensity, duration, timbre, and listener-specific ",
                            "factors."
                          ),
                          tags$p(
                            "Throughout the app and documentation we sometimes ",
                            "use ", tags$em("pitch"),
                            " informally because it is the more intuitive ",
                            "term, especially when describing tone shapes ",
                            "(", tags$em("rising pitch"), ", ",
                            tags$em("falling pitch"),
                            "). The underlying measurements, plots, models, ",
                            "and outputs are always f0 in Hz, or its ",
                            "by-speaker normalisation in semitones or ",
                            "z-scores. See ",
                            tags$a(href = "https://doi.org/10.1121/10.0032356",
                                   target = "_blank", rel = "noopener noreferrer",
                                   "Xu & Zhang (2024)"),
                            " for a discussion of methodological conventions ",
                            "in citation-tone research."
                          )
                        )
                      ),

                      # ============ R package ============
                      tags$div(id = "about-sec-rpackage",
                        style = "max-width: 900px; margin: 56px auto 0 auto; text-align: center; padding: 0 15px;",
                        tags$div(style = "width: 56px; height: 3px; background: #78c2ad; margin: 0 auto 24px auto; border-radius: 2px;"),
                        tags$h2(style = "color: #2c5f4f; margin: 0 0 8px 0; font-weight: 700;",
                                "Shinytone as an R package"),
                        tags$p(style = "color: #777; font-size: 0.95rem; margin: 0;",
                          "Everything this Shiny app does is also available as a scriptable, reproducible R package.")
                      ),

                      # --- Hero card: hex sticker + intro + install + badges ---
                      tags$div(class = "rpkg-card",

                        # Left column: hex sticker
                        tags$div(class = "rpkg-logo",
                          tags$a(
                            href   = "https://chenchenzi.github.io/citationtone_hub/",
                            target = "_blank", rel = "noopener noreferrer",
                            title  = "Open the shinytone pkgdown site",
                            tags$img(src = "logo.png", alt = "shinytone R package hex sticker")
                          )
                        ),

                        # Right column: intro, install, badges
                        tags$div(class = "rpkg-body",
                          tags$h3("shinytone ", tags$span(
                            style = "font-weight: 500; color: #78c2ad; font-size: 0.92rem; vertical-align: middle; margin-left: 6px;",
                            "v0.1.1"
                          )),
                          tags$p(class = "rpkg-sub",
                            "An R package for citation tone research — ",
                            "scriptable, reproducible, and CRAN-friendly."
                          ),
                          tags$p(
                            "If you prefer scripting to clicking, want to bake ",
                            "Shinytone's analyses into a reproducible R Markdown ",
                            "or Quarto report, or you'd like to run the whole ",
                            "app locally on private recordings, install it as ",
                            "an R package:"
                          ),

                          # Install block with syntax-style colouring
                          tags$div(class = "rpkg-install",
                            tags$span(class = "rpkg-comment", "# install from GitHub"), tags$br(),
                            "remotes::", tags$span(class = "rpkg-fn", "install_github"),
                            "(", tags$span(class = "rpkg-str", '"chenchenzi/citationtone_hub"'), ")",
                            tags$br(), tags$br(),
                            tags$span(class = "rpkg-comment", "# launch the Shiny app locally"), tags$br(),
                            "shinytone::", tags$span(class = "rpkg-fn", "run_app"), "()"
                          ),

                          # Highlight: same interface locally, no hosting limits
                          tags$div(class = "rpkg-highlight",
                            tags$span(class = "rpkg-highlight-icon", icon("desktop")),
                            tags$span(
                              tags$strong("Same app, no limits. "),
                              tags$code("run_app()"),
                              " serves the exact interface you're using right now, but on ",
                              "your own machine — with no upload-size cap, memory ceiling, or ",
                              "session timeout. Ideal for ",
                              tags$strong("large-scale datasets"),
                              " and recordings that never leave your computer."
                            )
                          ),

                          # Badge-style action links
                          tags$div(class = "rpkg-badges",
                            tags$a(
                              class  = "primary",
                              href   = "https://chenchenzi.github.io/citationtone_hub/",
                              target = "_blank", rel = "noopener noreferrer",
                              icon("book"), "Documentation"
                            ),
                            tags$a(
                              href   = "https://github.com/chenchenzi/citationtone_hub",
                              target = "_blank", rel = "noopener noreferrer",
                              icon("github"), "Source on GitHub"
                            ),
                            tags$a(
                              href   = "https://github.com/chenchenzi/citationtone_hub/blob/main/NEWS.md",
                              target = "_blank", rel = "noopener noreferrer",
                              icon("rss"), "Changelog"
                            ),
                            tags$a(
                              href   = "https://github.com/chenchenzi/citationtone_hub/issues",
                              target = "_blank", rel = "noopener noreferrer",
                              icon("bug"), "Issues & feedback"
                            )
                          )
                        )
                      ),

                      # --- Function-highlight tiles ---
                      tags$div(class = "rpkg-fn-grid",
                        tags$div(class = "rpkg-fn-tile",
                          tags$p(class = "rpkg-fn-name", "normalise_f0()"),
                          tags$p(class = "rpkg-fn-desc",
                            "Per-speaker f0 normalisation in semitones, ",
                            "z-scores, or ERB — the same conversions ",
                            "the app's Normalise tab uses.")
                        ),
                        tags$div(class = "rpkg-fn-tile",
                          tags$p(class = "rpkg-fn-name", "inspect_f0()"),
                          tags$p(class = "rpkg-fn-desc",
                            "Two-layer artefact inspection: per-token ",
                            "z-scores plus sample-level jump detection ",
                            "with median-aware flag placement.")
                        ),
                        tags$div(class = "rpkg-fn-tile",
                          tags$p(class = "rpkg-fn-name", "fit_gca() / fit_gamm()"),
                          tags$p(class = "rpkg-fn-desc",
                            "Growth-curve and generalised additive mixed ",
                            "models for tonal contours, with sensible ",
                            "defaults for citation-tone designs.")
                        ),
                        tags$div(class = "rpkg-fn-tile",
                          tags$p(class = "rpkg-fn-name", "contour_to_chao()"),
                          tags$p(class = "rpkg-fn-desc",
                            "Convert a normalised f0 contour into the ",
                            "Chao (1930) 1–5 tone-letter notation ",
                            "linguists already read fluently.")
                        )
                      ),
                      tags$p(
                        style = "max-width: 900px; margin: 14px auto 0 auto; padding: 0 15px; text-align: center; color: #6b7d75; font-size: 0.85rem;",
                        "Run ", tags$code("?shinytone"),
                        " or visit the ",
                        tags$a(
                          href   = "https://chenchenzi.github.io/citationtone_hub/reference/",
                          target = "_blank", rel = "noopener noreferrer",
                          "function reference"
                        ),
                        " for the full API. Licensed under GPL (≥ 3)."
                      ),

                      # --- FAQ ---
                      tags$div(id = "about-sec-faq",
                        style = "max-width: 900px; margin: 56px auto 0 auto; text-align: center; padding: 0 15px;",
                        tags$div(style = "width: 56px; height: 3px; background: #78c2ad; margin: 0 auto 24px auto; border-radius: 2px;"),
                        tags$h2(style = "color: #2c5f4f; margin: 0 0 8px 0; font-weight: 700;",
                                "Frequently Asked Questions")
                      ),
                      tags$div(style = "max-width: 900px; margin: 0 auto; padding: 20px 15px;",

                        # --- Privacy & Data ---
                        tags$div(class = "faq-section",
                          h4("Privacy & Data"),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Is my data stored or shared?")),
                            tags$p("No. When you upload a CSV or audio file, it is loaded into temporary server memory for in-session analysis only. It is never written to any persistent database, cloud storage, or file system, and is never transmitted to the developer or any third party.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("What happens when I close the app?")),
                            tags$p("All uploaded data and results are irrecoverably discarded when your session ends, the browser tab is closed, or the app reaches its idle timeout (typically 5 minutes of inactivity). No copy is retained. Download any outputs (CSV, plots) before closing.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Can anyone access my data during my session?")),
                            tags$p("No. No individual, including the developer, can access, retrieve, or view data uploaded during your session. The source code is publicly available for independent verification.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("What about audio recordings? Aren't those personal data?")),
                            tags$p("Yes. Voice recordings count as personal data under GDPR (which classifies them as biometric data), CCPA, and most institutional ethics frameworks, especially when paired with speaker IDs or demographic metadata. Shinytone processes audio in temporary server memory only and never writes it to persistent storage, but the data still passes through Posit and AWS infrastructure during your session."),
                            tags$p("Before uploading audio:"),
                            tags$ul(style = "margin-top: -4px;",
                              tags$li("We recommend having your participant consent or ethics approval cover web-based processing on third-party cloud services."),
                              tags$li("For sensitive recordings (children, clinical participants, endangered-language speakers, or any identifiable individuals), running Shinytone locally is strongly recommended. Install the package and run ", tags$code("shinytone::run_app()"), " (no need to clone the repo); your audio never leaves your machine."),
                              tags$li("If in doubt, consult your IRB or ethics committee.")
                            )
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Should I upload sensitive or personal data?")),
                            tags$p("Users should exercise appropriate caution and comply with their organisation's data-governance and privacy requirements. Although the app is designed so that uploaded session data is not accessible to the developer and connections are encrypted, the service still runs on third-party cloud infrastructure. We therefore recommend that you do not upload highly sensitive, confidential, or regulated information unless necessary. The developer accepts no liability for data uploaded in violation of applicable privacy regulations or ethics approvals.")
                          )
                        ),

                        # --- Hosting & Platform ---
                        tags$div(class = "faq-section",
                          h4("Hosting & Platform"),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Where is the app hosted?")),
                            tags$p("The app is hosted on ", tags$a(href = "https://www.shinyapps.io/", target = "_blank", "shinyapps.io"), ", operated by Posit, PBC, using Amazon Web Services (AWS) infrastructure in the us-east-1 region. All connections use HTTPS/TLS encryption. Note: Posit plans to migrate shinyapps.io users to ",
                              tags$a(href = "https://connect.posit.cloud/", target = "_blank", "Posit Connect Cloud"),
                              " by the end of 2026.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Does the platform collect any metadata?")),
                            tags$p("Posit may independently collect server-level metadata such as IP address, browser/device information, and request timestamps. See ", tags$a(href = "https://posit.co/about/privacy-policy/", target = "_blank", "Posit's privacy policy"), " for details.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Does this app use cookies?")),
                            tags$p("The app itself sets no cookies. Shiny/shinyapps.io sets a session-scoped cookie to route server responses to the correct client. It expires when the browser session ends.")
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Does this app use analytics?")),
                            tags$p("This app uses Google Analytics to collect anonymous usage statistics (e.g., page views, session duration). No personally identifiable information or uploaded data is sent to Google Analytics.")
                          )
                        ),

                        # --- Usage ---
                        tags$div(class = "faq-section",
                          h4("Usage"),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Is this app open source?")),
                            tags$p("Yes. The source code is available at ",
                              tags$a(href = "https://github.com/chenchenzi/citationtone_hub",
                                     target = "_blank", "github.com/chenchenzi/citationtone_hub"),
                              ". The package and app are licensed under ",
                              tags$a(href = "https://www.gnu.org/licenses/gpl-3.0.html",
                                     target = "_blank", "GPL (>= 3)"),
                              ". You can use, modify, and redistribute it freely; ",
                              "derivative works that you publish must also be GPL-licensed."
                            )
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Is there an R package?")),
                            tags$p("Yes. ", tags$code("shinytone"),
                              " is an R package that bundles both the analytical ",
                              "functions (",
                              tags$code("normalise_f0()"), ", ",
                              tags$code("inspect_f0()"), ", ",
                              tags$code("fit_gca()"), ", ",
                              tags$code("contour_to_chao()"), ", …) and the Shiny app. ",
                              "Install from GitHub with:"
                            ),
                            tags$pre(style = "background: #f6f8fa; padding: 8px 12px; border-radius: 4px; font-size: 0.85rem; margin-top: 4px;",
                              "remotes::install_github(\"chenchenzi/citationtone_hub\")"
                            ),
                            tags$p("Then launch the same UI you see online with ",
                              tags$code("shinytone::run_app()"),
                              ", or call any function programmatically from RMarkdown / a script. ",
                              "Full reference at ",
                              tags$a(href = "https://chenchenzi.github.io/citationtone_hub/",
                                     target = "_blank", "chenchenzi.github.io/citationtone_hub"),
                              "."
                            )
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("Can I run Shinytone locally?")),
                            tags$p("Yes, and it is strongly recommended for large datasets or ",
                              "sensitive recordings. The simplest way is to install the package ",
                              "(see above) and run ", tags$code("shinytone::run_app()"),
                              ". You do not need to clone the repository, since ",
                              tags$code("install_github()"), " downloads and installs it for you. ",
                              "Local runs have no file-size limits, no idle timeout, and your ",
                              "data never leaves your machine."
                            )
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("How do I cite Shinytone?")),
                            tags$p("Please cite both the software and the methodology paper:"),
                            tags$blockquote(style = "border-left: 3px solid #78c2ad; padding-left: 12px; color: #555; font-size: 0.88rem; margin-bottom: 6px;",
                              "Xu, C., & Zhang, C. (2024). A cross-linguistic review of ",
                              "citation tone production studies: Methodology and ",
                              "recommendations. ",
                              tags$em("The Journal of the Acoustical Society of America"),
                              ", 156(4), 2538–2565. ",
                              tags$a(href = "https://doi.org/10.1121/10.0032356",
                                     target = "_blank", "doi.org/10.1121/10.0032356")
                            ),
                            tags$blockquote(style = "border-left: 3px solid #78c2ad; padding-left: 12px; color: #555; font-size: 0.88rem;",
                              "Xu, C., & Zhang, C. (2026). ",
                              tags$em("shinytone: A citation tone research hub"),
                              ". R package version 0.1.1. ",
                              tags$a(href = "https://chenchenzi.github.io/citationtone_hub/",
                                     target = "_blank", "chenchenzi.github.io/citationtone_hub")
                            ),
                            tags$p(style = "font-size: 0.82rem; color: #777; margin-top: 6px;",
                              "After installing the package locally, ",
                              tags$code("citation(\"shinytone\")"),
                              " prints a formatted BibTeX entry."
                            )
                          ),
                          tags$div(class = "faq-item",
                            tags$p(tags$strong("I found a bug or have a feature request.")),
                            tags$p("Please open an issue on the ", tags$a(href = "https://github.com/chenchenzi/citationtone_hub/issues", target = "_blank", "GitHub repository"), ".")
                          )
                        ),

                        # --- Footer ---
                        tags$hr(),
                        tags$p(style = "color: #999; font-size: 0.8rem;", "Last updated: March 2026")
                      )
             ),
             # F0 modelling panel
             tabPanel("F0 Analysis",
                      #fluid = TRUE, 
                      icon = icon("chart-bar"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("F0 Time-series"),
                          # Conditional UI based on tabs
                          conditionalPanel("input.tabs_data == 'Start'", 
                                           uiOutput("ui_fileUpload"),
                                           # Horizontal line ----
                                           tags$hr(),
                                           uiOutput("ui_dataset_name"),
                                           uiOutput("ui_datasets")),
                          conditionalPanel("input.tabs_data == 'View'", 
                                           uiOutput("ui_View")),
                          conditionalPanel("input.tabs_data == 'Normalise'", 
                                           uiOutput("ui_normalise")),
                          conditionalPanel("input.tabs_data == 'Visualise'",
                                           uiOutput("ui_visualise")),
                          conditionalPanel("input.tabs_data == 'Inspect'",
                                           uiOutput("ui_inspect")),
                          conditionalPanel("input.tabs_data == 'Curate'",
                                           uiOutput("ui_curate")),
                          conditionalPanel("input.tabs_data == 'Model: Polynomials'",
                                           uiOutput("ui_model")),
                          conditionalPanel("input.tabs_data == 'Model: GCA'",
                                           uiOutput("ui_gca")),
                          conditionalPanel("input.tabs_data == 'Model: GAMM'",
                                           uiOutput("ui_gamm")),
                          conditionalPanel("input.tabs_data == 'Summarise'",
                                           uiOutput("ui_summarise"))
                        ),
                        mainPanel(
                          tabsetPanel(id = "tabs_data",
                                      tabPanel("Start",
                                               # Static welcome content (shown immediately, replaced once renderUI loads)
                                               tags$div(id = "static-welcome",
                                                 h2("Welcome!"),
                                                 tags$p("Upload a CSV file to get started. Your file should have column headers and ideally contain columns for time, f0, tone category, speaker ID, and token ID."),
                                                 tags$p("Once uploaded, a preview of the first 10 rows will appear below.")
                                               ),
                                               uiOutput("instruction_text"),
                                               tags$div(id = "f0a-data-preview-anchor",
                                                 h4(textOutput("preview_title")),
                                                 uiOutput("man_example")
                                               ),
                                               tags$script(HTML("
                                                 $(document).on('shiny:value', function(e) {
                                                   if (e.name === 'instruction_text') {
                                                     $('#static-welcome').remove();
                                                   }
                                                 });
                                               "))),
                                      tabPanel("View",
                                               uiOutput("view_guide"),
                                               DT::dataTableOutput("dataviewer")),
                                      tabPanel("Normalise",
                                               uiOutput("normalise_guide"),
                                               DT::dataTableOutput("normalised_data")),
                                      tabPanel("Visualise",
                                               uiOutput("visualise_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 plotOutput("ggplot_output",height = "auto", width = "auto"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("r_code_output")),
                                      tabPanel("Inspect",
                                               uiOutput("inspect_guide"),
                                               uiOutput("inspect_summary"),
                                               DT::dataTableOutput("inspect_data")),
                                      tabPanel("Curate",
                                               uiOutput("curate_guide"),
                                               uiOutput("curate_discovery"),
                                               uiOutput("curate_log_block")),
                                      tabPanel("Model: Polynomials",
                                               uiOutput("model_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("model_summary"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("model_plot_block"),
                                               DT::dataTableOutput("model_data"),
                                               uiOutput("model_r_code")),
                                      tabPanel("Model: GCA",
                                               uiOutput("gca_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("gca_summary"),
                                                 plotOutput("gca_plot", height = "500px", width = "800px"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("gca_r_code")),
                                      tabPanel("Model: GAMM",
                                               uiOutput("gamm_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("gamm_summary"),
                                                 plotOutput("gamm_plot", height = "500px", width = "800px"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("gamm_r_code")),
                                      tabPanel("Summarise",
                                               uiOutput("summarise_guide"),
                                               tags$div(class = "plot-spinner-wrap",
                                                 uiOutput("summarise_summary"),
                                                 plotOutput("summarise_plot", height = "auto", width = "auto"),
                                                 tags$div(class = "plot-spinner")
                                               ),
                                               uiOutput("summarise_r_code"))
                          )
                        )
                      )
             ),
             # Data collection panel
             # F0 Processing panel ------------------------------------------------
             tabPanel("F0 Processing",
                      icon = icon("wave-square"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Audio files"),
                          conditionalPanel("input.tabs_fp == 'Start'",
                                           uiOutput("ui_fp_upload")),
                          conditionalPanel("input.tabs_fp == 'Measure f0 with Praat'",
                                           uiOutput("ui_fp_praat_sidebar")),
                          conditionalPanel("input.tabs_fp == 'F0 Extraction'",
                                           uiOutput("ui_fp_extraction")),
                          conditionalPanel("input.tabs_fp == 'F0 Correction'",
                                           uiOutput("ui_fp_correction"))
                        ),
                        mainPanel(
                          tabsetPanel(id = "tabs_fp",
                                      tabPanel("Start",
                                               uiOutput("fp_start_guide"),
                                               uiOutput("fp_files_summary"),
                                               DT::dataTableOutput("fp_files_table")),
                                      tabPanel("Measure f0 with Praat",
                                               uiOutput("fp_praat_script_content")),
                                      tabPanel("F0 Extraction",
                                               uiOutput("fp_extraction_guide"),
                                               uiOutput("fp_extraction_results")),
                                      tabPanel("F0 Correction",
                                               uiOutput("fp_correction_guide"))
                          )
                        )
                      )
             ),
             tabPanel("Data Collection",
                      icon = icon("clipboard-check"),
                      div(style = "padding: 4px 32px 0 32px;",
                        tabsetPanel(id = "tabs_collection",
                          tabPanel("Filename",
                                   uiOutput("filename_guide_content")),
                          tabPanel("Checklist",
                                   uiOutput("checklist_content")),
                          tabPanel("Reading waveform",
                                   uiOutput("waveform_guide_content"))
                        )
                      )
             ),
  )
)
