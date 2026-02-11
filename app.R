# app.R
library(shiny)
library(leaflet)
library(readr)
library(DT)
library(jsonlite)

`%||%` <- function(a, b) if (!is.null(a)) a else b
TZ_LOCAL <- "America/Los_Angeles"  # Pacific

ui <- fluidPage(
  tags$h2("Movebank Data Viewer"),
  
  tags$style(HTML("
    .map-wrap { width: 100%; }
    .map-tall { height: 760px; }
    .map-short { height: 520px; }

    table.dataTable tbody th, table.dataTable tbody td { font-size: 12px; }
    table.dataTable thead th { font-size: 12px; }

    .tbl-wrap { height: 340px; overflow: auto; }

    div.dataTables_wrapper { width: 100%; }
    div.dataTables_scroll { width: 100%; }
    div.dataTables_scrollBody {
      overflow-x: auto !important;
      overflow-y: auto !important;
    }
    div.dataTables_scrollHeadInner,
    div.dataTables_scrollHeadInner table,
    div.dataTables_scrollBody table.dataTable {
      width: max-content !important;
    }
    table.dataTable { white-space: nowrap; }

    .color-row { display:flex; align-items:center; gap:10px; }
    .color-row label { margin:0; font-weight:600; }
    .color-row input[type='color'] { width:46px; height:30px; padding:0; border:none; background:transparent; }
  ")),
  
  fluidRow(
    column(
      4,
      wellPanel(
        tabsetPanel(
          id = "left_tabs",
          
          tabPanel(
            "Data",
            textInput("mb_user", "Movebank username", value = "r6snrf"),
            passwordInput("mb_pass", "Movebank password", value = ""),
            numericInput("study_id", "Study ID", value = 8038997102, min = 1, step = 1),
            textInput("sensor_type_id", "Sensor type id (optional)", value = ""),
            checkboxInput("accept_license", "accept_license=true", value = FALSE),
            
            tags$hr(),
            
            sliderInput(
              "max_points",
              "Max points to display (after filtering)",
              min = 200, max = 20000, value = 5000, step = 200
            ),
            tags$small(style="display:block; margin-top:-6px; color:#666;",
                       "If filtered data exceeds this, we keep the most recent points."),
            
            tags$hr(),
            
            actionButton("fetch", "Load Collar Data"),
            
            tags$hr(),
            
            tags$strong("Newest point (Pacific): "),
            textOutput("newest_pt_local", inline = TRUE),
            
            tags$hr(),
            
            
            
            
            checkboxInput("fit_data", "Zoom to data after plotting", value = TRUE),
            
            tags$hr(),
            verbatimTextOutput("status"),
            tags$hr(),
            tags$hr(),
            tags$div(style="color:#888; font-size:12px;", paste("Build:", Sys.time()))
            
          ),
          
          tabPanel(
            "Filtering",
            selectInput(
              "last_n_days",
              "Filter: last N days (anchored to newest loaded data point)",
              choices = c("Off" = "off", "1" = "1", "2" = "2", "3" = "3", "5" = "5", "10" = "10"),
              selected = "off"
            ),
            uiOutput("date_ui"),
            tags$small(style="display:block; margin-top:-6px; color:#666;",
                       "If 'last N days' is set, it overrides the date range."),
            checkboxInput(
              "keep_qpf_only",
              "Keep only good fixes (gps_fix_type_raw contains 'QFP')",
              value = FALSE
            ),
            
          ),
          
          tabPanel(
            "Table",
            checkboxInput("show_table", "Show table (split view)", value = TRUE),
            checkboxInput("fit_selected", "Zoom to selected rows", value = FALSE),
            tags$hr(),
            uiOutput("col_ui")
          ),
          
          tabPanel(
            "Display",
            sliderInput("pt_radius", "Point size", min = 1, max = 12, value = 3, step = 1),
            
            selectInput(
              "pt_color_mode",
              "Point color",
              choices = c("By individual" = "by_id", "Single color" = "single"),
              selected = "by_id"
            ),
            
            conditionalPanel(
              condition = "input.pt_color_mode === 'single'",
              tags$div(
                class = "color-row",
                tags$label(`for` = "pt_color_single", "Pick color:"),
                tags$input(id = "pt_color_single", type = "color", value = "#1f77b4")
              )
            ),
            
            tags$hr(),
            
            selectizeInput("ind_select", "Individuals for tracks (optional)", choices = NULL, multiple = TRUE),
            checkboxInput("show_tracks", "Draw tracks for selected individuals", value = FALSE),
            checkboxInput("show_track_key", "Show track color key", value = TRUE),
            
            
          ),
          tabPanel(
            "Export",
            helpText("Export the currently filtered table data."),
            tags$hr(),
            actionButton("btn_csv", "Download filtered CSV"),
            tags$div(style="height:8px;"),
            tags$hr(),
            actionButton("btn_pts_geojson", "Download Caltopo GeoJSON (points)"),
            tags$div(style="height:8px;"),
            tags$hr(),
            
            tags$div(style="height:8px;"),
            actionButton("btn_tracks_geojson", "Download Caltopo Tracks GeoJSON"),
            tags$small(style="display:block; color:#666;",
                       "On the Display tab, individual must be selected and tracks turned on. 
                       If tracks are not on, the export will be empty"),
            
            tags$hr(),
            tags$hr(),
            tags$small("Tip: The export uses the current filters (last N days / date range) and max points cap."),
            
            

            
          )
          
        )
      )
    ),
    
    column(
      8,
      tags$div(
        id = "map_container",
        class = "map-wrap map-short",
        leafletOutput("map", height = "100%")
      ),
      
      conditionalPanel(
        condition = "input.show_table === true",
        tags$div(style = "height: 12px;"),
        div(class = "tbl-wrap", DTOutput("tbl"))
      )
    )
  ),
  
  # ---- JS fetcher + map resize handler + color picker binding ----
  tags$script(HTML("
  function basicAuthHeader(user, pass) {
    return 'Basic ' + btoa(user + ':' + pass);
  }

  async function fetchMovebankCSV(payload) {
    const { study_id, sensor_type_id, accept_license, user, pass } = payload;

    let url = 'https://www.movebank.org/movebank/service/direct-read'
            + '?entity_type=event'
            + '&study_id=' + encodeURIComponent(study_id)
            + '&attributes=all'
            + '&format=csv';

    if (sensor_type_id && sensor_type_id.trim() !== '') {
      url += '&sensor_type_id=' + encodeURIComponent(sensor_type_id.trim());
    }
    if (accept_license) {
      url += '&accept_license=true';
    }

    try {
      Shiny.setInputValue('mb_status', 'Fetching…', {priority: 'event'});

      const res = await fetch(url, {
        method: 'GET',
        mode: 'cors',
        headers: { 'Authorization': basicAuthHeader(user, pass) }
      });

      if (!res.ok) {
        const txt = await res.text();
        throw new Error('HTTP ' + res.status + ': ' + txt.slice(0, 300));
      }

      const csvText = await res.text();
      Shiny.setInputValue('mb_csv_text', csvText, {priority: 'event'});
      Shiny.setInputValue('mb_status', 'Success: received ' + csvText.length + ' bytes', {priority: 'event'});
    } catch (err) {
      Shiny.setInputValue('mb_status', 'Error: ' + err.message, {priority: 'event'});
      Shiny.setInputValue('mb_csv_text', null, {priority: 'event'});
    }
  }

  Shiny.addCustomMessageHandler('fetch_movebank', function(payload) {
    fetchMovebankCSV(payload);
  });

  Shiny.addCustomMessageHandler('set_map_height', function(payload) {
    var el = document.getElementById('map_container');
    if (!el) return;

    el.classList.remove('map-tall','map-short');
    el.classList.add(payload.tall ? 'map-tall' : 'map-short');

    setTimeout(function() {
      if (window.LeafletWidget && LeafletWidget.maps && LeafletWidget.maps.map) {
        LeafletWidget.maps.map.invalidateSize(true);
      }
    }, 80);
  });

  // ---- Bind native <input type=\"color\"> to Shiny ----
  function bindColorPicker() {
  // In shinylive, Shiny may not exist yet at DOMContentLoaded
  if (!window.Shiny || typeof Shiny.setInputValue !== 'function') return;

  var el = document.getElementById('pt_color_single');
  if (!el) return;

  Shiny.setInputValue('pt_color_single', el.value, {priority: 'event'});

  el.addEventListener('input', function() {
    Shiny.setInputValue('pt_color_single', el.value, {priority: 'event'});
  });
  el.addEventListener('change', function() {
    Shiny.setInputValue('pt_color_single', el.value, {priority: 'event'});
  });
}

  document.addEventListener('DOMContentLoaded', function() {
    setTimeout(bindColorPicker, 0);
  });

  document.addEventListener('shiny:connected', function() {
    setTimeout(bindColorPicker, 0);
  });

  if (window.jQuery) {
    $(document).on('shiny:value', function() {
      setTimeout(bindColorPicker, 0);
    });
    
    // ---- client-side download helper (works on GitHub Pages) ----
  function downloadTextFile(filename, mimeType, text) {
    try {
      const blob = new Blob([text], { type: mimeType });
      const url = URL.createObjectURL(blob);
  
      const a = document.createElement('a');
      a.href = url;
      a.download = filename;
      document.body.appendChild(a);
      a.click();
  
      setTimeout(() => {
        URL.revokeObjectURL(url);
        a.remove();
      }, 100);
    } catch (e) {
      console.error('Download failed:', e);
      alert('Download failed:'  + e.message);
    }
  }

Shiny.addCustomMessageHandler('download_text_file', function(payload) {
  downloadTextFile(payload.filename, payload.mimetype, payload.text);
});

    
  }
  "))
)

server <- function(input, output, session) {
  
  raw_dat <- reactiveVal(NULL)
  
  output$newest_pt_local <- renderText({
    df <- raw_dat()
    if (is.null(df) || !("timestamp_utc" %in% names(df)) || all(is.na(df$timestamp_utc))) {
      return("—")
    }
    tmax <- suppressWarnings(max(df$timestamp_utc, na.rm = TRUE))
    if (!is.finite(tmax)) return("—")
    
    paste0(format(tmax, tz = TZ_LOCAL, usetz = TRUE), "  (n=", nrow(df), ")")
  })
  
  
  # ---- Track key ----
  track_key_html <- function(new_col, old_col) {
    sprintf("
    <div style='background: rgba(255,255,255,0.92); padding: 6px 8px; border-radius: 6px;
                box-shadow: 0 1px 4px rgba(0,0,0,0.25); font-size: 12px; line-height: 14px;'>
      <div style='display:flex; align-items:center; gap:8px;'>
        <span style='display:inline-block; width:14px; height:14px; background:%s; border:1px solid #333;'></span>
        <span>Newer</span>
      </div>
      <div style='display:flex; align-items:center; gap:8px; margin-top:4px;'>
        <span style='display:inline-block; width:14px; height:14px; background:%s; border:1px solid #333;'></span>
        <span>Older</span>
      </div>
    </div>", new_col, old_col
    )
  }
  
  # ---- track redraw trigger (stable) ----
  track_redraw <- reactiveVal(0)
  
  observeEvent(input$show_tracks, {
    if (isTRUE(input$show_tracks)) {
      session$onFlushed(function() {
        track_redraw(isolate(track_redraw()) + 1)
      }, once = TRUE)
    } else {
      leafletProxy("map") %>%
        clearGroup("tracks") %>%
        removeControl("track_key")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$ind_select, {
    if (isTRUE(input$show_tracks)) track_redraw(isolate(track_redraw()) + 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$date_rng, {
    if (isTRUE(input$show_tracks)) track_redraw(isolate(track_redraw()) + 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$last_n_days, {
    if (isTRUE(input$show_tracks)) track_redraw(isolate(track_redraw()) + 1)
  }, ignoreInit = TRUE)
  
  observeEvent(input$max_points, {
    if (isTRUE(input$show_tracks)) track_redraw(isolate(track_redraw()) + 1)
  }, ignoreInit = TRUE)
  
  # ---- Fetch ----
  observeEvent(input$fetch, {
    req(input$mb_user, input$mb_pass, input$study_id)
    session$sendCustomMessage("fetch_movebank", list(
      user = input$mb_user,
      pass = input$mb_pass,
      study_id = input$study_id,
      sensor_type_id = input$sensor_type_id,
      accept_license = isTRUE(input$accept_license)
    ))
  })
  
  observeEvent(input$mb_csv_text, {
    req(input$mb_csv_text)
    
    df <- readr::read_csv(I(input$mb_csv_text), show_col_types = FALSE)
    names(df) <- gsub("-", "_", names(df))
    
    req("location_lat" %in% names(df), "location_long" %in% names(df))
    
    if ("timestamp" %in% names(df)) {
      df$timestamp_utc <- suppressWarnings(as.POSIXct(df$timestamp, tz = "UTC"))
    } else {
      df$timestamp_utc <- as.POSIXct(NA)
    }
    
    df$timestamp_pacific <- ifelse(
      is.na(df$timestamp_utc),
      NA_character_,
      format(df$timestamp_utc, tz = TZ_LOCAL, usetz = TRUE)
    )
    
    df$row_id <- seq_len(nrow(df))
    raw_dat(df)
  })
  
  output$status <- renderText(input$mb_status %||% "Idle.")
  
  observeEvent(input$show_table, {
    session$sendCustomMessage("set_map_height", list(tall = !isTRUE(input$show_table)))
  }, ignoreInit = FALSE)
  
  # Date UI
  output$date_ui <- renderUI({
    df <- raw_dat()
    if (is.null(df) || all(is.na(df$timestamp_utc))) {
      return(helpText("Date range: (timestamp not available in data)"))
    }
    pac_dates <- as.Date(df$timestamp_utc, tz = TZ_LOCAL)
    rng <- range(pac_dates, na.rm = TRUE)
    
    dateRangeInput(
      "date_rng",
      paste0("Date range (", TZ_LOCAL, ")"),
      start = rng[1],
      end = rng[2]
    )
  })
  
  output$col_ui <- renderUI({
    df <- raw_dat()
    if (is.null(df)) return(NULL)
    
    desired <- c(
      "individual_local_identifier",
      "timestamp_pacific",
      "location_lat",
      "location_long",
      "gps_fix_type_raw"
    )
    default_cols <- desired[desired %in% names(df)]
    
    checkboxGroupInput(
      "show_cols",
      "Table columns",
      choices = names(df),
      selected = default_cols
    )
  })
  
  observeEvent(raw_dat(), {
    df <- raw_dat()
    if (is.null(df)) return()
    
    inds <- if ("individual_local_identifier" %in% names(df)) {
      sort(unique(df$individual_local_identifier))
    } else character(0)
    
    updateSelectizeInput(session, "ind_select", choices = inds, selected = character(0), server = TRUE)
  })
  
  # ---- Filtering + capping helpers ----
  apply_filters <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    
    df <- df[is.finite(df$location_lat) & is.finite(df$location_long), , drop = FALSE]
    if (nrow(df) == 0) return(df)
    
    # optional fix-quality filter
    if (isTRUE(input$keep_qpf_only)) {
      if ("gps_fix_type_raw" %in% names(df)) {
        df <- df[grepl("QFP", df$gps_fix_type_raw %||% "", fixed = TRUE), , drop = FALSE]
      } else {
        # if field missing, return empty rather than silently ignoring
        df <- df[0, , drop = FALSE]
      }
      if (nrow(df) == 0) return(df)
    }
    
    
    has_time <- "timestamp_utc" %in% names(df) && any(!is.na(df$timestamp_utc))
    
    # last N days anchored to newest loaded point wins
    if (has_time && !is.null(input$last_n_days) && input$last_n_days != "off") {
      nd <- as.integer(input$last_n_days)
      end_utc <- suppressWarnings(max(df$timestamp_utc, na.rm = TRUE))
      if (is.finite(end_utc)) {
        start_utc <- end_utc - (nd * 86400)
        df <- df[df$timestamp_utc >= start_utc & df$timestamp_utc <= end_utc, , drop = FALSE]
      }
      return(df)
    }
    
    # otherwise date range
    if (has_time && !is.null(input$date_rng) && all(!is.na(input$date_rng))) {
      start_local <- as.POSIXct(paste0(input$date_rng[1], " 00:00:00"), tz = TZ_LOCAL)
      end_local   <- as.POSIXct(paste0(input$date_rng[2], " 23:59:59"), tz = TZ_LOCAL)
      
      start_utc <- as.POSIXct(format(start_local, tz = "UTC", usetz = TRUE), tz = "UTC")
      end_utc   <- as.POSIXct(format(end_local,   tz = "UTC", usetz = TRUE), tz = "UTC")
      
      df <- df[df$timestamp_utc >= start_utc & df$timestamp_utc <= end_utc, , drop = FALSE]
    }
    
    df
  }
  
  cap_points <- function(df, nmax) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if (is.null(nmax) || !is.finite(nmax) || nrow(df) <= nmax) return(df)
    
    if ("timestamp_utc" %in% names(df) && any(!is.na(df$timestamp_utc))) {
      o <- order(df$timestamp_utc, decreasing = TRUE)
      df <- df[o, , drop = FALSE]
      df <- df[seq_len(nmax), , drop = FALSE]
      df <- df[order(df$timestamp_utc), , drop = FALSE]
    } else {
      df <- df[seq.int(max(1, nrow(df) - nmax + 1), nrow(df)), , drop = FALSE]
    }
    df
  }
  
  # ---- Caltopo GeoJSON builder ----
  make_caltopo_geojson <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return('{"type":"FeatureCollection","features":[]}')
    }

    # Ensure required fields exist
    req_cols <- c("location_lat", "location_long", "timestamp_pacific",
                  "individual_local_identifier", "gps_fix_type_raw")
    for (nm in req_cols) if (!nm %in% names(df)) df[[nm]] <- NA

    # Determine age order for gradient coloring (older -> newer)
    if ("timestamp_utc" %in% names(df) && any(!is.na(df$timestamp_utc))) {
      o <- order(df$timestamp_utc, na.last = TRUE)
    } else {
      o <- seq_len(nrow(df))
    }

    n <- nrow(df)
    grad_cols <- grDevices::colorRampPalette(c("#440154", "#21908C", "#FDE725"))(max(1, n))
    cols_by_row <- rep(NA_character_, n)
    cols_by_row[o] <- grad_cols[seq_len(n)]

    # Convert colors to Caltopo format: "FF0000" (no #, uppercase)
    cal_color <- toupper(gsub("^#", "", cols_by_row))

    # Build features
    features <- vector("list", n)
    for (i in seq_len(n)) {
      lon <- df$location_long[i]
      lat <- df$location_lat[i]
      if (!is.finite(lon) || !is.finite(lat)) next

      title <- as.character(df$timestamp_pacific[i] %||% "")
      desc <- paste0(
        "id: ", (df$individual_local_identifier[i] %||% ""), "\n",
        "gps type: ", (df$gps_fix_type_raw[i] %||% ""), "\n",
        "date_time: ", (df$timestamp_pacific[i] %||% "")
      )

      features[[i]] <- list(
        type = "Feature",
        geometry = list(
          type = "Point",
          coordinates = list(as.numeric(lon), as.numeric(lat))
        ),
        properties = list(
          `marker-symbol` = "point",
          `marker-color`  = cal_color[i],
          description     = desc,
          title           = title,
          `marker-size`   = "1.5",
          class           = "Marker",
          folderId        = NULL,
          `marker-rotation` = NULL
        )
      )
    }

    # Drop NULLs from skipped rows (if any)
    features <- Filter(Negate(is.null), features)

    geo <- list(type = "FeatureCollection", features = features)

    # jsonlite is the cleanest way to emit valid JSON
    jsonlite::toJSON(geo, auto_unbox = TRUE, null = "null", digits = 8)
  }

  
  
  
  
  plot_df <- reactive({
    df <- raw_dat()
    req(df)
    df2 <- apply_filters(df)
    cap_points(df2, input$max_points)
  })
  
  track_df <- reactive({
    df <- raw_dat()
    req(df)
    req(isTRUE(input$show_tracks))
    req(length(input$ind_select) > 0)
    
    if (!("individual_local_identifier" %in% names(df))) return(df[0, , drop = FALSE])
    
    df2 <- df[df$individual_local_identifier %in% input$ind_select, , drop = FALSE]
    df2 <- apply_filters(df2)
    df2 <- cap_points(df2, input$max_points)
    
    if ("timestamp_utc" %in% names(df2)) {
      df2 <- df2[order(df2$individual_local_identifier, df2$timestamp_utc), , drop = FALSE]
    } else {
      df2 <- df2[order(df2$individual_local_identifier), , drop = FALSE]
    }
    df2
  })
  
  
  # ## download json
  # output$dl_caltopo_geojson <- downloadHandler(
  #   filename = function() {
  #     paste0("caltopo_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".geojson")
  #   },
  #   content = function(file) {
  #     df <- plot_df()   # <-- this matches the table + map filters/cap
  #     txt <- make_caltopo_geojson(df)
  #     writeLines(txt, con = file, useBytes = TRUE)
  #   }
  # )
  
  
  output$dl_filtered_csv <- downloadHandler(
    filename = function() {
      paste0("movebank_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      df <- plot_df()  # filtered + capped, with all fields
      readr::write_csv(df, file)
    }
  )
  
  
  
  # Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addProviderTiles("CartoDB.DarkMatter", group = "Dark") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
      addTiles(
        urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSTopo/MapServer/tile/{z}/{y}/{x}",
        attribution = "USGS The National Map",
        group = "USGS Topo"
      ) %>% 
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Light", "Dark", "Satellite", "Topo", "USGS Topo"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  # Points + legend + zoom (shinylive-safe palettes)
  observeEvent(
    list(plot_df(), input$pt_radius, input$pt_color_mode, input$pt_color_single),
    {
      df <- plot_df()
      proxy <- leafletProxy("map") %>%
        clearGroup("points") %>%
        clearGroup("selected") %>%
        removeControl("points_legend")
      
      if (nrow(df) == 0) return()
      
      r <- input$pt_radius %||% 3
      mode <- input$pt_color_mode %||% "by_id"
      
      if (identical(mode, "single")) {
        col <- input$pt_color_single %||% "#1f77b4"
        cols <- rep(col, nrow(df))
      } else if ("individual_local_identifier" %in% names(df)) {
        ids <- as.character(df$individual_local_identifier)
        u <- sort(unique(ids))
        
        cols_u <- grDevices::hcl(
          h = seq(15, 375, length.out = length(u) + 1)[-(length(u) + 1)],
          c = 100, l = 55
        )
        col_map <- setNames(cols_u, u)
        cols <- unname(col_map[ids])
        
        proxy <- proxy %>% addLegend(
          position = "bottomright",
          colors = cols_u,
          labels = u,
          title = "individual_local_identifier",
          opacity = 1,
          layerId = "points_legend"
        )
      } else {
        cols <- rep("#1f77b4", nrow(df))
      }
      
      popup_txt <- paste0(
        "<b>individual:</b> ", (df$individual_local_identifier %||% ""),
        "<br><b>time (PT):</b> ", (df$timestamp_pacific %||% ""),
        "<br><b>lat:</b> ", df$location_lat,
        "<br><b>lon:</b> ", df$location_long
      )
      
      proxy %>%
        addCircleMarkers(
          data = df,
          lng = ~location_long,
          lat = ~location_lat,
          radius = r,
          stroke = FALSE,
          fillOpacity = 0.5,
          color = cols,
          popup = popup_txt,
          group = "points",
          layerId = ~row_id
        )
      
      if (isTRUE(input$fit_data)) {
        proxy %>% fitBounds(
          lng1 = min(df$location_long, na.rm = TRUE),
          lat1 = min(df$location_lat, na.rm = TRUE),
          lng2 = max(df$location_long, na.rm = TRUE),
          lat2 = max(df$location_lat, na.rm = TRUE)
        )
      }
    },
    ignoreInit = TRUE
  )
  
  # Tracks: gradient segments + mini key (shinylive-safe palette)
  observeEvent(list(track_df(), track_redraw()), {
    df <- track_df()
    proxy <- leafletProxy("map") %>%
      clearGroup("tracks") %>%
      removeControl("track_key")
    
    if (nrow(df) < 2) return()
    
    ids <- unique(df$individual_local_identifier)
    
    for (id in ids) {
      d <- df[df$individual_local_identifier == id, , drop = FALSE]
      if (nrow(d) < 2) next
      
      if ("timestamp_utc" %in% names(d) && any(!is.na(d$timestamp_utc))) {
        d <- d[order(d$timestamp_utc), , drop = FALSE]
      }
      
      nseg <- nrow(d) - 1
      if (nseg < 1) next
      
      # simple fixed gradient (no viridis dependency)
      pal_seg <- grDevices::colorRampPalette(c("#440154", "#21908C", "#FDE725"))(nseg)
      
      if (isTRUE(input$show_track_key)) {
        new_col <- pal_seg[nseg]
        old_col <- pal_seg[1]
        proxy <- proxy %>%
          addControl(
            html = track_key_html(new_col, old_col),
            position = "bottomright",
            layerId = "track_key"
          )
      }
      
      for (i in seq_len(nseg)) {
        proxy <- proxy %>%
          addPolylines(
            lng = c(d$location_long[i], d$location_long[i + 1]),
            lat = c(d$location_lat[i],  d$location_lat[i + 1]),
            group = "tracks",
            opacity = 0.9,
            weight = 4,
            color = pal_seg[i]
          )
      }
      
      proxy <- proxy %>%
        addCircleMarkers(
          lng = d$location_long[1],
          lat = d$location_lat[1],
          group = "tracks",
          radius = 5,
          stroke = TRUE,
          weight = 2,
          opacity = 1,
          fillOpacity = 1,
          color = "white",
          fillColor = pal_seg[1],
          popup = paste0("<b>", id, "</b><br>Older<br>", d$timestamp_pacific[1] %||% "")
        ) %>%
        addCircleMarkers(
          lng = d$location_long[nrow(d)],
          lat = d$location_lat[nrow(d)],
          group = "tracks",
          radius = 7,
          stroke = TRUE,
          weight = 2,
          opacity = 1,
          fillOpacity = 1,
          color = "white",
          fillColor = pal_seg[nseg],
          popup = paste0("<b>", id, "</b><br>Newer<br>", d$timestamp_pacific[nrow(d)] %||% "")
        )
    }
  }, ignoreInit = TRUE)
  
  # Table
  output$tbl <- renderDT({
    df <- plot_df()
    req(df)
    
    desired <- c(
      "individual_local_identifier",
      "timestamp_pacific",
      "location_lat",
      "location_long",
      "gps_fix_type_raw"
    )
    
    cols <- input$show_cols
    if (is.null(cols) || length(cols) == 0) cols <- desired
    cols <- intersect(cols, names(df))
    
    front <- intersect(desired, cols)
    rest  <- setdiff(cols, front)
    cols_ordered <- c(front, rest)
    
    df_show <- df[, cols_ordered, drop = FALSE]
    
    datatable(
      df_show,
      selection = list(mode = "multiple"),
      class = "compact stripe nowrap",
      options = list(
        paging = FALSE,
        scrollY = "320px",
        scrollX = TRUE,
        scrollXInner = "200%",
        scrollCollapse = TRUE,
        autoWidth = FALSE,
        deferRender = TRUE
      )
    )
  })
  
  
  make_caltopo_tracks_geojson <- function(df) {
    if (is.null(df) || nrow(df) < 2) {
      return('{"type":"FeatureCollection","features":[]}')
    }
    if (!("individual_local_identifier" %in% names(df))) {
      return('{"type":"FeatureCollection","features":[]}')
    }

    # order within id by time if available
    if ("timestamp_utc" %in% names(df) && any(!is.na(df$timestamp_utc))) {
      df <- df[order(df$individual_local_identifier, df$timestamp_utc), , drop = FALSE]
    } else {
      df <- df[order(df$individual_local_identifier), , drop = FALSE]
    }

    ids <- unique(as.character(df$individual_local_identifier))

    # define a consistent global gradient based on time across the *track_df*
    has_time <- "timestamp_utc" %in% names(df) && any(!is.na(df$timestamp_utc))
    if (has_time) {
      tmin <- suppressWarnings(min(df$timestamp_utc, na.rm = TRUE))
      tmax <- suppressWarnings(max(df$timestamp_utc, na.rm = TRUE))
      span <- as.numeric(difftime(tmax, tmin, units = "secs"))
      if (!is.finite(span) || span <= 0) span <- 1
    }

    features <- list()

    for (id in ids) {
      d <- df[df$individual_local_identifier == id, , drop = FALSE]
      d <- d[is.finite(d$location_lat) & is.finite(d$location_long), , drop = FALSE]
      if (nrow(d) < 2) next

      coords <- lapply(seq_len(nrow(d)), function(i) {
        list(as.numeric(d$location_long[i]), as.numeric(d$location_lat[i]))
      })

      # pick one color per track based on "recency" of last point (mirrors gradient idea)
      # if (has_time && any(!is.na(d$timestamp_utc))) {
      #   tlast <- suppressWarnings(max(d$timestamp_utc, na.rm = TRUE))
      #   p <- as.numeric(difftime(tlast, tmin, units = "secs")) / span
      #   p <- max(0, min(1, p))
      #   col_hex <- grDevices::colorRampPalette(c("#440154", "#21908C", "#FDE725"))(101)[1 + round(p * 100)]
      # } else {
      #   col_hex <- "#21908C"
      # }
      # cal_color <- toupper(gsub("^#", "", col_hex))
      cal_color <- "#440154"

      start_pt <- d$timestamp_pacific[1] %||% ""
      end_pt   <- d$timestamp_pacific[nrow(d)] %||% ""

      desc <- paste0(
        "id: ", id, "\n",
        "start: ", start_pt, "\n",
        "end: ", end_pt, "\n",
        "n_points: ", nrow(d)
      )

      features[[length(features) + 1]] <- list(
        type = "Feature",
        geometry = list(
          type = "LineString",
          coordinates = coords
        ),
        properties = list(
          title = paste0("track: ", id),
          description = desc,
          class = "Line",
          stroke = cal_color,      # common Caltopo style fields
          `stroke-width` = 3,
          `stroke-opacity` = 0.9
        )
      )
    }

    geo <- list(type = "FeatureCollection", features = features)
    jsonlite::toJSON(geo, auto_unbox = TRUE, null = "null", digits = 8)
  }

  # output$dl_caltopo_tracks_geojson <- downloadHandler(
  #   filename = function() {
  #     paste0("caltopo_tracks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".geojson")
  #   },
  #   content = function(file) {
  #     # track_df() already requires show_tracks==TRUE and selected individuals
  #     df <- track_df()
  #     txt <- make_caltopo_tracks_geojson(df)
  #     writeLines(txt, con = file, useBytes = TRUE)
  #   }
  # )
  
  
  observeEvent(input$btn_csv, {
    df <- plot_df()
    
    tmp <- tempfile(fileext = ".csv")
    readr::write_csv(df, tmp)
    txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    unlink(tmp)
    
    session$sendCustomMessage("download_text_file", list(
      filename = paste0("movebank_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      mimetype = "text/csv;charset=utf-8",
      text = txt
    ))
  })
  
  observeEvent(input$btn_pts_geojson, {
    df <- plot_df()
    txt <- make_caltopo_geojson(df)
    
    # IMPORTANT: drop jsonlite's "json" class so Shiny sends a string, not an object
    txt <- as.character(txt)
    
    session$sendCustomMessage("download_text_file", list(
      filename = paste0("caltopo_points_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".geojson"),
      mimetype = "application/geo+json;charset=utf-8",
      text = txt
    ))
  })
  
  
  observeEvent(input$btn_tracks_geojson, {
    df <- track_df()
    txt <- make_caltopo_tracks_geojson(df)
    
    # IMPORTANT: drop jsonlite's "json" class
    txt <- as.character(txt)
    
    session$sendCustomMessage("download_text_file", list(
      filename = paste0("caltopo_tracks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".geojson"),
      mimetype = "application/geo+json;charset=utf-8",
      text = txt
    ))
  })
  
  
  
  
  
  # Highlight selected rows on map
  observeEvent(input$tbl_rows_selected, {
    df <- plot_df()
    req(df)
    sel <- input$tbl_rows_selected
    
    proxy <- leafletProxy("map") %>% clearGroup("selected")
    if (length(sel) == 0) return()
    
    dsel <- df[sel, , drop = FALSE]
    if (nrow(dsel) == 0) return()
    
    proxy %>%
      addCircleMarkers(
        data = dsel,
        lng = ~location_long,
        lat = ~location_lat,
        radius = 7,
        stroke = TRUE,
        weight = 2,
        opacity = 1,
        fillOpacity = 0.95,
        group = "selected"
      )
    
    if (isTRUE(input$fit_selected)) {
      if (nrow(dsel) == 1) {
        proxy %>% setView(dsel$location_long[1], dsel$location_lat[1], zoom = 12)
      } else {
        proxy %>% fitBounds(
          lng1 = min(dsel$location_long, na.rm = TRUE),
          lat1 = min(dsel$location_lat, na.rm = TRUE),
          lng2 = max(dsel$location_long, na.rm = TRUE),
          lat2 = max(dsel$location_lat, na.rm = TRUE)
        )
      }
    }
  })
}

shinyApp(ui, server)
