library(querychat)
library(DBI)
library(odbc)
library(dplyr)
library(bslib)
library(shiny)
library(bsicons)
library(DT)
library(shinychat)
library(httr)
library(AzureAuth)
library(log4r)


tenant <- "f017cce5-ae05-41bc-ab46-d4bfe78b7c4c"
app <- "04ce4067-073d-4801-95ce-c116ec3ed36d"

redirect <- config::get("redirect")
port <- parse_url(redirect)$port
options(shiny.port = if (is.null(port)) 3838 else as.numeric(port))

pwd <- Sys.getenv("SHINY_CLIENT_SECRET")
if (pwd == "") {
  pwd <- NULL
}

resource <- c(
  "https://database.windows.net/.default",
  "openid",
  "offline_access"
)

table_name <- "consistenza_personale"
log <- logger()


ui <- function(req) {
  opts <- parseQueryString(req$QUERY_STRING)

  if (is.null(opts$code)) {
    auth_uri <- build_authorization_uri(
      resource,
      tenant,
      app,
      redirect_uri = redirect,
      version = 2
    )
    redir_js <- sprintf("location.replace(\"%s\");", auth_uri)
    tags$script(HTML(redir_js))
  } else {
    page_sidebar(
      title = HTML(sprintf(
        "<span>querychat with <code>%s</code></span>",
        table_name
      )),
      class = "bslib-page-dashboard",
      sidebar = sidebar(
        width = 400,
        height = "100%",
        fillable = TRUE,
        class = "querychat-sidebar",
        uiOutput("qc_sidebar")
      ),
      useBusyIndicators(pulse = TRUE, spinners = FALSE),
      card(
        fill = FALSE,
        style = css(max_height = "33%"),
        card_header(div(
          class = "hstack",
          div(
            bs_icon("terminal-fill"),
            textOutput("query_title", inline = TRUE)
          ),
          div(
            class = "ms-auto",
            uiOutput("ui_reset", inline = TRUE)
          )
        )),
        uiOutput("sql_output")
      ),
      card(
        full_screen = TRUE,
        card_header(bs_icon("table"), "Data"),
        DTOutput("dt")
      ),
      actionButton(
        "close_btn",
        label = "",
        class = "btn-close",
        style = "position: fixed; top: 6px; right: 6px;"
      )
    )
  }
}


server <- function(input, output, session) {
  opts <- parseQueryString(isolate(session$clientData$url_search))
  if (is.null(opts$code)) {
    return()
  }

  token <- get_azure_token(
    resource,
    tenant,
    app,
    password = pwd,
    auth_type = "authorization_code",
    authorize_args = list(redirect_uri = redirect),
    version = 2,
    use_cache = FALSE,
    auth_code = opts$code
  )
  # extract raw access token string
  access_token <- token$credentials$access_token

  if (!is.null(access_token)) {
    info(log, "Ottenuto token per accedere al database")
  } else {
    fatal(log, "Non è stato possibile ottenere un token d'accesso al database")
  }

  con <- dbConnect(
    odbc(),
    driver = config::get("driver"),
    server = "upipa-acs.database.windows.net",
    database = "IndicareSaluteLab",
    attributes = list(azure_token = access_token)
  )

  consistenza_personale <- tbl(con, I("consistenza.consistenza_personale")) |>
    left_join(tbl(con, I("general.enti")) |> select(ente, localita)) |>
    collect()

  qc <- reactive(querychat(
    consistenza_personale,
    client = "azure_openai/gpt-4.1",
    data_description = "consistenza_personale_description.md",
    extra_instructions = "instructions.md",
    greeting = "consistenza_personale_greeting.md"
  ))

  invalidator <- reactive({
    invalidateLater(1000)
    Sys.time()
  })

  qc_vals <- reactiveVal()

  observe(
    {
      qc()$server() |>
        qc_vals()
    }
  ) |>
    bindEvent(invalidator(), once = TRUE, ignoreInit = TRUE)

  output$query_title <- renderText({
    req(qc_vals())
    if (isTruthy(qc_vals()$title())) {
      qc_vals()$title()
    } else {
      "SQL Query"
    }
  })

  output$ui_reset <- renderUI({
    req(qc_vals())
    actionButton(
      "reset_query",
      label = "Reset Query",
      class = "btn btn-outline-danger btn-sm lh-1"
    )
  })

  observe(label = "on_reset_query", {
    qc_vals()$sql(NULL)
    qc_vals()$title(NULL)
  }) |>
    bindEvent(input$reset_query)

  output$dt <- renderDT({
    req(qc_vals())
    df <- qc_vals()$df()
    if (inherits(df, "tbl_sql")) {
      df <- collect(df)
    }
    datatable(
      df,
      fillContainer = TRUE,
      options = list(pageLength = 25, scrollX = TRUE)
    )
  })

  output$sql_output <- renderUI({
    req(qc_vals())
    sql <- if (isTruthy(qc_vals()$sql())) {
      qc_vals()$sql()
    } else {
      paste("SELECT * FROM", table_name)
    }
    sql_code <- paste(c("```sql", sql, "```"), collapse = "\n")
    output_markdown_stream(
      "sql_code",
      content = sql_code,
      auto_scroll = FALSE,
      width = "100%"
    )
  })

  observe(label = "on_close_btn", {
    stopApp(list(
      df = qc_vals()$df(),
      sql = qc_vals()$sql(),
      title = qc_vals()$title(),
      client = qc_vals()$client
    ))
  }) |>
    bindEvent(input$close_btn)

  output$qc_sidebar <- renderUI({
    qc()$ui()
  })
}

shinyApp(ui, server)
