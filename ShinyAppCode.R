library(shiny)
library(fpp3)
library(tidyverse)


# load and wrangle data
aus_wine <- read_csv(("data/AustralianWines.csv"),
                     na = "*",
                     col_types = cols(Rose = col_number()),
                     show_col_types = FALSE) |>
    fill(Rose, .direction = "down") |>
    mutate(Month = yearmonth(mdy(str_replace(Month, '-', '-01-')))) |>
    pivot_longer(cols = -Month, names_to = 'Varietal', values_to = 'Sales') |>
    as_tsibble(index = Month, key = Varietal)

varietals <- sort(unique(aus_wine$Varietal))

# Models per varietal (helper)
model_specs <- function(models, use_log) {
    all <- if (use_log) {
        list(TSLM = TSLM(log(Sales) ~ trend() + season()),
             ETS = ETS(log(Sales)),
             ARIMA = ARIMA(log(Sales)))
    } else {
        list(TSLM = TSLM(Sales ~ trend() + season()),
             ETS = ETS(Sales),
             ARIMA = ARIMA(Sales))
    }
    all[models]
}

fit_models <- function(data, models, use_log, combo, with_combo = combo) {
    fit <- model(data, !!!model_specs(models, use_log))
    if(with_combo && length(models) >= 2) {
        sum_expr <- Reduce(function(a, b) rlang::expr(!!a + !!b), rlang::syms(models))
        combo_expr <- rlang::expr((!!sum_expr) / !!length(models))
        fit <- mutate(fit, COMBO = !!combo_expr)
    }
    fit
}

# forecast (helper)
forecast_models <- function(fit, data, models, use_log, h) {
    attempt <- function(f) {
        fc <- forecast(f, h = h)
        invisible(hilo(fc, level = c(80, 95)))
        list(fit = f, fc = fc)
    }
    tryCatch(
        attempt(fit),
        error = function(e) {
            if ("COMBO" %in% names(fit)) {
                showNotification("The ensemble could not be forecast here and was dropped.",
                                 type = "warning")
                attempt(fit_models(data, models, use_log, combo = FALSE))
            } else stop(e)
        }
    )
}

# visualize (helper)
plot_forecast <- function(fc, hist, level, cut = NULL, title = NULL) {
    f <- fc |>
        hilo(level = level) |>
        as_tibble()
    hl <- f[[paste0(level, '%')]]
    f$.lo <- vctrs::field(hl, 'lower')
    f$.hi <- vctrs::field(hl, 'upper')
    p <- ggplot() +
        geom_line(data = as_tibble(hist), aes(as.Date(Month), Sales),
                  color = 'black', linewidth = 0.5) + 
        geom_ribbon(data = f, 
                    aes(as.Date(Month), ymin = .lo, ymax = .hi, fill = .model),
                    alpha = 0.15) +
        geom_line(data = f, aes(as.Date(Month), .mean, color = .model),
                  linewidth = 0.9) +
        facet_wrap(vars(Varietal), scales = "free_y", ncol = 1) +
        labs(x = NULL, y = 'Sales (monthly)', color = "Model", fill = 'Model',
             title = title,
             caption = paste0("Black = actual sales. Shaded bands = ", level,
                              "% prediction intervals.")) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "bottom")
    if (!is.null(cut)) {
        p <- p + geom_vline(xintercept = as.Date(cut), linetype = 'dashed') +
            labs(caption = paste0("Black = actual sales. Dashed line = end of training window. ",
                                  "Shaded bands = ", level, "% prediction intervals."))
    }
    p
}

# UI
ui <- fluidPage(
    titlePanel("Australian Wine Sales: Forecast Model Explorer"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            # inputs for filtering by varietals and date range
            h4("1. filter data"),
            selectInput('varietal', 'Varietal(s)', choices = varietals,
                        selected = head(varietals, 2), multiple = TRUE),
            helpText("Pick one or more varietals; every output updates together."),
            dateRangeInput("dates", "Date range",
                           start = as.Date(min(aus_wine$Month)),
                           end = as.Date(max(aus_wine$Month)),
                           min = as.Date(min(aus_wine$Month)),
                           max = as.Date(max(aus_wine$Month)),
                           format = "M yyyy", startview = 'year'),
            
            # inputs for train/validation split
            tags$hr(),
            h4('2. train/validation split'),
            dateInput('train_end', 'Training ends (train_end)',
                      value = as.Date(max(aus_wine$Month) - 12),
                      min = as.Date(min(aus_wine$Month)),
                      max = as.Date(max(aus_wine$Month)),
                      format = 'M yyyy', startview = 'year'),
            helpText('Months after this date form the validation window. ',
                     'Models are fit only on data up to and including it.'),
            
            # inputs for models
            tags$hr(), 
            h4('3. Models'),
            checkboxGroupInput('models', 'Models',
                               choices = c('TSLM', 'ETS', 'ARIMA'),
                               selected = c('TSLM', 'ETS', 'ARIMA'), inline = TRUE),
            checkboxInput('use_log', 'Log-transform sales (stabilizes seasonal variance)', TRUE),
            checkboxInput('combo', 'Add equal-weight ensemble (average of selected models)', TRUE),
            
            # inputs for forecasting
            tags$hr(),
            h4('4. Forecast'),
            numericInput('h', 'Future forecast horizon, h (months)',
                         value = 12, min = 1, max = 36, step = 1),
            
            # inputs for interval level
            tags$hr(),
            h4('6. Display'),
            radioButtons('level', 'Prediction interval', choices = c(80, 95),
                         selected = 95, inline = TRUE)
        ),
        mainPanel( 
            width = 9, 
            tabsetPanel(
                # data visualization
                tabPanel('Overview', plotOutput('overview', height = '520px')),
                tabPanel('Seasonality', plotOutput('season', height = '520px')),
                tabPanel('Decomposition',
                         uiOutput('decomp_pick'),
                         plotOutput('decomp', height = '520px')),
                # evaluate
                tabPanel('Model specs & accuracy',
                         h4('Best model on the validation window'),
                         tableOutput('best'),
                         h4('Model specifications'),
                         tableOutput('specs'),
                         h4('Accuracy: training vs valdation'),
                         tableOutput('metrics'),
                         downloadButton('dl_metrics', 'Download metrics (CSV)')),
                # forecast plots
                tabPanel("Validation forecasts",
                         plotOutput('fc_valid', height = '620px')),
                tabPanel('Future forecasts',
                         helpText('Models are re-fit on the full selected date range.'),
                         plotOutput('fc_future', height = '620px')),
                # Document
                tabPanel('About',
                         h4('Data source'),
                         p('Monthly Australian wine sales by varietal (AustralianWines.csv), ',
                           'course dataset. Missing Rose values (marked \'*\' in the CSV) are ',
                           'carried forward from the previous month.'),
                         h4('Modeling choices'),
                         tags$ul(
                             tags$li('TSLM: linear trend + monthly seasonality; ETS and ARIMA: auto-specified with fable.'),
                             tags$li('Validation: models are fit on data up to train_end, forecast over the remaining months, and scored against actual sales.'),
                             tags$li('Future forecasts: models are re-fit on all selected data and forecast h months ahead.'),
                             tags$li('Accuracy boosters: optional log transform and an equal-weight ensemble.')
                         ),
                         h4('How to reproduce a figure'),
                         tags$ol(
                             tags$li('Choose the varietal(s) and date range.'),
                             tags$li('Set the training-end month (train_end).'),
                             tags$li('Set the models, the two options (log transform, ensemble), the horizon h and the interval level.'),
                             tags$li('Open the tab for the figure or table; record these settings in the caption.')
                         ),
                         h4('Acknowledgements'),
                         p('Built with R, shiny, and the fpp3 packages (Hyndman & Athanasopoulos, ',
                           'Forecasting: Principles and Practice, 3rd ed.). ',
                           'Parts of this code were drafted ith help from an LLM (Claude); ',
                           'I reviewed, tested, and take responsibility for it.')
                ))
        )
    ))

# server
server <- function(input, output, session) {
    # load & wrangle data by varietal and date range
    dat <- reactive({
        validate(need(length(input$varietal) > 0, 'Select at least one varietal.'))
        req(input$dates)
        d <- aus_wine |>
            filter(Varietal %in% input$varietal,
                   Month >= yearmonth(input$dates[1]),
                   Month <= yearmonth(input$dates[2]))
        validate(need(nrow(d) >0, 'No data in this date range.'))
        d
    })
    # split data
    split <- reactive({
        d <- dat()
        req(input$train_end)
        train_end <- yearmonth(input$train_end)
        validate(need(train_end < max(d$Month),
                      'Training end must be earlier than the last month in the date range.'))
        training <- filter(d, Month <= train_end)
        validation <- filter(d, Month > train_end)
        n_train <- training |> as_tibble() |> count(Varietal) |> pull(n)
        validate(need(length(n_train) > 0 && min(n_train) >= 36,
                      paste0('Not enough training data: move the training end later or ',
                             'widen the date range (need at least 36 training months).')))
        list(train_end = train_end, training = training, validation = validation,
             n_valid = n_distinct(validation$Month))
    })
    models_sel <- reactive({
        validate(need(length(input$models) > 0, 'Select at least one model.'))
        input$models
    })
    # Models
    fit_train <- reactive({
        withProgress(message = 'Fitting models on the training window...', value = 0.5,
                     fit_models(split()$training, models_sel(), input$use_log, input$combo))
    })
    fit_full <- reactive({
        withProgress(message = 'Re-fitting on full data...', value = 0.5,
                     fit_models(dat(), models_sel(), input$use_log, input$combo))
    })
    # Forecast
    valid_run <- reactive({
        sp <- split()
        forecast_models(fit_train(), sp$training, models_sel(), input$use_log, sp$n_valid)
    })
    future_run <- reactive({
        validate(need(!is.na(input$h) && input$h >= 1, 'Enter a horizon of at least 1 month.'))
        forecast_models(fit_full(), dat(), models_sel(), input$use_log, input$h)
    })
    # Evaluate
    output$specs <- renderTable({
        fit <- valid_run()$fit
        mods <- setdiff(names(fit), 'Varietal')
        out <- dplyr::bind_rows(lapply(mods, function(m) {
            tibble(Varietal = fit$Varietal, Model = m,
                   Specification = vapply(fit[[m]], function(x) gsub("[<>]", "", format(x)),
                                          character(1)))
        }))
        out |>
            mutate(Specification = ifelse(Model == "TSLM", "TSLM: trend + monthly seasonality",
                                          Specification),
                   Scale = ifelse(input$use_log, 'log', 'original')) |>
            arrange(Varietal, Model)
    })
    metrics <- reactive({
        vr <- valid_run()
        ms <- list(RMSE = RMSE, MAE = MAE, MAPE = MAPE)
        tr <- accuracy(vr$fit, measures = ms) |> mutate(Window = 'Train')
        va <- accuracy(vr$fc, dat(), measures = ms) |> mutate(Window = "Validation")
        bind_rows(as_tibble(tr), as_tibble(va)) |>
            select(Varietal, Model = .model, Window, RMSE, MAE, MAPE) |>
            arrange(Varietal, factor(Window, levels = c('Train', 'Validation')), Model)
    })
    output$metrics <- renderTable(metrics(), digits = 2)
    output$best <- renderTable({
        metrics() |>
            filter(Window == 'Validation') |>
            group_by(Varietal) |>
            slice_min(RMSE, n = 1, with_ties = FALSE) |>
            ungroup() |>
            transmute(Varietal, 'Best model (validation RMSE)' = Model, 
                      'Validation RMSE' = RMSE)
    }, digits = 2)
    output$dl_metrics <- downloadHandler(
        filename = function() 'wine_model_metrics.csv',
        content = function(file) readr::write_csv(metrics(), file)
    )
    # Visualize
    output$overview <- renderPlot({
        ggplot(as_tibble(dat()), aes(as.Date(Month), Sales)) + 
            geom_line(color = '#2c7fb8') +
            facet_wrap(vars(Varietal), scales = 'free_y', ncol = 1) +
            labs(x = NULL, y = 'Sales (monthly)', title = 'Monthly sales by varietal') +
            theme_minimal(base_size = 13)
    })
    output$season <- renderPlot({
        validate(need(nrow(dat()) >= 24, 'Need at least 24 months for a seasonal plot.'))
        gg_season(dat(), Sales) +
            labs(title = 'Seasonal plot: each line is one year') +
            theme_minimal(base_size = 13)
    })
    output$decomp_pick <- renderUI({
        selectInput('decomp_v', 'Varietal to decompose', choices = input$varietal)
    })
    output$decomp <- renderPlot({
        req(input$decomp_v)
        d <- filter(dat(), Varietal == input$decomp_v)
        validate(need(nrow(d) >= 24, 'Need at least 24 months for STL decomposition,'))
        mdl <- if (input$use_log) {
            model(d, STL(log(Sales) ~ season(window = 'periodic')))
        } else{
            model(d, STL(Sales ~ season(window = 'periodic')))
        }
        autoplot(components(mdl)) + theme_minimal(base_size = 12)
    })
    output$fc_valid <- renderPlot({
        plot_forecast(valid_run()$fc, dat(), as.numeric(input$level),
                      cut = split()$train_end,
                      title = 'Validation: forecasts from models fit on the training window')
    })
    output$fc_future <- renderPlot({
        plot_forecast(future_run()$fc, dat(), as.numeric(input$level),
                      title = paste0('Future forecasts (', input$h, ' months ahead)'))
    })
}
shinyApp(ui, server)
