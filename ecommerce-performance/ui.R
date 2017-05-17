shinyUI(fluidPage(
    tags$head(tags$link(rel="shortcut icon", href="https://www.fameandpartners.com/favicon-194x194.png")),
    theme="styles.css",
    navbarPage(
        title = "eCommerce Performance",
        tabPanel(
            "Styles",
            fluidRow(
                column(
                    4,
                    dateRangeInput(
                        "order_dates",
                        label = "Order Date",
                        start = today() - 90,
                        end = today()
                    ),
                    selectInput(
                        "collections",
                        label = "Collection",
                        choices = sort(unique(products_sold$collection)),
                        multiple = TRUE
                    ),
                    selectInput(
                        "order_status",
                        label = "Order Status",
                        choices = sort(unique(products_sold$order_status)),
                        multiple = TRUE
                    ),
                    selectInput(
                        "taxons",
                        "Taxons",
                        choices = product_taxons$taxon_name %>% unique() %>% sort(),
                        multiple = TRUE
                    ),
                    selectInput(
                        "assigned_cohort",
                        "Cohort",
                        choices = products_sold$assigned_cohort %>% unique() %>% sort(),
                        multiple = TRUE
                    ),
                    sliderInput(
                        "us_size",
                        "Size (US):",
                        min = 0,
                        max = 22,
                        value = c(0,22)
                    ),
                    sliderInput(
                        "price_range",
                        "Price (USD):",
                        min = min(products_sold$price_usd),
                        max = max(products_sold$price_usd),
                        value = c(min(products_sold$price_usd), 
                                  max(products_sold$price_usd))
                    )
                ),
                column(
                    8,
                    div(tableOutput("kpis"), id = "kpi-wrapper"),
                    dataTableOutput("style_ranking"),
                    downloadButton("style_ranking_down")
                )
            ),
            
            hr(),
            
            fluidRow(
                h3c("Daily Sales"),
                plotOutput("daily_sales")
            ),
            
            fluidRow(
                column(
                    6,
                    h3c("Size Distribution"),
                    plotOutput("size_dist")
                ),
                column(
                    6,
                    h3c("Top Colors"),
                    plotOutput("top_colors")
                )
            ),
            
            fluidRow(
                h3c("Weekly Customization Rates"),
                plotOutput("cust_rates")
            ),
            
            fluidRow(
                downloadButton("download_all")
            )
        ),
        tabPanel(
            "Returns",
            fluidRow(
                column(
                    4,
                    dateRangeInput(
                        "ship_dates_r",
                        label = "Ship Date",
                        start = today() - 180,
                        end = today() - 30
                    ),
                    selectInput(
                        "collections_r",
                        label = "Collection",
                        choices = sort(unique(products_sold$collection)),
                        multiple = TRUE
                    ),
                    selectInput(
                        "style_r",
                        label = "Style",
                        choices = sort(unique(products_sold$style_name)),
                        multiple = TRUE
                    ),
                    selectInput(
                        "height_r",
                        label = "Height",
                        choices = c("Petite","Standard","Tall"),
                        multiple = TRUE
                    ),
                    sliderInput(
                        "us_size_r",
                        label = "Size (US)",
                        min = 0,
                        max = 22,
                        value = c(0,22)
                    )
                ),
                column(
                    8,
                    h3c("Monthly Return Rates"),
                    plotOutput("monthly_return_rates"),
                    downloadButton("return_rates_down")
                )
            ),
            
            fluidRow(
                column(
                    5,
                    h3c("Return Reasons"),
                    plotOutput("return_reasons"),
                    downloadButton("return_reasons_down")
                ),
                column(
                    7,
                    h3c("Primary and Secondary Return Reason Details"),
                    dataTableOutput("reason_details"),
                    downloadButton("reason_details_down")
                )
            ),
            
            fluidRow(
                column(
                    5,
                    h3c("Secondary Return Reasons"),
                    plotOutput("sec_return_reasons"),
                    downloadButton("sec_return_reasons_down")
                ),
                column(
                    7,
                    h3c("Return Rate by Height and Length"),
                    dataTableOutput("height_length_return_rate"),
                    downloadButton("height_length_return_rate_down")
                )
            ),
            
            hr(),
            
            h1("Returns Bulk Export"),
            p("The download will take up to 1 minute"),
            downloadButton("download_returns")
        ),
        tabPanel(
            "Conversions",
            fluidRow(
                column(
                    4,
                    dateRangeInput("conversion_dates", 
                                   start = today() - 365,
                                   end = today(),
                                   "Date"),
                    selectInput("cohort_select",
                                "Cohort",
                                choices = c("Prom","Bridal","Contemporary"),
                                multiple = TRUE),
                    h4("UTM Parameters:"),
                    selectInput("utm_source",
                                "Source",
                                choices = sort(unique(all_touches$utm_source)),
                                multiple = TRUE),
                    selectInput("utm_medium",
                                "Medium",
                                choices = sort(unique(all_touches$utm_medium)),
                                multiple = TRUE),
                    selectInput("utm_campaign",
                                "Campaign",
                                choices = sort(unique(all_touches$utm_campaign)),
                                multiple = TRUE)
                ),
                column(
                    8,
                    h3c("Monthly Carts, Orders and Conversion Rates"),
                    plotOutput("cart_to_purchase", height = "200px"),
                    plotOutput("carts_orders", height = "300px")
                )
            ),
            fluidRow(
                column(
                    4,
                    h3c("Revenue by UTM Source"),
                    plotOutput("utm_rev")
                ),
                column(
                    8,
                    h3c("Cohort Conversion Funnels"),
                    plotOutput("cohort_conversions")
                )
            ),
            fluidRow(h3c("Monthly Cohort Distribution"),
                     plotOutput("monthly_cohorts"),
                     downloadButton("monthly_cohorts_down")),
            fluidRow(
                column(
                    4,
                    h3c("UTM Source Conversions"),
                    plotOutput("source_conv", height = "800px")
                ),
                column(
                    4,
                    h3c("UTM Medium Conversions"),
                    plotOutput("med_conv", height = "800px")
                ),
                column(
                    4,
                    h3c("UTM Campaign Conversions"),
                    plotOutput("camp_conv", height = "800px"),
                    downloadButton("camp_conv_down")
                )
            )
        ),
        tabPanel(
            "FB & IG",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    selectInput("conv_prospecting",NULL, c("Prospecting","Retargeting"), selected = "Prospecting"),
                    dateRangeInput("conv_dates",NULL, start = today() - 14, end = today()),
                    selectInput("conv_platform", "Platform", ga_fb$Platform %>% unique, multiple = TRUE),
                    selectInput("conv_cohort", "Cohort", ga_fb$cohort %>% unique, multiple = TRUE),
                    selectInput("conv_target", "Target", ga_fb$target %>% unique, multiple = TRUE),
                    selectInput("conv_country", "Country", ga_fb$country %>% unique(), multiple = TRUE),
                    selectInput("conv_region", "Region", ga_fb$region %>% unique(), multiple = TRUE),
                    selectInput("conv_age", "Age", ga_fb$age %>% unique(), multiple = TRUE),
                    selectInput("conv_device_type", "Device Type", ga_fb$device_type %>% unique(), multiple = TRUE),
                    selectInput("conv_creative_type", "Creative Type", ga_fb$creative_type %>% unique(), multiple = TRUE),
                    selectInput("conv_creative_strategy", "Creative Strategy", ga_fb$creative_strategy %>% unique(), multiple = TRUE),
                    selectInput("conv_theme", "Theme", ga_fb$theme %>% unique(), multiple = TRUE),
                    selectInput("conv_ad_format", "Ad Format", ga_fb$ad_format %>% unique(), multiple = TRUE),
                    selectInput("conv_pic_source", "Pic Source", ga_fb$pic_source %>% unique(), multiple = TRUE),
                    selectInput("conv_copy_type", "Copy Type", ga_fb$copy_type %>% unique(), multiple = TRUE),
                    selectInput("conv_landing_page", "Landing Page", ga_fb$landing_page %>% unique(), multiple = TRUE),
                    selectInput("conv_product_category", "Product Category", ga_fb$product_category %>% unique(), multiple = TRUE)
                ),
                mainPanel(
                    width = 9,
                    fluidRow(div(tableOutput("conv_kpis"), id = "kpi-wrapper")),
                    fluidRow(h3c("Snapshot"),
                             dataTableOutput("conv_creative_summary"),
                             downloadButton("conv_creative_summary_down")),
                    fluidRow(h3c("Comparisons"),
                             tabsetPanel(
                                 tabPanel("Cohort", plotOutput("conv_cohort_comp")),
                                 tabPanel("Target", plotOutput("conv_target_comp")),
                                 tabPanel("Country", plotOutput("conv_country_comp")),
                                 #tabPanel("Region", plotOutput("conv_region_comp")),
                                 tabPanel("Age", plotOutput("conv_age_comp")),
                                 tabPanel("Device Type", plotOutput("conv_device_type_comp")),
                                 tabPanel("Creative Type", plotOutput("conv_creative_type_comp")),
                                 tabPanel("Creative Strategy", plotOutput("conv_creative_strategy_comp")),
                                 tabPanel("Theme", plotOutput("conv_theme_comp")),
                                 tabPanel("Ad Format", plotOutput("conv_ad_format_comp")),
                                 #tabPanel("Pic Source", plotOutput("conv_pic_source_comp")),
                                 tabPanel("Copy Type", plotOutput("conv_copy_type_comp")),
                                 tabPanel("Landing Page", plotOutput("conv_landing_page_comp")),
                                 tabPanel("Product Category", plotOutput("conv_product_category_comp"))
                             )
                    )
                )
            ),
            fluidRow(h3c("Trends"), 
                     tabsetPanel(
                         tabPanel("Acquisition", plotOutput("conv_acquisition")),
                         tabPanel("CPC", plotOutput("conv_cpc")),
                         tabPanel("CTR", plotOutput("conv_ctr")),
                         tabPanel("Reach", plotOutput("conv_reach")),
                         tabPanel("Sessions", plotOutput("conv_sessions")),
                         tabPanel("Clicks", plotOutput("conv_clicks")),
                         tabPanel("Purchases", plotOutput("conv_purchases"))
                     )
            )
        ),
        tabPanel(
            "Finances",
            sidebarLayout(
                sidebarPanel(
                    h2("Budget vs. Actual", align = "center"),
                    selectizeInput(
                        "year",
                        "Year",
                        seq(2017, today() %>% year())
                    ),
                    selectizeInput(
                        "quarter",
                        "Quarter",
                        c("All Year","Q1","Q2","Q3","Q4")
                    ),
                    hr(),
                    tags$table(
                        id = "finance_summary",
                        tags$th(
                            tags$td(strong("Actuals")),
                            tags$td(strong("Percent of Budget")),
                            tags$td(strong("Percent Change YoY"))
                        ),
                        tags$tr(
                            tags$td(h4("Gross Revenue:")),
                            tags$td(textOutput("gross_revenue_actual")),
                            tags$td(textOutput("gross_revenue_pob")),
                            tags$td(textOutput("gross_revenue_yoy"))
                        ),
                        tags$tr(
                            tags$td(h4("Units Shipped:")),
                            tags$td(textOutput("units_shipped_actual")),
                            tags$td(textOutput("units_shipped_pob")),
                            tags$td(textOutput("units_shipped_yoy"))
                        ),
                        tags$tr(
                            tags$td(h4("ASP:")),
                            tags$td(textOutput("asp_actual")),
                            tags$td(textOutput("asp_pob")),
                            tags$td(textOutput("asp_yoy"))
                        ),
                        tags$tr(
                            tags$td(h4("COGS:")),
                            tags$td(textOutput("cogs_actual")),
                            tags$td(textOutput("cogs_pob")),
                            tags$td(textOutput("cogs_yoy"))
                        ),
                        tags$tr(
                            tags$td(h4("Avg. Unit COGS:")),
                            tags$td(textOutput("average_unit_cogs_actual")),
                            tags$td(textOutput("average_unit_cogs_pob")),
                            tags$td(textOutput("average_unit_cogs_yoy"))
                        ),
                        tags$tr(
                            tags$td(h4("Returns:")),
                            tags$td(textOutput("returns_actual")),
                            tags$td(textOutput("returns_pob")),
                            tags$td(textOutput("returns_yoy"))
                        ),
                        tags$tr(
                            tags$td(h4("Return Rate:")),
                            tags$td(textOutput("return_rate_actual")),
                            tags$td(textOutput("return_rate_pob")),
                            tags$td(textOutput("return_rate_yoy"))
                        ),
                        tags$tr(
                            tags$td(h4("Returns per Unit:")),
                            tags$td(textOutput("returns_per_unit_actual")),
                            tags$td(textOutput("returns_per_unit_pob")),
                            tags$td(textOutput("returns_per_unit_yoy"))
                        ),
                        tags$tr(
                            tags$td(h4("Gross Margin:")),
                            tags$td(textOutput("gross_margin_actual")),
                            tags$td(textOutput("gross_margin_pob")),
                            tags$td(textOutput("gross_margin_yoy"))
                        )
                    )
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Sales",
                            h3c("Gross Revenue"),
                            plotOutput("gross_revenue", height = "300px"),
                            downloadButton("gross_revenue_down"),
                            hr(),
                            h3c("Units Shipped"),
                            plotOutput("units_shipped", height = "300px"),
                            downloadButton("units_shipped_down"),
                            hr(),
                            h3c("Avgerage Selling Price"),
                            plotOutput("average_selling_price", height = "300px"),
                            downloadButton("average_selling_price_down")
                        ),
                        tabPanel(
                            "COGS",
                            h3c("Cost of Goods Sold"),
                            plotOutput("cogs", height = "300px"),
                            downloadButton("cogs_down"),
                            hr(),
                            h3c("Avgerage Unit COGS"),
                            plotOutput("average_unit_cogs", height = "300px"),
                            downloadButton("average_unit_cogs_down")
                        ),
                        tabPanel(
                            "Returns",
                            h3c("Returns"),
                            p("These estimates are not accurate until 30 Days after month end.", align = "center") %>% em(),
                            plotOutput("returns", height = "300px"),
                            downloadButton("returns_down"),
                            hr(),
                            h3c("Return Rate"),
                            p("These estimates are not accurate until 30 Days after month end.", align = "center") %>% em(),
                            plotOutput("return_rate", height = "300px"),
                            downloadButton("return_rate_down"),
                            hr(),
                            h3c("Returns per Unit"),
                            p("These estimates are not accurate until 30 Days after month end.", align = "center") %>% em(),
                            plotOutput("returns_per_unit", height = "300px"),
                            downloadButton("returns_per_unit_down")
                        ),
                        tabPanel(
                            "Margin",
                            h3c("Gross Margin"),
                            p("These estimates are not accurate until 30 Days after month end.", align = "center") %>% em(),
                            plotOutput("gross_margin", height = "300px"),
                            downloadButton("gross_margin_down")
                        )
                    )
                )
            ),
            fluidRow(
                em("Line Item Details"),
                br(),
                downloadButton("download_finances")
            )
        )
    ),
    title = titlePanel(NULL, "eCommerce Performance")
))