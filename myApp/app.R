##### Setup =====
library(reformulas)
#declaring the pipeline operator instead of loading package to use the function
'%>%' <- magrittr::`%>%`

#UI------------------------------------
ui <-
  bslib::page_navbar(
    theme =bslib::bs_theme(bootswatch = "superhero"),
    title = "BreederQ",
    bslib::nav_panel( #Data upload page ----
                      "Data Upload",
                      bslib::layout_sidebar(
                        sidebar =  bslib::sidebar(
                          shiny::fileInput(inputId = "upload", "upload", accept = c(".csv", ".tsv")),
                          shiny::tags$br(),
                          shiny::selectInput(inputId = "stage_upload",label = "Stage", choices = NULL),
                          shiny::selectInput(inputId = 'trait_upload', label = "Trait", choices = NULL)
                        ),
                        bslib::card(
                          DT::dataTableOutput("summary")
                        )
                      )
    ),
    bslib::nav_panel( #LQI page ----
                      "LQI",
                      bslib::layout_sidebar(
                        sidebar = bslib::sidebar(
                          shiny::selectInput(inputId = "stage_lqi",label = "Stage", choices = NULL),
                          shiny::selectInput(inputId = "block_lqi", label = "Block", choices = NULL),
                          shiny::selectInput(inputId = 'trait_lqi', label = "Trait", choices = NULL)
                        ),
                        bslib::card(
                          bslib::card_header("Some viz -- lated"),
                          shiny::plotOutput("lqi_plot")
                        )
                      )
    ),
    bslib::nav_panel( #Plot QAQC page----
                      "QAQC",
                      bslib::layout_sidebar(
                        sidebar = bslib::sidebar(
                          shiny::selectInput(inputId = "stage",label = "Stage", choices = NULL),
                          shiny::selectInput(inputId = "block", label = "Block", choices = NULL),
                          shiny::selectInput(inputId = 'trait', label = "Trait", choices = NULL),
                          shiny::actionButton(inputId= "reset", label = "Reset Plots"),
                          shiny::actionButton(inputId= "save", label = "Save"),
                          shiny::actionButton(inputId= "remove", label = "Remove Decision")
                        ),
                        bslib::layout_columns(
                          bslib::card(
                            fill= FALSE,
                            shiny::plotOutput("residual", 
                                              height = "200px", 
                                              width= "100%")
                          ),
                          bslib::card(
                            fill = FALSE,
                            shiny::plotOutput("cooksd", 
                                              click = shiny::clickOpts(id= "plot_click"),
                                              height = "200px", 
                                              width= "100%")
                          ), 
                          bslib::card(
                            fill = FALSE,
                            shiny::plotOutput("obs_fit", 
                                              height = "200px", 
                                              width= "100%")
                          )
                        ),
                        bslib::layout_columns(
                          bslib::card(
                            fill = FALSE,
                            shiny::plotOutput("raw_data_1", 
                                              height = "260px", 
                                              width= "100%")
                          ),
                          bslib::card(
                            fill = FALSE,
                            shiny::plotOutput("raw_data_2", 
                                              height = "260px", 
                                              width= "100%")
                          )
                        )
                      )
    ),
    bslib::nav_panel( #Set level QAQC page----
                      "Set QAQC",
                      bslib::layout_sidebar(
                        sidebar = bslib::sidebar(
                          shiny::selectInput(inputId = "stage",label = "Stage", choices = NULL),
                          shiny::selectInput(inputId = "block", label = "Block", choices = NULL),
                          shiny::selectInput(inputId = 'trait', label = "Trait", choices = NULL)
                        ),
                        bslib::layout_columns(
                          bslib::card(
                            fill= FALSE,
                            shiny::plotOutput("set")
                          )
                        )
                      )
    ),
    bslib::nav_panel( #Deactivation download page ----
                      "Download",
                      bslib::layout_sidebar(
                        sidebar = bslib::sidebar(
                          shiny::downloadButton("download", label = "Download Deactivations",icon = shiny::icon("download"))
                        ),
                        bslib::layout_columns(
                          bslib::card(
                            DT::dataTableOutput("plot_deac")
                          ),
                          bslib::card(
                            DT::dataTableOutput("set_deac")
                          )
                        )
                      )
    )
  )
#Server-------------------------------------------------------------------
server <- function(input, output, session){
  #Apply theme to figures----
  thematic::thematic_shiny()
  options(shiny.maxRequestSize=100*1024^2)
  #Data upload ------
  raw_data <- shiny::reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ",",
                              col_types = c(set_name = "f", pedigree= 'f')),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t",
                              col_types = c(set_name = "f", pedigree= 'f')),
           validate("Invalid file; Please upload a .csv or .tsv file"))
  })
  #Data wrangling
  #Removing Plot deactivations and correcting other deactivation----
  clean_data <-
    shiny::reactive({
      raw_data() %>%
        dplyr::filter(is.na(.data[["plot_deactivated"]])) %>%
        dplyr::mutate(ans_deac= dplyr::if_else(.data[['isdeactivated']]== TRUE, TRUE, FALSE, missing= FALSE)) %>%
        dplyr::mutate(dsr_deac= dplyr::if_else(.data[['isdeactivated_dsr']]== TRUE, TRUE, FALSE, missing= FALSE)) %>%
        dplyr::mutate(qaqc_deac= dplyr::if_else(.data[['isdeactivated_qaqc']]== TRUE, TRUE, FALSE, missing= FALSE))
    })
  #Filtering data (only Checks for SC2)----
  analysed_data <-
    shiny::reactive({
      clean_data() %>%
        dplyr::filter(!is.na(.data[["value"]])) %>%
        dplyr::filter(ifelse (.data[["crop_material_stage"]]== "Screening 2", .data[["iscontrol"]]== TRUE, .data[["iscontrol"]] %in% c(TRUE, FALSE))) %>%
        dplyr::filter(.data[["block_name"]]== input$block, .data[['questioncode']]== input$trait)
    })
  #Update inputs----
  #Uplpoad tabset----
  #Stage
  shiny::observeEvent(clean_data(), {
    shiny::updateSelectInput(session, "stage_upload", "Stage", choices = sort(unique(clean_data()$crop_material_stage)))
  })
  
  #Trait
  shiny::observeEvent(raw_data(), {
    shiny::updateSelectInput(session, "trait_upload", "Trait", choices = sort(unique(clean_data()$questioncode)), selected = "GYLD")
  })
  #Obs-QAQC tabset----
  #Stage
  shiny::observeEvent(clean_data(), {
    shiny::updateSelectInput(session, "stage", "Stage", choices = sort(unique(clean_data()$crop_material_stage)))
  })
  #Block
  shiny::observeEvent(input$stage, {
    QAQC_block <- shiny::reactive({
      clean_data() %>% dplyr::filter(.data[["crop_material_stage"]]== input$stage)
    })
    shiny::updateSelectInput(session, "block", "Block", choices = sort(unique(QAQC_block()$block_name)))
  })
  #Trait
  shiny::observeEvent(raw_data(), {
    shiny::updateSelectInput(session, "trait", "Trait", choices = sort(unique(clean_data()$questioncode)), selected = "GYLD")
  })
  
  #Creating analysed file for viz----------
  #Model----
  model <-
    shiny::reactive({
      lme4::lmer(value ~ pedigree + (1|set_name), REML= FALSE, data= analysed_data(), na.action = na.exclude)
    })
  #Residual extraction (HLMDiag)----
  residual <-
    shiny::reactive({
      HLMdiag::hlm_resid(model(), level= 1, standardize= TRUE, include.ls= FALSE) %>%
        dplyr::select(set_name, pedigree,.fitted,  .std.resid) %>%
        dplyr::rename(fitted= .fitted, res_std= .std.resid)
    })
  #Influence diagonistics ----
  #Observation level
  infl_obs <-
    shiny::reactive({
      HLMdiag::hlm_influence(model(), level= 1, approx= TRUE) %>%
        dplyr::select(set_name, pedigree, cooksd, mdffits)
    })
  #Set level
  infl_set <-
    shiny::reactive({
      HLMdiag::hlm_influence(model(), level= "set_name") 
    })
  #Merging with data file
  analysed_output <-
    shiny::reactive({
      analysed_data() %>%
        dplyr::left_join(residual(), keep= NULL) %>%
        dplyr::left_join(infl_obs(), keep= NULL)
    })
  
  #Setting reactive values to capture selections----
  n_row <-shiny::reactive(nrow(analysed_output()))
  vals <- shiny::reactiveValues(keep_rows= shiny::reactive({rep(TRUE, n_row())}), 
                                deactivated= NULL, 
                                selected= NULL)
  
  shiny::observeEvent(input$plot_click, {
    keep_rows <- vals$keep_rows()
    near_points <- shiny::nearPoints(analysed_output(), input$plot_click, allRows = TRUE, threshold = 3, maxpoints = 1)
    vals$keep_rows <- shiny::reactive({xor(keep_rows, near_points$selected_)})
    vals$selected <- near_points %>% dplyr::filter(selected_== TRUE)
  })
  shiny::observeEvent(c(input$stage, input$block, input$trait, input$reset), {
    vals$keep_rows <- shiny::reactive({rep(TRUE, n_row())})
  })
  keep <- shiny::reactive({
    analysed_output()[vals$keep_rows(), , drop= FALSE]
  })
  exclude <-
    shiny::reactive({
      analysed_output()[!vals$keep_rows(), , drop= FALSE]
    })
  shiny::observeEvent(input$save, {
    vals$deactivated <- unique(rbind(vals$deactivated, exclude()))
  })
  shiny::observeEvent(input$remove, {
    vals$deactivated <- NULL
  })
  shiny::observeEvent(input$reset, {
    vals$selected <- NULL
  })
  #Visualizations
  #Data summary----
  output$summary <- 
    DT::renderDataTable({
      clean_data() %>%
        dplyr::filter(crop_material_stage== input$stage_upload,
                      questioncode== input$trait_upload) %>%
        dplyr::group_by(block_name) %>%
        dplyr::summarise(
          N= dplyr::n(),
          isdeactivated= plyr::count(ans_deac),
          isdeactivated_dsr= plyr::count(dsr_deac),
          isdeactivated_qaqc= plyr::count(qaqc_deac)
        )
    })
  #Residual vs Fitted ----
  output$residual <-
    shiny::renderPlot({
      analysed_output() %>%
        ggplot2::ggplot(ggplot2::aes(fitted, res_std))+
        ggplot2::geom_point(ggplot2::aes(color= dsr_deac), size= 1.75, shape= 1, stroke= .5)+
        ggplot2::scale_color_manual(values= c("#e0e0e0", "#4393c3"))+
        ggplot2::geom_smooth(method = 'lm', se= FALSE)+
        ggplot2::geom_point(data= exclude(), color= "#d53e4f", size= 1)+
        ggplot2::labs(title = "Stnderdized Residuals vs Fitted")
    })
  #Cooks distance plot----
  output$cooksd <-
    shiny::renderPlot({
      keep() %>%
        ggplot2::ggplot(ggplot2::aes(cooksd, res_std))+
        ggplot2::geom_point(size= 2, color= "#e0e0e0", shape= 1, stroke= 1)+
        ggplot2::geom_point(data= exclude(), color= "#bd0026", size= 2)+
        ggplot2::labs(title = "Cooks Distance")
    })
  #Fitted vs Observed plot----
  output$obs_fit <-
    shiny::renderPlot({
      analysed_output() %>%
        ggplot2::ggplot(ggplot2::aes(.data[["fitted"]], .data[["value"]]))+
        ggplot2::geom_point(ggplot2::aes(color= dsr_deac), size= 1.75, shape= 1, stroke= .5)+
        ggplot2::scale_color_manual(values= c("#e0e0e0", "#4393c3"))+
        ggplot2::geom_smooth(method = 'lm', se= FALSE)+
        ggplot2::geom_point(data= exclude(), color= '#d53e4f', size= 1)+
        ggplot2::labs(title = "Observed vs Fitted")
    })
  #Raw data 1----
  output$raw_data_1 <-
    shiny::renderPlot({
      analysed_output() %>%
        ggplot2::ggplot(ggplot2::aes(.data[["l3_name"]], .data[["value"]]))+
        ggplot2::geom_boxplot(outlier.colour = NULL)+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angl= 90), hjust= .5, vjust= 1)+
        ggplot2::geom_point(size= 1.75, shape= 1)+
        ggplot2::geom_point(data= exclude(), color= '#d53e4f',size= 1)+
        ggplot2::labs(title = "Raw Data")
    })
  #Raw data 2----
  output$raw_data_2 <-
    shiny::renderPlot({
      if(length(exclude()$pedigree) == 0){
        analysed_output() %>%
          ggplot2::ggplot(ggplot2::aes(.data[["l3_name"]], .data[["value"]]))+
          ggplot2::geom_boxplot(outlier.colour = NULL, outlier.fill = NULL)+
          ggplot2::theme(axis.text.x = ggplot2::element_text(angl= 90), hjust= 1, vjust= 1)+
          ggplot2::geom_point(ggplot2::aes(color= dsr_deac), size= 1.5, shape= 1, stroke= .5)+
          ggplot2::scale_color_manual(values= c("#e0e0e0", "#4393c3"))
      } else {
        analysed_output() %>%
          ggplot2::ggplot(ggplot2::aes(.data[["l3_name"]], .data[["value"]]))+
          ggplot2::geom_boxplot(outlier.colour = NULL, outlier.fill = NULL)+
          ggplot2::theme(axis.text.x = ggplot2::element_text(angl= 90))+
          ggplot2::geom_point(ggplot2::aes(color= dsr_deac), size= 1.5, shape= 1, stroke= .5)+
          ggplot2::scale_color_manual(values= c("#e0e0e0", "#4393c3"))+
          ggplot2::geom_point(data= analysed_output() %>% dplyr::filter(pedigree== vals$selected$pedigree), color= '#d53e4f', size= 1.5)+
          ggplot2::labs(title = "Datapoint in Locations")
      }
      
    })
  #Viz- Set level
  output$set <-
    shiny::renderPlot({
      infl_set() %>% 
        dplyr::arrange(cooksd) %>% 
        dplyr::mutate(name=factor(set_name, levels=set_name)) %>% 
        ggplot2::ggplot(ggplot2::aes(x=name, y=cooksd)) +
        ggplot2::geom_segment(ggplot2::aes(x=name, xend=name, y=0, yend=cooksd)) +
        ggplot2::geom_point( size=5, color="red", fill= ggplot2::alpha("orange", 0.3), shape=21, stroke=2)+
        ggplot2::coord_flip()+
        ggplot2::theme_minimal()
      
    })
  #Plot deactivation for download
  output$plot_deac <-
    DT::renderDataTable({
      vals$deactivated %>% 
        dplyr::select(crop_material_stage, entryid, setid, plot_id, 
                      set_name, block_name, l1_name, l2_name, l3_name,  field_name, 
                      isdeactivated, isdeactivated_dsr, isdeactivated_qaqc, plot_deactivated, 
                      pedigree, questioncode, value)
    })
  
  output$set_deac <-
    DT::renderDataTable({
      if(is.null(vals$selected)){
        return(NULL)
      }else{
        vals$selected %>% 
          dplyr::select(set_name, block_name, pedigree)
      }
    })
  
  output$download <- shiny::downloadHandler(
    filename = function() {
      paste("Deactivation_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vals$deactivated %>% 
                  dplyr::select(crop_material_stage, entryid, setid, plot_id, 
                                set_name, block_name, l1_name, l2_name, l3_name,  field_name, 
                                isdeactivated, isdeactivated_dsr, isdeactivated_qaqc, plot_deactivated, 
                                pedigree, questioncode, value), file)
    }
  )
}
shiny::shinyApp(ui = ui, server = server)