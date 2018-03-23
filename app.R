
library(RODBC)
library(data.table)
library(shiny)
library(shinydashboard)

mozart <- odbcConnect("Mozart")

# seller country: GC; dimension: No
data <- sqlQuery(mozart,"
                SELECT YEAR_ID, WEEK_OF_YEAR_ID, SLR_CNTRY, SLR_REGION_OF_GC, TRIM(DIMENSION) AS DIMENSION, LSTG_SITE, CUT, SUM(GMV) AS GMV
                FROM P_CI_MAP_T.yhz_GC_GMV_DT_exdJD_Sct2
                GROUP BY 1,2,3,4,5,6,7
                ")
raw_data <- data.table(data)

# define calculation functions here
abs_dif <- function(pre,post){post / pre - 1}
share_dif <- function(pre,post,pre_all,post_all){(post/post_all)/(pre/pre_all) - 1}
contr <- function(pre){pre / sum(pre)}
contr_var <- function(pre,post){(post - pre) / (sum(post) - sum(pre))}
exp_by_demand <- function(pre,post,pre_all,post_all){(post_all * pre / pre_all - pre) / (post - pre)}

# ui

ui <- dashboardPage(
  dashboardHeader(title = "Summary Table"),
  dashboardSidebar(
    sliderInput(inputId = "Post_Year_ID",label = "Post Year",min = 2012,max = 2018,value = 2017),
    sliderInput(inputId = "Post_WEEK_OF_YEAR_ID",label = "Post Week",min = 1,max = 53,value = 25),
    sliderInput(inputId = "Pre_Year_ID",label = "Pre Year",min = 2012,max = 2018,value = 2016),
    sliderInput(inputId = "Pre_WEEK_OF_YEAR_ID",label = "Pre Week",min = 1,max = 53,value = 25),
    checkboxGroupInput(inputId = "GC_REGION", label = "GC Seller Region", 
                       choices = list("CN" = "CN", "HK" = "HK", "TW" = "TW", "NonGC" = "Non GC"),
                       selected = c("CN","HK","TW")),
    checkboxGroupInput(inputId = "site", label = "Transaction Site",
                       choices = list("AU" = "AU", "DE" = "DE", "FRITES" = "FRITES", "Others" = "Others", "UK" = "UK", "US" = "US"),
                       selected = c("AU","DE","FRITES","Others","UK","US"))
  ),
  dashboardBody(
    fluidRow(
      column(width = 12,
             box(width = NULL,
                 title = "PART 1: Total GC GMV by listing site",
                 tableOutput(outputId = "pvt1")
             ),
             box(width = NULL,
                 title = "PART 2: Listing-site-level GC GMV by other dimensions",
                 tableOutput(outputId = "pvt2"),
                 tableOutput(outputId = "pvt3"),
                 tableOutput(outputId = "pvt4"),
                 tableOutput(outputId = "pvt5"),
                 tableOutput(outputId = "pvt6")
             )
      )
    )
  )
)

server <- function(input, output) {
  output$pvt1 <- renderTable({
    data_GC_post <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "No" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION,.(Sum_GMV=sum(GMV)),keyby=lstg_site]
    data_GC_pre <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "No" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION,.(Sum_GMV=sum(GMV)),keyby=lstg_site]
    data_all_post <- raw_data[DIMENSION == "No" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID,.(Sum_GMV=sum(GMV)),keyby=lstg_site]
    data_all_pre <- raw_data[DIMENSION == "No" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID,.(Sum_GMV=sum(GMV)),keyby=lstg_site]
    data.frame("Transaction Site" = data_GC_post$lstg_site, 
               "GC GMV - Post vs Pre" = abs_dif(data_GC_pre$Sum_GMV,data_GC_post$Sum_GMV),
               "Site GMV - Post vs Pre" = abs_dif(data_all_pre$Sum_GMV,data_all_post$Sum_GMV),
               "GC GMV Share - Post vs Pre" = share_dif(data_GC_pre$Sum_GMV,data_GC_post$Sum_GMV,data_all_pre$Sum_GMV,data_all_post$Sum_GMV),
               "GC GMV Contribution - Pre" = contr(data_GC_pre$Sum_GMV),
               "Contribution to GC GMV Post vs Pre Variance" = contr_var(data_GC_pre$Sum_GMV,data_GC_post$Sum_GMV),
               "Explained by site demand" = exp_by_demand(data_GC_pre$Sum_GMV,data_GC_post$Sum_GMV,data_all_pre$Sum_GMV,data_all_post$Sum_GMV),
               "Explained by GC GMV share change" = 1 - exp_by_demand(data_GC_pre$Sum_GMV,data_GC_post$Sum_GMV,data_all_pre$Sum_GMV,data_all_post$Sum_GMV)
    )
  }, align = 'c')
  
  output$pvt2 <- renderTable({
    data_diag1_site_GC_post <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "CBT_TYPE" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag1_site_GC_pre <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "CBT_TYPE" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag1_site_all_post <- raw_data[DIMENSION == "CBT_TYPE" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag1_site_all_pre <- raw_data[DIMENSION == "CBT_TYPE" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data.frame("CBT Type" = data_diag1_site_GC_post$cut, 
               "GC GMV - Post vs Pre" = abs_dif(data_diag1_site_GC_pre$Sum_GMV,data_diag1_site_GC_post$Sum_GMV),
               "Site GMV - Post vs Pre" = abs_dif(data_diag1_site_all_pre$Sum_GMV,data_diag1_site_all_post$Sum_GMV),
               "GC GMV Share - Post vs Pre" = share_dif(data_diag1_site_GC_pre$Sum_GMV,data_diag1_site_GC_post$Sum_GMV,data_diag1_site_all_pre$Sum_GMV,data_diag1_site_all_post$Sum_GMV),
               "GC GMV Contribution - Pre" = contr(data_diag1_site_GC_pre$Sum_GMV),
               "Contribution to GC GMV Post vs Pre Variance" = contr_var(data_diag1_site_GC_pre$Sum_GMV,data_diag1_site_GC_post$Sum_GMV),
               "Explained by site demand" = exp_by_demand(data_diag1_site_GC_pre$Sum_GMV,data_diag1_site_GC_post$Sum_GMV,data_diag1_site_all_pre$Sum_GMV,data_diag1_site_all_post$Sum_GMV),
               "Explained by GC GMV share change" = 1 - exp_by_demand(data_diag1_site_GC_pre$Sum_GMV,data_diag1_site_GC_post$Sum_GMV,data_diag1_site_all_pre$Sum_GMV,data_diag1_site_all_post$Sum_GMV)
    )
  }, align = 'c')
  
  output$pvt3 <- renderTable({
    data_diag2_cor_GC_post <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "CORRIDOR_CNTRY" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag2_cor_GC_pre <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "CORRIDOR_CNTRY" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag2_cor_all_post <- raw_data[DIMENSION == "CORRIDOR_CNTRY" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag2_cor_all_pre <- raw_data[DIMENSION == "CORRIDOR_CNTRY" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data.frame("Corridor Country" = data_diag2_cor_GC_post$cut, 
               "GC GMV - Post vs Pre" = abs_dif(data_diag2_cor_GC_pre$Sum_GMV,data_diag2_cor_GC_post$Sum_GMV),
               "Site GMV - Post vs Pre" = abs_dif(data_diag2_cor_all_pre$Sum_GMV,data_diag2_cor_all_post$Sum_GMV),
               "GC GMV Share - Post vs Pre" = share_dif(data_diag2_cor_GC_pre$Sum_GMV,data_diag2_cor_GC_post$Sum_GMV,data_diag2_cor_all_pre$Sum_GMV,data_diag2_cor_all_post$Sum_GMV),
               "GC GMV Contribution - Pre" = contr(data_diag2_cor_GC_pre$Sum_GMV),
               "Contribution to GC GMV Post vs Pre Variance" = contr_var(data_diag2_cor_GC_pre$Sum_GMV,data_diag2_cor_GC_post$Sum_GMV),
               "Explained by site demand" = exp_by_demand(data_diag2_cor_GC_pre$Sum_GMV,data_diag2_cor_GC_post$Sum_GMV,data_diag2_cor_all_pre$Sum_GMV,data_diag2_cor_all_post$Sum_GMV),
               "Explained by GC GMV share change" = 1 - exp_by_demand(data_diag2_cor_GC_pre$Sum_GMV,data_diag2_cor_GC_post$Sum_GMV,data_diag2_cor_all_pre$Sum_GMV,data_diag2_cor_all_post$Sum_GMV)
    )
  }, align = 'c')
  
  output$pvt4 <- renderTable({
    data_diag3_pr_GC_post <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "price_tranche" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag3_pr_GC_pre <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "price_tranche" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag3_pr_all_post <- raw_data[DIMENSION == "price_tranche" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag3_pr_all_pre <- raw_data[DIMENSION == "price_tranche" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data.frame("ASP Tranche" = data_diag3_pr_GC_post$cut, 
               "GC GMV - Post vs Pre" = abs_dif(data_diag3_pr_GC_pre$Sum_GMV,data_diag3_pr_GC_post$Sum_GMV),
               "Site GMV - Post vs Pre" = abs_dif(data_diag3_pr_all_pre$Sum_GMV,data_diag3_pr_all_post$Sum_GMV),
               "GC GMV Share - Post vs Pre" = share_dif(data_diag3_pr_GC_pre$Sum_GMV,data_diag3_pr_GC_post$Sum_GMV,data_diag3_pr_all_pre$Sum_GMV,data_diag3_pr_all_post$Sum_GMV),
               "GC GMV Contribution - Pre" = contr(data_diag3_pr_GC_pre$Sum_GMV),
               "Contribution to GC GMV Post vs Pre Variance" = contr_var(data_diag3_pr_GC_pre$Sum_GMV,data_diag3_pr_GC_post$Sum_GMV),
               "Explained by site demand" = exp_by_demand(data_diag3_pr_GC_pre$Sum_GMV,data_diag3_pr_GC_post$Sum_GMV,data_diag3_pr_all_pre$Sum_GMV,data_diag3_pr_all_post$Sum_GMV),
               "Explained by GC GMV share change" = 1 - exp_by_demand(data_diag3_pr_GC_pre$Sum_GMV,data_diag3_pr_GC_post$Sum_GMV,data_diag3_pr_all_pre$Sum_GMV,data_diag3_pr_all_post$Sum_GMV)
    )
  }, align = 'c')
  
  output$pvt5 <- renderTable({
    data_diag4_loc_GC_post <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "WH_FLAG" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag4_loc_GC_pre <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "WH_FLAG" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag4_loc_all_post <- raw_data[DIMENSION == "WH_FLAG" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag4_loc_all_pre <- raw_data[DIMENSION == "WH_FLAG" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data.frame("Item Location" = data_diag4_loc_GC_post$cut, 
               "GC GMV - Post vs Pre" = abs_dif(data_diag4_loc_GC_pre$Sum_GMV,data_diag4_loc_GC_post$Sum_GMV),
               "Site GMV - Post vs Pre" = abs_dif(data_diag4_loc_all_pre$Sum_GMV,data_diag4_loc_all_post$Sum_GMV),
               "GC GMV Share - Post vs Pre" = share_dif(data_diag4_loc_GC_pre$Sum_GMV,data_diag4_loc_GC_post$Sum_GMV,data_diag4_loc_all_pre$Sum_GMV,data_diag4_loc_all_post$Sum_GMV),
               "GC GMV Contribution - Pre" = contr(data_diag4_loc_GC_pre$Sum_GMV),
               "Contribution to GC GMV Post vs Pre Variance" = contr_var(data_diag4_loc_GC_pre$Sum_GMV,data_diag4_loc_GC_post$Sum_GMV),
               "Explained by site demand" = exp_by_demand(data_diag4_loc_GC_pre$Sum_GMV,data_diag4_loc_GC_post$Sum_GMV,data_diag4_loc_all_pre$Sum_GMV,data_diag4_loc_all_post$Sum_GMV),
               "Explained by GC GMV share change" = 1 - exp_by_demand(data_diag4_loc_GC_pre$Sum_GMV,data_diag4_loc_GC_post$Sum_GMV,data_diag4_loc_all_pre$Sum_GMV,data_diag4_loc_all_post$Sum_GMV)
    )
  }, align = 'c')
  
  output$pvt6 <- renderTable({
    data_diag5_vrtcl_GC_post <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "BSNS_VRTCL_NAME" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag5_vrtcl_GC_pre <- raw_data[SLR_CNTRY == "GC" & DIMENSION == "BSNS_VRTCL_NAME" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & SLR_REGION_OF_GC %in% input$GC_REGION & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag5_vrtcl_all_post <- raw_data[DIMENSION == "BSNS_VRTCL_NAME" & YEAR_ID == input$Post_Year_ID & WEEK_OF_YEAR_ID == input$Post_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data_diag5_vrtcl_all_pre <- raw_data[DIMENSION == "BSNS_VRTCL_NAME" & YEAR_ID == input$Pre_Year_ID & WEEK_OF_YEAR_ID == input$Pre_WEEK_OF_YEAR_ID & lstg_site %in% input$site,.(Sum_GMV=sum(GMV)),keyby=cut]
    data.frame("Vertical" = data_diag5_vrtcl_GC_post$cut, 
               "GC GMV - Post vs Pre" = abs_dif(data_diag5_vrtcl_GC_pre$Sum_GMV,data_diag5_vrtcl_GC_post$Sum_GMV),
               "Site GMV - Post vs Pre" = abs_dif(data_diag5_vrtcl_all_pre$Sum_GMV,data_diag5_vrtcl_all_post$Sum_GMV),
               "GC GMV Share - Post vs Pre" = share_dif(data_diag5_vrtcl_GC_pre$Sum_GMV,data_diag5_vrtcl_GC_post$Sum_GMV,data_diag5_vrtcl_all_pre$Sum_GMV,data_diag5_vrtcl_all_post$Sum_GMV),
               "GC GMV Contribution - Pre" = contr(data_diag5_vrtcl_GC_pre$Sum_GMV),
               "Contribution to GC GMV Post vs Pre Variance" = contr_var(data_diag5_vrtcl_GC_pre$Sum_GMV,data_diag5_vrtcl_GC_post$Sum_GMV),
               "Explained by site demand" = exp_by_demand(data_diag5_vrtcl_GC_pre$Sum_GMV,data_diag5_vrtcl_GC_post$Sum_GMV,data_diag5_vrtcl_all_pre$Sum_GMV,data_diag5_vrtcl_all_post$Sum_GMV),
               "Explained by GC GMV share change" = 1 - exp_by_demand(data_diag5_vrtcl_GC_pre$Sum_GMV,data_diag5_vrtcl_GC_post$Sum_GMV,data_diag5_vrtcl_all_pre$Sum_GMV,data_diag5_vrtcl_all_post$Sum_GMV)
    )
  }, align = 'c')
  
}

shinyApp(ui = ui, server = server)
