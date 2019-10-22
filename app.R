library(shiny)
library(shinydashboard)
library(DBI)
library(lubridate)
library(DT)
library(RColorBrewer)
library(dplyr)
library(Hmisc)
library(reshape2)
library(openxlsx)
library(echarts4r)
library(devtools)
library(RMySQL)
library(dashboardthemes)
library(htmlwidgets)
library(stringr)

# global ------------------------------------------------------------------

#连接数据库
conn <- dbConnect(MySQL(),
                  user='root',
                  password='Blbj123456',
                  dbname="blbj_crawler",
                  host='rm-bp16nmlmn159wru4reo.mysql.rds.aliyuncs.com',
                  port=3306)
# conn <- dbConnect(MySQL(),user='root',password='Blbj123456',dbname='blbj_bi',host='rm-bp16nmlmn159wru4reo.mysql.rds.aliyuncs.com',port=3306)
# dbSendQuery(conn,'SET NAMES utf8')###编码转为utf8,部署到linux服务器
dbSendQuery(conn,'SET NAMES gbk')###编码转为gbk
  
#读取数据表
#宏观经济
m2 <- dbReadTable(conn,"m2")
deposit_reserve_ratio <- dbReadTable(conn,"deposit_reserve_ratio")
benchmark_lending_rate <- dbReadTable(conn,"benchmark_lending_rate")
per_capita_disposable_income <- dbReadTable(conn,"per_capita_disposable_income")
cn_wz_cpi <- dbReadTable(conn,"cn_wz_cpi")
per_capita_disposable_income_detail <- dbReadTable(conn,"per_capita_disposable_income_detail")
cpi <- dbReadTable(conn,"cpi")
wenzhou_population <- dbReadTable(conn,"wenzhou_population")
people <- dbReadTable(conn,"people")

#一手房
first_hand_area_deal_supply <- dbReadTable(conn,"各区域供销比_python") %>% 
  as.tbl() %>% 
  mutate("区域"=substring(所在地区,first = 1,last = 2))
first_hand_deal_project_list <- dbReadTable(conn,"楼盘成交排行榜_python") %>% 
  as.tbl() %>% 
  mutate("日期"=ymd(日期),
         "单套面积"=建筑面积/套数)
first_hand_deal_project_list[is.na(first_hand_deal_project_list$项目推广名),"项目推广名"] <- 
  "暂无数据"
first_hand_history_supply <- dbReadTable(conn,"历史供应数据_python") %>% 
  as.tbl() %>% 
  mutate("开盘日期"=ymd(开盘日期),
         "单套面积"=项目测算面积/房源总量,
         "在售面积"=round(项目测算面积/房源总量*待售房源/10000,2),
         "房源总供应量"=房源总量-非销售房产,
         "去化率"=paste0(
           round((房源总量-非销售房产-待售房源)/(房源总量-非销售房产),2)*100,"%"
         ))
first_hand_history_supply[is.na(first_hand_history_supply$项目推广名),"项目推广名"] <- "暂无数据"
first_hand_history_supply[first_hand_history_supply$住宅均价.元...==0,"住宅均价.元..."] <- "暂无数据"
first_hand_history_supply[first_hand_history_supply$商业均价.元...==0,"商业均价.元..."] <- "暂无数据"

regional_inventory <- dbReadTable(conn,"各区域库存_python") %>% 
  as.tbl() %>% 
  mutate("日期"=ymd(日期),
         "区域"=substring(所在地区,first = 1,last = 2),
         "年份"=year(日期))
regional_inventory_area_rank <- dbReadTable(conn,"各区域供销比_面积段_python") %>% 
  as.tbl() %>% 
  mutate("日期"=ymd(日期))
project_name <- dbReadTable(conn,"项目推广名称_view_task") %>% 
  as.tbl() %>% 
  mutate("日期"=ymd(开盘日期))

first_hand_deal_single_room <- dbReadTable(conn,"楼盘房源详情修改_python") %>% 
  as.tbl() %>% 
  mutate("总价"=总建筑面积*单价)
first_hand_deal_single_room[is.na(first_hand_deal_single_room$总建筑面积),"总建筑面积"] <- 0
first_hand_deal_single_room[is.na(first_hand_deal_single_room$属性),"属性"] <- "暂无数据"
first_hand_deal_single_room[is.na(first_hand_deal_single_room$总价),"总价"] <- "暂无数据"
first_hand_deal_single_room[is.na(first_hand_deal_single_room$项目名称),"项目名称"] <- "暂无数据"

#土地网,二手房
land_view <- dbReadTable(conn,"土地网_view_task") %>% 
  as.tbl() %>% 
  mutate("月份"=substring(成交时间,first = 1,last = 7),
         "挂牌月份"=substring(挂牌开始时间,first = 1,last = 7),
         成交时间=ymd(成交时间),
         拍卖开始时间=ymd(拍卖开始时间),
         挂牌开始时间=ymd(挂牌开始时间),
         挂牌截止时间=ymd(挂牌截止时间),
         保证金截止时间=ymd(保证金截止时间),
         结束时间=ymd(结束时间),
         终止时间=ymd(终止时间),
         报名开始时间.拍卖.=ymd(报名开始时间.拍卖.),
         报名截止时间.拍卖.=ymd(报名截止时间.拍卖.),
         "挂牌楼面价(元/㎡)"=round(起始价*10000/(最大容积率*总用地面积.平方米.)),
         "出让楼面价(元/㎡)"=round(成交价*10000/(最大容积率*总用地面积.平方米.)),
         "占地面积(亩)"=paste0(总用地面积.平方米.,"㎡(",round(总用地面积.平方米./2000*3,1),"亩)"))
land_view[land_view$地块详情=="javascript:goRes('13752','01');","地块位置"] <- 
  "瓯海区娄桥街道杆南村"#玕,json不识别
#JSON不识别部分中文:https://bbs.csdn.net/topics/390877083?list=29549440
# land_view[is.na(land_view$土地用途),"土地用途"] <- "暂无数据"
# land_view[is.na(land_view$竞得单位),"竞得单位"] <- "暂无数据"
# land_view[is.na(land_view$出让面积),"出让面积"] <- 0
land_view[is.na(land_view$总用地面积.平方米.),"总用地面积.平方米."] <- 0
# land_view[is.na(land_view$划拨面积.平方米.),"划拨面积.平方米."] <- 0
# land_view[is.na(land_view$住宅面积.平方米.),"住宅面积.平方米."] <- 0
# land_view[is.na(land_view$商业面积.平方米.),"商业面积.平方米."] <- 0
# land_view[is.na(land_view$办公面积.平方米.),"办公面积.平方米."] <- 0
# land_view[is.na(land_view$其他面积.平方米.),"其他面积.平方米."] <- 0
# land_view[is.na(land_view$建筑密度),"建筑密度"] <- 0
# land_view[is.na(land_view$建筑高度.米.),"建筑高度.米."] <- 0
# land_view[is.na(land_view$绿地率),"绿地率"] <- 0
# land_view[is.na(land_view$总建筑面积.平方米.),"总建筑面积.平方米."] <- 0
# land_view[is.na(land_view$备注),"备注"] <- "暂无数据"
# land_view[is.na(land_view$综合楼面价),"综合楼面价"] <- "暂无数据"
# land_view[is.na(land_view$出让年限),"出让年限"] <- "暂无数据"
# land_view[is.na(land_view$最高报价单位),"最高报价单位"] <- "暂无数据"
# land_view[is.infinite(land_view$`挂牌楼面价(元/㎡)`),"挂牌楼面价(元/㎡)"] <- 0
# land_view[is.nan(land_view$`挂牌楼面价(元/㎡)`),"挂牌楼面价(元/㎡)"] <- 0
# land_view[is.infinite(land_view$`出让楼面价(元/㎡)`),"出让楼面价(元/㎡)"] <- 0
# land_view[is.nan(land_view$`出让楼面价(元/㎡)`),"出让楼面价(元/㎡)"] <- 0

House_market <- dbReadTable(conn,"房管网_房产市场") %>% 
  as.tbl() %>% 
  mutate("月份"=substring(日期,first = 1,last = 7))

land_supply_report_view_2 <- dbReadTable(conn,"土地报告_土地供应_view_2_task") %>% 
  as.tbl() %>% 
  mutate("供应时间"=gsub('/','-',供应时间),
         "月份"=substring(供应时间,first = 1,last = 7))

land_deal_report_view_2 <- dbReadTable(conn,"土地报告_土地成交_view_2_task") %>% 
  as.tbl() %>% 
  mutate("成交时间"=gsub('/','-',成交时间),
         "月份"=substring(成交时间,first = 1,last = 7))

land_deal_supply_report_view_2 <- dbReadTable(conn,"土地报告_土地供销比_view_2_task") %>% 
  as.tbl() %>% 
  filter(日期>"1999/00/01") %>% 
  mutate("日期"=gsub('/','-',日期),
         "月份"=substring(日期,first = 1,last = 7))#以此为日期筛选

land_deal_chain_ratio <- dbReadTable(conn,"土地报告_土地成交金额环比_view_2_task") %>% 
  as.tbl() %>% 
  mutate("成交时间"=gsub('/','-',成交时间),
         "月份"=substring(成交时间,first = 1,last = 7))

land_floor_price_chain_ratio <- dbReadTable(conn,"土地报告_土地楼面价环比_view_2_task") %>% 
  as.tbl() %>% 
  mutate("成交日期"=gsub('/','-',成交日期),
         "月份"=substring(成交日期,first = 1,last = 7))

land_area_floor_price_house_floor_price <- 
  dbReadTable(conn,"土地报告_月度土地楼面价与涉宅土地楼面价对比_view_2_task") %>% 
  as.tbl() %>% 
  mutate("日期"=gsub('/','-',日期),
         "月份"=substring(日期,first = 1,last = 7))

land_house_deal_supply <- dbReadTable(conn,"土地报告_涉宅土地供销比_view_2_task") %>% 
  as.tbl() %>% 
  mutate("日期"=gsub('/','-',日期),
         "月份"=substring(日期,first = 1,last = 7))

year_land_area_floor_price_house_floor_price <- 
  dbReadTable(conn,"土地报告_年度土地楼面价与涉宅土地楼面价对比_view_2_task")

year_land_house_deal_supply <- dbReadTable(conn,"土地报告_年度涉宅土地供销比_view_2_task")

land_area_sale_deal <- 
  dbReadTable(conn,"土地报告_各区域经营性用地成交总额环比情况_view_2_task") %>% 
  as.tbl() %>% 
  mutate("成交时间"=gsub('/','-',成交时间),
         "月份"=substring(成交时间,first = 1,last = 7),
         "区域"=substring(所属行政区,first = 1,last = 2))
land_area_sale_deal_mean_price <- 
  dbReadTable(conn,"土地报告_各区域经营性用地成交额均价环比情况_view_2_task") %>% 
  as.tbl() %>% 
  mutate("成交时间"=gsub('/','-',成交时间),
         "月份"=substring(成交时间,first = 1,last = 7),
         "区域"=substring(所属行政区,first = 1,last = 2))
#日期大小比较可以将月份转化为日期再比较大小,或者直接用字符串月份比较


# UI ----------------------------------------------------------------------

ui <- dashboardPage(title = "伯乐百家",
  dashboardHeader(title = 
                    shinyDashboardLogoDIY(
                      boldText = "伯乐百家",
                      mainText = "BI",
                      textSize = 18,
                      badgeText = "BETA",
                      badgeTextColor = "white",
                      badgeTextSize = 2,
                      badgeBackColor = "#D95850",
                      badgeBorderRadius = 3
                    )
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("土地市场",
               tabName = "land_market",
               icon = icon("dashboard"),
               menuSubItem("土地地块信息查询",
                           tabName = "land_block_information_query",
                           icon = icon("dashboard")),
               menuSubItem("土地挂牌出让报告(一)",
                           tabName = "land_listing_transfer_report_1",
                           icon = icon("dashboard")),
               menuSubItem("土地挂牌出让报告(二)",
                           tabName = "land_listing_transfer_report_2",
                           icon = icon("dashboard")),
               menuSubItem("土地挂牌出让报告(三)",
                           tabName = "land_listing_transfer_report_3",
                           icon = icon("dashboard"))),
      menuItem("新房数据",
               tabName = "first_hand_house_data",
               icon = icon("shopping-cart"),
               menuSubItem("单楼盘成交详情",
                           tabName = "details_of_single_building_dealing",
                           icon = icon("shopping-cart")),
               menuSubItem("新房成交数据",
                           tabName = "first_hand_house_transaction_data",
                           icon = icon("shopping-cart")),
               menuSubItem("新房供应数据",
                           tabName = "first_hand_house_supply_data",
                           icon = icon("shopping-cart")),
               menuSubItem("新房供销数据",
                           tabName = "first_hand_house_supply_and_marketing_report",
                           icon = icon("shopping-cart")),
               menuSubItem("新房去化及量价报告",
                           tabName = "first_hand_house_sales_and_volume_price_report",
                           icon = icon("shopping-cart"))),
      menuItem("二手房市",
               tabName = "second_hand_housing_market",
               icon = icon("angle-double-right")),
      menuItem("客户概况",
               tabName = "customer_profile",
               icon = icon("angle-double-right"),
               menuSubItem("伯乐客户",
                           tabName = "bole_customer",
                           icon = icon("angle-double-right")),
               menuSubItem("客户分析",
                           tabName = "customer_analysis",
                           icon = icon("angle-double-right"))),
      menuItem("宏观经济",
               tabName = "macro_economy",
               icon = icon("angle-double-right"),
               menuSubItem("金融政策",
                           tabName = "financial_policy",
                           icon = icon("angle-double-right")),
               menuSubItem("人口概况",
                           tabName = "population_profile",
                           icon = icon("angle-double-right")),
               menuSubItem("经济形势",
                           tabName = "economic_situation",
                           icon = icon("angle-double-right")))
    )
  ),
  
  dashboardBody(
    shinyDashboardThemes(theme = "grey_light"),
    tags$head(
      tags$link(rel = "stylesheet",type = "text/css",href = "style.css")
    ),
    tabItems(
      

# 土地地块信息查询_inf ----------------------------------------------------------------

      tabItem(
        tabName = "land_block_information_query",
        fluidPage(
          titlePanel("土地地块信息查询"),
          fluidRow(
            column(4,
                   dateRangeInput("land_block_information_query_date",
                                  label = "挂牌日期",
                                  start = Sys.Date() - 365,
                                  end = Sys.Date() - 1,
                                  language = "zh-CN",
                                  separator = "-",
                                  width = "100%")
            ),
            column(4,
                   dateRangeInput("land_block_information_query_deal_date",
                                  label = "成交日期",
                                  start = Sys.Date() - 365,
                                  end = Sys.Date() - 1,
                                  language = "zh-CN",
                                  separator = "-",
                                  width = "100%")
            ),
            column(2,
                   selectInput("land_block_information_query_area",
                               "区域(可多选)",
                               c(unique(as.character(land_view$所属行政区))),
                               multiple=T,
                               selected = "温州市(本级)",
                               width = "100%")),
            column(2,
                   selectInput("land_block_information_query_num",
                               "地块编号(可多选)",
                               c(unique(as.character(land_view$X.地块编号))),
                               multiple = T,
                               width = "100%")),
            column(4,
                   selectizeInput("land_block_information_query_location",
                               "地块位置(可多选)",
                               c(unique(as.character(land_view$地块位置))),
                               multiple = T,
                               width = "100%")),
            column(4,
                   selectInput("land_block_information_query_uses",
                               "土地用途(可多选)",
                               c(unique(as.character(land_view$土地用途))),
                               multiple = T,
                               width = "100%")),
            column(2,
                   textInput("land_block_information_query_min",
                             "最小面积",
                             width = "100%",
                             placeholder = ">=")),
            column(2,
                   textInput("land_block_information_query_max",
                             "最大面积",
                             width = "100%",
                             placeholder = "<="))
          ),
          fluidRow(
            valueBoxOutput("land_block_information_query_valuebox_1",width = 4),
            valueBoxOutput("land_block_information_query_valuebox_2",width = 4),
            valueBoxOutput("land_block_information_query_valuebox_3",width = 4),
            valueBoxOutput("land_block_information_query_valuebox_4",width = 4),
            valueBoxOutput("land_block_information_query_valuebox_5",width = 4),
            valueBoxOutput("land_block_information_query_valuebox_6",width = 4)
          ),
          fluidRow(
            column(12,
                   dataTableOutput("land_block_information_query_datatable"))
          )
        )
      ),
# 土地挂牌出让报告(一)_inf -------------------------------------------------------------

      
      tabItem(
        tabName = "land_listing_transfer_report_1",
        fluidPage(
          titlePanel("土地挂牌出让报告(一)"),
          fluidRow(
            column(3,
                   selectInput("land_supply_report_view_2_month",
                               "日期",
                               c(unique(as.character(land_supply_report_view_2$月份))),
                               selected = "2019-02",
                               width = "100%"))
          ),
          fluidRow(
            valueBoxOutput("land_supply_report_view_2_count",width = 4),
            valueBoxOutput("land_supply_report_view_2_area",width = 4),
            valueBoxOutput("land_supply_report_view_2_chain_ratio",width = 4),
            valueBoxOutput("land_deal_report_view_2_count",width = 4),
            valueBoxOutput("land_deal_report_view_2_area",width = 4),
            valueBoxOutput("land_deal_report_view_2_chain_ratio",width = 4)
          ),
          fluidRow(
            column(3,
                   selectInput("land_deal_supply_report_view_2_month_1",
                               "开始日期",
                               land_deal_supply_report_view_2$月份,
                               selected = "2018-07",
                               width = "100%")),
            column(3,
                   selectInput("land_deal_supply_report_view_2_month_2",
                               "截止日期",
                               land_deal_supply_report_view_2$月份,
                               selected = "2019-02",
                               width = "100%"))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("land_deal_supply_report_1")),
                column(6,
                       echarts4rOutput("land_deal_supply_report_2")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("land_deal_supply_report_3")),
                column(6,
                       echarts4rOutput("land_deal_supply_report_4")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("land_deal_supply_report_5")),
                column(6,
                       echarts4rOutput("land_deal_supply_report_6")))
          ),
          fluidRow(
            box(width = 12,
                column(12,
                       echarts4rOutput("land_deal_supply_report_7")))
          )
        )
      ),


# 土地挂牌出让报告(二)_inf ---------------------------------------------------------

      tabItem(
        tabName = "land_listing_transfer_report_2",
        fluidPage(
          titlePanel("土地挂牌出让报告(二)"),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("year_area_draw_1")),
                column(6,
                       echarts4rOutput("year_area_draw_2")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("year_area_draw_3")),
                column(6,
                       echarts4rOutput("year_area_draw_4")))
          ),
          fluidRow(
            box(width = 12,
                column(12,
                       echarts4rOutput("year_area_draw_5")))
          ),
          fluidRow(
            box(width = 12,
                selectInput("wz_area_deal_month",
                            "出让时间",
                            str_sort(unique(land_view$月份))[-1],
                            selected = "2019-01"),
                column(4,
                       echarts4rOutput("land_pie_1")),
                column(4,
                       echarts4rOutput("land_pie_2")),
                column(4,
                       echarts4rOutput("land_pie_3")))
          ),
          fluidRow(
            box(width = 12,
                fluidRow(
                  column(3,
                         selectInput("land_area_sale_deal_area_select",
                                     "区域(可多选)",
                                     c(unique(as.character(land_area_sale_deal$区域))),
                                     multiple=T,
                                     selected = c(unique(as.character(land_area_sale_deal$区域))),
                                     width = "100%")),
                  column(3,
                         selectInput("land_area_sale_deal_month",
                                     "出让时间",
                                     str_sort(unique(land_area_sale_deal$月份)),
                                     selected = "2019-01",
                                     width = "100%"))
                ),
                fluidRow(
                  column(6,
                         echarts4rOutput("land_area_sale_deal_draw_1")),
                  column(6,
                         echarts4rOutput("land_area_sale_deal_draw_2"))))
          ),
          fluidRow(
            box(width = 12,
                fluidRow(
                  column(3,
                         selectInput("land_for_sale_draw_1_month_1",
                                     "挂牌开始日期",
                                     str_sort(unique(land_view$挂牌月份))[-1],
                                     selected = "2018-10",
                                     width = "100%")),
                  column(3,
                         selectInput("land_for_sale_draw_1_month_2",
                                     "挂牌截止日期",
                                     str_sort(unique(land_view$挂牌月份))[-1],
                                     selected = "2019-04",
                                     width = "100%"))
                ),
                fluidRow(
                  column(6,
                         echarts4rOutput("land_for_sale_draw_1")),
                  column(6,
                         echarts4rOutput("land_for_sale_draw_2")))
                )
          )
        )
      ),


# 土地挂牌出让报告(三)_inf ---------------------------------------------------------

      tabItem(
        tabName ="land_listing_transfer_report_3",
        fluidPage(
          titlePanel("土地挂牌出让报告(三)"),
          fluidRow(
            column(3,
                   selectInput("land_for_sale_draw_3_month_1",
                               "开始日期",
                               str_sort(unique(land_view$月份))[-1],
                               selected = "2018-10",
                               width = "100%")),
            column(3,
                   selectInput("land_for_sale_draw_3_month_2",
                               "截止日期",
                               str_sort(unique(land_view$月份))[-1],
                               selected = "2019-05",
                               width = "100%"))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("land_listing_transfer_report_3_1")),
                column(6,
                       echarts4rOutput("land_listing_transfer_report_3_2")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("land_listing_transfer_report_3_3")),
                column(6,
                       echarts4rOutput("land_listing_transfer_report_3_4")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("land_listing_transfer_report_3_5")),
                column(6,
                       echarts4rOutput("land_listing_transfer_report_3_6")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("land_listing_transfer_report_3_7")),
                column(6,
                       echarts4rOutput("land_listing_transfer_report_3_8")))
          ),
          fluidRow(
            column(12,
                   dataTableOutput("land_listing_transfer_report_datatable"))
          )
        )
      ),


# 单楼盘成交详情_inf ------------------------------------------------------------------

      tabItem(
        tabName = "details_of_single_building_dealing",
        fluidPage(
          titlePanel("单楼盘成交详情"),
          fluidRow(
            column(2,
                   selectInput("details_of_single_building_dealing_item",
                               "项目名称",
                               c(unique(as.character(first_hand_deal_single_room$项目推广名))),
                               multiple=T,
                               selected = "多弗绿城·翠湖里",
                               width = "100%")),
            column(2,
                   textInput("details_of_single_building_dealing_min",
                             "最小面积",
                             width = "100%",
                             placeholder = ">=")),
            column(2,
                   textInput("details_of_single_building_dealing_max",
                             "最大面积",
                             width = "100%",
                             placeholder = "<=")),
            column(2,
                   selectInput("details_of_single_building_dealing_building",
                               "幢名",
                               unique(first_hand_deal_single_room$幢名),
                               multiple=T,
                               width = "100%")),
            column(2,
                   selectInput("details_of_single_building_dealing_type",
                               "属性",
                               unique(first_hand_deal_single_room$属性),
                               multiple=T,
                               width = "100%"))
          ),
          fluidRow(
            valueBoxOutput("details_of_single_building_dealing_valuebox_1",width = 4),
            valueBoxOutput("details_of_single_building_dealing_valuebox_2",width = 4),
            valueBoxOutput("details_of_single_building_dealing_valuebox_3",width = 4)
          ),
          fluidRow(
            column(12,
                   dataTableOutput("details_of_single_building_dealing_datatable_1"))
          )
        )
      ),

# 新房成交数据_inf ------------------------------------------------------------------

      tabItem(
        tabName = "first_hand_house_transaction_data",
        fluidPage(
          titlePanel("新房成交数据"),
          fluidRow(
            column(4,
              dateRangeInput("first_hand_house_transaction_data_date",
                             label = "成交日期",
                             start = Sys.Date() - 30,
                             end = Sys.Date() - 1,
                             language = "zh-CN",
                             separator = "-",
                             width = "100%")
            ),
            column(3,
                   selectizeInput("first_hand_house_transaction_data_area",
                               "区域(可多选)",
                               c(unique(as.character(first_hand_deal_project_list$所在地区))),
                               multiple=T,
                               selected = "鹿城区",
                               width = "100%",
                              options = list(maxItems  = 3))),
            column(1,
                   selectInput("first_hand_house_transaction_data_type",
                               "类别",
                               c(unique(as.character(first_hand_deal_project_list$`类别_2`))),
                               selected = "住宅",
                               width = "100%")),
            column(2,
                   textInput("first_hand_house_transaction_data_min",
                                "最小面积",
                             width = "100%",
                             placeholder = ">=")),
            column(2,
                   textInput("first_hand_house_transaction_data_max",
                                "最大面积",
                             width = "100%",
                             placeholder = "<="))
          ),
          fluidRow(
            valueBoxOutput("first_hand_house_transaction_data_valuebox_1",width = 4),
            valueBoxOutput("first_hand_house_transaction_data_valuebox_2",width = 4),
            valueBoxOutput("first_hand_house_transaction_data_valuebox_3",width = 4)
          ),
          fluidRow(
            column(12,
                   dataTableOutput("first_hand_house_transaction_data_datatable"))
          )
        )
      ),


# 新房供应数据_inf ------------------------------------------------------------------
      tabItem(
        tabName = "first_hand_house_supply_data",
        fluidPage(
          titlePanel("新房供应数据"),
          fluidRow(
            column(2,
                   selectInput("first_hand_house_supply_data_item",
                               "项目名称",
                               c(unique(as.character(first_hand_history_supply$项目推广名))),
                               multiple=T,
                               width = "100%")),
            column(4,
                   dateRangeInput("first_hand_house_supply_data_date",
                                  label = "供应日期",
                                  start = Sys.Date() - 180,
                                  end = Sys.Date() - 1,
                                  language = "zh-CN",
                                  separator = "-",
                                  width = "100%")
            ),
            column(2,
                   selectInput("first_hand_house_supply_data_area",
                               "区域(可多选)",
                               c(unique(as.character(first_hand_history_supply$所在地区))),
                               multiple=T,
                               selected = "鹿城区",
                               width = "100%")),
            column(2,
                   textInput("first_hand_house_supply_data_min",
                             "最小面积",
                             width = "100%",
                             placeholder = ">=")),
            column(2,
                   textInput("first_hand_house_supply_data_max",
                             "最大面积",
                             width = "100%",
                             placeholder = "<="))
          ),
          fluidRow(
            valueBoxOutput("first_hand_house_supply_data_valuebox_1",width = 4),
            valueBoxOutput("first_hand_house_supply_data_valuebox_2",width = 4),
            valueBoxOutput("first_hand_house_supply_data_valuebox_3",width = 4)
          ),
          fluidRow(
            column(12,
                   dataTableOutput("first_hand_house_supply_data_datatable"))
          )
        )
      ),

# 新房供销报告_inf ------------------------------------------------------------------

      tabItem(
        tabName = "first_hand_house_supply_and_marketing_report",
        fluidPage(
          id = "first_hand_house_supply_and_marketing_report",
          titlePanel("新房供销报告"),
          tabBox(
            width = 12,
            tabPanel("在售&成交",
                     fluidRow(
                       column(4,
                              dateRangeInput("first_hand_house_supply_and_marketing_report_date",
                                             label = "日期",
                                             start = Sys.Date() - 180,
                                             end = Sys.Date() - 1,
                                             language = "zh-CN",
                                             separator = "-",
                                             width = "100%")
                       ),
                       column(2,
                              selectInput("first_hand_house_supply_and_marketing_report_area",
                                          "区域(可多选)",
                                          c(unique(as.character(first_hand_history_supply$所在地区))),
                                          multiple=T,
                                          selected = "鹿城区",
                                          width = "100%")),
                       column(2,
                              textInput("first_hand_house_supply_and_marketing_report_min",
                                        "最小面积",
                                        width = "100%",
                                        placeholder = ">")),
                       column(2,
                              textInput("first_hand_house_supply_and_marketing_report_max",
                                        "最大面积",
                                        width = "100%",
                                        placeholder = "<="))
                     ),
                     fluidRow(
                       valueBoxOutput(
                         "first_hand_house_supply_and_marketing_report_1_1",width = 2),
                       valueBoxOutput(
                         "first_hand_house_supply_and_marketing_report_1_2",width = 2),
                       valueBoxOutput(
                         "first_hand_house_supply_and_marketing_report_1_3",width = 2),
                       valueBoxOutput(
                         "first_hand_house_supply_and_marketing_report_1_4",width = 3),
                       valueBoxOutput(
                         "first_hand_house_supply_and_marketing_report_1_5",width = 3)
                     )),
            tabPanel("库存",
                     fluidRow(
                       column(2,
                              dateInput("regional_inventory_date",
                                             label = "选择截止日期",
                                             value = Sys.Date() -1,
                                             language = "zh-CN",
                                             width = "100%")
                       ),
                       column(2,
                              selectInput("regional_inventory_area",
                                          "区域(可多选)",
                                          c(unique(as.character(regional_inventory$所在地区))),
                                          multiple=T,
                                          selected = "鹿城区",
                                          width = "100%")),
                       column(4,
                              selectInput("regional_inventory_square",
                                        "面积段(可多选)",
                                        c(unique(as.character(regional_inventory$面积段))),
                                        multiple=T,
                                        selected = c(unique(as.character(regional_inventory$面积段))),
                                        width = "100%"))
                     ),
                     fluidRow(
                       valueBoxOutput("regional_inventory_valuebox_1",width = 6),
                       valueBoxOutput("regional_inventory_valuebox_2",width = 6)
                     )),
            tabPanel("供销比",
                     fluidRow(
                       column(4,
                              dateRangeInput("regional_inventory_area_rank_date",
                                             label = "日期",
                                             start = Sys.Date() - 60,
                                             end = Sys.Date() - 1,
                                             language = "zh-CN",
                                             separator = "-",
                                             width = "100%")),
                       column(2,
                              selectInput("regional_inventory_area_rank_area",
                                          "区域(可多选)",
                                          c(unique(as.character(regional_inventory_area_rank$所在地区))),
                                          multiple=T,
                                          selected = "鹿城区",
                                          width = "100%")),
                       column(4,
                              selectInput("regional_inventory_area_rank_square",
                                          "面积段(可多选)",
                                          c(unique(as.character(regional_inventory_area_rank$面积段))),
                                          multiple=T,
                                          selected = 
                                            c(unique(as.character(regional_inventory_area_rank$面积段))),
                                          width = "100%"))
                     ),
                     fluidRow(
                       column(width = 4,
                              echarts4rOutput("regional_inventory_area_rank_draw_1")),
                       column(width = 8,
                              echarts4rOutput("regional_inventory_area_rank_draw_2"))
                     ))
          ),
          fluidRow(
            box(width = 12,
                fluidRow(
                  column(4,
                         dateRangeInput("regional_inventory_area_rank_regional_date",
                                        label = "日期",
                                        start = Sys.Date() - 180,
                                        end = Sys.Date() - 1,
                                        language = "zh-CN",
                                        separator = "-",
                                        width = "100%"))
                ),
                fluidRow(
                  column(12,
                         echarts4rOutput("regional_inventory_area_rank_area_deal_supply_draw"))
                ))
          ),
          fluidRow(
            box(width = 12,
                fluidRow(
                  column(2,
                         selectInput("regional_inventory_year_select",
                                     "选择截止年份",
                                     c(
                                       sort(unique(as.character(regional_inventory$年份)),decreasing = T)
                                     ),
                                     selected = max(regional_inventory$年份)))
                ),
                fluidRow(
                  column(6,
                         echarts4rOutput("regional_inventory_year_draw_1")),
                  column(6,
                         echarts4rOutput("regional_inventory_year_draw_2"))
                ))
          )
        )
      ),
# 新房去化及量价报告_inf ---------------------------------------------------------------

      tabItem(
        tabName = "first_hand_house_sales_and_volume_price_report",
        fluidPage(
          titlePanel("新房去化及量价报告"),
          fluidRow(
            box(width = 12,
                dateRangeInput('first_hand_deal_project_list_date',
                               label = '日期',
                               start = Sys.Date() - 30, 
                               end = Sys.Date() - 1,
                               language = "zh-CN",
                               separator = "-"
                ),
                column(6,
                       echarts4rOutput("first_hand_deal_project_list_draw_1")),
                column(6,
                       echarts4rOutput("first_hand_deal_project_list_draw_2")))
          ),
          fluidRow(
            box(width = 12,
                fluidRow(
                  column(3,
                         selectInput("first_hand_deal_project_list_area",
                                     "区域(可多选)",
                                     c(unique(as.character(first_hand_deal_project_list$所在地区))),
                                     multiple=T,
                                     selected = "鹿城区",
                                     width = "100%"))
                ),
                fluidRow(
                  column(6,
                         echarts4rOutput("first_hand_deal_project_list_draw_3")),
                  column(6,
                         echarts4rOutput("first_hand_deal_project_list_draw_4"))
                ))
          ),
          fluidRow(
            column(width = 12,
                   dataTableOutput("first_hand_deal_project_list_datatable"))
          )
          )
      ),


# 二手房市_inf --------------------------------------------------------------------

      tabItem(
        tabName = "second_hand_housing_market",
        fluidPage(
          titlePanel("二手房市"),
          fluidRow(
            column(2,
                   selectInput("housing_market_month_select",
                               "月份",
                               c(unique(as.character(House_market$月份))),
                               selected = last(unique(as.character(House_market$月份))))),
            column(3,
                   selectInput("housing_market_area_select",
                               "区域(可多选)",
                               c(unique(as.character(House_market$所在区域))),
                               multiple=T,
                               selected = c(unique(as.character(House_market$所在区域))),
                               width = "100%"))
          ),
          fluidRow(
            box(
              width = 12,
              column(12,
                     echarts4rOutput("housing_market_month_select_deal_area"))
            )
          ),
          fluidRow(
            box(
              width = 12,
              column(6,
                     echarts4rOutput("house_market_deal")),
              column(6,
                     echarts4rOutput("house_market_deal_area"))
            )
          )
        )
      ),

# 金融政策_inf ----------------------------------------------------------------

      
      tabItem(
        tabName = "financial_policy",
        fluidPage(
          titlePanel("金融政策"),
          fluidRow(
            valueBoxOutput("last_month_m1",width = 4),
            valueBoxOutput("last_month_m2",width = 4),
            valueBoxOutput("benchmark_lending_rate_last",width = 4)
          ),
          fluidRow(
            valueBoxOutput("deposit_reserve_ratio_l",width = 2),
            valueBoxOutput("deposit_reserve_ratio_ms",width = 2),
            valueBoxOutput("per_capita_disposable_income_city",width = 2),
            valueBoxOutput("per_capita_disposable_income_country",width = 2),
            valueBoxOutput("cn_wz_cpi_cn",width = 2),
            valueBoxOutput("cn_wz_cpi_wz",width = 2)
          ),
          fluidRow(
            box(width = 12,
                selectInput("wz_area_select_income_detail",
                            "区域",
                            unique(per_capita_disposable_income_detail$区域),
                            selected = "全市"),
                column(6,
                       echarts4rOutput("per_capita_disposable_income_draw")),
                column(6,
                       echarts4rOutput("per_capita_disposable_income_detail_draw")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("cn_wz_people_cpi")),
                column(6,
                       echarts4rOutput("deposit_reserve_ratio_line")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("benchmark_lending_rate_cn")),
                column(6,
                       echarts4rOutput("cn_M2_real_estate_loan_quantity")))
          ),
          fluidRow(
            box(width = 12,
                column(6,
                       echarts4rOutput("cn_his_M1_M2_increase")),
                column(6,
                       tags$p(tags$strong("1、M1"),"是重要的流动性和经济活跃度指标。文章《",tags$strong("M1"),"对股市、房地产市场的影响》认为：",tags$strong("M1"),"增加，投资者信心增强，经济活跃度提高，股市和房地产市场上涨；反之，",tags$strong("M1"),"减少，股市和房地产市场下跌，因此，股市和房地产市场具有晴雨表功能，并对货币变化具有放大效应。"),
                       tags$p(tags$strong("2、如果M1增速大于M2"),"，意味着企业的活期存款增速大于定期存款增速，企业和居民交易活跃，",tags$strong("微观主体盈利能力较强，经济景气度上升。")),
                       tags$p(tags$strong("3、如果M1增速小于M2"),"，表明企业和居民选择将资金以定期的形式存在银行，",tags$strong("微观个体盈利能力下降，未来可选择的投资机会有限，多余的资金开始从实体经济中沉淀下来，经济运行回落。")),
                       tags$p(tags$strong("4、M1"),"与上证综指的相互影响关系中，文章《货币供应与股票价格关系的实证分析》认为：",tags$strong("上证综指的变动作为原因引起M1的变动是占主导地位的"),"，而",tags$strong("M1"),"的变动引起上证综指的变动则是次要的。")))
          )
        )
      ),

# 人口概况_inf ----------------------------------------------------------------

      
      tabItem(
        tabName = "population_profile",
        fluidPage(
          titlePanel("人口概况"),
          fluidRow(
            valueBoxOutput("wz_people",width = 6),
            valueBoxOutput("wz_city_people_per",width = 6)
          ),
          fluidRow(
            box(width = 12,
                selectInput("wz_area_select_people",
                            "区域",
                            unique(people$区域),
                            selected = "全市"),
                column(4,
                       echarts4rOutput("wz_area_population_per")),
                column(4,
                       echarts4rOutput("wz_total_people_growth_rate")),
                column(4,
                       echarts4rOutput("wz_city_population_per")))
          )
        )
      ),

# 经济形势_inf ----------------------------------------------------------------

      
      tabItem(
        tabName = "economic_situation",
        fluidPage(
          titlePanel("经济形势"),
          fluidRow(
            valueBoxOutput("wz_GDP_total",width = 6),
            valueBoxOutput("wz_per_GDP",width = 6)
          ),
          fluidRow(
            box(width = 12,
                selectInput("area_select_right",
                            "区域",
                            unique(people$区域),
                            selected = "全市"),
                column(4,
                       echarts4rOutput("wz_area_percent_GDP")),
                column(8,
                       echarts4rOutput("wz_GDP_total_line")))
          )
        )
      )
    )
    
  )
)


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
# 土地地块信息查询 ----------------------------------------------------------------

  output$land_block_information_query_valuebox_1 <- renderValueBox({
    a <- if(is.null(input$land_block_information_query_num)){
      unique(land_view$X.地块编号)
    }else{
      input$land_block_information_query_num
    }
    b <- if(is.null(input$land_block_information_query_location)){
      unique(land_view$地块位置)
    }else{
      input$land_block_information_query_location
    }
    c <- if(is.null(input$land_block_information_query_uses)){
      unique(land_view$土地用途)
    }else{
      input$land_block_information_query_uses
    }
    land_view %>%
      as.tbl() %>%
      filter(挂牌开始时间>=input$land_block_information_query_date[1]&
                     挂牌开始时间<=input$land_block_information_query_date[2]&
                     成交时间>=input$land_block_information_query_deal_date[1]&
                     成交时间<=input$land_block_information_query_deal_date[2]&
                     所属行政区%in%input$land_block_information_query_area) %>%
      filter(X.地块编号%in%a&
               地块位置%in%b&
               土地用途%in%c) %>%
      filter(总用地面积.平方米.>=if_else(input$land_block_information_query_min=="",
                          0,
                          as.numeric(input$land_block_information_query_min))) %>% 
      filter(总用地面积.平方米.<=if_else(input$land_block_information_query_max=="",
                           100000000,
                           as.numeric(input$land_block_information_query_max))) %>%
      summarise("挂牌宗数"=n()) %>%
      paste0("宗") %>%
      valueBox(subtitle = "挂牌宗数")
  })
  
  output$land_block_information_query_valuebox_2 <- renderValueBox({
    a <- if(is.null(input$land_block_information_query_num)){
      unique(land_view$X.地块编号)
    }else{
      input$land_block_information_query_num
    }
    b <- if(is.null(input$land_block_information_query_location)){
      unique(land_view$地块位置)
    }else{
      input$land_block_information_query_location
    }
    c <- if(is.null(input$land_block_information_query_uses)){
      unique(land_view$土地用途)
    }else{
      input$land_block_information_query_uses
    }
    land_view %>%
      as.tbl() %>%
      filter(挂牌开始时间>=input$land_block_information_query_date[1]&
                     挂牌开始时间<=input$land_block_information_query_date[2]&
                     成交时间>=input$land_block_information_query_deal_date[1]&
                     成交时间<=input$land_block_information_query_deal_date[2]&
                     所属行政区%in%input$land_block_information_query_area) %>%
      filter(X.地块编号%in%a&
               地块位置%in%b&
               土地用途%in%c) %>%
      filter(总用地面积.平方米.>=if_else(input$land_block_information_query_min=="",
                                 0,
                                 as.numeric(input$land_block_information_query_min))) %>% 
      filter(总用地面积.平方米.<=if_else(input$land_block_information_query_max=="",
                                 100000000,
                                 as.numeric(input$land_block_information_query_max))) %>%
      summarise("挂牌面积"=round(sum(总用地面积.平方米.)/10000,1)) %>%
      paste0("万㎡") %>%
      valueBox(subtitle = "挂牌面积")
  })
  
  output$land_block_information_query_valuebox_3 <- renderValueBox({
    a <- if(is.null(input$land_block_information_query_num)){
      unique(land_view$X.地块编号)
    }else{
      input$land_block_information_query_num
    }
    b <- if(is.null(input$land_block_information_query_location)){
      unique(land_view$地块位置)
    }else{
      input$land_block_information_query_location
    }
    c <- if(is.null(input$land_block_information_query_uses)){
      unique(land_view$土地用途)
    }else{
      input$land_block_information_query_uses
    }
    land_view %>%
      as.tbl() %>%
      filter(挂牌开始时间>=input$land_block_information_query_date[1]&
                     挂牌开始时间<=input$land_block_information_query_date[2]&
                     成交时间>=input$land_block_information_query_deal_date[1]&
                     成交时间<=input$land_block_information_query_deal_date[2]&
                     所属行政区%in%input$land_block_information_query_area) %>%
      filter(X.地块编号%in%a&
               地块位置%in%b&
               土地用途%in%c) %>%
      filter(总用地面积.平方米.>=if_else(input$land_block_information_query_min=="",
                                 0,
                                 as.numeric(input$land_block_information_query_min))) %>% 
      filter(总用地面积.平方米.<=if_else(input$land_block_information_query_max=="",
                                 100000000,
                                 as.numeric(input$land_block_information_query_max))) %>%
      summarise("挂牌楼面价"=round(
        sum(起始价,na.rm=T)*10000/sum(最大容积率*总用地面积.平方米.,na.rm=T),0
      )) %>%
      paste0("元/㎡") %>%
      valueBox(subtitle = "挂牌楼面价")
  })
  
  output$land_block_information_query_valuebox_4 <- renderValueBox({
    a <- if(is.null(input$land_block_information_query_num)){
      unique(land_view$X.地块编号)
    }else{
      input$land_block_information_query_num
    }
    b <- if(is.null(input$land_block_information_query_location)){
      unique(land_view$地块位置)
    }else{
      input$land_block_information_query_location
    }
    c <- if(is.null(input$land_block_information_query_uses)){
      unique(land_view$土地用途)
    }else{
      input$land_block_information_query_uses
    }
    land_view %>%
      as.tbl() %>%
      filter(挂牌开始时间>=input$land_block_information_query_date[1]&
                     挂牌开始时间<=input$land_block_information_query_date[2]&
                     成交时间>=input$land_block_information_query_deal_date[1]&
                     成交时间<=input$land_block_information_query_deal_date[2]&
                     所属行政区%in%input$land_block_information_query_area&
                   成交价>0) %>%
      filter(X.地块编号%in%a&
               地块位置%in%b&
               土地用途%in%c) %>%
      filter(总用地面积.平方米.>=if_else(input$land_block_information_query_min=="",
                                 0,
                                 as.numeric(input$land_block_information_query_min))) %>% 
      filter(总用地面积.平方米.<=if_else(input$land_block_information_query_max=="",
                                 100000000,
                                 as.numeric(input$land_block_information_query_max))) %>%
      summarise("出让宗数"=n()) %>%
      paste0("宗") %>%
      valueBox(subtitle = "出让宗数")
  })
  
  output$land_block_information_query_valuebox_5 <- renderValueBox({
    a <- if(is.null(input$land_block_information_query_num)){
      unique(land_view$X.地块编号)
    }else{
      input$land_block_information_query_num
    }
    b <- if(is.null(input$land_block_information_query_location)){
      unique(land_view$地块位置)
    }else{
      input$land_block_information_query_location
    }
    c <- if(is.null(input$land_block_information_query_uses)){
      unique(land_view$土地用途)
    }else{
      input$land_block_information_query_uses
    }
    land_view %>%
      as.tbl() %>%
      filter(挂牌开始时间>=input$land_block_information_query_date[1]&
                     挂牌开始时间<=input$land_block_information_query_date[2]&
                     成交时间>=input$land_block_information_query_deal_date[1]&
                     成交时间<=input$land_block_information_query_deal_date[2]&
                     所属行政区%in%input$land_block_information_query_area&
                   成交价>0) %>%
      filter(X.地块编号%in%a&
               地块位置%in%b&
               土地用途%in%c) %>%
      filter(总用地面积.平方米.>=if_else(input$land_block_information_query_min=="",
                                 0,
                                 as.numeric(input$land_block_information_query_min))) %>% 
      filter(总用地面积.平方米.<=if_else(input$land_block_information_query_max=="",
                                 100000000,
                                 as.numeric(input$land_block_information_query_max))) %>%
      summarise("出让面积"=round(sum(总用地面积.平方米.)/10000,1)) %>%
      paste0("万㎡") %>%
      valueBox(subtitle = "出让面积")
  })
  
  output$land_block_information_query_valuebox_6 <- renderValueBox({
    a <- if(is.null(input$land_block_information_query_num)){
      unique(land_view$X.地块编号)
    }else{
      input$land_block_information_query_num
    }
    b <- if(is.null(input$land_block_information_query_location)){
      unique(land_view$地块位置)
    }else{
      input$land_block_information_query_location
    }
    c <- if(is.null(input$land_block_information_query_uses)){
      unique(land_view$土地用途)
    }else{
      input$land_block_information_query_uses
    }
    land_view %>%
      as.tbl() %>%
      filter(挂牌开始时间>=input$land_block_information_query_date[1]&
                     挂牌开始时间<=input$land_block_information_query_date[2]&
                     成交时间>=input$land_block_information_query_deal_date[1]&
                     成交时间<=input$land_block_information_query_deal_date[2]&
                     所属行政区%in%input$land_block_information_query_area&
                   成交价>0) %>%
      filter(X.地块编号%in%a&
               地块位置%in%b&
               土地用途%in%c) %>%
      filter(总用地面积.平方米.>=if_else(input$land_block_information_query_min=="",
                                 0,
                                 as.numeric(input$land_block_information_query_min))) %>% 
      filter(总用地面积.平方米.<=if_else(input$land_block_information_query_max=="",
                                 100000000,
                                 as.numeric(input$land_block_information_query_max))) %>%
      summarise("出让楼面价"=round(
        sum(成交价,na.rm=T)*10000/sum(最大容积率*总用地面积.平方米.,na.rm=T),0
      )) %>%
      paste0("元/㎡") %>%
      valueBox(subtitle = "出让楼面价")
  })
  
  output$land_block_information_query_datatable <- renderDataTable({
    a <- if(is.null(input$land_block_information_query_num)){
      unique(land_view$X.地块编号)
    }else{
      input$land_block_information_query_num
    }
    b <- if(is.null(input$land_block_information_query_location)){
      unique(land_view$地块位置)
    }else{
      input$land_block_information_query_location
    }
    c <- if(is.null(input$land_block_information_query_uses)){
      unique(land_view$土地用途)
    }else{
      input$land_block_information_query_uses
    }
    land_view %>% 
      as.tbl() %>% 
      filter(挂牌开始时间>=input$land_block_information_query_date[1]&
                     挂牌开始时间<=input$land_block_information_query_date[2]&
                     成交时间>=input$land_block_information_query_deal_date[1]&
                     成交时间<=input$land_block_information_query_deal_date[2]&
                     所属行政区%in%input$land_block_information_query_area) %>%
      filter(X.地块编号%in%a&
               地块位置%in%b&
               土地用途%in%c) %>%
      filter(总用地面积.平方米.>=if_else(input$land_block_information_query_min=="",
                                 0,
                                 as.numeric(input$land_block_information_query_min))) %>% 
      filter(总用地面积.平方米.<=if_else(input$land_block_information_query_max=="",
                                 100000000,
                                 as.numeric(input$land_block_information_query_max))) %>% 
      select(地块状态,
             "区域"=所属行政区,
             地块名称,
             "地块编号"=X.地块编号,
             地块位置,
             土地用途,
             "挂牌时间"=挂牌开始时间,
             挂牌截止时间,
             终止时间,
             成交时间,
             竞得单位,
             `占地面积(亩)`,
             容积率,
             "住宅面积(㎡)"=住宅面积.平方米.,
             "商业面积(㎡)"=商业面积.平方米.,
             "建筑面积(㎡)"=总建筑面积.平方米.,
             "挂牌价(万元)"=起始价,
             "出让价(万元)"=成交价,
             建筑密度,
             绿地率,
             `挂牌楼面价(元/㎡)`,
             `出让楼面价(元/㎡)`) %>% 
      datatable(class = "cell-border",width = "100%",extensions = "Scroller",
                options = list(autoWidth = T,ordering = F,scroller = T,scrollX = TRUE,
                               scrollY = 240,dom = 't',deferRender=T,
                               columnDefs = list(
                                 list(width = '10%',className = 'dt-center', 
                                      targets = c(2)),
                                 list(width = '15%',className = 'dt-center', 
                                      targets = c(1,4,5,12,13)),
                                 list(width = '20%',className = 'dt-center', 
                                      targets = c(6,7,8,9,10,14,15,16,17)),
                                 list(width = '25%',className = 'dt-center',
                                      targets = c(3,11,18,19,20,21,22))),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({
                                 'background-color': '#E5E7EE', 'color': '#444444'
                                 });",
                                 "}")))
  })
  
# 土地挂牌出让报告(一) -------------------------------------------------------------

  
  output$land_supply_report_view_2_count <- renderValueBox({
    land_supply_report_view_2 %>% 
      as.tbl() %>% 
      filter(月份%in%input$land_supply_report_view_2_month) %>% 
      select(土地供应宗数) %>% 
      paste0("宗") %>%
      valueBox(subtitle = "土地挂牌宗数")
  })
  
  output$land_supply_report_view_2_area <- renderValueBox({
    land_supply_report_view_2 %>% 
      as.tbl() %>% 
      filter(月份%in%input$land_supply_report_view_2_month) %>% 
      transmute(土地供应面积/10000) %>%
      round(2) %>% 
      paste0("万㎡") %>%
      valueBox(subtitle = "土地挂牌面积")
  })
  
  output$land_supply_report_view_2_chain_ratio <- renderValueBox({
    land_supply_report_view_2 %>% 
      as.tbl() %>% 
      filter(月份%in%input$land_supply_report_view_2_month) %>% 
      transmute(土地供应量环比*100) %>%
      round(1) %>% 
      paste0("%") %>%
      valueBox(subtitle = "土地挂牌量环比")
  })
  
  output$land_deal_report_view_2_count <- renderValueBox({
    land_deal_report_view_2 %>% 
      as.tbl() %>% 
      filter(月份%in%input$land_supply_report_view_2_month) %>% 
      select(土地成交宗数) %>% 
      paste0("宗") %>%
      valueBox(subtitle = "土地出让宗数")
  })
  
  output$land_deal_report_view_2_area <- renderValueBox({
    land_deal_report_view_2 %>% 
      as.tbl() %>% 
      filter(月份%in%input$land_supply_report_view_2_month) %>% 
      transmute(土地成交面积/10000) %>% 
      round(2) %>% 
      paste0("万㎡") %>% 
      valueBox(subtitle = "土地出让面积")
  })
  
  output$land_deal_report_view_2_chain_ratio <- renderValueBox({
    land_deal_report_view_2 %>% 
      as.tbl() %>% 
      filter(月份%in%input$land_supply_report_view_2_month) %>% 
      transmute(土地供应量环比*100) %>% 
      round(1) %>% 
      paste0("%") %>% 
      valueBox(subtitle = "土地出让量环比")
  })
  
  output$land_deal_supply_report_1 <- renderEcharts4r({
    land_deal_supply_report_view_2 %>% 
      as.tbl() %>% 
      arrange(日期) %>% 
      filter(月份>=input$land_deal_supply_report_view_2_month_1&
                 月份<=input$land_deal_supply_report_view_2_month_2) %>% 
      mutate(供销比=round(供销比,1)) %>% 
      e_charts(月份) %>% 
      e_area(供销比,smooth=T,legend=F) %>% 
      e_y_axis(name = "挂牌出让比",
               nameLocation="center",
               nameGap=40,
               splitLine=list(show=F),
               min=0.04,
               minInterval=0.01) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度土地挂牌出让比走势图")
  })
  
  output$land_deal_supply_report_2 <- renderEcharts4r({
    land_deal_chain_ratio %>% 
      as.tbl() %>% 
      arrange(成交时间) %>% 
      filter(月份>=input$land_deal_supply_report_view_2_month_1&
                 月份<=input$land_deal_supply_report_view_2_month_2) %>% 
      e_charts(月份) %>% 
      e_bar(成交金额,name = "出让金额") %>% 
      e_bar(上一期成交金额,name = "上一期出让金额") %>% 
      e_line(y_index = 1,成交金额环比,smooth=T,name = "出让金额环比") %>% 
      e_y_axis(name="出让金额(元)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="环比",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("土地出让金额环比")
  })
  
  output$land_deal_supply_report_3 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(月份>=input$land_deal_supply_report_view_2_month_1&
                 月份<=input$land_deal_supply_report_view_2_month_2&
                 成交价>0) %>% 
      group_by(月份) %>% 
      summarise("占地面积"=sum(总用地面积.平方米.,na.rm=T),
                "宗数"=n()) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(占地面积) %>% 
      e_line(宗数,y_index = 1,smooth = T) %>% 
      e_y_axis(name="占地面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter=JS("function(value){
                            return(value/10000 + '万')
               }")) %>% 
      e_y_axis(index = 1,
               name="宗数(宗)",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom=10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度出让土地宗数&面积情况")
  })
  
  output$land_deal_supply_report_4 <- renderEcharts4r({
    land_floor_price_chain_ratio %>% 
      as.tbl() %>% 
      filter(月份>=input$land_deal_supply_report_view_2_month_1&
                 月份<=input$land_deal_supply_report_view_2_month_2) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(楼面价) %>% 
      e_bar(上一期楼面价) %>% 
      e_line(y_index = 1,楼面价环比,smooth=T) %>% 
      e_y_axis(name="楼面价(元/㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_y_axis(index = 1,
               name="环比",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度出让土地平均楼面价金额环比情况")
  })
  
  output$land_deal_supply_report_5 <- renderEcharts4r({
    land_area_floor_price_house_floor_price %>% 
      as.tbl() %>% 
      filter(月份>=input$land_deal_supply_report_view_2_month_1&
                 月份<=input$land_deal_supply_report_view_2_month_2) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(楼面价) %>% 
      e_bar(涉宅楼面价) %>% 
      e_y_axis(name="楼面价(元/㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_legend(bottom=10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度土地平均楼面价&涉宅土地平均楼面价对比情况")
  })
  
  output$land_deal_supply_report_6 <- renderEcharts4r({
    land_house_deal_supply %>% 
      as.tbl() %>% 
      filter(月份>=input$land_deal_supply_report_view_2_month_1&
                 月份<=input$land_deal_supply_report_view_2_month_2) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(供应,name = "挂牌面积") %>% 
      e_bar(成交,name = "出让面积") %>% 
      e_line(y_index = 1,供销比,smooth=T,name = "挂牌出让比") %>% 
      e_y_axis(name="挂牌出让面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="挂牌出让比",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月市区涉宅土地挂牌出让面积情况")
  })
  
  output$land_deal_supply_report_7 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  所属行政区%in%c("温州市(本级)")&
                  土地用途%in%c("住宿餐饮用地",
                            "养老服务用地",
                            "城镇住宅用地(普通商品房)",
                            "普通商品住房用地",
                            "旅馆用地")) %>% 
      filter(月份>=input$land_deal_supply_report_view_2_month_1&
                 月份<=input$land_deal_supply_report_view_2_month_2) %>% 
      arrange(月份) %>% 
      group_by(月份) %>% 
      summarise("总建筑面积"=sum(总建筑面积.平方米.,na.rm=T),
                "容积率"=round(mean(as.numeric(最大容积率),na.rm=T),2)) %>% 
      e_charts(月份) %>% 
      e_bar(总建筑面积) %>% 
      e_line(容积率,y_index = 1,smooth = T) %>%
      e_y_axis(name="建筑面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="容积率",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度市区涉宅土地出让建筑面积和平均容积率对比情况")
      
  })

# 土地挂牌出让报告(二) -------------------------------------------------------------
  output$year_area_draw_1 <- renderEcharts4r({
    year_land_area_floor_price_house_floor_price %>% 
      as.tbl() %>% 
      filter(year(日期)>=2018) %>% 
      mutate("年份"=as.factor(year(日期))) %>% 
      e_charts(年份) %>% 
      e_bar(楼面价) %>% 
      e_bar(涉宅楼面价) %>% 
      e_y_axis(name="楼面价(元/㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/1000 + 'k')
             }")) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("年度土地楼面价&涉宅土地楼面价对比情况")
  })
  
  output$year_area_draw_2 <- renderEcharts4r({
    year_land_house_deal_supply %>% 
      as.tbl() %>% 
      filter(year(日期)>=2018) %>% 
      mutate("年份"=as.factor(year(日期))) %>% 
      e_charts(年份) %>% 
      e_bar(供应,name="挂牌面积") %>% 
      e_bar(成交,name="出让面积") %>% 
      e_line(供销比,name="挂牌出让比",smooth=T,y_index=1) %>% 
      e_y_axis(name="挂牌出让面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="挂牌出让比",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("年度市区涉宅土地挂牌出让面积情况")
  })
  
  output$year_area_draw_3 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>%
      filter(substring(成交时间,first = 1,last = 4)>=2017&
               成交价>0) %>% 
      mutate("年份"=substring(成交时间,first = 1,last = 4)) %>% 
      group_by(年份) %>% 
      summarise("建筑面积"=sum(总建筑面积.平方米.,na.rm=T),
                "出让宗数"=n()) %>% 
      e_charts(年份) %>% 
      e_bar(建筑面积) %>% 
      e_line(出让宗数,smooth=T,y_index=1) %>% 
      e_y_axis(name="建筑面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="出让宗数(宗)",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("年度土地出让宗数&面积情况")
  })
  
  output$year_area_draw_4 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>%
      filter(substring(成交时间,first = 1,last = 4)>=2017&
               成交价>0&
               所属行政区%in%c("温州市(本级)")&
               土地用途%in%c("住宿餐饮用地",
                         "养老服务用地",
                         "城镇住宅用地(普通商品房)",
                         "普通商品住房用地",
                         "旅馆用地")) %>% 
      mutate("年份"=substring(成交时间,first = 1,last = 4)) %>% 
      group_by(年份) %>% 
      summarise("建筑面积"=sum(总建筑面积.平方米.,na.rm=T),
                "容积率"=round(mean(as.numeric(最大容积率),na.rm=T),2)) %>% 
      e_charts(年份) %>% 
      e_bar(建筑面积) %>% 
      e_line(容积率,smooth=T,y_index=1) %>% 
      e_y_axis(name="建筑面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="平均容积率",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("年度市区涉宅土地出让建筑面积&平均容积率对比情况")
  })
  
  output$year_area_draw_5 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>%
      filter(substring(挂牌开始时间,first = 1,last = 4)>=2017&
               !(土地用途%in%c("公共设施用地",
                           "公用设施用地",
                           "养老服务用地",
                           "医卫慈善用地",
                           "教育用地",
                           "港口码头用地",
                           "社会福利用地",
                           "科教用地",
                           "街巷用地"))) %>% 
      mutate("年份"=substring(挂牌开始时间,first = 1,last = 4)) %>% 
      group_by(年份) %>% 
      summarise("占地面积"=sum(总用地面积.平方米.,na.rm=T)) %>% 
      e_charts(年份) %>% 
      e_bar(占地面积) %>% 
      e_y_axis(name="占地面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("年度经营性土地挂牌占地面积情况")
  })
  
  output$land_pie_1 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  !(土地用途%in%c("公共设施用地",
                              "公用设施用地",
                              "养老服务用地",
                              "医卫慈善用地",
                              "教育用地",
                              "港口码头用地",
                              "社会福利用地",
                              "科教用地",
                              "街巷用地"))) %>% 
      filter(月份%in%input$wz_area_deal_month) %>%
      group_by(所属行政区) %>% 
      summarise("出让总额"=sum(成交价,na.rm=T)) %>% 
      arrange(desc(出让总额)) %>% 
      e_charts(所属行政区) %>% 
      e_pie(出让总额,radius = c("50%","70%"),legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("各区域经营性用地出让总额分布情况")
  })
  
  output$land_pie_2 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  !(土地用途%in%c("公共设施用地",
                              "公用设施用地",
                              "养老服务用地",
                              "医卫慈善用地",
                              "教育用地",
                              "港口码头用地",
                              "社会福利用地",
                              "科教用地",
                              "街巷用地"))) %>% 
      filter(月份%in%input$wz_area_deal_month) %>%
      group_by(所属行政区) %>% 
      summarise("出让均价"=round(mean(成交价,na.rm=T),2)) %>% 
      arrange(desc(出让均价)) %>% 
      e_charts(所属行政区) %>% 
      e_pie(出让均价,radius = c("50%","70%"),legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("各区域经营性用地出让额均价分布情况")
  })
  
  output$land_pie_3 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  !(土地用途%in%c("公共设施用地",
                              "公用设施用地",
                              "养老服务用地",
                              "医卫慈善用地",
                              "教育用地",
                              "港口码头用地",
                              "社会福利用地",
                              "科教用地",
                              "街巷用地"))) %>% 
      filter(月份%in%input$wz_area_deal_month) %>%
      group_by(所属行政区) %>% 
      summarise("总用地面积"=sum(总用地面积.平方米.,na.rm=T)) %>% 
      arrange(desc(总用地面积)) %>% 
      e_charts(所属行政区) %>% 
      e_pie(总用地面积,radius = c("50%","70%"),legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("各区域经营性用地出让总占地面积分布")
  })
  
  output$land_area_sale_deal_draw_1 <- renderEcharts4r({
    land_area_sale_deal %>% 
      as.tbl() %>% 
      filter(区域%in%input$land_area_sale_deal_area_select&
                 月份%in%input$land_area_sale_deal_month) %>% 
      e_charts(区域) %>% 
      e_bar(成交金额,name="出让金额") %>%
      e_bar(上一期成交金额,name="上一期出让金额") %>% 
      e_line(成交金额环比,name="出让金额环比",smooth=T,y_index=1) %>% 
      e_y_axis(name="出让金额(元)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/100000000 + '亿')
             }")) %>% 
      e_y_axis(index = 1,
               name="环比",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("各区域经营性用地出让总额环比情况")
  })
  
  output$land_area_sale_deal_draw_2 <- renderEcharts4r({
    land_area_sale_deal_mean_price %>% 
      as.tbl() %>% 
      filter(区域%in%input$land_area_sale_deal_area_select&
                 月份%in%input$land_area_sale_deal_month) %>% 
      e_charts(区域) %>% 
      e_bar(成交金额,name="出让均价") %>%
      e_bar(上一期成交金额,name="上一期出让均价") %>% 
      e_line(成交额均价环比,name="出让均价环比",smooth=T,y_index=1) %>% 
      e_y_axis(name="出让均价(元)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/100000000 + '亿')
             }")) %>% 
      e_y_axis(index = 1,
               name="环比",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("各区域经营性用地出让额均价环比情况")
  })
  
  output$land_for_sale_draw_1 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(!(土地用途%in%c("公共设施用地",
                         "公用设施用地",
                         "养老服务用地",
                         "医卫慈善用地",
                         "教育用地",
                         "港口码头用地",
                         "社会福利用地",
                         "科教用地",
                         "街巷用地"))) %>% 
      filter(挂牌月份>=input$land_for_sale_draw_1_month_1&
                   挂牌月份<=input$land_for_sale_draw_1_month_2) %>% 
      group_by(挂牌月份) %>% 
      summarise("占地面积"=sum(总用地面积.平方米.,na.rm=T)) %>% 
      e_charts(挂牌月份) %>% 
      e_bar(占地面积,legend = F) %>% 
      e_y_axis(name="占地面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度经营性土地挂牌占地面积情况")
  })
  
  output$land_for_sale_draw_2 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(!(土地用途%in%c("公共设施用地",
                         "公用设施用地",
                         "养老服务用地",
                         "医卫慈善用地",
                         "教育用地",
                         "港口码头用地",
                         "社会福利用地",
                         "科教用地",
                         "街巷用地"))) %>% 
      filter(挂牌月份>=input$land_for_sale_draw_1_month_1&
                   挂牌月份<=input$land_for_sale_draw_1_month_2) %>% 
      group_by(所属行政区) %>% 
      summarise("总用地面积"=sum(总用地面积.平方米.,na.rm=T)) %>% 
      arrange(desc(总用地面积)) %>% 
      e_charts(所属行政区) %>% 
      e_pie(总用地面积,radius = c("50%","70%"),legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("各区域经营性用地挂牌占地面积情况")
  })
  

# 土地挂牌出让报告(三) -------------------------------------------------------------

  output$land_listing_transfer_report_3_1 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  !(土地用途%in%c("公共设施用地",
                              "公用设施用地",
                              "养老服务用地",
                              "医卫慈善用地",
                              "教育用地",
                              "港口码头用地",
                              "社会福利用地",
                              "科教用地",
                              "街巷用地"))) %>% #含有土地用途NA的记录
      filter(月份>=input$land_for_sale_draw_3_month_1&
                 月份<=input$land_for_sale_draw_3_month_2) %>% 
      group_by(月份) %>% 
      summarise("出让面积"=sum(总用地面积.平方米.,na.rm=T),
                "总建筑面积"=sum(总建筑面积.平方米.,na.rm=T),
                "出让楼面价"=
                  round(sum(成交价,na.rm=T)*10000/(sum(最大容积率*总用地面积.平方米.,na.rm=T)))) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(出让面积,name = "出让面积") %>% 
      e_bar(总建筑面积,name = "总建筑面积") %>% 
      e_line(y_index = 1,出让楼面价,smooth=T,name = "出让楼面价") %>% 
      e_y_axis(name="面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="汇总出让楼面价",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度全市经营性用地出让&建筑面积&平均楼面价情况")
  })  
  
  output$land_listing_transfer_report_3_2 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  !(土地用途%in%c("公共设施用地",
                              "公用设施用地",
                              "养老服务用地",
                              "医卫慈善用地",
                              "教育用地",
                              "港口码头用地",
                              "社会福利用地",
                              "科教用地",
                              "街巷用地"))&
                  所属行政区=="温州市(本级)") %>% #含有土地用途NA的记录
      filter(月份>=input$land_for_sale_draw_3_month_1&
                 月份<=input$land_for_sale_draw_3_month_2) %>% 
      group_by(月份) %>% 
      summarise("出让面积"=sum(总用地面积.平方米.,na.rm=T),
                "总建筑面积"=sum(总建筑面积.平方米.,na.rm=T),
                "出让楼面价"=
                  round(sum(成交价,na.rm=T)*10000/(sum(最大容积率*总用地面积.平方米.,na.rm=T)))) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(出让面积,name = "出让面积") %>% 
      e_bar(总建筑面积,name = "总建筑面积") %>% 
      e_line(y_index = 1,出让楼面价,smooth=T,name = "出让楼面价") %>% 
      e_y_axis(name="面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="汇总出让楼面价",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度市区经营性用地出让&建筑面积&平均楼面价情况")
  })
  
  output$land_listing_transfer_report_3_3 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  土地用途%in%c("住宿餐饮用地",
                            "养老服务用地",
                            "城镇住宅用地(普通商品房)",
                            "普通商品住房用地",
                            "旅馆用地")) %>% #含有土地用途NA的记录
      filter(月份>=input$land_for_sale_draw_3_month_1&
                 月份<=input$land_for_sale_draw_3_month_2) %>% 
      group_by(月份) %>% 
      summarise("出让面积"=sum(总用地面积.平方米.,na.rm=T),
                "总建筑面积"=sum(总建筑面积.平方米.,na.rm=T),
                "出让楼面价"=
                  round(sum(成交价,na.rm=T)*10000/(sum(最大容积率*总用地面积.平方米.,na.rm=T)))) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(出让面积,name = "出让面积") %>% 
      e_bar(总建筑面积,name = "总建筑面积") %>% 
      e_line(y_index = 1,出让楼面价,smooth=T,name = "出让楼面价") %>% 
      e_y_axis(name="面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="汇总出让楼面价",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度全市涉宅用地出让&建筑面积&平均楼面价情况")
  })
  
  output$land_listing_transfer_report_3_4 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  土地用途%in%c("住宿餐饮用地",
                            "养老服务用地",
                            "城镇住宅用地(普通商品房)",
                            "普通商品住房用地",
                            "旅馆用地")&
                  所属行政区=="温州市(本级)") %>% #含有土地用途NA的记录
      filter(月份>=input$land_for_sale_draw_3_month_1&
                 月份<=input$land_for_sale_draw_3_month_2) %>% 
      group_by(月份) %>% 
      summarise("出让面积"=sum(总用地面积.平方米.,na.rm=T),
                "总建筑面积"=sum(总建筑面积.平方米.,na.rm=T),
                "出让楼面价"=
                  round(sum(成交价,na.rm=T)*10000/(sum(最大容积率*总用地面积.平方米.,na.rm=T)))) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(出让面积,name = "出让面积") %>% 
      e_bar(总建筑面积,name = "总建筑面积") %>% 
      e_line(y_index = 1,出让楼面价,smooth=T,name = "出让楼面价") %>% 
      e_y_axis(name="面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="汇总出让楼面价",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度市区涉宅用地出让&建筑面积&平均楼面价情况")
  })
  
  output$land_listing_transfer_report_3_5 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0) %>% #含有土地用途NA的记录
      filter(月份>=input$land_for_sale_draw_3_month_1&
                 月份<=input$land_for_sale_draw_3_month_2) %>% 
      group_by(月份) %>% 
      summarise("商业面积"=sum(商业面积.平方米.,na.rm=T),
                "住宅面积"=sum(住宅面积.平方米.,na.rm=T)) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(商业面积,name = "商业面积") %>% 
      e_bar(住宅面积,name = "住宅面积") %>% 
      e_y_axis(name="面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度温州全市土地出让占地面积结构")
  })
  
  output$land_listing_transfer_report_3_6 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  所属行政区=="温州市(本级)") %>% #含有土地用途NA的记录
      filter(月份>=input$land_for_sale_draw_3_month_1&
                 月份<=input$land_for_sale_draw_3_month_2) %>% 
      group_by(月份) %>% 
      summarise("商业面积"=sum(商业面积.平方米.,na.rm=T),
                "住宅面积"=sum(住宅面积.平方米.,na.rm=T)) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(商业面积,name = "商业面积") %>% 
      e_bar(住宅面积,name = "住宅面积") %>% 
      e_y_axis(name="面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度温州市区土地出让占地面积结构")
  })
  
  output$land_listing_transfer_report_3_7 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  所属行政区=="温州市(本级)") %>% #含有土地用途NA的记录
      filter(月份>=input$land_for_sale_draw_3_month_1&
                 月份<=input$land_for_sale_draw_3_month_2) %>% 
      group_by(月份) %>% 
      summarise("住宅面积"=sum(住宅面积.平方米.,na.rm=T)) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(住宅面积,name = "住宅面积") %>% 
      e_y_axis(name="面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("市区住宅用地出让占地面积结构")
  })
  
  output$land_listing_transfer_report_3_8 <- renderEcharts4r({
    land_view %>% 
      as.tbl() %>% 
      filter(成交价>0&
                  所属行政区=="温州市(本级)") %>% #含有土地用途NA的记录
      filter(月份>=input$land_for_sale_draw_3_month_1&
                 月份<=input$land_for_sale_draw_3_month_2) %>% 
      group_by(月份) %>% 
      summarise("商业面积"=sum(商业面积.平方米.,na.rm=T)) %>% 
      arrange(月份) %>% 
      e_charts(月份) %>% 
      e_bar(商业面积,name = "商业面积") %>% 
      e_y_axis(name="面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("市区商业用地出让占地面积结构")
  })
  
  output$land_listing_transfer_report_datatable <- renderDataTable({
    land_view %>% 
      as.tbl() %>% 
      filter(!(成交价>0)&挂牌截止时间>=(Sys.Date()-15)) %>% 
      select("区域"=所属行政区,
             地块名称,
              "地块编号"=X.地块编号,
             土地用途,
             挂牌开始时间,
             挂牌截止时间,
             "占地面积(㎡)"=`总用地面积.平方米.`,
             "挂牌金额(万元)"=起始价) %>% 
      arrange(desc(挂牌开始时间,挂牌截止时间)) %>% 
      datatable(class = "cell-border",width = "100%",extensions = "Scroller",
                options = list(autoWidth = T,ordering = F,scroller = T,scrollX = T,
                               scrollY = 550,dom = 't',deferRender=T,
                               columnDefs = list(
                                 list(width = '10%',className = 'dt-center', 
                                      targets = c(1,4,5,6,7,8)),
                                 list(width = '20%',className = 'dt-center',
                                      targets = c(2,3))),#整体超过100%,则自动设置宽度
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({
                                 'background-color': '#E5E7EE', 'color': '#444444'
                                 });",
                                 "}")))
  })
  

# 单楼盘成交详情 -----------------------------------------------------------------

  output$details_of_single_building_dealing_valuebox_1 <- renderValueBox({
    a <- if(is.null(input$details_of_single_building_dealing_item)){
      unique(first_hand_deal_single_room$项目推广名)
    }else{
      input$details_of_single_building_dealing_item
    }
    b <- if(is.null(input$details_of_single_building_dealing_building)){
      unique(first_hand_deal_single_room$幢名)
    }else{
      input$details_of_single_building_dealing_building
    }
    
    first_hand_deal_single_room %>%
      as.tbl() %>%
      filter(项目推广名%in%a&
                    幢名%in%b) %>%
      filter(总建筑面积>=if_else(input$details_of_single_building_dealing_min=="",
                                 0,
                                 as.numeric(input$details_of_single_building_dealing_min))) %>% 
      filter(总建筑面积<=if_else(input$details_of_single_building_dealing_max=="",
                                 50000,
                                 as.numeric(input$details_of_single_building_dealing_max))) %>%
      summarise("房源供应量"=n()) %>%
      paste0("套") %>%
      valueBox(subtitle = "房源供应量")
  })
  
  output$details_of_single_building_dealing_valuebox_2 <- renderValueBox({
    a <- if(is.null(input$details_of_single_building_dealing_item)){
      unique(first_hand_deal_single_room$项目推广名)
    }else{
      input$details_of_single_building_dealing_item
    }
    b <- if(is.null(input$details_of_single_building_dealing_building)){
      unique(first_hand_deal_single_room$幢名)
    }else{
      input$details_of_single_building_dealing_building
    }
    
    first_hand_deal_single_room %>%
      as.tbl() %>%
      filter(项目推广名%in%a&
                    幢名%in%b) %>%
      filter(总建筑面积>=if_else(input$details_of_single_building_dealing_min=="",
                            0,
                            as.numeric(input$details_of_single_building_dealing_min))) %>% 
      filter(总建筑面积<=if_else(input$details_of_single_building_dealing_max=="",
                            50000,
                            as.numeric(input$details_of_single_building_dealing_max))) %>%
      filter(属性%in%c("已售","已登记","已认购","安置房")) %>% 
      summarise("成交套数"=n()) %>%
      paste0("套") %>%
      valueBox(subtitle = "成交套数")
  })
  
  output$details_of_single_building_dealing_valuebox_3 <- renderValueBox({
    a <- if(is.null(input$details_of_single_building_dealing_item)){
      unique(first_hand_deal_single_room$项目推广名)
    }else{
      input$details_of_single_building_dealing_item
    }
    b <- if(is.null(input$details_of_single_building_dealing_building)){
      unique(first_hand_deal_single_room$幢名)
    }else{
      input$details_of_single_building_dealing_building
    }
    
    first_hand_deal_single_room %>%
      as.tbl() %>%
      filter(项目推广名%in%a&
                    幢名%in%b) %>%
      filter(总建筑面积>=if_else(input$details_of_single_building_dealing_min=="",
                            0,
                            as.numeric(input$details_of_single_building_dealing_min))) %>% 
      filter(总建筑面积<=if_else(input$details_of_single_building_dealing_max=="",
                            50000,
                            as.numeric(input$details_of_single_building_dealing_max))) %>%
      filter(属性%in%c("已售","已登记","已认购","安置房")) %>% 
      summarise("成交面积"=round(sum(总建筑面积,na.rm = T)/10000,2)) %>%
      paste0("万㎡") %>%
      valueBox(subtitle = "成交面积")
  })
  
  output$details_of_single_building_dealing_datatable_1 <- renderDataTable({
    a <- if(is.null(input$details_of_single_building_dealing_item)){
      unique(first_hand_deal_single_room$项目推广名)
    }else{
      input$details_of_single_building_dealing_item
    }
    b <- if(is.null(input$details_of_single_building_dealing_building)){
      unique(first_hand_deal_single_room$幢名)
    }else{
      input$details_of_single_building_dealing_building
    }
    c <- if(is.null(input$details_of_single_building_dealing_type)){
      unique(first_hand_deal_single_room$属性)
    }else{
      input$details_of_single_building_dealing_type
    }
    
    first_hand_deal_single_room %>%
      as.tbl() %>%
      filter(项目推广名%in%a&
                    幢名%in%b&
                    属性%in%c) %>%
      filter(总建筑面积>=if_else(input$details_of_single_building_dealing_min=="",
                            0,
                            as.numeric(input$details_of_single_building_dealing_min))) %>% 
      filter(总建筑面积<=if_else(input$details_of_single_building_dealing_max=="",
                            50000,
                            as.numeric(input$details_of_single_building_dealing_max))) %>%
      select("项目名称"=项目推广名,
             "项目备案名"=项目名称,
             销售日期,
             幢名,
             室号,
             属性,
             "总面积(㎡)"=总建筑面积,
             "单价(元/㎡)"=单价,
             "总价(元)"=总价) %>% 
      arrange(幢名,室号) %>% 
      datatable(class = "cell-border",width = "100%",extensions = "Scroller",
                options = list(autoWidth = T,ordering = F,scroller = T,scrollX = T,
                               scrollY = 510,dom = 't',deferRender=T,
                               columnDefs = list(
                                 list(width = '10%',className = 'dt-center', 
                                      targets = c(4,5,6,7,8,9)),
                                 list(width = '15%',className = 'dt-center', 
                                      targets = c(3)),
                                 list(width = '20%',className = 'dt-center',
                                      targets = c(1,2))),#整体超过100%,则自动设置宽度
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({
                                 'background-color': '#E5E7EE', 'color': '#444444'
                                 });",
                                 "}")))
  })
  
# 新房成交数据 ------------------------------------------------------------------
  
  output$first_hand_house_transaction_data_valuebox_1 <- renderValueBox({
    first_hand_deal_project_list %>%
      as.tbl() %>%
      filter(日期>=input$first_hand_house_transaction_data_date[1]&
                 日期<=input$first_hand_house_transaction_data_date[2]&
                 所在地区%in%input$first_hand_house_transaction_data_area&
                 `类别_2`%in%input$first_hand_house_transaction_data_type) %>%
      filter(单套面积>=if_else(input$first_hand_house_transaction_data_min=="",
                          0,
                          as.numeric(input$first_hand_house_transaction_data_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_transaction_data_max=="",
                           50000,
                           as.numeric(input$first_hand_house_transaction_data_max))) %>% 
      summarise("套数"=sum(套数)) %>%
      paste0("套") %>%
      valueBox(subtitle = "成交套数")
  })
  
  output$first_hand_house_transaction_data_valuebox_2 <- renderValueBox({
    first_hand_deal_project_list %>% 
      as.tbl() %>% 
      filter(日期>=input$first_hand_house_transaction_data_date[1]&
                 日期<=input$first_hand_house_transaction_data_date[2]&
                 所在地区%in%input$first_hand_house_transaction_data_area&
                 `类别_2`%in%input$first_hand_house_transaction_data_type) %>%
      filter(单套面积>if_else(input$first_hand_house_transaction_data_min=="",
                          0,
                          as.numeric(input$first_hand_house_transaction_data_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_transaction_data_max=="",
                           50000,
                           as.numeric(input$first_hand_house_transaction_data_max))) %>% 
      summarise("建筑面积"=round(sum(建筑面积)/10000,2)) %>% 
      paste0("万㎡") %>% 
      valueBox(subtitle = "成交面积")
  })
  
  output$first_hand_house_transaction_data_valuebox_3 <- renderValueBox({
    first_hand_deal_project_list %>% 
      as.tbl() %>% 
      filter(住宅均价.元...>0) %>% 
      filter(日期>=input$first_hand_house_transaction_data_date[1]&
                 日期<=input$first_hand_house_transaction_data_date[2]&
                 所在地区%in%input$first_hand_house_transaction_data_area&
                 `类别_2`%in%input$first_hand_house_transaction_data_type) %>%
      filter(单套面积>if_else(input$first_hand_house_transaction_data_min=="",
                          0,
                          as.numeric(input$first_hand_house_transaction_data_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_transaction_data_max=="",
                           50000,
                           as.numeric(input$first_hand_house_transaction_data_max))) %>% 
      summarise("成交均价"=round(
        sum(建筑面积*住宅均价.元...,na.rm=T)/sum(建筑面积,na.rm=T)/10000,
        2
      )) %>% 
      paste0("万元/㎡") %>% 
      valueBox(subtitle = "成交均价")
  })
  
  output$first_hand_house_transaction_data_datatable <- renderDT({
    first_hand_deal_project_list %>% 
      as.tbl() %>% 
      filter(日期>=input$first_hand_house_transaction_data_date[1]&
                 日期<=input$first_hand_house_transaction_data_date[2]&
                 所在地区%in%input$first_hand_house_transaction_data_area&
                 `类别_2`%in%input$first_hand_house_transaction_data_type) %>%
      filter(单套面积>if_else(input$first_hand_house_transaction_data_min=="",
                          0,
                          as.numeric(input$first_hand_house_transaction_data_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_transaction_data_max=="",
                           50000,
                           as.numeric(input$first_hand_house_transaction_data_max))) %>% 
      group_by(项目备案名,项目推广名,所在地区) %>% 
      summarise("成交套数"=sum(套数,na.rm = T),
                "成交面积(㎡)"=sum(建筑面积,na.rm = T),
                `参考毛坯均价(元/㎡)`=if_else(is.na(round(mean(住宅均价.元...,na.rm = T))),
                                      "暂无数据",
                                      as.character(round(mean(住宅均价.元...,na.rm = T))))) %>% 
      select("项目名称"=项目推广名,
             项目备案名,
             "区域"=所在地区,
             成交套数,
             `成交面积(㎡)`,
             `参考毛坯均价(元/㎡)`) %>% 
      arrange(desc(成交套数)) %>% 
      datatable(class = "cell-border",width = "100%",extensions = "Scroller",
                options = list(autoWidth = T,ordering = F,scroller = T,scrollX = T,
                               scrollY = 550,dom = 't',deferRender=T,
                               columnDefs = list(
                                 list(width = '10%',className = 'dt-center', 
                                      targets = c(3,4,5)),
                                 list(width = '20%',className = 'dt-center', 
                                      targets = c(6)),
                                 list(width = '25%',className = 'dt-center',
                                      targets = c(1,2))),#整体超过100%,则自动设置宽度
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({
                                 'background-color': '#E5E7EE', 'color': '#444444'
                                 });",
                                 "}")))
  })


# 新房供应数据 ------------------------------------------------------------------

  output$first_hand_house_supply_data_valuebox_1 <- renderValueBox({
    a <- if(is.null(input$first_hand_house_supply_data_item)){
      unique(first_hand_history_supply$项目推广名)
    }else{
      input$first_hand_house_supply_data_item
    }
    
    first_hand_history_supply %>% 
      as.tbl() %>% 
      filter(开盘日期>=input$first_hand_house_supply_data_date[1]&
                     开盘日期<=input$first_hand_house_supply_data_date[2]&
                     所在地区%in%input$first_hand_house_supply_data_area) %>%
      filter(项目推广名%in%a) %>%
      filter(单套面积>=if_else(input$first_hand_house_supply_data_min=="",
                                 0,
                                 as.numeric(input$first_hand_house_supply_data_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_supply_data_max=="",
                                 50000,
                                 as.numeric(input$first_hand_house_supply_data_max))) %>%
      summarise("房源总供应量"=sum(房源总供应量,na.rm = T)) %>% 
      paste0("套") %>% 
      valueBox(subtitle = "房源总供应量")
  })
  
  output$first_hand_house_supply_data_valuebox_2 <- renderValueBox({
    a <- if(is.null(input$first_hand_house_supply_data_item)){
      unique(first_hand_history_supply$项目推广名)
    }else{
      input$first_hand_house_supply_data_item
    }
    
    first_hand_history_supply %>% 
      as.tbl() %>% 
      filter(开盘日期>=input$first_hand_house_supply_data_date[1]&
                   开盘日期<=input$first_hand_house_supply_data_date[2]&
                   所在地区%in%input$first_hand_house_supply_data_area) %>%
      filter(项目推广名%in%a) %>%
      filter(单套面积>=if_else(input$first_hand_house_supply_data_min=="",
                           0,
                           as.numeric(input$first_hand_house_supply_data_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_supply_data_max=="",
                           50000,
                           as.numeric(input$first_hand_house_supply_data_max))) %>%
      summarise("在售套数"=sum(待售房源,na.rm = T)) %>% 
      paste0("套") %>% 
      valueBox(subtitle = "在售套数")
  })
  
  output$first_hand_house_supply_data_valuebox_3 <- renderValueBox({
    a <- if(is.null(input$first_hand_house_supply_data_item)){
      unique(first_hand_history_supply$项目推广名)
    }else{
      input$first_hand_house_supply_data_item
    }
    
    first_hand_history_supply %>% 
      as.tbl() %>% 
      filter(开盘日期>=input$first_hand_house_supply_data_date[1]&
                   开盘日期<=input$first_hand_house_supply_data_date[2]&
                   所在地区%in%input$first_hand_house_supply_data_area) %>%
      filter(项目推广名%in%a) %>%
      filter(单套面积>=if_else(input$first_hand_house_supply_data_min=="",
                           0,
                           as.numeric(input$first_hand_house_supply_data_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_supply_data_max=="",
                           50000,
                           as.numeric(input$first_hand_house_supply_data_max))) %>%
      summarise("在售面积"=sum(在售面积,na.rm = T)) %>% 
      paste0("万㎡") %>% 
      valueBox(subtitle = "在售面积")
  })
  
  output$first_hand_house_supply_data_datatable <- renderDataTable({
    a <- if(is.null(input$first_hand_house_supply_data_item)){
      unique(first_hand_history_supply$项目推广名)
    }else{
      input$first_hand_house_supply_data_item
    }
    
    first_hand_history_supply %>% 
      as.tbl() %>% 
      filter(待售房源>0) %>% 
      filter(开盘日期>=input$first_hand_house_supply_data_date[1]&
                   开盘日期<=input$first_hand_house_supply_data_date[2]&
                   所在地区%in%input$first_hand_house_supply_data_area) %>%
      filter(项目推广名%in%a) %>%
      filter(单套面积>=if_else(input$first_hand_house_supply_data_min=="",
                           0,
                           as.numeric(input$first_hand_house_supply_data_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_supply_data_max=="",
                           50000,
                           as.numeric(input$first_hand_house_supply_data_max))) %>%
      mutate(在售面积=在售面积*10000) %>% 
      select("项目名称"=项目推广名,
             "项目备案名"=项目名称,
             "区域"=所在地区,
             "供应套数"=房源总供应量,
             "在售套数"=待售房源,
             "在售面积(㎡)"=在售面积,
             去化率,
             "住宅均价(元/㎡)"=住宅均价.元...,
             "商业均价(元/㎡)"=商业均价.元...) %>% 
      arrange(desc(在售套数)) %>%  
      datatable(class = "cell-border",width = "100%",extensions = "Scroller",
                options = list(autoWidth = T,ordering = F,scroller = T,scrollX = T,
                               scrollY = 500,dom = 't',deferRender=T,
                               columnDefs = list(
                                 list(width = '10%',className = 'dt-center', 
                                      targets = c(7)),
                                 list(width = '15%',className = 'dt-center', 
                                      targets = c(3,4,5,6,8,9)),
                                 list(width = '20%',className = 'dt-center',
                                      targets = c(1,2))),#整体超过100%,则自动设置宽度
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({
                                 'background-color': '#E5E7EE', 'color': '#444444'
                                 });",
                                 "}")))
  })
  
# 新房供销报告 ------------------------------------------------------------------

  output$first_hand_house_supply_and_marketing_report_1_1 <- renderValueBox({
    first_hand_history_supply %>% 
      as.tbl() %>% 
      filter(开盘日期>=input$first_hand_house_supply_and_marketing_report_date[1]&
                 开盘日期<=input$first_hand_house_supply_and_marketing_report_date[2]&
                 所在地区%in%input$first_hand_house_supply_and_marketing_report_area) %>% 
      filter(单套面积>=if_else(input$first_hand_house_supply_and_marketing_report_min=="",
                           0,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_supply_and_marketing_report_max=="",
                           50000,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_max))) %>%
      summarise("房源总供应量"=sum(房源总量-非销售房产)) %>% 
      paste0("套") %>% 
      valueBox(subtitle = "房源总供应量")
  })
  
  output$first_hand_house_supply_and_marketing_report_1_2 <- renderValueBox({
    first_hand_history_supply %>% 
      as.tbl() %>% 
      filter(开盘日期>=input$first_hand_house_supply_and_marketing_report_date[1]&
                   开盘日期<=input$first_hand_house_supply_and_marketing_report_date[2]&
                   所在地区%in%input$first_hand_house_supply_and_marketing_report_area) %>% 
      filter(单套面积>=if_else(input$first_hand_house_supply_and_marketing_report_min=="",
                           0,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_supply_and_marketing_report_max=="",
                           50000,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_max))) %>%
      summarise("房源在售总量"=sum(待售房源)) %>% 
      paste0("套") %>% 
      valueBox(subtitle = "房源在售总量")
  })
  
  output$first_hand_house_supply_and_marketing_report_1_3 <- renderValueBox({
    first_hand_history_supply %>% 
      as.tbl() %>% 
      filter(开盘日期>=input$first_hand_house_supply_and_marketing_report_date[1]&
                   开盘日期<=input$first_hand_house_supply_and_marketing_report_date[2]&
                   所在地区%in%input$first_hand_house_supply_and_marketing_report_area) %>% 
      filter(单套面积>=if_else(input$first_hand_house_supply_and_marketing_report_min=="",
                           0,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_min))) %>% 
      summarise("房源在售总面积"=round(sum(项目测算面积/房源总量*待售房源,na.rm = T)/10000,1)) %>% 
      paste0("万/㎡") %>% 
      valueBox(subtitle = "房源在售总面积")
  })
  
  output$first_hand_house_supply_and_marketing_report_1_4 <- renderValueBox({
    first_hand_deal_project_list %>% 
      as.tbl() %>% 
      filter(日期>=input$first_hand_house_supply_and_marketing_report_date[1]&
                 日期<=input$first_hand_house_supply_and_marketing_report_date[2]&
                 所在地区%in%input$first_hand_house_supply_and_marketing_report_area) %>% 
      filter(单套面积>=if_else(input$first_hand_house_supply_and_marketing_report_min=="",
                           0,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_supply_and_marketing_report_max=="",
                           50000,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_max))) %>%
      summarise("套数"=sum(套数)) %>% 
      paste0("套") %>% 
      valueBox(subtitle = "房源成交总量")
  })
  
  output$first_hand_house_supply_and_marketing_report_1_5 <- renderValueBox({
    first_hand_deal_project_list %>% 
      as.tbl() %>% 
      filter(日期>=input$first_hand_house_supply_and_marketing_report_date[1]&
                 日期<=input$first_hand_house_supply_and_marketing_report_date[2]&
                 所在地区%in%input$first_hand_house_supply_and_marketing_report_area) %>% 
      filter(单套面积>=if_else(input$first_hand_house_supply_and_marketing_report_min=="",
                           0,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_min))) %>% 
      filter(单套面积<=if_else(input$first_hand_house_supply_and_marketing_report_max=="",
                           50000,
                           as.numeric(input$first_hand_house_supply_and_marketing_report_max))) %>%
      summarise("套数"=round(sum(建筑面积,na.rm = T)/10000,1)) %>% 
      paste0("万/㎡") %>% 
      valueBox(subtitle = "房源成交总面积")
  })
  
  output$regional_inventory_valuebox_1 <- renderValueBox({
    regional_inventory %>% 
      as.tbl() %>% 
      filter(日期<=input$regional_inventory_date[1]&
                 所在地区%in%input$regional_inventory_area&
                 面积段%in%input$regional_inventory_square) %>% 
      summarise("库存量"=(sum(房源总量-非销售房产,na.rm = T)-sum(套数,na.rm = T))) %>% 
      paste0("套") %>% 
      valueBox(subtitle = "库存量")
  })
  
  output$regional_inventory_valuebox_2 <- renderValueBox({
    regional_inventory %>% 
      as.tbl() %>% 
      filter(日期<=input$regional_inventory_date[1]&
                 所在地区%in%input$regional_inventory_area&
                 面积段%in%input$regional_inventory_square) %>% 
      summarise("库存面积"=round((sum(建筑面积_y,na.rm = T)/sum(房源总量,na.rm = T)*
                                (sum(房源总量-非销售房产,na.rm = T)-sum(套数,na.rm = T)))/10000,1)) %>% 
      paste0("万㎡") %>% 
      valueBox(subtitle = "库存面积")
  })
  
  output$regional_inventory_area_rank_draw_1 <- renderEcharts4r({
    regional_inventory_area_rank %>% 
      as.tbl() %>% 
      filter(日期>=input$regional_inventory_area_rank_date[1]&
                 日期<=input$regional_inventory_area_rank_date[2]&
                 所在地区%in%input$regional_inventory_area_rank_area&
                 面积段%in%input$regional_inventory_area_rank_square) %>% 
      summarise("供销比"=round(sum(房源总量-非销售房产,na.rm = T)/sum(套数,na.rm = T),2)) %>% 
      as.numeric() -> a
    e_charts() %>% 
      e_gauge(a,"供销比",max = 3) %>% 
      e_title("供销比")
  })
  
  output$regional_inventory_area_rank_draw_2 <- renderEcharts4r({
    regional_inventory_area_rank %>% 
      as.tbl() %>% 
      filter(日期>=input$regional_inventory_area_rank_date[1]&
                 日期<=input$regional_inventory_area_rank_date[2]&
                 所在地区%in%input$regional_inventory_area_rank_area&
                 面积段%in%input$regional_inventory_area_rank_square) %>%
      mutate("月份"=substring(日期,first = 1,last = 7)) %>% 
      group_by(月份) %>% 
      summarise("供销比"=round(sum(房源总量-非销售房产,na.rm = T)/sum(套数,na.rm = T),2)) %>% 
      e_charts(月份) %>% 
      e_line(供销比,legend = F,smooth = T) %>% 
      e_y_axis(name="供销比",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("月度一手房供销比走势图")
  })
  
  output$regional_inventory_area_rank_area_deal_supply_draw <- renderEcharts4r({
    regional_inventory %>% 
      as.tbl() %>% 
      filter(日期>=input$regional_inventory_area_rank_regional_date[1]&
                 日期<=input$regional_inventory_area_rank_regional_date[2]) %>% 
      group_by(区域) %>% 
      summarise("供销比"=round(sum(房源总量-非销售房产,na.rm = T)/sum(套数,na.rm = T),2)) %>% 
      arrange(desc(供销比)) %>% 
      e_charts(区域) %>% 
      e_bar(供销比,legend = F) %>% 
      e_y_axis(name="供销比",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("各区域供销比对比图")
  })
  
  output$regional_inventory_year_draw_1 <- renderEcharts4r({
    regional_inventory %>% 
      as.tbl() %>% 
      filter(年份<=input$regional_inventory_year_select) %>% 
      group_by(区域) %>% 
      summarise("库存量"=(sum(房源总量-非销售房产,na.rm = T)-sum(套数,na.rm = T))) %>% 
      arrange(desc(库存量)) %>% 
      e_charts(区域) %>% 
      e_bar(库存量,legend = F) %>% 
      e_y_axis(name="库存商品住宅(套)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter=JS("function(value){
                            return(value/10000 + '万')
               }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("库存商品住宅区域分布图")
  })
  
  output$regional_inventory_year_draw_2 <- renderEcharts4r({
    regional_inventory %>% 
      as.tbl() %>% 
      filter(年份<=input$regional_inventory_year_select) %>% 
      group_by(面积段) %>% 
      summarise("库存量"=(sum(房源总量-非销售房产,na.rm = T)-sum(套数,na.rm = T))) %>% 
      arrange(desc(库存量)) %>% 
      e_charts(面积段) %>% 
      e_pie(库存量,radius = c("50%","70%"),legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("库存商品住宅建筑面积结构图")
  })
  
# 新房去化及量价报告 ---------------------------------------------------------------

  output$first_hand_deal_project_list_draw_1 <- renderEcharts4r({
    first_hand_area_deal_supply %>% 
      as.tbl() %>% 
      filter(日期<=(as.Date(format(Sys.Date(), "%Y-%m-01"))-1)) %>% 
      group_by(区域) %>% 
      summarise("去化率"=(sum(套数,na.rm = T)/(sum(房源总量,na.rm = T)-sum(非销售房产,na.rm = T)))) %>% 
      arrange(desc(去化率)) %>% 
      e_charts(区域) %>% 
      e_bar(去化率,legend = F) %>% 
      e_y_axis(name="去化率",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("各区域去化率对比图")
  })
  
  output$first_hand_deal_project_list_draw_2 <- renderEcharts4r({
    first_hand_deal_project_list %>% 
      as.tbl() %>% 
      filter(日期>=input$first_hand_deal_project_list_date[1]&
                 日期<=input$first_hand_deal_project_list_date[2]) %>% 
      group_by(面积段) %>% 
      summarise("套数"=sum(套数)) %>% 
      arrange(desc(套数)) %>% 
      e_charts(面积段) %>% 
      e_pie(套数,radius = c("50%","70%"),legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("建筑面积成交结构图")
  })
  
  output$first_hand_deal_project_list_draw_3 <- renderEcharts4r({
    first_hand_deal_project_list %>% 
      as.tbl() %>% 
      filter(住宅均价.元...>0) %>%#选择住宅均价>0,不然成交均价会被无价房拉低
      filter(所在地区%in%input$first_hand_deal_project_list_area) %>%
      group_by("年份"=as.factor(year(日期))) %>% 
      summarise("套数"=sum(套数),
                "成交均价"=sum(建筑面积*住宅均价.元...,na.rm=T)/sum(建筑面积,na.rm=T)) %>% 
      arrange(年份) %>% 
      e_charts(年份) %>% 
      e_bar(套数) %>% 
      e_area(成交均价,y_index = 1,smooth=T) %>% 
      e_y_axis(name="成交量(套)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>%
      e_y_axis(index = 1,
               name="均价(元/㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               min=JS("function(value){
                      return(value.min-1000)
               }"),
               formatter=JS("function(value){
                          return(Math.round(value/100)/100 + '万')
             }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_legend(bottom=10) %>% 
      e_title("年度成交商品房量价走势图")
  })
  
  output$first_hand_deal_project_list_draw_4 <- renderEcharts4r({
    first_hand_deal_project_list %>% 
      as.tbl() %>% 
      filter(住宅均价.元...>0) %>% 
      filter(所在地区%in%input$first_hand_deal_project_list_area) %>%
      group_by("年份"=as.factor(year(日期))) %>% 
      summarise("成交均价"=sum(建筑面积*住宅均价.元...,na.rm=T)/sum(建筑面积,na.rm=T)) %>% 
      arrange(年份) -> a
    for (i in 2:nrow(a)) {
      a[i,"成交均价环比"] <- round(a[i,"成交均价"]/a[i-1,"成交均价"]-1,4)
    }
    
    a %>% 
      e_charts(年份) %>% 
      e_line(成交均价环比,legend = F) %>% 
      e_tooltip(trigger = "item",
                e_tooltip_item_formatter(style = "percent")) %>% 
      e_y_axis(splitLine=list(show=F),
               min=JS("function(value){
                      return(value.min-0.01)
               }"),
               max=JS("function(value){
                      return(value.max+0.01)
               }")) %>% 
      e_title("年度成交均价环比走势图")
  })
  
  output$first_hand_deal_project_list_datatable <- renderDataTable({
    project_name %>% 
      as.tbl() %>% 
      filter(日期 >= as.Date(format(Sys.Date(), "%Y-%m-01"))) %>% 
      arrange(desc(日期)) %>% 
      mutate("项目推广名"=if_else(is.na(项目推广名),
                                      "暂无数据",
                                      项目推广名)) %>% 
      select(项目名称,
             项目推广名,
             "区域"=所在地区,
             "开盘日期"=日期,
             套数,
             `面积(㎡)`=面积) %>% 
      datatable(class = "cell-border",width = "100%",extensions = "Scroller",
                options = list(autoWidth = T,ordering = F,scroller = T,scrollX = 25,
                               scrollY = 550,dom = 't',
                               columnDefs = list(
                                 list(width = '10%',className = 'dt-center', 
                                      targets = c(2,5,6)),
                                 list(width = '20%',className = 'dt-center', 
                                      targets = c(3,4)),
                                 list(width = '30%',className = 'dt-center',
                                      targets = c(1))),
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({
                                 'background-color': '#E5E7EE', 'color': '#444444'
                                 });",
                                 "}")))
  })
  
# 二手房市 --------------------------------------------------------------------

  
  output$housing_market_month_select_deal_area <- renderEcharts4r({
    House_market %>% 
      as.tbl() %>% 
      filter(月份%in%input$housing_market_month_select&
                 所在区域%in%input$housing_market_area_select) %>% 
      group_by(所在区域) %>% 
      summarise("成交套数" = sum(`X.二手房住宅.交易套数`,na.rm = T),
                "成交面积" = sum(`X.二手房住宅.交易面积`,na.rm = T)*10000) %>% 
      e_charts(所在区域) %>% 
      e_bar(成交套数) %>% 
      e_line(成交面积,y_index=1,smooth = T) %>% 
      e_y_axis(name="成交套数(套)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_y_axis(index = 1,
               name="成交面积(㎡)",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F),
               formatter=JS("function(value){
                            return(value/10000 + '万')
               }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_legend(bottom = 10) %>% 
      e_title("各区域二手房成交套数&面积情况")
  })
  
  output$house_market_deal <- renderEcharts4r({
    House_market %>% 
      as.tbl() %>% 
      arrange(日期) %>%
      filter(所在区域%in%input$housing_market_area_select) %>% 
      group_by(所在区域) %>% 
      e_charts(月份) %>% 
      e_bar(`X.二手房住宅.交易套数`) %>% 
      e_y_axis(name="成交套数(套)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>% 
      e_tooltip(trigger = "item") %>% 
      e_legend(bottom = 10) %>% 
      e_title("各区域二手房成交套数情况")
  })
  
  output$house_market_deal_area <- renderEcharts4r({
    House_market %>% 
      as.tbl() %>% 
      arrange(日期) %>%
      mutate("二手房住宅交易面积" = X.二手房住宅.交易面积*10000) %>% 
      filter(所在区域%in%input$housing_market_area_select) %>% 
      group_by(所在区域) %>% 
      e_charts(月份) %>% 
      e_bar(二手房住宅交易面积) %>% 
      e_y_axis(name="成交面积(㎡)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               formatter=JS("function(value){
                          return(value/10000 + '万')
             }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_legend(bottom = 10) %>% 
      e_title("各区域二手房成交面积情况")
  })
  
# 金融政策 --------------------------------------------------------------------

  output$last_month_m1 <- renderValueBox({
    m2 %>%
      as.tbl() %>%
      arrange(desc(riqi)) %>%
      slice(1L) %>%
      mutate("M1"=M1数量.亿元./10000) %>% 
      select(M1) %>%
      paste0("万亿元") %>%
      valueBox(subtitle = "上月M1数据")

  })
  
  output$last_month_m2 <- renderValueBox({
    m2 %>%
      as.tbl() %>%
      arrange(desc(riqi)) %>%
      slice(1L) %>%
      mutate("M2"=M2数量.亿元./10000) %>% 
      select(M2) %>%
      paste0("万亿元") %>%
      valueBox(subtitle = "上月M2数据")
    
  })
  
  output$benchmark_lending_rate_last <- renderValueBox({
    benchmark_lending_rate %>% 
      as.tbl() %>% 
      arrange(desc(date)) %>% 
      slice(1L) %>% 
      transmute(贷款基准利率...*100) %>% 
      paste0("%") %>% 
      valueBox(subtitle = "最新贷款基准利率")
  })
  
  output$deposit_reserve_ratio_l <- renderValueBox({
    deposit_reserve_ratio %>% 
      as.tbl() %>% 
      arrange(desc(date)) %>% 
      slice(1L) %>% 
      transmute(大型金融机构*100) %>% 
      paste0("%") %>% 
      valueBox(subtitle = "存款准备金率 : 大型金融机构")
  })
  
  output$deposit_reserve_ratio_ms <- renderValueBox({
    deposit_reserve_ratio %>% 
      as.tbl() %>% 
      arrange(desc(date)) %>% 
      slice(1L) %>% 
      transmute(中小金融机构*100) %>% 
      paste0("%") %>% 
      valueBox(subtitle = "存款准备金率 : 中小金融机构")
  })
  
  output$per_capita_disposable_income_city <- renderValueBox({
    per_capita_disposable_income %>% 
      as.tbl() %>% 
      arrange(desc(date)) %>% 
      slice(1L) %>% 
      transmute(round(城镇常住居民人均可支配收入/10000,2)) %>% 
      paste0("万元") %>% 
      valueBox(subtitle = "最新温州城镇人均可支配收入")
  })
  
  output$per_capita_disposable_income_country <- renderValueBox({
    per_capita_disposable_income %>% 
      as.tbl() %>% 
      arrange(desc(date)) %>% 
      slice(1L) %>% 
      transmute(round(农村常住居民人均可支配收入/10000,2)) %>% 
      paste0("万元") %>% 
      valueBox(subtitle = "最新温州农村人均可支配收入")
  })
  
  output$cn_wz_cpi_cn <- renderValueBox({
    cn_wz_cpi %>% 
      as.tbl() %>% 
      arrange(desc(riqi)) %>% 
      slice(1L) %>% 
      select(全国CPI) %>% 
      valueBox(subtitle = "最新全国居民消费价格指数(CPI)")
  })
  
  output$cn_wz_cpi_wz <- renderValueBox({
    cn_wz_cpi %>% 
      as.tbl() %>% 
      arrange(desc(riqi)) %>% 
      slice(1L) %>% 
      select(温州市CPI) %>% 
      valueBox(subtitle = "最新温州市居民消费价格指数")
  })
  
  output$per_capita_disposable_income_draw <- renderEcharts4r({
    per_capita_disposable_income %>% 
      as.tbl() %>% 
      mutate("year"=as.factor(year(date))) %>% 
      e_charts(year) %>% 
      e_area(城镇常住居民人均可支配收入,smooth = T,legend = F) %>% 
      e_area(农村常住居民人均可支配收入,smooth = T,legend = F) %>% 
      e_x_axis(name = "年份",nameLocation="center",nameGap=25) %>% 
      e_y_axis(name = "人均可支配收入(元)",
               nameLocation="center",
               nameGap=40,
               splitLine = list(show = F),
               formatter = JS("function(value){
                              return(value/1000 + 'k')
               }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("温州市历年城镇&农村人均可支配收入")
  })
  
  output$per_capita_disposable_income_detail_draw <- renderEcharts4r({
    per_capita_disposable_income_detail %>% 
      as.tbl() %>% 
      filter(区域%in%input$wz_area_select_income_detail) %>% 
      arrange(riqi) %>% 
      mutate("year"=as.factor(year(riqi))) %>% 
      e_charts(year) %>% 
      e_bar(城镇常住居民人均可支配收入.元.,legend = F) %>% 
      e_x_axis(name = "年份",nameLocation="center",nameGap=25) %>% 
      e_y_axis(name = "温州市城镇常住居民人均可支配收入(元)",
               nameLocation="center",
               nameGap=40,
               formatter = JS("function(value){
                              return (value/1000 + 'k')
               }"),
               splitLine = list(show = F)) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("温州市城镇常住居民人均可支配收入")
  })
  
  output$cn_wz_people_cpi <- renderEcharts4r({
    cn_wz_cpi %>% 
      as.tbl() %>% 
      arrange(riqi) %>% 
      mutate("year"=as.factor(year(riqi))) %>% 
      filter(year(riqi)>=2013) %>% 
      e_charts(year) %>% 
      e_area(全国CPI,smooth = T,legend = F) %>% 
      e_area(温州市CPI,smooth = T,legend = F) %>% 
      e_x_axis(name = "年份",nameLocation="center",nameGap=25) %>% 
      e_y_axis(name = "CPI",
               nameLocation="center",
               nameGap=40,
               splitLine = list(show = F),
               min=JS("function(value){
                    return(value.min-0.7)
             }"),
               minInterval=1) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("全国&温州市历年居民消费价格指数(CPI)")
  })
  
  output$deposit_reserve_ratio_line <- renderEcharts4r({
    deposit_reserve_ratio %>% 
      as.tbl() %>% 
      filter(year(date)>=2015) %>% 
      arrange(date) %>% 
      e_charts(date) %>% 
      e_line(大型金融机构,smooth=T,legend=F) %>% 
      e_line(中小金融机构,smooth=T,legend=F) %>% 
      e_x_axis(name = "日期",nameLocation="center",nameGap=25) %>% 
      e_y_axis(name = "占比",
               nameLocation="center",
               nameGap=40,
               splitLine = list(show = F),
               min=JS("function(value){
                    return(value.min-0.01)
             }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("全国存款准备金率")
  })
  
  output$benchmark_lending_rate_cn <- renderEcharts4r({
    benchmark_lending_rate %>% 
      as.tbl() %>% 
      arrange(date) %>% 
      e_charts(date) %>% 
      e_area(贷款基准利率...,smooth=T,legend=F) %>% 
      e_x_axis(name = "日期",nameLocation="center",nameGap=25) %>% 
      e_y_axis(name = "贷款基准利率",
               nameLocation="center",
               nameGap=40,
               splitLine=list(show=F),
               min=0.04,
               minInterval=0.01) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("全国贷款利率")
  })
  
  output$cn_M2_real_estate_loan_quantity <- renderEcharts4r({
    cpi %>% 
      as.tbl() %>% 
      filter(year(date)>=2018) %>% 
      arrange(date) %>% 
      mutate("月份"=substring(date,first = 1,last = 7)) %>% 
      e_charts(月份) %>% 
      e_bar(房地产贷款量.亿元.,legend = F) %>% 
      e_line(货币供应量.亿元.,y_index = 1,smooth=T,legend=F) %>% 
      e_y_axis(name="房地产贷款量(亿元)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F)) %>%
      e_y_axis(index = 1,
               name="货币供应量(亿元)",
               nameLocation="center",
               nameGap=45,
               splitLine=list(show=F),
               min=JS("function(value){
                    return(value.min-10000)
             }"),
               formatter=JS("function(value){
                          return(Math.round(value/10000) + '万')
             }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("全国货币供应量(M2)VS房地产贷款量")
  })
  
  output$cn_his_M1_M2_increase <- renderEcharts4r({
    m2 %>% 
      as.tbl() %>% 
      arrange(desc(riqi)) -> m2_t
    for (i in 1:(nrow(m2_t)-1)) {
      m2_t[i,"M1同比增长"] <- round(m2_t[i,"M1数量.亿元."]/m2_t[i+12,"M1数量.亿元."]-1,4)
      m2_t[i,"M2同比增长"] <- round(m2_t[i,"M2数量.亿元."]/m2_t[i+12,"M2数量.亿元."]-1,4)
    }
    m2_t %>% 
      as.tbl() %>% 
      head(10) %>% 
      mutate("月份"=substring(riqi,first = 1,last = 7)) %>% 
      arrange(riqi) %>% 
      e_charts(月份) %>% 
      e_bar(M1数量.亿元.) %>% 
      e_bar(M2数量.亿元.) %>% 
      e_line(y_index = 1,M1同比增长,smooth=T) %>% 
      e_line(y_index = 1,M2同比增长,smooth=T) %>% 
      e_y_axis(name="M1&M2",
               nameLocation="center",
               nameGap=45,
               min=400000,
               maxInterval=200000,
               splitLine=list(show=F),
               formatter = JS("function(value){
                            return(value/10000 + '万')
             }")) %>% 
      e_y_axis(index = 1,
               name="同比增长",
               nameLocation="center",
               nameGap=35,
               splitLine=list(show=F)) %>% 
      e_legend(bottom = 10) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("全国M1,M2历史数据&同比增长")
  })

# 人口概况 --------------------------------------------------------------------

  output$wz_people <- renderValueBox({
    wenzhou_population %>% 
      as.tbl() %>% 
      filter(year(date)=="2017") %>% 
      select(`人口.万人.`) %>% 
      paste0("万人") %>% 
      valueBox(subtitle = "温州市人口总量")
      
  })
  
  output$wz_city_people_per <- renderValueBox({
    wenzhou_population %>% 
      as.tbl() %>% 
      filter(year(date)=="2017") %>% 
      select(`城镇人口比重`) -> a
    valueBox(paste0(a*100,"%"),subtitle = "温州市城镇人口比重")
  })
  
  output$wz_area_population_per <- renderEcharts4r({
    people %>% 
      as.tbl() %>% 
      filter(year(date)==max(year(date)),
             !(区域%in%c("全市","市区")))%>% 
      mutate("所在地区"=substring(区域,1,2)) %>% 
      arrange(desc(人口)) %>% 
      e_charts(所在地区) %>% 
      e_pie(人口,radius = c("50%","70%"),legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("2017年末温州市各区域人口数量分布情况")
  })
  
  output$wz_total_people_growth_rate <- renderEcharts4r({
    people %>% 
      as.tbl() %>%
      filter(区域%in%input$wz_area_select_people) %>% 
      arrange(date) -> a
    for (i in 1:(nrow(a)-1)) {
      a[i+1,"人口增长率"] <- round(a[i+1,"人口"]/a[i,"人口"]-1,4)
    }
    a %>% 
      mutate("year"=as.factor(year(date))) %>% 
      e_charts(year) %>% 
      e_bar(人口,legend = F) %>% 
      e_line(人口增长率,y_index = 1,smooth = T,legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_x_axis(name = "年份",nameLocation="center",nameGap=25) %>% 
      e_y_axis(min=JS("function(value){
                      return(value.min-10000)
    }"),
               minInterval = 10000,
               splitLine = list(show = F),
               formatter = JS("
                            function(value){
                              return(Math.floor(value/10000) + '万')
                            }")) %>%
      e_y_axis(index = 1,
               serie = c(min(人口增长率),max(人口增长率)),
               splitLine = list(show = F)) %>% 
      e_title("温州市人口总量&增长率")#刻度间隔和刻度最小值要与刻度formatter相对应
  })
  
  output$wz_city_population_per <- renderEcharts4r({
    people %>% 
      as.tbl() %>% 
      filter(year(date)=="2017",
             区域%in%input$wz_area_select_people) %>% 
      mutate("城镇人口比重"=round(城镇人口/人口*100)) %>% 
      select(城镇人口比重) %>% 
      as.numeric() -> a 
    e_charts() %>%
      e_gauge(a,"城镇人口比重") %>% 
      e_title("2017年温州市城镇人口比重")
  })
  
# 经济形势 --------------------------------------------------------------------

  
  output$wz_GDP_total <- renderValueBox({
    wenzhou_population %>% 
      as.tbl() %>% 
      filter(year(date)=="2017") %>% 
      select(`GDP.亿元.`) %>% 
      paste0("亿元") %>% 
      valueBox(subtitle = "2017年温州市GDP总量")
  })
  
  output$wz_per_GDP <- renderValueBox({
    wenzhou_population %>% 
      as.tbl() %>% 
      filter(year(date)=="2017") %>% 
      mutate("人均GDP"=`GDP.亿元.`*10000/`人口.万人.` ) %>% 
      select(人均GDP) %>% 
      round(0) %>% 
      paste0("元") %>% 
      valueBox(subtitle = "2017年温州市人均GDP")
  })
  
  output$wz_area_percent_GDP <- renderEcharts4r({
    people %>% 
      as.tbl() %>% 
      filter(year(date)==max(year(date)),
             !(区域%in%c("全市","市区")))%>% 
      mutate("所在地区"=substring(区域,1,2)) %>% 
      arrange(desc(GDP)) %>% 
      e_charts(所在地区) %>% 
      e_pie(GDP,radius = c("50%","70%"),legend = F) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("2017年温州市各区GDP占比情况")
  })
  
  output$wz_GDP_total_line <- renderEcharts4r({
    people %>% 
      as.tbl() %>%
      filter(区域%in%input$area_select_right) %>% 
      arrange(date) %>% 
      mutate("year"=as.factor(year(date))) %>% 
      e_charts(year) %>% 
      e_area(GDP,legend = F,smooth = T) %>% 
      e_y_axis(serie = c(min(GDP),max(GDP)),
               splitLine = list(show = F),
               formatter = JS("function(value){
                              return(Math.round(value/10000) + '亿元')
               }")) %>% 
      e_tooltip(trigger = "item") %>% 
      e_title("温州市GDP总量走势图")
  })
  
  session$onSessionEnded(
    function(){
      dbDisconnect(conn)
    }
  )
} 

shinyApp(server = server, ui = ui)






