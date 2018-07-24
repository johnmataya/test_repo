library(shiny)
library(d3r)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(scales)
library(ggthemes) 
library(plotly)
library(maps)

# read the data frames
batting_df <- read.csv("batting_sum.csv")
team_sal_df <- read.csv("team_wins_sal.csv")
player_sal_df <- read.csv("player_sal.csv")
batting_states_df <- read.csv("batting_states.csv")
states_sum_df <- read.csv("states_sum.csv")

glist <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

ui <- fluidPage(
  
  theme = shinytheme("cerulean"),  
  h1("MLB Data Exploration"),
  HTML("<div style='color:grey;font-size:9;font-style:italic;'>Source: Lahman's Baseball Database, copyright 1996-2017 by Sean Lahman</div>"),
  br(),
  tabsetPanel(
    
    tabPanel("Batting Stats",     
             br(),
             fluidRow(
               column(3,
                      sliderInput("year", "Year:", min=1985, max=2016, step=1, value=2016, sep=""),
                      selectInput("stat", "Choose a stat:", choices=c("Hits","Home Runs","Runs Batted In","Stolen Bases"), selected="Home Runs")   
               ),
               column(9,
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "barstyle.css")),
                      tags$script(src="https://d3js.org/d3.v3.min.js"),
                      HTML("<div style='font-size:16px;'>"),
                      htmlOutput("user_selection"),
                      HTML("</div>"),
                      tags$div(id="div_bar"),
                      tags$script(src="d3barchartscript.js")
               )
             )
    ),
    
    tabPanel("Wins vs Salary ", 
             br(),
             fluidRow(
               column(12,
                      sliderInput("year_in","Year:", min=1985, max=2016, value=2016, step=1,sep="") 
               )
             ), 
             fluidRow(
               column(7, 
                      plotOutput("team_plot", click = "team_click", height = "565px")
               ),
               column(5,
                      fluidRow(
                        column(12, 
                               HTML("<div style='font-size:15px;'>"),
                               htmlOutput("team_info"),
                               HTML("</div>")
                        )
                      ),
                      fluidRow(
                        column(12,
                               br(),
                               HTML("<div style='font-size:15px;'>"),
                               htmlOutput("team_value"),
                               br(),
                               tableOutput("player_rows"),
                               HTML("</div>")
                        )
                      )
               )
             ) 
    ),
    
    tabPanel("Home State",
             br(),
             fluidRow(
               column(12, 
                      sliderInput("slider","Year:",min=1985, max=2016, step=1, value=2016, sep="")
               )
             ),
             
             fluidRow(        
               column(7, 
                      plotlyOutput("plot")
               ),
               column(5, 
                      fluidRow(
                        column(12,
                               HTML("<div style='font-size:15px;'>"),
                               htmlOutput("click"),
                               HTML("</div>")
                        )
                      ),
                      fluidRow(
                        column(12,
                               br(),
                               HTML("<div style='font-size:15px;'>"),
                               htmlOutput("other_countries"),
                               tableOutput("other_rows"),
                               HTML("</div>")
                        )
                      )
               )
             )
    )
  )
)


server <- function(input, output, session) {
  
  ########################   TAB1   ###################################
  observe({
    
    if(input$stat == 'Home Runs'){
      sel_stat = 'HR'
    }
    else if (input$stat == 'Hits'){
      sel_stat = 'H'
    }
    else if (input$stat == 'Runs Batted In'){
      sel_stat = 'RBI'
    }
    else if (input$stat == 'Stolen Bases'){
      sel_stat = 'SB'
    }
    #convert data to json format suitable for D3
    batting_json <- batting_df %>%
      filter(Year == input$year) %>%
      select(League,Division,Team,Player,sel_stat)
    
    names(batting_json)[5]<-"total"
    
    batting_json <- batting_json %>% 
      filter(total > 0) %>%
      d3_nest(value_cols="total", root="Major League Baseball") 
    
    #push data to the d3 script
    session$sendCustomMessage(type="jsondata", toString(batting_json))
    
  })
  
  
  output$user_selection <- renderText({
    HTML("<strong>", input$stat, "in ", input$year,"</strong>")
  })
  
  ########################   TAB2   ###################################
  vals <- reactiveValues(selected_team=c(), hovered_team=c())   
  
  observeEvent(input$team_click, {   
    team_sal_df_sel <- team_sal_df %>%
      filter(Year == input$year_in)        
    # find the closest data point to the click action 
    # and assign the team to vals$selected_team
    tc <- nearPoints(team_sal_df_sel, input$team_click, threshold=10, maxpoints=1)
    vals$selected_team <- tc$teamID
  })
  
  observeEvent(input$team_hover, {   
    team_sal_df_sel <- team_sal_df %>%
      filter(Year == input$year_in)   
    # find the closest data point to the hover action 
    # and assign the team to vals$hovered_team  
    th <- nearPoints(team_sal_df_sel, input$team_hover, threshold=10, maxpoints=1)
    vals$hovered_team <- th$teamID
  })
  
  output$team_info <- renderText ({
    
    if (length(vals$selected_team) != 0L) # if valid click
    {
      htdf <- team_sal_df %>%
        filter(Year == input$year_in & teamID==vals$selected_team)
      
      htdf <- transform(htdf, TeamName = as.character(TeamName))
      
      HTML("<br>Team: <strong>", htdf$TeamName,
           "</strong><br>Wins: <strong>", htdf$Wins,
           "</strong><br>Salary: <strong>", 
           paste0("$", formatC(as.numeric(htdf$Salary), format="f", digits=0, big.mark=",")), 
           "</strong><br>Attendance: <strong>", formatC(as.numeric(htdf$Attendance), format="f", digits=0, big.mark=","),
           "</strong>")
    }   
    else
    {           
      HTML("<br>Click a team marker for more info.")
    }
    
  }) 
  
  output$team_plot <- renderPlot({
    
    team_sal_df_sel <- team_sal_df %>%
      filter(Year == input$year_in)
    
    p <- ggplot(data=team_sal_df_sel, aes(x=Salary, y=Wins)) + geom_point(aes(color=Attendance), size=7) 
    
    p <- p + geom_text(aes(label = Team), color="grey50", check_overlap = TRUE, size=6, vjust=2)
    p <- p + scale_color_gradient(low="lightblue", high="blue",labels=comma)
    
    # to highlight a particular state, you can add this (like in a hover event)
    p <- p + geom_point(data=subset(team_sal_df_sel, teamID==vals$hovered_team), shape=21, size=6) 
    
    p <- p + scale_x_continuous(labels = dollar)
    
    p <- p + theme_minimal()
    p <- p + theme(line = element_line(size=0.1), axis.line = element_line(linetype = "dashed"))
    p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
    p <- p + theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
    
    p <- p + ggtitle(paste("Wins vs Salary By Team in",input$year_in)) + theme(plot.title = element_text(size=16))
    
    p   
  })
  
  # print the name of the value selected
  output$team_value <- renderText({
    
    if (length(vals$selected_team) != 0L) # if valid click
    {
      stdf <- team_sal_df %>%
        filter(Year == input$year_in & teamID==vals$selected_team)
      
      stdf <- transform(stdf, TeamName = as.character(TeamName))
      
      HTML("Top 10 Highest Paid Players for the ", stdf$TeamName, "<br>")
    }
  })
  
  # print the rows of the data frame which match the selected value
  output$player_rows <- renderTable({
    
    if (length(vals$selected_team) != 0L) # if valid click
    {
      
      player_sal_df_sel <- player_sal_df %>% 
        filter(teamID == vals$selected_team & Year == input$year_in) %>%
        select(Player, Salary) %>%
        arrange(desc(Salary)) %>%
        top_n(10) %>%
        mutate(Salary = (paste0("$", formatC(as.numeric(Salary), format="f", digits=0, big.mark=","))))
      
      head(player_sal_df_sel,10)
    }
  }, rownames = TRUE)
  
  ########################   TAB3   ###################################
  
  # create the map here   
  output$plot <- renderPlotly({
    
    # the following map depends on reac$year; will be executed each time year is changed by user.      
    states_sum_df_sel <- states_sum_df %>%
      filter(Year == input$slider)
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    
    plot_geo(states_sum_df_sel, locationmode = 'USA-states') %>%
      add_trace(z = ~Count, locations = ~birthState, color = ~Count, colors = 'Blues', source="A") %>%
      layout(title = paste('MLB Players By Birth State in',input$slider), geo = glist)
  })
  
  # print the name of the value selected
  output$other_countries <- renderText({
    
    HTML("Top 10 Countries of MLB Players in ", input$slider, "<br><br>")
    
  })
  
  # print the rows of the data frame which match the selected value
  output$other_rows <- renderTable({
    
    batting_states_df_sel <- batting_states_df %>% 
      filter(Year == input$slider) %>%
      group_by(birthCountry) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) %>%
      top_n(10)
    
    batting_states_df_sel <- transform(batting_states_df_sel, birthCountry = as.character(birthCountry))
    batting_states_df_sel <- rename(batting_states_df_sel, BirthCountry = birthCountry)
    head(batting_states_df_sel,10)
  }, rownames = TRUE)
  
  output$click <- renderText({
    
    # this is how we catch plotly mouse events 
    d <- event_data("plotly_click", source="A")
    if (is.null(d)) {
      print("Click on a state for more info.<br><br><br><br>")
    } 
    else {
      
      states_sum_df_sel <- states_sum_df %>%
        filter(Year == input$slider)
      
      states_sum_df_sel$hover <- with(states_sum_df_sel, 
                                      paste("State: <strong>",gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(State), perl=TRUE),
                                            "</strong><br>Total Players: <strong>", Count, 
                                            "</strong><br>Total Salary: <strong>", paste0("$", formatC(as.numeric(Salary), format="f", digits=0, big.mark=",")), 
                                            "</strong><br>Total Home Runs: <strong>", HR, "</strong>")
      )
      
      print(states_sum_df_sel$hover[d$pointNumber+1])
    }
  })
  
}

shinyApp(ui=ui, server=server)