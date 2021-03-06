library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)

shinyApp(
  
  ui = dashboardPagePlus(
    header = dashboardHeaderPlus(
      fixed = TRUE,
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears",
      left_menu = tagList(
        dropdownBlock(
          id = "mydropdown",
          title = "Dropdown 1",
          icon = icon("sliders"),
          sliderInput(
            inputId = "n",
            label = "Number of observations",
            min = 10, max = 100, value = 30
          ),
          prettyToggle(
            inputId = "na",
            label_on = "NAs keeped",
            label_off = "NAs removed",
            icon_on = icon("check"),
            icon_off = icon("remove")
          )
        ),
        dropdownBlock(
          id = "mydropdown2",
          title = "Dropdown 2",
          icon = icon("sliders"),
          prettySwitch(
            inputId = "switch4",
            label = "Fill switch with status:",
            fill = TRUE,
            status = "primary"
          ),
          prettyCheckboxGroup(
            inputId = "checkgroup2",
            label = "Click me!",
            thick = TRUE,
            choices = c("Click me !", "Me !", "Or me !"),
            animation = "pulse",
            status = "info"
          )
        )
      ),
      dropdownMenu(
        type = "tasks",
        badgeStatus = "danger",
        taskItem(value = 20, color = "aqua", "Refactor code"),
        taskItem(value = 40, color = "green", "Design new layout"),
        taskItem(value = 60, color = "yellow", "Another task"),
        taskItem(value = 80, color = "red", "Write documentation")
      )
    ),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      shinythemes::themeSelector(),
      setShadow(class = "dropdown-menu")
    ),
  ),
  
  server = function(input, output) { }
)