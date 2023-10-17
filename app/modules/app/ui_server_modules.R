

value_box_UI <- function(id) {
  ns <- NS(id)
  tagList(
      div(
          class = "custom-value-box", 
          valueBoxOutput(ns("value_box"))
      )
  )
}

value_box_Server <- function(id, data = reactive(NULL), filter_col = "capacity_type", 
                             filter_value = "Bed", sum_col = NULL,
                             sub_title = reactive(NULL), icon = NULL) {
    moduleServer(
        id,
        function(input, output, session) {
            
            output$value_box <- renderValueBox({
                
                value <- shiny::req(data()) %>% 
                    filter(!!rlang::sym(filter_col) == filter_value) %>% 
                    pull(!!rlang::sym(sum_col)) %>% 
                    sum() %>% 
                    scales::comma()
                
                valueBox(
                    value    = value,
                    subtitle = sub_title(),
                    icon     = icon(icon, lib = "font-awesome")
                )
            })
        }
    )
}