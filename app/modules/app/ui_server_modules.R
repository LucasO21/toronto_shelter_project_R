

value_box_UI <- function(id) {
  ns <- NS(id)
  tagList(
      div(
          class = "custom-value-box", 
          valueBoxOutput(ns("value_box"), width = "50px")
      )
  )
}

value_box_Server <- function(id, data = reactive(NULL), value = reactive(NULL), 
                             sub_title = reactive(NULL), icon = NULL) {
    moduleServer(
        id,
        function(input, output, session) {
            
            output$value_box <- renderValueBox({
                
                # value <- shiny::req(data()) %>% 
                #     filter(!!rlang::sym(filter_col) == filter_value) %>% 
                #     pull(!!rlang::sym(sum_col)) %>% 
                #     sum() %>% 
                #     scales::comma()
                
                value_tbl <- shiny::req(data()) %>% 
                    dplyr::summarise(
                        sum_capacity = sum(pred_capacity_actual),
                        sum_occupied = sum(pred_occupied_adj),
                        .by = capacity_type
                    )
                
                v <- switch(
                    shiny::req(value()),
                    "capacity_bed"  = pull(value_tbl[1, ][2]),
                    "occupied_bed"  = pull(value_tbl[1, ][3]),
                    "bed_rate"      = pull(value_tbl[1, ][3] / value_tbl[1, ][2]) %>% scales::percent(),
                    "capacity_room" = pull(value_tbl[2, ][2]),
                    "occupied_room" = pull(value_tbl[2, ][3]),
                    "room_rate"     = pull(value_tbl[2, ][3] / value_tbl[2, ][2]) %>% scales::percent()
                )
                
                valueBox(
                    value    = v,
                    subtitle = sub_title(),
                    icon     = icon(icon, lib = "font-awesome")
                )
            })
        }
    )
}