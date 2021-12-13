
# PFF Theme ---------------------------------------------------------------

gt_theme_pff <- function(data, ...) {
  data %>%
    # Add team logos w/ web_image
    text_transform(
      locations = cells_body(
        columns = c(team_logo_espn)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    # if missing, replace NA w/ ---
    fmt_missing(
      columns = everything(),
      missing_text = '---'
    ) %>%
    # hide spanner with transparent color
    # Change font color and weight for numeric col
    tab_style(
      style = list(
        cell_text(color = '#3a3d42', weight = 'bold')
      ),
      locations = cells_body(
        columns = 3:ncol(data$`_data`)
      )
    ) %>%
    # Make column labels and spanners all caps
    opt_all_caps() %>%
    # add row striping
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = '#585d63',
      table_body.hlines.color = 'white',
      table.border.top.width = px(3),
      table.border.top.color = 'white',
      table.border.bottom.color = 'white',
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = 'white',
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = 'white',
      row.striping.background_color = '#f9f9fb',
      data_row.padding = px(3)
    ) %>%
    # change color of border separating the text from the sourcenote
    tab_style(
      style = cell_borders(
        sides = 'bottom', color = '#585d63', weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = nrow(data$`_data`)
      )
    ) %>%
    # change font to Lato throughout (note no need to have Lato locally!)
    opt_table_font(
      font = c(
        google_font(name = 'Lato'),
        default_fonts()
      )
    ) %>%
    # add source note
    tab_source_note(
      source_note = md('**Data:** @nflfastR<br>**Table:** Colin Welsh')
    )
}

# Dark Theme --------------------------------------------------------------

gt_theme_cw <- function(data, ...) {
  
  # get_list_from_ellipsis(...)
  
  data %>%
    # Add team logos w/ web_image
    # text_transform(
    #   locations = cells_body(c(posteam)),
    #   fn = function(x) web_image(url = glue('https://a.espncdn.com/i/teamlogos/nfl/500/{x}.png'))
    # ) %>%
    text_transform(
      locations = cells_body(
        columns = c(headshot_url)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    text_transform(
      locations = cells_body(
        columns = c(team_logo_espn)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    # if missing, replace NA w/ ---
    fmt_missing(
      columns = everything(),
      missing_text = '---'
    ) %>%
    # hide spanner with transparent color
    # Change font color and weight for numeric col
    tab_style(style = cell_text(font = "Chivo", size = 'xx-large', weight = 'normal'), locations = cells_title(groups = 'title')) %>% 
    tab_style(style = cell_text(font = "Chivo", size = 'medium', weight = 'normal'), locations = cells_title(groups = 'subtitle')) %>% 
    tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>% 
    tab_style(style = cell_text(align = 'left'), locations = cells_body(c(1:3))) %>% 
    tab_style(
      style = list(
        cell_text(color = color_cw[5], weight = 'normal')
      ),
      locations = cells_body(
        columns = 4:ncol(data$`_data`)
      )
    ) %>% 
    tab_style(
      style = cell_text(font = 'Chivo', weight = 'normal'),
      locations = cells_body(
        columns = c(1:3)
      )
    ) %>% 
    tab_style(
      style = cell_text(font = 'Montserrat', weight = 'normal'),
      locations = cells_body(
        columns = c(4:ncol(data$`_data`))
      )
    ) %>% 
    # change overall table styling for borders and striping
    cols_width(c(posteam) ~ px(45)) %>% 
    tab_options(
      table.font.color = color_cw[5],
      data_row.padding = '2px',
      row_group.padding = '3px',
      column_labels.border.bottom.color = color_cw[5],
      column_labels.border.bottom.width = 1.4,
      column_labels.font.weight = 'bold',
      table_body.border.top.color = color_cw[5],
      row_group.border.top.width = 1.5,
      row_group.border.top.color = '#999999',
      table_body.border.bottom.width = 0.7,
      table_body.border.bottom.color = '#999999',
      row_group.border.bottom.width = 1,
      row_group.border.bottom.color = color_cw[5],
      # table.border.top.color = 'transparent',
      table.background.color = color_cw[1],
      # table.border.bottom.color = 'transparent',
      row.striping.background_color = color_cw[2],
      row.striping.include_table_body = TRUE
    ) %>% 
    # change color of border separating the text from the sourcenote
    tab_style(
      style = cell_borders(
        sides = 'bottom', 
        color = '#585d63', 
        weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        rows = nrow(data$`_data`)
      )
    ) %>% 
    opt_table_font(
      font = c(
        google_font(name = 'Montserrat'),
        default_fonts()
      )
    ) %>% 
    # add source note
    tab_source_note(
      source_note = md('**Data:** @nflfastR<br>**Table:** Colin Welsh')
    )
}
