my_gt_style <- function(.data) {
  .data |>
    gt::gt() |>
    gt::tab_options(
      table.width = gt::pct(80),
      table.font.names = "Fira Sans",
      table.font.size = "10px",
      heading.align = "left",
      heading.title.font.weight = "bold",
      heading.title.font.size = "14px",
      heading.subtitle.font.size = "12px",
      column_labels.font.size = "11px",
      source_notes.font.size = "10px",
      row.striping.background_color = "#faf0fa"
      ) |>
    gt::opt_row_striping() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = "right", v_align = "middle"),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_body()
      )) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold", align = "left"),
      locations = gt::cells_column_labels(columns = 1)
      ) |>
    gt::tab_style(
      style = gt::cell_text(align = "right"),
      locations = gt::cells_source_notes()
    )
}
