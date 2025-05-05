# customValueBox.R
customValueBox <- function(title, value, box_color) {
  # Create a card-like HTML block using the provided title, value, and box color.
  HTML(paste0(
    "<div style='padding: 20px; border: 2px solid ", box_color, 
         "; border-radius: 5px; background-color: #f9f9f9;'>",
      "<h4 style='font-size: 20px; margin-top: 0; margin-bottom: 10px; color: ", box_color, ";'>", title, "</h4>",
      "<p style='font-size: 16px; font-weight: bold; margin-bottom: 0; color: ", box_color, ";'>", 
        value,
      "</p>",
    "</div>"
  ))
}
