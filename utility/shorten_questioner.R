shorten_text_column <- function(text_data, text, model_path, 
                                desired_pos_tags = c("NOUN", "VERB", "ADJ", "ADV")) {
  
  # Load udpipe model
  model <- udpipe_load_model(file = model_path) 
  
  # Annotate text with POS tags
  annotated <- udpipe(text_data$text, model) 
  
  # Filter for desired POS tags
  filtered_text <- annotated %>%
    filter(upos %in% desired_pos_tags) %>%
    group_by(doc_id) %>%
    summarize(shortened_text = paste(token, collapse = " "))
  
  # Join shortened text with original data
  # output_data <- left_join(text_data, filtered_text, by = "doc_id")
  
  return(filtered_text)
}
