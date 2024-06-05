truly_group_by <- function(data, ...){
  dots <- quos(...)
  data <- group_by( data, !!!dots )
  
  labels <- attr( data, "labels" )
  labnames <- names(labels)
  labels <- mutate( labels, ..index.. =  attr(data, "indices") )
  
  expanded <- labels %>%
    tidyr::expand( !!!dots ) %>%
    left_join( labels, by = labnames ) %>%
    mutate( ..index.. = map(..index.., ~if(is.null(.x)) integer() else .x ) )
  
  indices <- pull( expanded, ..index..)
  group_sizes <- map_int( indices, length)
  labels <- select( expanded, -..index..)
  
  attr(data, "labels")  <- labels
  attr(data, "indices") <- indices
  attr(data, "group_sizes") <- group_sizes
  
  data
}