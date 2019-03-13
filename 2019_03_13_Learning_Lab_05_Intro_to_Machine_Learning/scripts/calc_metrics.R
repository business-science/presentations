calc_metrics <- function(model, new_data, truth) {
    
    truth_expr <- enquo(truth)
    
    suppressWarnings({
        model %>%
            predict(new_data = new_data) %>%
            bind_cols(new_data %>% select(!! truth_expr)) %>%
            metrics(truth = !! truth_expr, 
                    estimate = .pred) %>%
            select(-.estimator) %>%
            spread(.metric, .estimate)
    })
    
}