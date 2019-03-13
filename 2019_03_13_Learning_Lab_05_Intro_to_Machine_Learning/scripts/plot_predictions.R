plot_predictions <- function(model, new_data) {
    
    suppressWarnings({
        g <- model %>% 
            predict(new_data) %>%
            bind_cols(new_data %>% select(Price_num, Model, Category, ProductFamily, ModelBase, Weight_lb)) %>%
            rename(Prediction = .pred, Actual = Price_num) %>%
            mutate(observation = row_number() %>% as.character() %>% as_factor()) %>%
            gather(key = "key", value = "value", Prediction, Actual, factor_key = TRUE) %>%
            mutate(key = fct_rev(key)) %>%
            mutate(label_text = str_glue("Price: {scales::dollar(value)} ({key})
                                          Model : {Model}
                                          Category: {Category}
                                          ProductFamily: {ProductFamily}
                                          Weight: {Weight_lb} lbs")) %>%
            
            # Visualize
            ggplot(aes(x = observation, y = value, color = key, text = label_text)) +
            geom_point(size = 4) +
            expand_limits(y = 0) +
            theme_tq() +
            scale_color_tq() +
            coord_flip() +
            labs(title = "Prediction vs Actual")
    })
    
    ggplotly(g, tooltip = "text")
}
