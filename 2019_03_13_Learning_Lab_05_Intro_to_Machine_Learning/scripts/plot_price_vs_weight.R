plot_price_vs_weight <- function() {

    g <- price_vs_weight_tbl %>%
        
        mutate(
            label_text = str_glue("Model: {Model}
                              Category: {Category}
                              ProductFamily: {ProductFamily}
                              Price: {scales::dollar(Price_num)}
                              Weight: {Weight_lb} lbs")
        ) %>%
        
        ggplot(aes(Weight_lb, Price_num, color = Category)) + 
        geom_point(aes(text = label_text)) +
        geom_smooth(span = 3, se = FALSE) +
        scale_color_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        theme_tq() +
        labs(title = "2019 Cannondale Bicycles: Understanding the Pricing Model",
             y = "Price (USD)", x = "Weight (lbs)")
    
    plotly::ggplotly(g, tooltip = "text") %>%
        layout(legend = list(x = 0.8, y = 0.9))
}