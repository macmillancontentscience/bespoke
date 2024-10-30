# Can fit and predict bespoke_classifications

    Code
      set.seed(4242L)
      predict(oil_fit_formula, to_predict, type = "prob")
    Output
      # A tibble: 96 x 7
         .pred_corn .pred_olive .pred_peanut .pred_pumpkin .pred_rapeseed
              <dbl>       <dbl>        <dbl>         <dbl>          <dbl>
       1          0           1            0             0              0
       2          0           0            0             1              0
       3          0           0            1             0              0
       4          1           0            0             0              0
       5          0           1            0             0              0
       6          0           0            0             0              1
       7          0           0            1             0              0
       8          0           0            0             1              0
       9          0           1            0             0              0
      10          0           0            0             0              0
      # i 86 more rows
      # i 2 more variables: .pred_soybean <dbl>, .pred_sunflower <dbl>

# Function outputs are validated.

    Code
      set.seed(4242L)
      predict(oil_fit_corn, to_predict)
    Output
      # A tibble: 96 x 1
         .pred_class
         <fct>      
       1 corn       
       2 corn       
       3 corn       
       4 corn       
       5 corn       
       6 corn       
       7 corn       
       8 corn       
       9 corn       
      10 corn       
      # i 86 more rows

---

    Code
      set.seed(4242L)
      predict(oil_fit_corn_factor, to_predict)
    Output
      # A tibble: 96 x 1
         .pred_class
         <fct>      
       1 corn       
       2 corn       
       3 corn       
       4 corn       
       5 corn       
       6 corn       
       7 corn       
       8 corn       
       9 corn       
      10 corn       
      # i 86 more rows

# It also works in a parsnip context.

    Code
      set.seed(4242L)
      predict(bespoke_fit, new_data = to_predict, type = "class")
    Output
      # A tibble: 96 x 1
         .pred_class
         <fct>      
       1 olive      
       2 pumpkin    
       3 peanut     
       4 corn       
       5 olive      
       6 rapeseed   
       7 peanut     
       8 pumpkin    
       9 olive      
      10 sunflower  
      # i 86 more rows

---

    Code
      set.seed(4242L)
      predict(bespoke_fit, new_data = to_predict, type = "prob")
    Output
      # A tibble: 96 x 7
         .pred_corn .pred_olive .pred_peanut .pred_pumpkin .pred_rapeseed
              <dbl>       <dbl>        <dbl>         <dbl>          <dbl>
       1          0           1            0             0              0
       2          0           0            0             1              0
       3          0           0            1             0              0
       4          1           0            0             0              0
       5          0           1            0             0              0
       6          0           0            0             0              1
       7          0           0            1             0              0
       8          0           0            0             1              0
       9          0           1            0             0              0
      10          0           0            0             0              0
      # i 86 more rows
      # i 2 more variables: .pred_soybean <dbl>, .pred_sunflower <dbl>

