combine_function <-
function(results1, results2) {
  results1 %>% 
    bind_rows(results2) %>% 
    relocate(`R^2`, .after = last_col()) %>% 
    relocate(group, .before = 1) %>% 
    group_by(group)
}
