## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(conformr)

## ----load_code_dict-----------------------------------------------------------
library(magrittr)
library(dplyr)

# import correspondence table
code_dict <- #conformr:::toy_AB$codes_BA 
  dplyr::tribble(~ std_B, ~ std_A,
                                "A1", "x1111", # one-to-one
                                "B2", "x2222", # many-to-one
                                "B2", "x3333",
                                "C3", "x4444", # one-to-many (4)
                                "C4", "x4444",
                                "C4", "x6666", # many-to-many
                                "C5", "x4444",
                                "C6", "x4444",
                                "C7", "x5555", # one-to-many (3)
                                "C8", "x5555",
                                "C9", "x5555"
                                )

# define code_in & code_out references
code_in <- sym("std_A")
code_out <- sym("std_B")

code_dict %>%
  select({{code_in}}, {{code_out}}) %>%
  arrange({{code_in}}, {{code_out}})

## ----panel-map-with-cases-----------------------------------------------------
panel_map_case <- code_dict %>%
  # work out where the values for each code_in will go
  group_by({{ code_in }}) %>%
  mutate(n_dest = n_distinct({{ code_out }}),
         split_in = 1/n_dest) %>%
  ungroup() %>%
  # work out how many values are going to each destination code_out
  group_by({{ code_out }}) %>%
  mutate(dest_size = n_distinct({{ code_in }})) %>%
  # label each case
  mutate(case_in_to_out = case_when(n_dest == 1 ~ "no split",
                                    n_dest != 1 ~ "split")) %>%
  mutate(case_out_sum = case_when(dest_size == 1 ~ "no sum",
                                  dest_size != 1 ~ "sum up")) %>%
  mutate(map_case = case_when(n_dest == 1 & dest_size == 1 ~ "1-to-1",
                              n_dest != 1 & dest_size == 1 ~ "1-to-many",
                              n_dest == 1 & dest_size != 1 ~ "many-to-1",
                              n_dest != 1 & dest_size != 1 ~ "many-to-many"))
  # mutate(value_trans = case_when(map_case == "1-to-1" ~ "no change",
  #                                map_case == "many-to-1" ~ "add up",
  #                                map_case == "1-to-many" ~ "split",
  #                                map_case == "many-to-many" ~ "split & add up"))

## ----summary_in_to_out--------------------------------------------------------
# summary table of what happens to each value_in for every code_in
panel_map_case %>%
  mutate(dest_w_split = paste0("", signif(split_in, 2) * 100, "%", " to ", {{ code_out }})) %>%
  group_by({{ code_in }}, case_in_to_out) %>%
  summarise(value_in_split_to_out = paste(dest_w_split, collapse = ", "),
            .groups = "drop") %>%
  select({{ code_in }}, value_in_split_to_out)

## ----summary_out_by_in--------------------------------------------------------
panel_map_case %>%
  mutate(source_w_split = paste0({{ code_in }}, " * [", signif(split_in, 2), "]")) %>%
  group_by({{ code_out }}, case_out_sum) %>%
  summarise(value_out_sum = paste(source_w_split, collapse = " + "),
            .groups = "drop") %>%
  select({{ code_out }}, value_out_sum)

## ----alluvial-panel-map-------------------------------------------------------
library(ggplot2)
library(ggalluvial)

# TODO: fix these cases to be exclusive
# TODO: color 
panel_map_case %>%
  ggplot(., 
         aes(axis1 = {{code_in}}, axis2 = {{code_out}},
             y = n_dest)) +
  scale_x_discrete(limits = c(deparse(code_in), deparse(code_out)), expand = c(.2, .05)) +
  scale_fill_viridis_d() +
  xlab("Classification") +
  geom_alluvium(aes(fill = map_case)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()
  #facet_grid(rows=vars(map_case))
  #           scales = "free")

## ---- eval=FALSE--------------------------------------------------------------
#  ##
#  library(tidyverse)
#  
#  toy_AB$codes_BA[5,] %>%
#    dplyr::bind_rows(conformr:::toy_AB$codes_BA) %>%
#    ## TODO: show mistake in weights caused by duplicate codes
#    ## make_panel_map_equal(., std_A, std_B) %>%
#    dplyr::arrange(std_A, std_B)

