# packages ----------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(UpSetR)
library(patchwork)
library(ggtext)

# function ----------------------------------------------------------------
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))

  grouped %>%
    group_split() %>%
    rlang::set_names(names)
}

# data --------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 29)

# wrangle the data to output a named list of the episode where each character
# caught the vilain
upsetr_data <-
  scoobydoo %>%
  select(index,
         starts_with("caught")) %>%
  mutate(across(starts_with("caught"), as.logical)) %>%
  pivot_longer(starts_with("caught"),
               values_to = "caught") %>%
  mutate(
    name =
      name %>%
      str_remove("caught_") %>%
      str_to_title()
  ) %>%
  filter(! name %in% c("Other", "Not")) %>%
  named_group_split(name) %>%
  map(~ .x %>%
        filter(caught) %>%
        pull(index))

upset_plot <-
  upset(
    data = fromList(upsetr_data), nintersects = 11,
    queries = lst(
      lst(query = intersects,
          params = list("Shaggy", "Scooby"),
          color = "#B3B274",
          active = TRUE),
      lst(query = intersects,
          params = list("Shaggy"),
          color = "#a4c97c",
          active = TRUE),
      lst(query = intersects,
          params = list("Scooby"),
          color = "#c29a6c",
          active = TRUE)
    )
  )

upset_plot

legend <-
  ggplot() +
  geom_textbox(data = tibble(x = 0,
                             y = 0,
                             label = "<b style='font-size: 18pt'>Shaggy needs Scooby more than Scooby needs Shaggy</b>") %>%
                 mutate(label = str_replace_all(label, "Shaggy", "<span style='color: #a4c97c'>Shaggy</span>"),
                        label = str_replace_all(label, "Scooby", "<span style='color: #c29a6c'>Scooby</span>")),
               aes(x = x, y = y, label = label),

               family = "Playfair Display",
               box.color = "#FFFFFF00") +
  theme_void()

legend

# Combin the plots ------------------------------------------------------------
upset_plot_part <-
  wrap_elements(upset_plot$Main_bar) / wrap_elements(upset_plot$Matrix) +
  plot_layout(design =
                "A
                 A
                 B")

legend + upset_plot_part + plot_layout(design = "ABB")

ggsave("plot/2021_29/2021_29_scooby.png", device = ragg::agg_png)
