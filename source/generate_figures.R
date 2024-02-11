# author: Tiffany Timbers & Jordan Bourak
# date: 2021-11-22
# Adapted from github.com/ttimbers/equine_numbers_value_canada_parameters

"
Loads horse population data (downloaded from
http://open.canada.ca/data/en/dataset/43b3a9b3-3842-45e7-8bc8-c4c27b9462ab and
http://open.canada.ca/data/en/dataset/b374f60b-9580-44dc-83f6-c0a850c15f30)
and generates figures and tables related to horse count across
provinces.

Usage: src/generate_figures.R --input_dir=<input_dir>  --out_dir=<output_dir> 

Options:
--input_dir=<input_dir>		Path (including filename) to raw data
--out_dir=<output_dir>		Path to directory where the results should be saved
" -> doc

library(docopt)
library(gridExtra)
library(knitr)
library(tidyverse)

opt <- docopt(doc)

main <- function(input_dir, out_dir) {
  # Create out_dir if it does not exist
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

	# Load the data and do some pre-processing
	horse_pop <- read_csv(input_dir) |>
	  filter(DATE == "At June 1 (x 1,000)", GEO != "Canada") |>
	  mutate(GEO = ifelse(GEO == "Prince Edward Island", "P.E.I.", GEO)) |>
	  mutate(Value = Value * 1000)

  # Generate plot for historical number of horses per province in Canada
	horse_pops_plot <- ggplot(horse_pop, aes(x = Ref_Date, y = Value)) +
      geom_point() +
      geom_line() +
      xlab("Year") +
      ylab("Number of horses") +
      ggtitle("Historical number of horses per province in Canada") +
      facet_grid(~GEO) +
      scale_y_continuous(labels = scales::comma) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave("horse_pops_plot.png", device = "png", path = out_dir, width = 10, height = 3)

  # Generate table with max, min, and standard deviation of number of horses
  # for each province, in descending order of standard deviation
  horses_sd <- horse_pop |>
      select(GEO, Value) |>
      rename(Province = GEO) |> 
      group_by(Province) |>
      summarize(Std = sd(Value)) |>
      arrange(desc(Std))
  write_csv(horses_sd,
            file.path(out_dir, "horses_sd.csv"))
    
  # Generate plot for historical number of horses for the province
  # with the largest standard deviation only
  largest_sd_prov <- horses_sd$Province[1]
	horse_pop_plot_largest_sd <- horse_pop |>
      filter(GEO == largest_sd_prov) |>
      ggplot(aes(x = Ref_Date, y = Value)) +
          geom_point() +
          geom_line() +
          xlab("Year") +
          ylab("Number of horses") +
          ggtitle(paste("Historical number of horses in",
                        largest_sd_prov)) +
          scale_y_continuous(labels = scales::comma) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave("horse_pop_plot_largest_sd.png", device = "png", path = out_dir, width = 5, height = 4)
}

main(opt[["--input_dir"]], opt[["--out_dir"]])