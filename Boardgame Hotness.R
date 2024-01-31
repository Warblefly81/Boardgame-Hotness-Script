library(httr)
library(xml2)
library(dplyr)

get_hotness_games <- function() {
  url <- "https://www.boardgamegeek.com/xmlapi2/hot?boardgamepage=1"
  
  response <- GET(url)
  
  if (http_error(response)) {
    stop("Failed to load XML data")
  }
  
  xml_content <- content(response, "text")
  
  # Parse the XML data
  parsed_xml <- read_xml(xml_content)
  
  # Extract relevant information
  games <- xml_find_all(parsed_xml, ".//item") %>%
    map_df(~data.frame(
      name = xml_attr(xml_find_first(.x, ".//name"), "value"),
      year = as.integer(xml_attr(xml_find_first(.x, ".//yearpublished"), "value")),
      rank = as.integer(xml_attr(.x, "rank"))
    ))
  
  return(games)
}

calculate_points <- function(games) {
  games <- games %>%
    mutate(points = 50 - rank + 1)
  return(games)
}

save_daily_rankings <- function(games, day) {
  # Create a folder on the desktop named "Daily_CSV" if it doesn't exist
  daily_folder <- file.path(Sys.getenv("HOME"), "Desktop", "Daily_CSV")
  dir.create(daily_folder, showWarnings = FALSE)
  
  sorted_games <- games %>%
    arrange(desc(points)) %>%
    mutate(rank = row_number())
  
  # Construct the file path for the CSV file within the "Daily_CSV" folder
  csv_file_path <- file.path(daily_folder, paste0("daily_rankings_day_", day, ".csv"))
  
  # Print daily rankings to CSV file
  write.csv(sorted_games[, c("name", "year", "rank", "points")], file = csv_file_path, row.names = FALSE)
}

calculate_weekly_totals <- function(games_list, output_filename) {
  weekly_totals <- bind_rows(games_list) %>%
    group_by(name) %>%
    summarise(weekly_total = sum(points))
  
  # Create a folder on the desktop named "Daily_CSV" if it doesn't exist
  daily_folder <- file.path(Sys.getenv("HOME"), "Desktop", "Daily_CSV")
  dir.create(daily_folder, showWarnings = FALSE)
  
  # Construct the file path for the weekly totals CSV file within the "Daily_CSV" folder
  csv_file_path <- file.path(daily_folder, output_filename)
  
  write.csv(weekly_totals, file = csv_file_path, row.names = FALSE)
}


main <- function() {
  # Create a folder on the desktop named "Daily_CSV" if it doesn't exist
  daily_folder <- file.path(Sys.getenv("HOME"), "Desktop", "Daily_CSV")
  dir.create(daily_folder, showWarnings = FALSE)
  
  # Determine the current day
  current_day <- as.numeric(format(Sys.Date(), "%u")) %% 7 + 1  # Day of the week (1-7, Sunday to Saturday)
  print(paste("Current day:", current_day))
  
  # Fetch hotness games
  hotness_games <- get_hotness_games()
  
  # Calculate points for each game
  games_with_points <- calculate_points(hotness_games)
  
  # Save daily rankings only if it's the corresponding day
  if (current_day <= 7) {
    save_daily_rankings(games_with_points, current_day)
    print(paste("Saved daily rankings for day", current_day))
  } else {
    print("Skipping daily rankings save.")
  }
  
  # Delay for 1 second before the next iteration
  Sys.sleep(1)
  
  # Only calculate and save weekly totals after day 7
  if (current_day == 7) {
    games_list <- list()
    
    for (day in 1:7) {
      csv_path <- file.path(daily_folder, paste0("daily_rankings_day_", day, ".csv"))
      if (file.exists(csv_path)) {
        games_list[[day]] <- read.csv(csv_path)
        print(paste("Loaded daily rankings for day", day))
      } else {
        print(paste("File not found for day", day))
      }
    }
    
    # Calculate and save weekly totals
    calculate_weekly_totals(games_list, "weekly_totals.csv")
    print("Saved weekly totals")
  }
}

# Run the main function
main()
