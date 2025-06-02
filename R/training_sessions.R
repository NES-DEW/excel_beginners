training_sessions <- function(tr_type = "all",
                                start_date = "today",
                                end_date = "2026-12-31",
                                session_level = "all",
                                hide_area = FALSE,
                                n = 0) {
    sesh <- readr::read_csv(
      "https://raw.githubusercontent.com/NES-DEW/KIND-training/main/data/KIND_training_sessions.csv"
    )
    
    schedule <- readr::read_csv(
      "https://raw.githubusercontent.com/NES-DEW/KIND-training/main/data/training_schedule.csv"
    )
    if (tr_type != "all") {
      poss_sesh <- sesh |>
        dplyr::filter(`Platform / area` == tr_type) |>
        dplyr::pull(Title)
      
      schedule <- schedule |>
        dplyr::filter(`session title` %in% poss_sesh)
    }
    
    if (start_date == "today") {
      schedule <- schedule |>
        dplyr::filter(start > lubridate::today() + lubridate::days(1)) |>
        dplyr::filter(start < end_date)
    } else {
      schedule <- schedule |>
        dplyr::filter(start > lubridate::ymd(start_date)) |>
        dplyr::filter(start < lubridate::ymd(end_date))
    }
    
    if(n){
      schedule <- schedule |>
        dplyr::arrange(start) |>
        dplyr::slice(1:n)
    }
    
    output <- schedule |>
      dplyr::left_join(sesh, by = dplyr::join_by(`session title` == Title)) |>
      dplyr::rowwise() |>
      dplyr::mutate(linky = paste0("<a href='", url, "'>", `session title`, "</a>")) |>
      dplyr::mutate(Level = factor(
        Level,
        levels = c(
          "pre-beginner",
          "beginner",
          "intermediate",
          "advanced",
          "managerial"
        )
      )) |>
      dplyr::mutate(
        desc2 = dplyr::case_when(
          stringr::str_detect(Level, "pre-beginner") ~ "🥬: <b>pre-beginner-level</b>",
          stringr::str_detect(Level, "beginner") ~ paste0(
            "<style='color:red'>",
            "🌶",
            "</style> :<b>beginner-level</b>"
          ),
          stringr::str_detect(Level, "inter") ~ paste0(
            "<style='color:red'>",
            "🌶🌶",
            "</style>: <b>intermediate-level</b>"
          ),
          stringr::str_detect(Level, "advanced") ~ paste0(
            "<style='color:red'>",
            "🌶🌶🌶",
            "</style>: <b>advanced-level</b>"
          ),
          stringr::str_detect(Level, "manag") ~ "💼: <b>non-technical</b>"
        )
      ) |>
      dplyr::mutate(end = start + lubridate::minutes(`Duration (minutes)`)) |>
      dplyr::mutate(friendly_date = paste0(format(start, "%H:%M"), "-", nice_date(end))) |>
      dplyr::select(
        Session = linky,
        Date = friendly_date,
        Level2 = Level,
        Area = `Platform / area`,
        Level = desc2,
        start
      ) |>
      dplyr::arrange(start, Level2) |>
      dplyr::select(!c(Level2, start)) |>
      dplyr::ungroup()
    
    if (session_level != "all") {
      output <- output |>
        dplyr::filter(stringr::str_detect(Level, session_level))
    }
    
    if (hide_area) {
      output <- output |>
        dplyr::select(-Area)
    }
    
    output |>
      kableExtra::kbl(escape = FALSE)
    
  }