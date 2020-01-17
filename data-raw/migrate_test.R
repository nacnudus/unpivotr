library(usethis)

migrate_test <-
  tibble::tribble(
       ~row, ~col, ~.value,  ~gender,      ~name, ~subject_type,   ~subject,
         4L,   4L,     "1", "Female",  "Matilda",  "Humanities", "Classics",
         4L,   5L,     "2", "Female",   "Olivia",  "Humanities", "Classics",
         5L,   4L,     "3", "Female",  "Matilda",  "Humanities",  "History",
         5L,   5L,     "4", "Female",   "Olivia",  "Humanities",  "History",
         6L,   4L,     "5", "Female",  "Matilda", "Performance",    "Music",
         6L,   5L,     "6", "Female",   "Olivia", "Performance",    "Music",
         7L,   4L,     "7", "Female",  "Matilda", "Performance",    "Drama",
         7L,   5L,     "8", "Female",   "Olivia", "Performance",    "Drama",
         4L,   6L,     "3",   "Male", "Nicholas",  "Humanities", "Classics",
         4L,   7L,     "0",   "Male",     "Paul",  "Humanities", "Classics",
         5L,   6L,     "5",   "Male", "Nicholas",  "Humanities",  "History",
         5L,   7L,     "1",   "Male",     "Paul",  "Humanities",  "History",
         6L,   6L,     "9",   "Male", "Nicholas", "Performance",    "Music",
         6L,   7L,     "2",   "Male",     "Paul", "Performance",    "Music",
         7L,   6L,    "12",   "Male", "Nicholas", "Performance",    "Drama",
         7L,   7L,     "3",   "Male",     "Paul", "Performance",    "Drama"
       )

usethis::use_data(migrate_test,overwrite = TRUE)