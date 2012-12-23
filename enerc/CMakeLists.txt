
file(GLOB COMMON_SRC
  "common/*.cpp"
  "common/*.h"
)

add_library(checkers_common
  ${COMMON_SRC}
)

include_directories("common")

add_subdirectory(checkers/enerc)
add_subdirectory(checkers/grappa)

