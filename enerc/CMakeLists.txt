
file(GLOB COMMON_SRC
  "common/*.cpp"
  "common/*.h"
)

add_clang_library(checkers_common
  ${COMMON_SRC}
)
target_link_libraries(checkers_common
  libclang
)

include_directories("common")

add_subdirectory(checkers/enerc)
add_subdirectory(checkers/grappa)

