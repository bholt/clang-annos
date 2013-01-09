
set(SOURCES
  simple.cpp
  make_global.cpp
)
#file(GLOB SOURCES "*.cpp")

set(LLVM_OUTFILES) # initialize empty list

foreach(_file ${SOURCES})
  string(REPLACE ".cpp" ".llvm" _out ${_file})
  add_custom_command(
    OUTPUT  ${_out}
    COMMAND ${CMAKE_BINARY_DIR}/bin/clang -cc1 -load ${CMAKE_BINARY_DIR}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}GrappaTypeChecker${CMAKE_SHARED_LIBRARY_SUFFIX} -add-plugin grappa-type-checker ${CMAKE_CURRENT_SOURCE_DIR}/${_file} -S -emit-llvm -o ${_out}
    DEPENDS clang GrappaTypeChecker
  )
  list(APPEND LLVM_OUTFILES ${_out})
endforeach()

add_custom_target(
  quals-examples
  DEPENDS ${LLVM_OUTFILES}
)
