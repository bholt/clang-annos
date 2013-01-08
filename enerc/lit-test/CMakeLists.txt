# LLVM Lit testing infrastructure setup.

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg
  )

set(CLANG_TEST_DEPS
  clang clang-headers
  c-index-test diagtool arcmt-test c-arcmt-test
  clang-check
  llvm-dis llc opt FileCheck count not
  GrappaTypeChecker
)

set(CLANG_TEST_PARAMS
  clang_site_config=${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg
)

add_lit_testsuite(check-checkers "Running the Clang regression tests"
  ${CMAKE_CURRENT_SOURCE_DIR}
  PARAMS ${CLANG_TEST_PARAMS}
  DEPENDS ${CLANG_TEST_DEPS}
  ARGS ${CLANG_TEST_EXTRA_ARGS}
)
set_target_properties(check-checkers PROPERTIES FOLDER "Clang tests")
