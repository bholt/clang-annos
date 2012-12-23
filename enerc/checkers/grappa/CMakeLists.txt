set(SOURCES
  GrappaTypeChecker.cpp
)

set(LIBRARIES
  libclang
  checkers_common
)

if( LLVM_ENABLE_PIC )
  set(SHARED_LIBRARY TRUE)
  add_clang_library(GrappaTypeChecker ${SOURCES})
  target_link_libraries(GrappaTypeChecker ${LIBRARIES})

  set_target_properties(GrappaTypeChecker
    PROPERTIES
    OUTPUT_NAME "GrappaTypeChecker"
    DEFINE_SYMBOL _CINDEX_LIB_
  )

  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(GRAPPA_LINK_FLAGS
      "-Wl,-compatibility_version -Wl,1 -Wl,-dead_strip")
    set_target_properties(GrappaTypeChecker
      PROPERTIES
      LINK_FLAGS "${GRAPPA_LINK_FLAGS}"
      INSTALL_NAME_DIR "@executable_path/../lib")
  endif()

  set(GRAPPA_STATIC_TARGET_NAME GrappaTypeChecker_static)
else()
  set(GRAPPA_STATIC_TARGET_NAME GrappaTypeChecker)
endif()

if( NOT BUILD_SHARED_LIBS AND NOT WIN32 )
  add_clang_library(${GRAPPA_STATIC_TARGET_NAME} STATIC ${SOURCES})
  target_link_libraries(${GRAPPA_STATIC_TARGET_NAME} ${LIBRARIES})

  set_target_properties(${GRAPPA_STATIC_TARGET_NAME}
    PROPERTIES
    OUTPUT_NAME "GrappaTypeChecker")
endif()

