set(SOURCES
  EnerCTypeChecker.cpp
)

set(LIBRARIES
  libclang
  checkers_common
)

if( LLVM_ENABLE_PIC )
  set(SHARED_LIBRARY TRUE)
  add_clang_library(EnerCTypeChecker ${SOURCES})
  target_link_libraries(EnerCTypeChecker ${LIBRARIES})

  set_target_properties(EnerCTypeChecker
    PROPERTIES
    OUTPUT_NAME "EnerCTypeChecker"
    DEFINE_SYMBOL _CINDEX_LIB_
  )

  if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(ENERC_LINK_FLAGS
      "-Wl,-compatibility_version -Wl,1 -Wl,-dead_strip")
    set_target_properties(EnerCTypeChecker
      PROPERTIES
      LINK_FLAGS "${ENERC_LINK_FLAGS}"
      INSTALL_NAME_DIR "@executable_path/../lib")
  endif()

  set(ENERC_STATIC_TARGET_NAME EnerCTypeChecker_static)
else()
  set(ENERC_STATIC_TARGET_NAME EnerCTypeChecker)
endif()

if( NOT BUILD_SHARED_LIBS AND NOT WIN32 )
  add_clang_library(${ENERC_STATIC_TARGET_NAME} STATIC ${SOURCES})
  target_link_libraries(${ENERC_STATIC_TARGET_NAME} ${LIBRARIES})

  set_target_properties(${ENERC_STATIC_TARGET_NAME}
    PROPERTIES
    OUTPUT_NAME "EnerCTypeChecker")
endif()

