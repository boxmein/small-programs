cmake_minimum_required(VERSION 3.12)

include(FetchContent)

FetchContent_declare(
  fcyaml
  GIT_REPOSITORY https://github.com/jbeder/yaml-cpp.git
  GIT_TAG master
  UPDATE_COMMAND ""
)
FetchContent_GetProperties(fcyaml)
if(NOT fcyaml_POPULATED)
  FetchContent_Populate(fcyaml)
endif()
add_subdirectory(${fcyaml_SOURCE_DIR} ${fcyaml_BINARY_DIR})

#
# libfmt
#
FetchContent_declare(
  fclibfmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG 5d32ccfc3130fdd122c344b37087ac7fd8c2b62e
  UPDATE_COMMAND ""
)

FetchContent_GetProperties(fclibfmt)
if(NOT fclibfmt_POPULATED)
  FetchContent_Populate(fclibfmt)
endif()
add_subdirectory(${fclibfmt_SOURCE_DIR} ${fclibfmt_BINARY_DIR})

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
SET(CMAKE_POSITION_INDEPENDENT_CODE TRUE)

project(process-runner
        LANGUAGES C CXX
        DESCRIPTION "Runs programs"
        VERSION 1.0.0
)

add_executable(
  procrunner
  src/procrunner.cpp
  src/configparser.cpp
  src/prebootrunner.cpp
)

SET(CPACK_PACKAGE_NAME "procrunner")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Runs programs")
SET(CPACK_PACKAGE_VENDOR "boxmein")
SET(CPACK_PACKAGE_CONTACT "boxmein@boxmein.net")
SET(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "CMake ${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")

include(CPack)

target_link_libraries(
  procrunner
  PUBLIC
    fmt::fmt
    yaml-cpp
)

if (CMAKE_BUILD_TYPE EQUAL "Release")
    set_property(TARGET procrunner PROPERTY INTERPROCEDURAL_OPTIMIZATION True)
endif()
