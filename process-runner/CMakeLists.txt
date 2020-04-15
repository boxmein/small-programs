cmake_minimum_required(VERSION 3.13)
include(ExternalProject)
include(FetchContent)

project(
  process-runner
  LANGUAGES C CXX
  VERSION 0.1.2
  DESCRIPTION "A thing that starts your dev environment"
  HOMEPAGE_URL "https://github.com/boxmein/small-programs/tree/master/process-runner"
)

## Conan setup
# Download automatically, you can also just copy the conan.cmake file
if(NOT EXISTS "${CMAKE_BINARY_DIR}/conan.cmake")
   file(DOWNLOAD "https://github.com/conan-io/cmake-conan/raw/v0.15/conan.cmake"
                 "${CMAKE_BINARY_DIR}/conan.cmake")
endif()

include(${CMAKE_BINARY_DIR}/conan.cmake)
## /Conan setup

add_subdirectory(src)
