add_executable(
  main
  main.cpp
)

target_link_libraries(
  main
  ${CONAN_LIBS}
)

target_compile_features(
  main
  PUBLIC
  cxx_std_17
)
set(CMAKE_CXX_STANDARD 17)
