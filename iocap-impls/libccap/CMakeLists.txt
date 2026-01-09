cmake_minimum_required(VERSION 3.8)
project(libccap)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 17)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wall")

include_directories(include)

find_package(OpenSSL)

add_library(ccap STATIC src/libccap.c)

add_executable(libccap_cmd src_cmdline/main.cpp)
target_link_libraries(libccap_cmd ccap OpenSSL::Crypto)
