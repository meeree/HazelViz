cmake_minimum_required(VERSION 2.8)

if (MSVC)
    link_libraries(opengl32)
else()
    link_libraries(GL)
endif()
link_libraries(glfw)
link_libraries(glad)

# Find Python and Boost - components required for Boost.Python
find_package(PythonLibs 3.10 REQUIRED)
find_package(Boost REQUIRED COMPONENTS python310)

# Include directories for Python and Boost
include_directories(${PYTHON_INCLUDE_DIRS})
include_directories(${Boost_INCLUDE_DIRS})

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -O3 -fPIC")
set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -DUSE_DEBUG_CONTEXT -g")

if (BUILD_SHARED_LIBS)
    link_libraries("${MATH_LIBRARY}")
endif()

if (MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

set (LODEPNG_SOURCE "../deps/lodepng/lodepng.cpp")

file(GLOB CUSTOM_SOURCE "cpp/otviz.cpp" "cpp/oglwrap_example.cpp" ${LODEPNG_SOURCE})

if (CMAKE_BUILD_TYPE MATCHES "RELEASE")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DOGLWRAP_DEBUG=0")
endif()

# For building an executable, not shared library.
#set (CUSTOM_BINARY_NAME "otviz")
#add_executable(${CUSTOM_BINARY_NAME} WIN32 ${CUSTOM_SOURCE} ${ICON})
#set(WINDOWS_BINARIES ${CUSTOM_BINARY_NAME})
#set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR})


# For building shared library.
add_library(otviz SHARED ${CUSTOM_SOURCE} ${ICON})
set_property(TARGET otviz PROPERTY POSITION_INDEPENDENT_CODE ON)
set_target_properties(otviz PROPERTIES PREFIX "")
target_link_libraries(otviz ${Boost_LIBRARIES} ${PYTHON_LIBRARIES})
set_property(TARGET otviz PROPERTY CXX_STANDARD 11)
set_property(TARGET otviz PROPERTY LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
