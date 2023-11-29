// Copyright (c), Tamas Csala

#include "oglwrap_example.hpp"

OglwrapExample::OglwrapExample() {
    if (!glfwInit()) {
        std::terminate();
    }

    // Get monitor extents and set window size based on them.
    auto monitor = glfwGetPrimaryMonitor();
    int xpos, ypos, width, height;
    glfwGetMonitorWorkarea(monitor, &xpos, &ypos, &width, &height);
    kScreenWidth = width;
    kScreenHeight = height;

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_RESIZABLE, false);

    window_ = glfwCreateWindow(kScreenWidth, kScreenHeight, "OTViz", nullptr, nullptr);

    if (!window_) {
        std::cerr << "FATAL: Couldn't create a glfw window. Aborting now." << std::endl;
        glfwTerminate();
        std::terminate();
    }

    glfwMakeContextCurrent(window_);

    bool success = gladLoadGL();
    if (!success) {
        std::cerr << "gladLoadGL failed" << std::endl;
        std::terminate();
    }
}

OglwrapExample::~OglwrapExample() {
    glfwTerminate();
}

void OglwrapExample::RunMainLoop() {
    while (!glfwWindowShouldClose(window_)) {
        gl::Clear().Color().Depth();
        glfwPollEvents();

        Render ();

        glfwSwapBuffers(window_);
    }
}

std::string OglwrapExample::GetProjectDir() {
    std::string current_file = __FILE__;
    size_t found = current_file.find_last_of("/\\");
    assert (found != std::string::npos);
    std::string dir = current_file.substr(0, found);
    return dir + "/../..";
}
