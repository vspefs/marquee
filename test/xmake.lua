target("dev")
    add_rules("c++.with.stdmodule")
    set_kind("binary")
    add_deps("marquee")

    add_files("dev.cpp")