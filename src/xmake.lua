target("marquee")
    add_rules("c++.with.stdmodule")
    set_kind("moduleonly")

    add_files("main.cppm",
              "types_view.cppm")

    add_files("infra/fixed_string.cppm",
              "infra/nttp_string.cppm",
              "infra/meta_iter.cppm")
              
    add_files("utils/fnzz.cppm")
