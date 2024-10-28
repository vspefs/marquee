set_project("Marquee")
set_version("0.0.1dev")
set_description("A C++ helper library for metaprogramming types. Shitty-ly written.")

add_rules("mode.debug", "mode.release")

includes("xmake")
includes("src")
includes("test")