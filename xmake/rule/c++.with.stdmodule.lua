rule("c++.with.stdmodule")
    on_load(function (target)
        target:add("rules", "mode.debug", "mode.release")
        target:add("languages", "c++latest")
        target:set("policy", "build.c++.modules", true)

        -- only clang toolchain supports std module yet, so...
        target:set("toolchains", "clang")
        target:set("runtimes", "c++_shared")
    end)