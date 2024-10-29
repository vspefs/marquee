export module vspefs.marquee : types_view;

import std;
import : infra.meta_iter;

// ---- class definitions ---- //

export namespace mrq
{
    template <typename... Ts>
    struct types_view;
}

namespace mrq
{
    template <typename... Ts>
    struct types_view
    {
        struct sentinel
        {
            static constexpr std::size_t index = sizeof...(Ts);
            using type = void;
            using view_type = types_view<Ts...>;
            using next_type = sentinel;
            using prev_type = sentinel;

            consteval types_view<void> operator*() const noexcept { return {}; }
            consteval sentinel prev() const noexcept { return {}; }
            consteval sentinel next() const noexcept { return {}; }
        };

        template <std::size_t At>
        struct iterator
        {
            static constexpr std::size_t index = At;

            using type = Ts...[index];
            using view_type = types_view<Ts...>;
            using next_type = std::conditional_t<At == sizeof...(Ts) - 1, sentinel, iterator<index + 1>>;
            using prev_type = std::conditional_t<At == 0, sentinel, iterator<index - 1>>;

            consteval types_view<type> operator*() const noexcept { return {}; }
            consteval next_type next() const noexcept { return {}; }
            consteval prev_type prev() const noexcept { return {}; }
        };

        static constexpr std::size_t size = sizeof...(Ts);

        template <template<typename...> typename Target, typename... Args>
        constexpr auto to(Args... args) const noexcept { return Target<Ts...>(std::forward<Args>(args)...); }

        template <template<typename...> typename Target>
        using to_t = Target<Ts...>;
    };

    template<>
    struct types_view<>
    {
        struct sentinel
        {
            static constexpr std::size_t index = 0;
            using type = void;
            using view_type = types_view<>;
            using next_type = sentinel;
            using prev_type = sentinel;

            consteval types_view<void> operator*() const noexcept { return {}; }
            consteval sentinel prev() const noexcept { return {}; }
            consteval sentinel next() const noexcept { return {}; }
        };

        template <std::size_t At>   // Do we need `requires At == 0`?
        using iterator = sentinel;

        static constexpr std::size_t size = 0;
    };
}

// ---- concepts ---- //

namespace mrq
{
    template <typename T>
    concept is_types_view_meta_iterator = requires (T t)
    {
        typename T::type;
        { *t } -> std::same_as<types_view<typename T::type>>;
    };

    export template <typename T>
    concept type_iterator = infra::meta_iterator<T> && is_types_view_meta_iterator<T>;

    export template <typename T>
    concept type_iterator_non_sentinel = infra::meta_iterator_non_sentinel<T> && is_types_view_meta_iterator<T>;

    export template <typename T>
    concept type_sentinel = infra::meta_sentinel<T> && is_types_view_meta_iterator<T>;

    template <typename View, size_t I>
    concept has_valid_type_iterator_at = (
          I == View::size
        ? type_sentinel<typename View::sentinel>
        : type_iterator_non_sentinel<typename View::template iterator<I>>
    );

    template <typename View, size_t... Is>
    consteval bool standard_types_view_helper(std::integer_sequence<size_t, Is...>)
    {
        if constexpr (View::size == 0)
        {
            return type_sentinel<typename View::sentinel>;
        }
        else
        {
            auto ret = true;
            auto seq = std::to_array({ has_valid_type_iterator_at<View, Is>... });
            for (auto valid : seq)
                ret = ret && valid;
            return ret;
        }
    }

    export template <typename T>
    concept standard_types_view = 
           infra::standard_view<T> 
        && standard_types_view_helper<T>(std::make_integer_sequence<size_t, T::size>{})
        && (requires (T t) { { t.template to<std::tuple>() } -> std::same_as<typename T::template to_t<std::tuple>>; }
         || T::size == 0);
}

// ---- basic iterator support ---- //

export namespace mrq
{
    template <type_iterator It>
    using type_t = typename It::type;
}

// ---- range algorithms --- //

namespace mrq
{
    template <typename Ret, typename Pred, typename View, size_t At>
    concept invocable_at_r = (
          At == View::size
        ? std::is_invocable_r_v<bool, Pred, sentinel_t<View>>
        : std::is_invocable_r_v<bool, Pred, iterator_at_t<View, At>>
    );

    template <typename Ret, typename Pred, standard_types_view View, size_t... Is>
    consteval bool standard_types_view_invocable_helper(std::integer_sequence<size_t, Is...>)
    {
        if constexpr (sizeof...(Is) == 0)
        {
            return invocable_at_r<Ret, Pred, View, View::size>;
        }
        else
        {
            auto ret = true;
            auto seq = std::to_array({ invocable_at_r<Ret, Pred, View, Is>... });
            for (auto valid : seq)
                ret = ret && valid;
            return ret;
        }
    }

    template <typename Ret, typename Pred, typename View>
    concept standard_types_view_invocable_r = standard_types_view_invocable_helper<Ret, Pred, View>(std::make_integer_sequence<size_t, View::size>());
}

export namespace mrq
{
    template <standard_types_view View0, standard_types_view View1>
    consteval auto concat(View0, View1) noexcept;

    template <typename Pred, standard_types_view View> requires standard_types_view_invocable_r<bool, Pred, View>
    consteval auto find_if(Pred, View) noexcept;

    template <type_iterator Begin, type_iterator End> requires std::same_as<view_type_t<Begin>, view_type_t<End>>
    consteval auto itoa(Begin begin, End end) noexcept;
}

namespace mrq
{
    template <standard_types_view View, typename T, size_t... Is>
    consteval auto add_type_helper(std::integer_sequence<size_t, Is...>) noexcept
    {
        return types_view<typename View::template iterator<Is>::type..., T>{};
    }

    template <typename T, standard_types_view View>
    consteval auto add_type(View view = View{}) noexcept
    {
        return add_type_helper<View, T>(std::make_integer_sequence<size_t, size(view)>());
    }

    template <standard_types_view View0, standard_types_view View1>
    consteval auto concat(View0 view0, View1 view1) noexcept
    {
        if constexpr (empty(view1))
        {
            return view0;
        }
        else
        {
            return concat(
                add_type<type_t<begin_t<View1>>>(view0),
                itoa(next(begin(view1)), sentinel(view1))
            );
        }
    }

    template <typename Pred, type_iterator Begin, type_iterator End>
    consteval auto find_if(Pred pred, Begin begin, End end) noexcept
    {
        if constexpr (index(begin) == index(end))
        {
            return end;
        }
        else if constexpr (std::invoke(pred, begin))
        {
            return begin;
        }
        else
        {
            return find_if(pred, next(begin), end);
        }
    }

    template <typename Pred, standard_types_view View> requires standard_types_view_invocable_r<bool, Pred, View>
    consteval auto find_if(Pred pred, View view) noexcept
    {
        return find_if(pred, begin(view), sentinel(view));
    }

    template <type_iterator Current, type_iterator End, typename... Ts> requires std::same_as<view_type_t<Current>, view_type_t<End>>
    consteval auto itoa_helper(Current, End) noexcept
    {
        if constexpr (std::same_as<Current, End>)
            return types_view<Ts...>{};
        else
            return itoa_helper<next_t<Current>, End, Ts..., type_t<Current>>(next(Current{}), End{});
    }

    template <type_iterator Begin, type_iterator End> requires std::same_as<view_type_t<Begin>, view_type_t<End>>
    consteval auto itoa(Begin begin, End end) noexcept
    {
        return itoa_helper(begin, end);
    }
}

// ---- view algorithms ---- //

// ---- range algorithms ---- //
/*
export namespace mrq
{
    template <typename Target, standard_view View>
    consteval auto find(View) noexcept;

    template <typename Return, typename Functor, standard_view View>
    struct evaluate;

    template <standard_view View, typename Functor, typename... Args>
    requires requires (Functor fn)
    {
        fn.run(sentinel_t<View>{});
        fn.run(begin_t<View>{});
    }
    struct for_each;
}

// ---- implementations ---- //

namespace mrq
{
    template <typename... Ts>
    struct types_view
    {
        struct sentinel
        {
            static constexpr std::size_t index = sizeof...(Ts);
            using type = void;
            using view_type = types_view<Ts...>;
            using next_type = sentinel;
            using prev_type = sentinel;

            consteval types_view<void> operator*() const noexcept { return {}; }
            consteval sentinel prev() const noexcept { return {}; }
            consteval sentinel next() const noexcept { return {}; }
        };

        template <std::size_t At>
        struct iterator
        {
            static constexpr std::size_t index = At;

            using type = Ts...[index];
            using view_type = types_view<Ts...>;
            using next_type = std::conditional_t<At == sizeof...(Ts) - 1, sentinel, iterator<index + 1>>;
            using prev_type = std::conditional_t<At == 0, sentinel, iterator<index - 1>>;

            consteval types_view<type> operator*() const noexcept { return {}; }
            consteval next_type next() noexcept { return {}; }
            consteval prev_type prev() noexcept { return {}; }
        };

        static constexpr std::size_t size = sizeof...(Ts);
    };

    template<>
    class types_view<>
    {
        struct sentinel
        {
            static constexpr std::size_t index = 0;
            using type = void;
            using view_type = types_view<>;
            using next_type = sentinel;
            using prev_type = sentinel;

            consteval types_view<void> operator*() const noexcept { return {}; }
            consteval sentinel prev() const noexcept { return {}; }
            consteval sentinel next() const noexcept { return {}; }
        };

        template <std::size_t At>
        using iterator = sentinel;

        static constexpr std::size_t size = 0;
    };

    template <type_iterator Current, type_iterator End, typename... Ts> requires std::same_as<view_type_t<Current>, view_type_t<End>>
    consteval auto itoa_helper(Current, End) noexcept
    {
        if constexpr (std::same_as<Current, End>)
            return types_view<Ts...>{};
        else
            return itoa_helper<next_t<Current>, End, Ts..., type_t<Current>>(Current{}.next(), End{});
    }

    template <type_iterator Begin, type_iterator End> requires std::same_as<view_type_t<Begin>, view_type_t<End>>
    consteval auto itoa(Begin, End) noexcept
    {
        return itoa_helper(Begin{}, End{});
    }

    template <typename Target, standard_view View, type_iterator Current> requires std::same_as<View, view_type_t<Current>>
    consteval auto find(View, Current it) noexcept
    {
        if constexpr (std::same_as<Current, sentinel_t<View>>)
            return it;
        else if constexpr (std::same_as<Target, type_t<Current>>)
            return it;
        else
            return find<Target>(View{}, next(it));
    }

    template <typename Target, standard_view View>
    consteval auto find(View) noexcept
    {
        return find<Target>(View{}, begin(View{}));
    }

    template <typename Return, typename Functor, standard_view View>
    struct evaluate
    {
        Functor fn;
        using result_type = std::array<Return, View::size + 1>;

        template <typename... Args> requires std::constructible_from<Functor, Args...>
        consteval evaluate(Args... args) noexcept : fn{std::forward<Args>(args)...} {}

        template <typename... Args> requires std::constructible_from<Functor, Args...>
        consteval evaluate(View, Args... args) noexcept : fn{std::forward<Args>(args)...} {}

        template <type_iterator It = begin_t<View>, typename... Args>
        requires requires (It it, result_type ret, Functor fn, Args... args)
        {
            ret[index(it)] = fn.run(it, std::forward<Args>(args)...);
        }
        [[nodiscard]] consteval auto run(It it, result_type ret, Args... args) noexcept
        {
            ret[index(it)] = fn.run(it, std::forward<Args>(args)...);
            if constexpr (!sentinel<It>)
                return run(it.next(), ret, std::forward<Args>(args)...);
            else
                return std::move(ret);
        }

        template <type_iterator It = begin_t<View>, typename... Args>
        [[nodiscard]] consteval auto operator()(Args... args) noexcept
        {
            return run(begin(View{}), {}, std::forward<Args>(args)...);
        }
    };

    template <standard_view View, typename Functor, typename... Args>
    requires requires (Functor fn)
    {
        fn.run(sentinel_t<View>{});
        fn.run(begin_t<View>{});
    }
    struct for_each
    {
        Functor fn;

        template <typename... Ts> requires std::constructible_from<Functor, Ts...>
        consteval for_each(Ts... args) noexcept : fn{std::forward<Ts>(args)...} {}

        template <type_iterator It = begin_t<View>>
        consteval void run(It it = begin_t<View>{}, Args... args) const noexcept
        {
            fn.run(it, args...);
            if constexpr (!sentinel<It>) run(it.next(), std::forward<Args>(args)...);
        }

        consteval auto operator()(Args... args) const noexcept
        {
            run(begin(View{}), std::forward<Args>(args)...);
        }
    };
}*/