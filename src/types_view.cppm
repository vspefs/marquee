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
    concept has_valid_type_iterator_at = 
           (I == View::size && type_sentinel<typename View::sentinel>)
        || (I != View::size && type_iterator_non_sentinel<typename View::template iterator<I>>);

    template <typename View, size_t... Is>
    consteval bool standard_types_view_helper(std::integer_sequence<size_t, Is...>)
    {
        return (has_valid_type_iterator_at<View, Is> && ...);
    }

    export template <typename T>
    concept standard_types_view = 
           infra::standard_view<T> 
        && standard_types_view_helper<T>(std::make_integer_sequence<size_t, T::size + 1>{})
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
    concept invocable_at_r =
           (At == View::size && std::is_invocable_r_v<bool, Pred, sentinel_t<View>>)
        || (At != View::size && std::is_invocable_r_v<bool, Pred, iterator_at_t<View, At>>);

    template <typename Pred, typename View, size_t At>
    concept invocable_at =
           (At == View::size && std::is_invocable_v<Pred, sentinel_t<View>>)
        || (At != View::size && std::is_invocable_v<Pred, iterator_at_t<View, At>>);

    template <typename Ret, typename Pred, standard_types_view View, size_t... Is>
    consteval bool standard_types_view_invocable_helper(std::integer_sequence<size_t, Is...>)
    {
        if constexpr (std::same_as<void, Ret>)
            return (invocable_at<Pred, View, Is> && ...);
        else
            return (invocable_at_r<Ret, Pred, View, Is> && ...);
    }

    template <typename Ret, typename Pred, typename View>
    concept standard_types_view_invocable_r = standard_types_view_invocable_helper<Ret, Pred, View>(std::make_integer_sequence<size_t, View::size + 1>());

    template <typename View, typename Pred>
    concept standard_types_view_invocable = standard_types_view_invocable_helper<void, Pred, View>(std::make_integer_sequence<size_t, View::size + 1>());

    template <typename Final, typename Initial, typename Pred, typename... Leftovers>
    consteval bool chain_invocable_helper() noexcept
    {
        using ret_t = std::invoke_result_t<Pred, Initial>;
        if constexpr (sizeof...(Leftovers) == 0)
        {
            if constexpr (std::same_as<Final, void>)
                return true;
            else
                return std::same_as<Final, ret_t>;
        }
        return chain_invocable_helper<Final, ret_t, Leftovers...>();
    }

    template <typename Final, typename Initial, typename... Preds>
    concept chain_invocable_r = sizeof...(Preds) != 0 && chain_invocable_helper<Final, Initial, Preds...>();

    template <typename Initial, typename... Preds>
    concept chain_invocable = sizeof...(Preds) != 0 && chain_invocable_helper<void, Initial, Preds...>();

    export template <type_iterator Begin, type_iterator End> requires std::same_as<view_type_t<Begin>, view_type_t<End>>
    consteval auto itoa(Begin begin, End end) noexcept; // forward declaration

    consteval bool stadnard_types_view_chain_invocable_helper_each(standard_types_view auto current, standard_types_view auto preds, standard_types_view auto final) noexcept
    {
        if constexpr (size(preds) == 1)
        {
            if constexpr (empty(final))
                return std::invocable<type_t<begin_t<decltype(preds)>>, type_t<begin_t<decltype(current)>>>;
            else
                return std::same_as<
                    std::invoke_result_t<type_t<begin_t<decltype(preds)>>, type_t<begin_t<decltype(current)>>>,
                    type_t<begin_t<decltype(final)>>
                >;
        }
        else
        {
            return stadnard_types_view_chain_invocable_helper_each(current, itoa(next(begin(preds)), end(preds)), final);
        }
    }

    template <typename Final, standard_types_view View, standard_types_view PredsView, size_t... Is>
    consteval bool stadnard_types_view_chain_invocable_helper(std::integer_sequence<size_t, Is...>) noexcept
    {
        if constexpr (std::same_as<Final, void>)
        {
            return (
                stadnard_types_view_chain_invocable_helper_each(
                    types_view<iterator_at_or_sentinel_t<View, Is>>{},
                    PredsView{},
                    types_view<>{}
                ) 
                && ...
            );
        }
        else
        {
            return (
                stadnard_types_view_chain_invocable_helper_each(
                    types_view<iterator_at_or_sentinel_t<View, Is>>{},
                    PredsView{},
                    types_view<Final>{}
                ) 
                && ...
            );
        }
    }

    template <typename Final, typename View, typename... Preds>
    concept standard_types_view_chain_invocable_r = 
           sizeof...(Preds) != 0
        && stadnard_types_view_chain_invocable_helper<Final, View, types_view<Preds...>>(
               std::make_integer_sequence<size_t, View::size + 1>()
           );

    template <typename View, typename... Preds>
    concept standard_types_view_chain_invocable = 
           sizeof...(Preds) != 0
        && stadnard_types_view_chain_invocable_helper<void, View, types_view<Preds...>>(
               std::make_integer_sequence<size_t, View::size + 1>()
           );
}

export namespace mrq
{
    template <standard_types_view View0, standard_types_view View1, standard_types_view... Leftovers>
    consteval auto concat(View0 view0, View1 view1, Leftovers...) noexcept;

    template <standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_invocable_r<bool, Pred, View> && (true && ... && standard_types_view_invocable_r<bool, Preds, View>)
    consteval auto find_if(View view, Pred pred, Preds... others) noexcept;

    template <type_iterator Begin, type_iterator End> requires std::same_as<view_type_t<Begin>, view_type_t<End>>
    consteval auto itoa(Begin begin, End end) noexcept;

    template <standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_chain_invocable<View, Pred, Preds...>
    consteval auto evaluate(View view, Pred pred, Preds... others) noexcept;

    template <typename Ret, standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_chain_invocable_r<Ret, View, Pred, Preds...>
    consteval auto evaluate_to(View view, Pred pred, Preds... others) noexcept -> std::array<Ret, size(view)>;

    template <standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_invocable_r<bool, Pred, View> && (true && ... && standard_types_view_invocable_r<bool, Preds, View>)
    consteval auto filter(View view, Pred pred, Preds... others) noexcept;
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

    template <standard_types_view View0, standard_types_view View1, standard_types_view... Leftovers>
    consteval auto concat(View0 view0, View1 view1, Leftovers... leftovers) noexcept
    {
        if constexpr (sizeof...(Leftovers) == 0)
        {
            if constexpr (empty(view1))
            {
                return view0;
            }
            else
            {
                return concat(
                    add_type<type_t<begin_t<View1>>>(view0),
                    itoa(next(begin(view1)), sentinel(view1)),
                    leftovers...
                );
            }
        }
        else
            return concat(view0, concat(view1, leftovers...));
    }

    template <typename Pred, type_iterator Begin, type_iterator End>
    consteval auto find_if_helper(Pred pred, Begin begin, End end) noexcept
    {
        if constexpr (begin <=> end == std::strong_ordering::equal)
        {
            return end;
        }
        else if constexpr (std::invoke(pred, begin))
        {
            return begin;
        }
        else
        {
            return find_if_helper(pred, next(begin), end);
        }
    }

    template <standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_invocable_r<bool, Pred, View> && (true && ... && standard_types_view_invocable_r<bool, Preds, View>)
    consteval auto find_if(View view, Pred pred, Preds... others) noexcept
    {
        auto all_preds = [=](type_iterator auto it) consteval noexcept
        {
            return std::invoke(pred, it) && (true && ... && std::invoke(others, it));
        };

        return find_if_helper(all_preds, begin(view), sentinel(view));
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

    template <typename Initial, typename Pred, typename... Preds>
    consteval auto evaluate_helper() noexcept
    {
        using ret_t = std::invoke_result_t<Pred, Initial>;
        if constexpr (sizeof...(Preds) == 0)
            return types_view<ret_t>{};
        else
            return evaluate_helper<ret_t, Preds...>();
    }

    template <standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_chain_invocable<View, Pred, Preds...>
    consteval auto evaluate(View view, Pred pred, Preds... others) noexcept
    {
        using result_t = type_t<begin_t<decltype(evaluate_helper<begin_t<View>, Pred, Preds...>())>>;
        return evaluate_to<result_t, View, Pred, Preds...>(view, pred, others...);
    }

    template <typename Initial, typename Pred, typename... Preds>
    consteval auto evaluate_to_helper_invoker(Initial initial) noexcept
    {
        constexpr auto ret = std::invoke(Pred{}, initial);
        if constexpr (sizeof...(Preds) == 0)
            return ret;
        else
            return evaluate_to_helper_invoker<decltype(ret), Preds...>(ret);
    }

    template <typename Ret, type_iterator It, typename... Preds>
    consteval auto evaluate_to_helper(Ret ret) noexcept
    {
        if constexpr (std::same_as<It, sentinel_t<view_type_t<It>>>)
            return ret;
        else
        {
            ret[index(It{})] = evaluate_to_helper_invoker<It, Preds...>(It{});
            return evaluate_to_helper<decltype(ret), next_t<It>, Preds...>(ret);
        }
    }

    template <typename Ret, standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_chain_invocable_r<Ret, View, Pred, Preds...>
    consteval auto evaluate_to(View view, Pred pred, Preds... others) noexcept -> std::array<Ret, size(view)>
    {
        return evaluate_to_helper<std::array<Ret, size(view)>, begin_t<View>, Pred, Preds...>({});
    }

    template <standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_invocable_r<bool, Pred, View> && (true && ... && standard_types_view_invocable_r<bool, Preds, View>)
    consteval auto filter(View view, Pred pred, Preds... others) noexcept
    {
        if constexpr (sizeof...(Preds) != 0)
            return filter(view, [=](type_iterator auto it) consteval noexcept { return std::invoke(pred, it) && (true && ... && std::invoke(others, it)); });
        
        if constexpr (constexpr auto it = find_if(view, pred); it <=> sentinel(view) == std::strong_ordering::equal)
        {
            return types_view<>{};
        }
        else
        {
            return concat(
                types_view<type_t<decltype(it)>>{},
                filter(itoa(next(it), sentinel(view)), pred)
            );
        }
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