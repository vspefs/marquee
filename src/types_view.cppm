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

    template <standard_types_view View, standard_types_view PredsView, size_t... Is>
    consteval bool stadnard_types_view_chain_invocable_helper(std::integer_sequence<size_t, Is...>) noexcept
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

    template <typename View, typename... Preds>
    concept standard_types_view_chain_invocable = 
           sizeof...(Preds) != 0
        && stadnard_types_view_chain_invocable_helper<View, types_view<Preds...>>(
               std::make_integer_sequence<size_t, View::size + 1>()
           );
}

export namespace mrq
{
    template <standard_types_view View0, standard_types_view View1, standard_types_view... Leftovers>
    consteval auto concat(View0 view0, View1 view1, Leftovers...) noexcept;

    template <standard_types_view View, predicate_on<View>... Preds>
    consteval auto find_all(View view, Preds...) noexcept;

    template <standard_types_view View, predicate_on<View>... Preds>
    consteval auto find_first(View view, Preds...) noexcept;

    template <standard_types_view View, predicate_on<View>... Preds>
    consteval auto find_last(View view, Preds...) noexcept;

    template <standard_types_view View, invocable_on<View>... Fns>
    consteval auto evaluate(View view, Fns...) noexcept;

    template <typename Ret, standard_types_view View, invocable_on_r<Ret, View>... Fns>
    consteval std::array<Ret, View::size> evaluate_to(View view, Fns...) noexcept;

    template <type_iterator Begin, type_iterator End> requires std::same_as<view_type_t<Begin>, view_type_t<End>>
    consteval auto itoa(Begin begin, End end) noexcept;

    template <standard_types_view View, typename Pred, typename... Preds> requires standard_types_view_invocable_r<bool, Pred, View> && (true && ... && standard_types_view_invocable_r<bool, Preds, View>)
    consteval auto filter(View view, Pred pred, Preds... others) noexcept;
}

namespace mrq
{
    template <typename... View0Types, typename... View1Types>
    consteval auto concat_helper(types_view<View0Types...>, types_view<View1Types...>) noexcept
    {
        return types_view<View0Types..., View1Types...>{};
    }

    template <standard_types_view View0, standard_types_view View1, standard_types_view... Leftovers>
    consteval auto concat(View0 view0, View1 view1, Leftovers... leftovers) noexcept
    {
        if constexpr (sizeof...(Leftovers) == 0)
            return concat_helper(view0, view1);
        else
            return concat(view0, concat(view1, leftovers...));
    }

    template <typename Current, typename Pred, typename ResultView>
    consteval auto find_all_helper(Pred pred, ResultView ret) noexcept
    {
        if constexpr (std::same_as<Current, sentinel_t<view_type_t<Current>>>)
            return ret;
        else if constexpr (std::invoke(pred, Current{}))
            return find_all_helper<next_t<Current>>(pred, concat(ret, types_view<Current>{}));
        else
            return find_all_helper<next_t<Current>>(pred, ret);
    }

    template <standard_types_view View, predicate_on<View>... Preds>
    consteval auto find_all(View view, Preds...) noexcept
    {
        constexpr auto final_cond = utils::preds<Preds...>{};
        return find_all_helper<begin_t<View>>(final_cond, types_view<>{});
    }

    template <standard_types_view View, invocable_on<View>... Fns>
    consteval auto evaluate(View view, Fns...) noexcept
    {
        using ret_t = std::invoke_result_t<utils::fnzz<Fns...>, begin_t<View>>;
        return evaluate_to<ret_t, View, Fns...>(view);
    }

    template <typename Ret, typename View, typename Fn, size_t... Is>
    consteval std::array<Ret, View::size> evaluate_to_helper(Fn fn, std::integer_sequence<size_t, Is...>) noexcept
    {
        return { static_cast<Ret>(std::invoke(fn, iterator_at_t<View, Is>{}))... };
    }

    template <typename Ret, standard_types_view View, invocable_on_r<Ret, View>... Fns>
    consteval std::array<Ret, View::size> evaluate_to(View view, Fns...) noexcept
    {
        constexpr auto all_fn = utils::fnzz<Fns...>{};
        return evaluate_to_helper<Ret, View>(all_fn, std::make_integer_sequence<size_t, size(view)>());
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