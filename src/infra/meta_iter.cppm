export module vspefs.marquee : infra.meta_iter;

import std;

// ---- concepts ---- //

namespace mrq::infra
{
    export using difference_t = std::ptrdiff_t;
    export using size_t = std::size_t;

    export template <typename T>
    concept meta_iterator = requires(T t)
    {
        typename T::view_type;
        requires std::same_as<std::remove_cvref_t<decltype(T::index)>, size_t>;
        requires std::same_as<std::remove_cvref_t<decltype(T::view_type::size)>, size_t>;
        { t.next() } -> std::same_as<typename T::next_type>;
        { t.prev() } -> std::same_as<typename T::prev_type>;
    };

    export template <typename T>
    concept meta_sentinel = meta_iterator<T> && (T::index == T::view_type::size);

    export template <typename T>
    concept meta_iterator_non_sentinel = meta_iterator<T> && (!meta_sentinel<T>);

    template <typename View, size_t I>
    concept has_valid_iterator_at = (
          I == View::size
        ? meta_sentinel<typename View::sentinel>
        : meta_iterator_non_sentinel<typename View::template iterator<I>>
    );

    template <typename View, size_t... Is>
    consteval bool standard_view_helper(std::integer_sequence<size_t, Is...>)
    {
        if constexpr (sizeof...(Is) == 0)
        {
            return meta_sentinel<typename View::sentinel> && std::same_as<typename View::template iterator<0>, typename View::sentinel>;
        }
        else
        {
            auto ret = true;
            auto seq = std::to_array({ has_valid_iterator_at<View, Is>... });
            for (auto valid : seq)
                ret = ret && valid;
            return ret;
        }
    }

    export template <typename View>
    concept standard_view =
           std::same_as<std::remove_cvref_t<decltype(View::size)>, size_t> 
        && standard_view_helper<View>(std::make_integer_sequence<size_t, View::size>());
}

// ---- basic iterator support ---- //

export namespace mrq::infra
{
    template <typename View, size_t At> requires standard_view<View> && (At < View::size || View::size == 0)
    using iterator_at_t = View::template iterator<At>;

    template <typename It> requires meta_iterator<It>
    using view_type_t = typename It::view_type;

    template <typename It> requires meta_iterator<It>
    using prev_t = typename It::prev_type;

    template <typename It> requires meta_iterator<It>
    using next_t = typename It::next_type;

    template <typename View> requires standard_view<View>
    using sentinel_t = View::sentinel;

    template <typename View> requires standard_view<View>
    using begin_t = iterator_at_t<View, 0>;

    template <standard_view View>
    consteval auto begin(View) noexcept
    {
        return begin_t<View>{};
    }

    template <standard_view View>
    consteval auto sentinel(View) noexcept
    {
        return sentinel_t<View>{};
    }

    template <meta_iterator It>
    consteval size_t index(It) noexcept
    {
        return It::index;
    }

    template <size_t At, standard_view View>
    consteval auto at(View) noexcept
    {
        return iterator_at_t<View, At>{};
    }

    template <standard_view View>
    consteval size_t size(View) noexcept
    {
        return View::size;
    }

    template <standard_view View>
    consteval bool empty(View) noexcept
    {
        return View::size == 0;
    }

    template <difference_t N = 1, meta_iterator It>
    consteval auto next(It it) noexcept
    {
        if constexpr (N > 0)
            return next<N - 1, next_t<It>>(it.next());
        else if constexpr (N < 0)
            return next<N + 1, prev_t<It>>(it.prev());
        else
            return it;
    }

    template <difference_t N = 1, meta_iterator It>
    consteval auto prev(It it) noexcept
    {
        return next<-N, It>(it);
    }

    template <meta_iterator From, meta_iterator To>
    consteval difference_t distance(From from, To to) noexcept
    {
        if constexpr (index(to) >= index(from))
            return index(to) - index(from);
        else
            return -static_cast<difference_t>(index(from) - index(to));
    }
}

// ---- exporting necessity to `mrq` namespace ---- //

// One day I'll murder the guy who decided that we enable this warning for people who are exporting `using` decls for a module.
// NOLINTBEGIN(misc-unused-using-decls)

export namespace mrq
{
    using ::mrq::infra::iterator_at_t;
    using ::mrq::infra::at;
    using ::mrq::infra::view_type_t;
    
    using ::mrq::infra::index;
    using ::mrq::infra::empty;

    using ::mrq::infra::distance;
    using ::mrq::infra::difference_t;

    using ::mrq::infra::begin;
    using ::mrq::infra::begin_t;

    using ::mrq::infra::sentinel;
    using ::mrq::infra::sentinel_t;

    using ::mrq::infra::size;
    using ::mrq::infra::size_t;

    using ::mrq::infra::next;
    using ::mrq::infra::next_t;

    using ::mrq::infra::prev;
    using ::mrq::infra::prev_t;
}

// NOLINTEND(misc-unused-using-decls)