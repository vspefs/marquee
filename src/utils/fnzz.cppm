export module vspefs.marquee : utils.fnzz;

import std;

export namespace mrq::utils
{
    template <typename... Fns>
    class fnzz
    {
    private:
        template <typename ToFn, typename... ToFns, typename... Args> requires std::invocable<ToFn, Args...> && (sizeof...(ToFns) != 0)
        constexpr auto invoke(Args&&... args) const noexcept
        {
            return invoke<ToFns...>(std::invoke(ToFn{}, std::forward<Args>(args)...));
        }

        template <typename ToFn, typename... Args> requires std::invocable<ToFn, Args...>
        constexpr auto invoke(Args&&... args) const noexcept
        {
            return std::invoke(ToFn{}, std::forward<Args>(args)...);
        }

    public:
        template <typename... Args>
        constexpr auto operator()(Args&&... args) const noexcept
        {
            return invoke<Fns...>(std::forward<Args>(args)...);
        }

        constexpr fnzz(Fns...) noexcept {};
        constexpr fnzz() noexcept {};
    };

    template <typename Ret, typename... Fns>
    class fnll
    {
    public:
        using ret_t = std::array<Ret, sizeof...(Fns)>;

        template <typename... Args> requires (std::convertible_to<std::invoke_result_t<Fns, Args>, Ret> && ...)
        constexpr auto operator()(Args&&... args) const noexcept -> ret_t
        {
            return { static_cast<Ret>(std::invoke(Fns{}, std::forward<Args>(args)...))... };
        }
    };

    template <typename Ret, typename... Fns>
    consteval auto make_fnll(Fns... fns) noexcept -> fnll<Ret, Fns...>
    {
        return {};
    }

    template <typename... Fns>
    struct preds
    {
        template <typename... Args> requires (std::invocable<Fns, Args...> && ...)
        constexpr bool operator()(Args&&... args) const noexcept
        {
            return (std::invoke_r<bool>(Fns{}, std::forward<Args>(args)...) && ...);
        }

        constexpr auto operator!() const noexcept
        {
            return preds{ [](auto&&... args) { return !std::invoke_r<bool>(Fns{}, std::forward<std::remove_cvref_t<decltype(args)>>(args)...); }... };
        }

        constexpr preds(Fns...) noexcept {};
        constexpr preds() noexcept {};
    };
}