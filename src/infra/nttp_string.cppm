export module vspefs.marquee : infra.nttp_string;

import std;
export import : infra.fixed_string;

export namespace mrq::infra
{
    template<std::size_t N>
    class nttp_string
    {
    public:
        mrq::fixed_string<N> data;

        constexpr explicit(false) nttp_string(char ch) : data(ch) {}
        consteval explicit(false) nttp_string(const char (&str)[N + 1]) : data(str) {}
        constexpr explicit(false) nttp_string(const mrq::fixed_string<N>& str) : data(str) {}
        
        [[nodiscard]] constexpr bool empty() const noexcept { return data.empty(); }

        [[nodiscard]] constexpr operator const mrq::fixed_string<N>&() const noexcept { return data; }
        
        template <std::size_t N2>
        [[nodiscard]] friend constexpr nttp_string<N + N2> operator+(const nttp_string& lhs, const nttp_string<N2>& rhs) noexcept
        {
            return nttp_string<N + N2>{ lhs.data + rhs.data };
        }

        template <std::size_t N2>
        [[nodiscard]] friend constexpr auto operator<=>(const nttp_string& lhs, const nttp_string<N2>& rhs) noexcept
        {
            return lhs.data <=> rhs.data;
        }

        template <std::size_t N2>
        [[nodiscard]] friend constexpr bool operator==(const nttp_string& lhs, const nttp_string<N2>& rhs) noexcept
        {
            return lhs.data == rhs.data;
        }
    };

    nttp_string(char) -> nttp_string<1>;

    template<std::size_t N>
    nttp_string(const char (&)[N]) -> nttp_string<N - 1>;

    template<std::size_t N>
    nttp_string(const mrq::fixed_string<N>&) -> nttp_string<N>;
}