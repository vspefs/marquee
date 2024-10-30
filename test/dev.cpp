import std;
import vspefs.marquee;

int main()
{
    mrq::types_view<std::string, int, double, std::array<int, 3>, float, std::vector<short>> tv;

    constexpr auto ranges = mrq::filter(
        tv, 
        [](mrq::type_iterator auto it) consteval noexcept -> bool
        {
            return std::ranges::range<mrq::type_t<decltype(it)>>;
        }
    );

    constexpr auto fundamentals = mrq::filter(
        tv, 
        [](mrq::type_iterator auto it) consteval noexcept -> bool
        {
            return std::is_arithmetic_v<mrq::type_t<decltype(it)>>;
        }
    );

    constexpr auto new_tv = mrq::concat(ranges, fundamentals);
    auto t = new_tv.to<std::tuple>(
        "hello", 
        std::to_array({ 3, 4, 5 }), 
        std::vector<short>{ 6, 7, 8 },
        
        15,
        2.0,
        7.5f
    );
}