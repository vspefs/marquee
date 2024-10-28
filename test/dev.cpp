import std;
import vspefs.marquee;

int main()
{
    mrq::types_view<int, double> tv0;
    mrq::types_view<float, long> tv1;
    auto tv = mrq::concat(tv0, tv1);

    auto t = tv.to<std::tuple>(0, 2.0, 1.0f, 111l);
    std::println("int: {}", std::get<int>(t));
    std::println("double: {}", std::get<double>(t));
    std::println("float: {}", std::get<float>(t));
    std::println("long: {}", std::get<long>(t));
}