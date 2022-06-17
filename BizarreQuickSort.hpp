#include <cassert>
#include <compare>
#include <concepts>
#include <iterator>
#include <ranges>
#include <type_traits>
#include <utility>


template <std::ranges::bidirectional_range BidirRange, typename Comparer = std::compare_three_way>
    requires std::permutable<std::ranges::iterator_t<BidirRange>>
constexpr
void insertionSort(BidirRange&& range, Comparer cmp = {})
    noexcept(std::is_nothrow_move_constructible_v<std::ranges::range_value_t<BidirRange>>
          && std::is_nothrow_move_assignable_v<std::ranges::range_value_t<BidirRange>>)
{
    assert(not std::ranges::empty(range));

    const auto [begin, end] = std::ranges::subrange(range);
    for (auto curr = std::ranges::next(begin); curr != end; ++curr) {
        auto tmp = std::move(*curr); // move construct
        auto pos = curr;
        while (pos != begin) {
            const auto posPrev = std::ranges::prev(pos);
            const auto cmpResult = cmp(tmp, *posPrev);
            if constexpr (std::same_as<std::partial_ordering, decltype(cmpResult)>) {
                assert(cmpResult != std::partial_ordering::unordered);
            }

            if (cmpResult >= 0) {
                break;
            }

            *pos = std::move(*posPrev);
            pos = posPrev;
        }
        *pos = std::move(tmp);
    }
}

template <std::ranges::random_access_range RandomRange, typename Comparer = std::compare_three_way>
    requires std::ranges::common_range<RandomRange>
          && std::permutable<std::ranges::iterator_t<RandomRange>>
[[nodiscard]] constexpr
auto bizarrePartition(RandomRange&& range, Comparer cmp = {})
    noexcept(std::is_nothrow_copy_constructible_v<std::ranges::range_value_t<RandomRange>>
          && std::is_nothrow_swappable_v<std::ranges::range_value_t<RandomRange>>)
    -> std::ranges::iterator_t<RandomRange>
{
    using std::ranges::swap;
    const auto [begin, end] = std::ranges::subrange(range);
    const auto size = std::ranges::ssize(range);

    if (size < 128) [[likely]] {
        insertionSort(range, cmp);
        return end;
    }

    {// optional for correctness, but necessary for efficiency
        const auto mid = begin + size / 2;
        swap(*begin, *mid);
    }

    auto lMaxIt = end;
    auto rMaxIt = end;

    auto [left, right] = std::ranges::subrange(range);
    for (auto it = left + 1; it != right; /*empty*/) {
        const auto cmpResult = cmp(*it, *left);
        if constexpr (std::same_as<std::partial_ordering, decltype(cmpResult)>) {
            assert(cmpResult != std::partial_ordering::unordered);
        }

        if (cmpResult < 0) { // less than
            swap(*it, *left);

            if (lMaxIt == end || cmp(*lMaxIt, *left) < 0) [[unlikely]] {
                lMaxIt = left;
            }
            ++left;
            ++it;
        }
        else if (cmpResult > 0) { // greater than
            --right;
            swap(*it, *right);

            if (rMaxIt == end || cmp(*rMaxIt, *right) < 0) [[unlikely]] {
                rMaxIt = right;
            }
        }
        else { assert(cmpResult == 0);
            ++it;
        }
    }

    auto pivot = begin;

    if (lMaxIt != end) [[likely]] { assert(left != begin);
        swap(*begin, *lMaxIt);
    }
    else { // optional
        pivot = right;
    }

    if (rMaxIt != end) [[likely]] { assert(right != end);
        swap(*right, *rMaxIt);
    }
    else if (pivot == right) [[unlikely]] { // optional
        pivot = end;
    }

    return pivot;
}

template <std::ranges::random_access_range RandomRange, typename Comparer = std::compare_three_way>
constexpr
void bizarreQuickSort(RandomRange&& range, Comparer cmp = {})
    noexcept(noexcept(bizarrePartition(range, cmp)))
{
    auto itBegin = bizarrePartition(range, cmp);
    const auto end = std::ranges::cend(range);
    while (itBegin != end) [[likely]] {
        auto maxIt = itBegin;
        for (auto it = itBegin; it != end; /*empty*/) {
            do {
                ++it;
            } while (it != end && cmp(*it, *maxIt) <= 0); // not exceeding max

            if (it - maxIt > 1) {
                const auto pivot = bizarrePartition(std::ranges::subrange{maxIt, it}, cmp);
                if (itBegin == maxIt) [[unlikely]] { // optional
                    itBegin = pivot;
                }
            }
            else /*if (it == maxIt + 1) */{
                if (itBegin == maxIt) [[unlikely]] {
                    itBegin = it;
                }
            }
            maxIt = it;
        }
    }
}
