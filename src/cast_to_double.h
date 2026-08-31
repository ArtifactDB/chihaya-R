#ifndef CAST_TO_DOUBLE_H
#define CAST_TO_DOUBLE_H

#include <limits>
#include <vector>

#include "sanisizer/sanisizer.hpp"

template<typename Integer_>
double cast_to_double(const Integer_ val) {
    if (val < 0) {
        // to_float expects non-negative integers, so we first convert 'val' into a non-negative unsigned integer. 
        return -sanisizer::to_float<double>(static_cast<std::make_unsigned_t<Integer_> >(-(val + 1)) + 1);
    } else {
        return sanisizer::to_float<double>(val);
    }
}

template<typename Integer_>
void cast_to_double(const std::vector<Integer_>& input, double* output) {
    const auto n = input.size();
    for (I<decltype(n)> i = 0; i < n; ++i) {
        output[i] = cast_to_double(input[i]);
    }
}

#endif
