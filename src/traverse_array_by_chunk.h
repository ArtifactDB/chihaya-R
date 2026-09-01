#ifndef TRAVERSE_ARRAY_BY_CHUNK_H
#define TRAVERSE_ARRAY_BY_CHUNK_H

#include "H5Cpp.h"

#include "sanisizer/sanisizer.hpp"

#include <vector>
#include <algorithm>
#include <cassert>

#include "utils.h"

inline std::vector<hsize_t> extract_chunk_dims(const H5::DSetCreatPropList& dcpl, const std::vector<hsize_t>& dims) {
    const auto ndims = dims.size();
    auto chunkdims = sanisizer::create<std::vector<hsize_t> >(ndims, 1);

    if (dcpl.getLayout() == H5D_CHUNKED) {
        dcpl.getChunk(ndims, chunkdims.data());
    } else {
        hsize_t remaining = 10000;
        auto d = ndims - 1; 
        while (true) {
            if (remaining <= dims[d]) {
                chunkdims[d] = remaining;
                break;
            }
            chunkdims[d] = dims[d];
            if (d == 0) {
                break;
            }
            --d;
        }
    }

    return chunkdims;
}

inline hsize_t get_chunk_length(const std::vector<hsize_t>& chunkdims) {
    hsize_t chunklen = 1; 
    for (const auto cd : chunkdims) {
        chunklen = sanisizer::product<hsize_t>(chunklen, cd);
    }
    return chunklen;
}

template<typename PreOuterLoop_, typename InnerLoop_, typename PostOuterLoop_>
void traverse_array_by_chunk(
    const std::vector<hsize_t>& dims,
    const std::vector<hsize_t>& chunkdims,
    PreOuterLoop_ pre_outer_loop,
    InnerLoop_ inner_loop,
    PostOuterLoop_ post_outer_loop
) {
    const auto ndim = dims.size();
    assert(ndim > 0);
    std::vector<hsize_t> offsets(ndim), counts(chunkdims), position(ndim - 1);
    H5::DataSpace file_space(ndim, dims.data());
    H5::DataSpace mem_space;

    while (true) {
        mem_space.setExtentSimple(ndim, counts.data());
        mem_space.selectAll();
        file_space.selectHyperslab(H5S_SELECT_SET, counts.data(), offsets.data()); 
        [[maybe_unused]] auto scope = pre_outer_loop(file_space, mem_space);

        std::fill(position.begin(), position.end(), 0);
        while (true) {
            hsize_t chunk_offset = 0, full_offset = 0;
            for (I<decltype(ndim)> d = 1; d < ndim; ++d) {
                full_offset += position[d - 1] + offsets[d - 1];
                full_offset *= dims[d];
                chunk_offset += position[d - 1];
                chunk_offset *= counts[d];
            }
            full_offset += offsets.back();

            const auto nfastest = counts.back();
            for (hsize_t i = 0; i < nfastest; ++i) {
                inner_loop(chunk_offset + i, full_offset + i);
            }

            bool okay = false;
            if (ndim > 1) {
                // We don't need to increment on the last dimension, as we already inner loop on that dimension.
                I<decltype(ndim)> d = ndim - 2;
                while (1) {
                    ++position[d];
                    if (position[d] == counts[d]) {
                        position[d] = 0;
                        if (d) {
                            --d;
                        } else {
                            break;
                        }
                    } else {
                        okay = true;
                        break;
                    }
                }
            }
            if (!okay) {
                break;
            }
        }

        post_outer_loop(file_space, mem_space);

        bool okay = false;
        I<decltype(ndim)> d = ndim - 1;
        while (1) {
            offsets[d] += counts[d];
            if (offsets[d] == dims[d]) {
                offsets[d] = 0;
                if (d) {
                    --d;
                } else {
                    break;
                }
            } else {
                counts[d] = sanisizer::min(dims[d] - offsets[d], chunkdims[d]);
                okay = true;
                break;
            }
        }
        if (!okay) {
            break;
        }
    }
}

#endif    
