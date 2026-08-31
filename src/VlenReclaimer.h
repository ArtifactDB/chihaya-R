#ifndef VLEN_RECLAIMER_H
#define VLEN_RECLAIMER_H

#include "H5Cpp.h"

class VlenReclaimer {
public:
    VlenReclaimer(const char** ptrs, const H5::DataType& type, const H5::DataSpace& space) :
        my_ptrs(ptrs), my_type(type), my_space(space) 
    {}

    VlenReclaimer(const VlenReclaimer&) = delete;
    VlenReclaimer(VlenReclaimer&&) = delete;
    VlenReclaimer& operator=(const VlenReclaimer&) = delete;
    VlenReclaimer& operator=(VlenReclaimer&&) = delete;

    ~VlenReclaimer() {
        H5Treclaim(my_type.getId(), my_space.getId(), H5::DSetMemXferPropList::DEFAULT.getId(), my_ptrs);
    }

private:
    const char** my_ptrs;
    const H5::DataType& my_type;
    const H5::DataSpace& my_space;
};

#endif
