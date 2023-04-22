// "ega.c" code for Enhanced GC assertions.
//
// Created by Guang Yang on 2023/04/22.
//

#include "scm.h"

static char s_assert_dead[] = "assert-dead";
SCM assert_dead(SCM ptr) {
    return UNSPECIFIED;
}

static iproc subr1s[] = {
        {s_assert_dead, assert_dead},
        {0, 0}
};

void init_ega() {
    init_iprocs(subr1s, tc7_subr_1);
    add_feature("ega");
}