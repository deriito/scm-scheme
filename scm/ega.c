// "ega.c" code for Enhanced GC assertions.
//
// Created by Guang Yang on 2023/04/22.
//

#include "scm.h"

SCM *gc_traced = NULL;

static char s_assert_dead[] = "assert-dead";
SCM assert_dead(SCM ptr) {
    if (!is_user_defined_data_type_instance(ptr)) {
        wta(ptr, (char *)ARG1, s_assert_dead);
    }
    if (is_assert_dead_marked(ptr)) {
        return UNSPECIFIED;
    }
    set_assert_mark(ptr);
    return UNSPECIFIED;
}

char *type_str(SCM ptr) {
    if (IMP(ptr)) {
        return "immediate";
    }
    switch (TYP7(ptr)) {
        case tcs_cons_nimcar:
        case tcs_cons_imcar:
        case tcs_cons_gloc:
            return "cons";
        case tcs_closures:
            return "closure";
        case tc7_specfun:
            return "specfun";
        case tc7_vector:
            if (!is_user_defined_data_type_instance(ptr)) {
                return "vector";
            }
            return instance_type_name(ptr);
        case tc7_contin:
            return "contin";
        case tc7_string:
            return "string";
        case tc7_msymbol:
        case tc7_ssymbol:
            return "symbol";
        case tc7_VfixN8:
        case tc7_VfixZ8:
        case tc7_VfixZ16:
        case tc7_VfixN16:
        case tc7_VfixZ32:
        case tc7_VfixN32:
        case tc7_VfixZ64:
        case tc7_VfixN64:
            return "vfix";
        case tc7_VfloR32:
        case tc7_VfloC32:
        case tc7_VfloR64:
        case tc7_VfloC64:
            return "vflo";
        case tc7_Vbool:
            return "vbool";
        case tcs_subrs:
            return "subr";
        case tc7_port:
            return "port";
        case tc7_smob:
            switch TYP16(ptr) {
                case tc_free_cell:
                    return "freeCell";
                case tcs_bignums:
                    return "bignum";
                case tc16_flo:
                    return "flo";
                default:
                    return "smob";
            }
        default:
            return "unsupported";
    }
}

void process_dead_marked_obj(SCM ptr, long last_gc_traced_index) {
    char suspect_type_str[40] = "";
    strcpy(suspect_type_str, type_str(ptr));

    long path_info_length = 1L;
    path_info_length += last_gc_traced_index < 0 ? 0 : last_gc_traced_index;

    char path_info[(path_info_length + 1L) * (40 + 10 + 10)];
    memset(path_info, 0, sizeof(path_info));

    for (long i = 0; i <= last_gc_traced_index; ++i) {
        SCM p = gc_traced[i];

        strcat(path_info, type_str(p));
        strcat(path_info, "; ");

        if (i != last_gc_traced_index) {
            strcat(path_info, "->\n");
        }
    }

    printf("\033[31mWarning: an object that was asserted dead is reachable.\n"
           "Type: %s;\nPath to object: %s\n\n\033[0m",
           suspect_type_str, path_info);
}

void init_gc_traced() {
    gc_traced = (SCM *) malloc(heap_cells * sizeof(SCM));
}

static iproc subr1s[] = {
        {s_assert_dead, assert_dead},
        {0, 0}
};

void init_ega() {
    init_gc_traced();
    init_iprocs(subr1s, tc7_subr_1);
    add_feature("ega");
}