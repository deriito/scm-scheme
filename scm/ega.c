// "ega.c" code for Enhanced GC assertions.
//
// Created by Guang Yang on 2023/04/22.
//

#include "scm.h"

#define NUM_HASH_BUCKETS 137

char is_process_all_ref_paths = 0;
long line_num_quantity_of_a_ref_pattern_at_least = 1L;
long gc_count_of_a_ref_pattern_at_most = 3L;
char is_print_result = 1;
unsigned long current_gc_count = 0;

GcTracedInfo *gc_traced = NULL;
FocusingRefPathList *focusing_ref_path_list = NULL;
CollectedInfoHash **collect_info_hash_map = NULL;

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

static CollectedInfoHash *find_hash_entry_from_collected_info(SCM data_type_def) {
    char *type_name = data_type_name(data_type_def);
    unsigned long hash_code = strhash(type_name, strlen(type_name), NUM_HASH_BUCKETS);

    CollectedInfoHash *info_list = collect_info_hash_map[hash_code];

    CollectedInfoHash *result = NULL;
    for (CollectedInfoHash *tmp = info_list; NULL != tmp; tmp = tmp->next) {
        if (tmp->data_type_def == data_type_def) {
            result = tmp;
            break;
        }
    }

    if (NULL == result) {
        result = (CollectedInfoHash *) malloc(sizeof(CollectedInfoHash));
        result->data_type_def = data_type_def;

        long field_num = field_number(data_type_def);
        result->field_ref_info = (FieldRefDataTypeInfo *) malloc(field_num * sizeof(FieldRefDataTypeInfo));
        for (long i = 0; i < field_num; ++i) {
            result->field_ref_info[i].ref_data_types = (SCM *) malloc(FIELD_REF_INFO_ALLOCATED_LEN * sizeof(SCM));
            memset(result->field_ref_info[i].ref_data_types, -1, FIELD_REF_INFO_ALLOCATED_LEN);
            result->field_ref_info[i].is_to_add = (int *) malloc(FIELD_REF_INFO_ALLOCATED_LEN * sizeof(int));
            memset(result->field_ref_info[i].is_to_add, -1, FIELD_REF_INFO_ALLOCATED_LEN);
            result->field_ref_info[i].allocated_len = FIELD_REF_INFO_ALLOCATED_LEN;
            result->field_ref_info[i].len = 0;
        }
        result->next = NULL;

        if (NULL == info_list) {
            collect_info_hash_map[hash_code] = result;
        } else {
            CollectedInfoHash *last_entry = info_list;
            while (NULL != last_entry->next) {
                last_entry = last_entry->next;
            }
            last_entry->next = result;
        }
    }

    return result;
}

static void try_expand_types_and_adds_info(FieldRefDataTypeInfo *field_ref_info) {
    long cur_len = field_ref_info->len;
    if (cur_len < field_ref_info->allocated_len) {
        return;
    }

    size_t new_len = (size_t) (FIELD_REF_INFO_ALLOCATED_LEN_EXPAND_TIMES * cur_len);

    SCM *new_ref_data_types = (SCM *) malloc(new_len * sizeof(SCM));
    memset(new_ref_data_types, -1, new_len);
    for (long i = 0; i < cur_len; ++i) {
        new_ref_data_types[i] = field_ref_info->ref_data_types[i];
    }
    free(field_ref_info->ref_data_types);
    field_ref_info->ref_data_types = new_ref_data_types;

    int *new_is_to_add = (int *) malloc(new_len * sizeof(int));
    memset(new_is_to_add, -1, new_len);
    for (long i = 0; i < cur_len; ++i) {
        new_is_to_add[i] = field_ref_info->is_to_add[i];
    }
    free(field_ref_info->is_to_add);
    field_ref_info->is_to_add = new_is_to_add;

    field_ref_info->allocated_len = new_len;
}

long search_index_from_ref_types(CollectedInfoHash *hash_entry, RefPathEntry *path_entry, SCM ref_type_def) {
    long found_index = -1L;
    for (long i = 0; i < hash_entry->field_ref_info[path_entry->ref_field_index].len; ++i) {
        if (ref_type_def == hash_entry->field_ref_info[path_entry->ref_field_index].ref_data_types[i]) {
            found_index = i;
            break;
        }
    }
    return found_index;
}

static char s_try_record_metadata[] = "try_record_metadata";
static void try_record_metadata(RefPath *ref_path) {
    if (ref_path->len <= 1L) {
        // 只处理长度为两个以上的link
        return;
    }

    RefPathEntry *prev_entry = NULL;
    RefPathEntry *curr_entry = NULL;
    RefPathEntry *post_entry = NULL;

    if (ref_path->active_index == 0) { // 先頭
        curr_entry = &ref_path->entries[ref_path->active_index];
        post_entry = &ref_path->entries[ref_path->active_index + 1L];
    } else if (ref_path->active_index == ref_path->len - 1L) { // 末尾
        prev_entry = &ref_path->entries[ref_path->active_index - 1L];
    } else if (0 < ref_path->active_index && ref_path->active_index < ref_path->len - 1L) { // 真ん中
        prev_entry = &ref_path->entries[ref_path->active_index - 1L];
        curr_entry = &ref_path->entries[ref_path->active_index];
        post_entry = &ref_path->entries[ref_path->active_index + 1L];
    } else {
        wta(UNDEFINED, "WTF???", s_try_record_metadata);
    }

    if (NULL != prev_entry && is_user_defined_data_type_instance(prev_entry->ptr)) {
        if (NULL != curr_entry && is_user_defined_data_type_instance(curr_entry->ptr)) {
            SCM prev_type_def = get_data_type_def_identifier(prev_entry->ptr);
            CollectedInfoHash *prev_hash_entry = find_hash_entry_from_collected_info(prev_type_def);
            SCM curr_type_def = get_data_type_def_identifier(curr_entry->ptr);

            long found_index = search_index_from_ref_types(prev_hash_entry, prev_entry, curr_type_def);
            if (found_index < 0) {
                try_expand_types_and_adds_info(&prev_hash_entry->field_ref_info[prev_entry->ref_field_index]);
                long this_idx_field_info_len = prev_hash_entry->field_ref_info[prev_entry->ref_field_index].len;
                prev_hash_entry->field_ref_info[prev_entry->ref_field_index].ref_data_types[this_idx_field_info_len] = curr_type_def;
                prev_hash_entry->field_ref_info[prev_entry->ref_field_index].is_to_add[this_idx_field_info_len] = 0;
                prev_hash_entry->field_ref_info[prev_entry->ref_field_index].len += 1L;
            }
        }
    }

    if (NULL != curr_entry && is_user_defined_data_type_instance(curr_entry->ptr)) {
        if (is_user_defined_data_type_instance(post_entry->ptr)) {
            SCM curr_type_def = get_data_type_def_identifier(curr_entry->ptr);
            CollectedInfoHash *curr_hash_entry = find_hash_entry_from_collected_info(curr_type_def);
            SCM post_type_def = get_data_type_def_identifier(post_entry->ptr);

            long found_index = search_index_from_ref_types(curr_hash_entry, curr_entry, post_type_def);
            if (found_index < 0) {
                try_expand_types_and_adds_info(&curr_hash_entry->field_ref_info[curr_entry->ref_field_index]);
                long this_index_field_info_len = curr_hash_entry->field_ref_info[curr_entry->ref_field_index].len;
                curr_hash_entry->field_ref_info[curr_entry->ref_field_index].ref_data_types[this_index_field_info_len] = post_type_def;
                curr_hash_entry->field_ref_info[curr_entry->ref_field_index].is_to_add[this_index_field_info_len] = 1;
                curr_hash_entry->field_ref_info[curr_entry->ref_field_index].len += 1L;
            } else {
                curr_hash_entry->field_ref_info[curr_entry->ref_field_index].is_to_add[found_index] = 1;
            }
        }
    }
}

void try_gather_new_ref_path(SCM ptr, long last_gc_traced_index) {
    if (!is_user_defined_data_type_instance(ptr)) {
        return;
    }

    if (!is_assert_dead_marked(ptr)) {
        return;
    }

    if (is_ref_path_info_recorded(ptr)) {
        return;
    }

    RefPath *new_ref_path = (RefPath *) malloc(sizeof(RefPath));

    new_ref_path->active_index = 0;
    new_ref_path->gc_count_of_active_index = 0;
    new_ref_path->len = 1L + (last_gc_traced_index < 0 ? 0 : last_gc_traced_index);
    new_ref_path->entries = (RefPathEntry *) malloc(new_ref_path->len * sizeof(RefPathEntry));
    new_ref_path->prev = NULL;
    new_ref_path->next = NULL;

    for (long i = 0; i <= last_gc_traced_index; ++i) {
        new_ref_path->entries[i].ptr = gc_traced[i].ptr;
        long data_type_inst_field_idx = gc_traced[i].ref_field_index - 1L;
        new_ref_path->entries[i].ref_field_index = (i == last_gc_traced_index ? -1L : data_type_inst_field_idx);
        new_ref_path->entries[i].line_num_quantity = 0;
        new_ref_path->entries[i].line_nums = NULL;
    }

    if (NULL == focusing_ref_path_list->paths) {
        focusing_ref_path_list->paths = new_ref_path;
        focusing_ref_path_list->last = new_ref_path;
    } else {
        focusing_ref_path_list->last->next = new_ref_path;
        new_ref_path->prev = focusing_ref_path_list->last;
    }

    set_ref_path_info_recorded(ptr);
    try_record_metadata(new_ref_path);
}

static long partition(long **arr_p, long low, long high) {
    long t = (*arr_p)[low];
    while (low < high) {
        while (low < high && (*arr_p)[high] >= t) {
            high -= 1;
        }
        (*arr_p)[low] = (*arr_p)[high];

        while (low < high && (*arr_p)[low] <= t) {
            low += 1;
        }
        (*arr_p)[high] = (*arr_p)[low];
    }
    (*arr_p)[low] = t;
    return low;
}

static void q_sort(long **arr_p, long low, long high) {
    if (low < high) {
        long t = partition(arr_p, low, high);
        q_sort(arr_p, low, t - 1);
        q_sort(arr_p, t + 1, high);
    }
}

static void quick_sort(long **arr_p, long len) {
    q_sort(arr_p, 0, len - 1);
}

static long rm_duplicates(long **in_arr_p, long in_arr_len) {
    long j = 0;
    for (int i = 0; i < in_arr_len; ++i) {
        if ((*in_arr_p)[i] != (*in_arr_p)[i + 1]) {
            (*in_arr_p)[j++] = (*in_arr_p)[i];
        }
    }
    return j;
}

/**
 * merge two arrays of long
 * @param arr1_p
 * @param arr2_p
 * @param out_arr_p
 * @param arr1_len
 * @param arr2_len
 * @return length of out_arr
 */
static long merge_arrays(long **arr1_p, long **arr2_p, long **out_arr_p, long arr1_len, long arr2_len) {
    long *arr1 = *arr1_p;
    long *arr2 = *arr2_p;

    long *merge_roughly = (long *) malloc((arr1_len + arr2_len) * sizeof(long));
    memcpy(merge_roughly, arr1, arr1_len * sizeof(long));
    memcpy(merge_roughly + arr1_len, arr2, arr2_len * sizeof(long));

    quick_sort(&merge_roughly, arr1_len + arr2_len);
    long new_len = rm_duplicates(&merge_roughly, arr1_len + arr2_len);

    long *result = (long *) malloc(new_len * sizeof(long));
    memcpy(result, merge_roughly, new_len * sizeof(long));

    out_arr_p = &result;
    free(merge_roughly);

    return new_len;
}

static char s_try_gather_new_line_num[] = "try_gather_new_line_num";
void try_gather_new_line_num(SCM ptr, long last_gc_traced_index) {
    if (last_gc_traced_index < 1L) {
        return;
    }

    if (!is_user_defined_data_type_instance(ptr)) {
        return;
    }

    SCM prev_ptr = gc_traced[last_gc_traced_index - 1L].ptr;
    if (!is_user_defined_data_type_instance(prev_ptr)) {
        return;
    }

    if (!is_user_defined_data_type_instance_with_rec_slots(prev_ptr)) {
        return;
    }

    long prev_obj_field_index = gc_traced[last_gc_traced_index - 1L].ref_field_index - 1L;
    SCM rec_slot_of_field = get_rec_slot_of_field(prev_ptr, prev_obj_field_index);
    if (NULLP(rec_slot_of_field)) {
        return;
    }

    SCM prev_ptr_type_def = get_data_type_def_identifier(prev_ptr);
    SCM curr_ptr_type_def = get_data_type_def_identifier(ptr);

    for (RefPath *tmp_path = focusing_ref_path_list->paths; NULL != tmp_path && !is_process_all_ref_paths; tmp_path = tmp_path->next) {
        long len = tmp_path->len;
        if (len <= 1L) {
            continue;
        }

        long active_index = tmp_path->active_index;
        if (active_index == len - 1L) {
            continue;
        }

        //参照パターン(a.f -> b)の比較
        SCM a = tmp_path->entries[active_index].ptr;
        if (!is_user_defined_data_type_instance(a)) {
            continue;
        }
        SCM b = tmp_path->entries[active_index + 1L].ptr;
        if (!is_user_defined_data_type_instance(b)) {
            continue;
        }
        if (get_data_type_def_identifier(a) != prev_ptr_type_def) {
            continue;
        }
        if (tmp_path->entries[active_index].ref_field_index != gc_traced[last_gc_traced_index - 1L].ref_field_index - 1L) {
            continue;
        }
        if (get_data_type_def_identifier(b) != curr_ptr_type_def) {
            continue;
        }

        // 收集分散存储的行号
        SCM ln_vector_of_ref_type = EOL;
        for (long i = 1L; i < INUM(VELTS(rec_slot_of_field)[1]) - 1L; ++i) {
            SCM tmp_ln_vector_of_ref_type = VELTS(rec_slot_of_field)[i];
            if (VELTS(tmp_ln_vector_of_ref_type)[2] <= MAKINUM(3)) {
                // e.g. ln_vector_of_ref_type <= [internal-vector-sym, ref-type-def, used-len, EOL, EOL, ...]
                break;
            }
            if (VELTS(tmp_ln_vector_of_ref_type)[1] != curr_ptr_type_def) {
                continue;
            }
            ln_vector_of_ref_type = tmp_ln_vector_of_ref_type;
            break;
        }

        if (NULLP(ln_vector_of_ref_type)) {
            return;
        }

        long tmp_len = INUM(VELTS(ln_vector_of_ref_type)[2]) - 3;
        long *tmp_lns = (long *) malloc(tmp_len * sizeof(long));
        for (long i = 0; i < tmp_len; ++i) {
            tmp_lns[i] = INUM(VELTS(ln_vector_of_ref_type)[i + 3L]);
        }

        if (tmp_path->entries[active_index].line_num_quantity <= 0) {
            tmp_path->entries[active_index].line_nums = tmp_lns;
            tmp_path->entries[active_index].line_num_quantity = tmp_len;
        } else {
            long *new_line_nums;
            long new_len = merge_arrays(&tmp_path->entries[active_index].line_nums,
                                        &tmp_lns,
                                        &new_line_nums,
                                        tmp_path->entries[active_index].line_num_quantity,
                                        tmp_len);
            free(tmp_path->entries[active_index].line_nums);
            tmp_path->entries[active_index].line_nums = new_line_nums;
            tmp_path->entries[active_index].line_num_quantity = new_len;
        }
    }
}

static void print_result(RefPath *ref_path) {
    if (!is_print_result) {
        return;
    }

    printf("\033[31mWarning: an object that was asserted dead is reachable.\n"
           "Type: %s;\n"
           "Path to object: ",
           type_str(ref_path->entries[ref_path->len -1L].ptr));

    for (long i = 0; i < ref_path->len; ++i) {
        SCM p = ref_path->entries[i].ptr;

        if (ref_path->entries[i].line_num_quantity <= 0) {
            printf("%s", type_str(p));
        } else {
            printf("%s@ln", type_str(p));
            for (long index = 0; index < ref_path->entries[i].line_num_quantity; ++index) {
                printf("%ld", ref_path->entries[i].line_nums[index]);
                if (index != ref_path->entries[i].line_num_quantity - 1L) {
                    printf(",");
                }
            }
        }

        if (i != ref_path->len - 1L) {
            printf("; -> ");
        } else {
            printf(";");
        }
    }

    printf("\n\n\033[0m");
}

static void remove_path_info(RefPath *ref_path) {
    // try to break the relationship between the node and the whole linked list
    if (NULL == ref_path->prev && NULL == ref_path->next) {
        focusing_ref_path_list->paths = NULL;
        focusing_ref_path_list->last = NULL;
    } else if (NULL != ref_path->prev && NULL != ref_path->next) {
        ref_path->prev->next = ref_path->next;
        ref_path->next->prev = ref_path->prev;
    } else if (NULL == ref_path->prev) {
        ref_path->next->prev = NULL;
        focusing_ref_path_list->paths = ref_path->next;
    } else if (NULL == ref_path->next) {
        ref_path->prev->next = NULL;
        focusing_ref_path_list->last = ref_path->prev;
    }

    // free memory of the node
    free(ref_path->entries->line_nums);
    free(ref_path->entries);
    free(ref_path);
}

static void check_ref_path_list_after_gc() {
    typedef struct pointer_info {
        RefPath *p;
        struct pointer_info *next;
    } PointerInfo;

    PointerInfo *fst = NULL;
    PointerInfo *lst = NULL;

    for (RefPath *tmp_path = focusing_ref_path_list->paths; NULL != tmp_path && !is_process_all_ref_paths; tmp_path = tmp_path->next) {
        tmp_path->gc_count_of_active_index += 1L;
        if (tmp_path->gc_count_of_active_index >= gc_count_of_a_ref_pattern_at_most
            || tmp_path->entries[tmp_path->active_index].line_num_quantity >= line_num_quantity_of_a_ref_pattern_at_least) {
            tmp_path->active_index += 1L;
            try_record_metadata(tmp_path);
        }

        if (tmp_path->active_index < tmp_path->len - 1L) {
            continue;
        }

        print_result(tmp_path);

        // collect RefPath that to be removed
        PointerInfo *tmp_pointer_info = (PointerInfo *) malloc(sizeof(PointerInfo));
        tmp_pointer_info->p = tmp_path;
        tmp_pointer_info->next = NULL;
        if (NULL == fst) {
            fst = tmp_pointer_info;
        } else {
            lst->next = tmp_pointer_info;
        }
        lst = tmp_pointer_info;
    }

    for (PointerInfo *tmp = fst, *nxt; NULL != tmp; tmp = nxt) {
        nxt = tmp->next;
        remove_path_info(tmp->p);
        free(tmp);
    }
}

static void plus_current_gc_count() {
    current_gc_count += 1L;
}

void ega_process_at_gc_start() {
    plus_current_gc_count();
}

void ega_process_after_gc() {
    check_ref_path_list_after_gc();
    scm_evstr("(begin"
              "(display \"System will be exit!\\n\")"
              "(disk-save)"
              "(display \"Current executing status has been saved successfully!\\n\")"
              "(exit))");
}

static void wb_add_or_rm(SCM data_type_def, long field_index, int is_add) {
    char *type_name_str = data_type_name(data_type_def);
    char *field_name_str = data_type_field_name(data_type_def, field_index);

    char str0[] = "(set! ";
    char str1[] = "set-";
    char str2[] = "! set-";
    char str3[] = "!-with-wb)";
    char str4[] = "!-bakup)";
    char set_str[] = "set";
    char hyphen[] = "-";
    char over[] = "\0";

    char *exec_code;
    size_t exec_code_len = 1; // 文字列の末尾には「\0」があるから
    if (is_add) {
        exec_code_len += (strlen(str0) + strlen(str1) + strlen(type_name_str)
                            + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
                            + strlen(type_name_str) + strlen(hyphen) + strlen(field_name_str)
                            + strlen(str3));
        exec_code = (char *) malloc(exec_code_len * sizeof(char));
        memcpy(exec_code, str0, strlen(str0));
        memcpy(exec_code + strlen(str0), str1, strlen(str1));
        memcpy(exec_code + strlen(str0) + strlen(str1), type_name_str, strlen(type_name_str));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str), hyphen, strlen(hyphen));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
                            + strlen(hyphen), field_name_str, strlen(field_name_str));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
                            + strlen(hyphen) + strlen(field_name_str), str2, strlen(str2));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
                            + strlen(hyphen) + strlen(field_name_str) + strlen(str2), type_name_str, strlen(type_name_str));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
                            + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
                            + strlen(type_name_str), hyphen, strlen(hyphen));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
                            + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
                            + strlen(type_name_str) + strlen(hyphen), field_name_str, strlen(field_name_str));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
                            + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
                            + strlen(type_name_str) + strlen(hyphen) + strlen(field_name_str), str3, strlen(str3));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
                            + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
                            + strlen(type_name_str) + strlen(hyphen) + strlen(field_name_str)
                            + strlen(str3), over, strlen(over));
        scm_evstr(exec_code); // (set! set-xxx-fx! set-xxx-fx!-with-wb)
    } else {
        exec_code_len += (strlen(str0) + strlen(str1) + strlen(type_name_str)
                          + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
                          + strlen(type_name_str) + strlen(hyphen) + strlen(field_name_str)
                          + strlen(str4));
        exec_code = (char *) malloc(exec_code_len * sizeof(char));
        memcpy(exec_code, str0, strlen(str0));
        memcpy(exec_code + strlen(str0), str1, strlen(str1));
        memcpy(exec_code + strlen(str0) + strlen(str1), type_name_str, strlen(type_name_str));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str), hyphen, strlen(hyphen));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
               + strlen(hyphen), field_name_str, strlen(field_name_str));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
               + strlen(hyphen) + strlen(field_name_str), str2, strlen(str2));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
               + strlen(hyphen) + strlen(field_name_str) + strlen(str2), type_name_str, strlen(type_name_str));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
               + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
               + strlen(type_name_str), hyphen, strlen(hyphen));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
               + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
               + strlen(type_name_str) + strlen(hyphen), field_name_str, strlen(field_name_str));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
               + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
               + strlen(type_name_str) + strlen(hyphen) + strlen(field_name_str), str4, strlen(str4));
        memcpy(exec_code + strlen(str0) + strlen(str1) + strlen(type_name_str)
               + strlen(hyphen) + strlen(field_name_str) + strlen(str2)
               + strlen(type_name_str) + strlen(hyphen) + strlen(field_name_str)
               + strlen(str4), over, strlen(over));
        scm_evstr(exec_code); // (set! set-xxx-fx! set-xxx-fx!-bakup)
    }

    free(exec_code);
}

static char s_update_type_def_and_wb[] = "update-type-def-and-wb";
static SCM update_type_def_and_wb() {
    for (long i = 0; i < NUM_HASH_BUCKETS; ++i) {
        CollectedInfoHash *info_list = collect_info_hash_map[i];
        while (NULL != info_list) {
            SCM tmp_type_def = info_list->data_type_def;
            long field_num = field_number(tmp_type_def);

            // type_def_objの更新
            try_update_data_type_def(info_list);

            // ライトバリアの更新
            for (long fi = 0; fi < field_num; ++fi) {
                int is_add = 0;
                for (long m = 0; m < info_list->field_ref_info[fi].len; ++m) {
                    if (info_list->field_ref_info->is_to_add[m]) {
                        is_add = 1;
                        break;
                    }
                }
                wb_add_or_rm(tmp_type_def, fi, is_add);
            }

            info_list = info_list->next;
        }
    }

    // CollectInfoHashのメモリの解放
    for (long i = 0; i < NUM_HASH_BUCKETS; ++i) {
        CollectedInfoHash *info_list = collect_info_hash_map[i];
        while (NULL != info_list) {
            free(info_list->field_ref_info->ref_data_types);
            free(info_list->field_ref_info->is_to_add);
            free(info_list->field_ref_info);
            CollectedInfoHash *tmp = info_list;
            info_list = info_list->next;
            free(tmp);
        }
        collect_info_hash_map[i] = NULL;
    }
    return UNSPECIFIED;
}

void init_gc_traced() {
    gc_traced = (GcTracedInfo *) malloc(heap_cells * sizeof(GcTracedInfo));
    focusing_ref_path_list = (FocusingRefPathList *) malloc(sizeof(FocusingRefPathList));
    focusing_ref_path_list->paths = NULL;
    focusing_ref_path_list->last = NULL;
    collect_info_hash_map = (CollectedInfoHash **) malloc(NUM_HASH_BUCKETS * sizeof(CollectedInfoHash *));
    for (int i = 0; i < NUM_HASH_BUCKETS; ++i) {
        collect_info_hash_map[i] = NULL;
    }
}

static iproc subr0s[] = {
        {s_update_type_def_and_wb, update_type_def_and_wb},
        {0,                        0}
};

static iproc subr1s[] = {
        {s_assert_dead, assert_dead},
        {0, 0}
};

void init_ega() {
    init_gc_traced();
    init_iprocs(subr0s, tc7_subr_0);
    init_iprocs(subr1s, tc7_subr_1);
    add_feature("ega");
}

void init_ega_disk_saved() {
    init_iprocs(subr0s, tc7_subr_0);
    init_iprocs(subr1s, tc7_subr_1);
}
