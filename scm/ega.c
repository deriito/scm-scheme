// "ega.c" code for Enhanced GC assertions.
//
// Created by Guang Yang on 2023/04/22.
//

#include "scm.h"

static char s_gc_log_filename[] = "gc_cost_time.log";
static char s_exec_log_filename[] = "exec_cost_time.log";

size_t line_num_quantity_of_a_ref_pattern_at_least = 3L;
size_t gc_count_of_a_ref_pattern_at_most = 3L;
char is_print_result = 1;
size_t current_gc_count = 0;
char is_dynamic_check_mode = 1;
char is_show_ega_debug_info = 0;
char is_disk_save_on = 0;
char is_show_gc_related_info = 0;
char is_gc_cost_time_recording = 0;
size_t gc_idx_gc_cost_recording_start_at = 0;
double current_gc_start_time = 0;
double gc_cost_time_sum = 0;
char is_exec_cost_time_recoding = 0;
double exec_recoding_start_time = 0;
double exec_recoding_tmp_gc_start_time = 0;
double exec_recoding_gc_cost_time_sum = 0;
size_t gc_idx_exec_recoding_start_at = 0;

GcTracedInfo *gc_traced = NULL;
RefPath *focusing_ref_path_list = NULL;
WriteBarrierUpdateMetadata *wb_update_metadata_hash = NULL;

UT_icd *ref_path_entry_icd = NULL;

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

/**
 * 尝试去更新WB相关的情报
 */
static char s_try_record_metadata[] = "try_record_metadata";
static void try_record_metadata(RefPath *ref_path) {
    long len = utarray_len(ref_path->entries);
    if (len <= 1L) {
        // 只处理长度为两个以上的link
        return;
    }

    for (long i = 0; i < len - 1L; ++i) {
        RefPathEntry *curr_entry = (RefPathEntry *) utarray_eltptr(ref_path->entries, i);
        RefPathEntry *next_entry = (RefPathEntry *) utarray_eltptr(ref_path->entries, i + 1L);
        if (!is_user_defined_data_type_instance(curr_entry->ptr)) {
            continue;
        }

        WriteBarrierUpdateMetadata *curr_type_metadata;
        SCM curr_entry_class_obj = get_data_type_def_identifier(curr_entry->ptr);
        HASH_FIND(hh, wb_update_metadata_hash, &curr_entry_class_obj, sizeof(SCM), curr_type_metadata);
        if (NULL == curr_type_metadata) {
            curr_type_metadata = (WriteBarrierUpdateMetadata *) malloc(sizeof(WriteBarrierUpdateMetadata));
            if (NULL == curr_type_metadata) {
                fprintf(stderr, "[curr_type_metadata]内存分配失败!\n");
                exit(-1);
            }
            curr_type_metadata->data_type_def = curr_entry_class_obj;
            curr_type_metadata->field_ref_info = NULL;
            HASH_ADD(hh, wb_update_metadata_hash, data_type_def, sizeof(SCM), curr_type_metadata);
        }

        MetadataPerField *curr_field_ref_info;
        HASH_FIND(hh, curr_type_metadata->field_ref_info, &(curr_entry->ref_field_index), sizeof(long), curr_field_ref_info);
        if (NULL == curr_field_ref_info) {
            curr_field_ref_info = (MetadataPerField *) malloc(sizeof(MetadataPerField));
            if (NULL == curr_field_ref_info) {
                fprintf(stderr, "[curr_field_ref_info]内存分配失败!\n");
                exit(-1);
            }
            curr_field_ref_info->field_index = curr_entry->ref_field_index;
            curr_field_ref_info->update_state_by_ref_type = NULL;
            HASH_ADD(hh, curr_type_metadata->field_ref_info, field_index, sizeof(long), curr_field_ref_info);
        }

        UpdateStateByRefType *curr_ref_type_state;
        SCM next_entry_class_obj_or_fixed_type_code;
        if (is_user_defined_data_type_instance(next_entry->ptr)) {
            next_entry_class_obj_or_fixed_type_code = get_data_type_def_identifier(next_entry->ptr);
        } else {
            next_entry_class_obj_or_fixed_type_code = IMP(next_entry->ptr) ? -1L : TYP7(next_entry->ptr);
        }
        HASH_FIND(hh, curr_field_ref_info->update_state_by_ref_type, &next_entry_class_obj_or_fixed_type_code, sizeof(SCM), curr_ref_type_state);
        if (NULL == curr_ref_type_state) {
            curr_ref_type_state = (UpdateStateByRefType *) malloc(sizeof(UpdateStateByRefType));
            if (NULL == curr_ref_type_state) {
                fprintf(stderr, "[curr_ref_type_state]内存分配失败!\n");
                exit(-1);
            }
            curr_ref_type_state->ref_data_type = next_entry_class_obj_or_fixed_type_code;
            curr_ref_type_state->is_to_add = curr_entry->is_active;
            HASH_ADD(hh, curr_field_ref_info->update_state_by_ref_type, ref_data_type, sizeof(SCM), curr_ref_type_state);
        } else {
            if (curr_ref_type_state->is_to_add <= 0) {
                curr_ref_type_state->is_to_add = curr_entry->is_active;
            }
        }
    }
}

/**
 * 只检查对象的类型信息是否一致
 * @param a
 * @param b
 * @return
 */
static int is_the_same_type(SCM a, SCM b) {
    if (is_user_defined_data_type_instance(a)) {
        if (!is_user_defined_data_type_instance(b)) {
            return 0;
        }
        return get_data_type_def_identifier(a) == get_data_type_def_identifier(b);
    }

    if (IMP(a)) {
        if (NIMP(b)) {
            return 0;
        }
        return 1;
    }

    return TYP7(a) == TYP7(b);
}

/**
 * 检查类型和指向下一个对象的field_idx是否一致
 * @param a
 * @param b
 * @return
 */
static int is_the_same_ref_path_entries(RefPathEntry *a, RefPathEntry *b) {
    if (NULL == a || NULL == b) {
        fprintf(stderr, "[is_the_same_ref_path_entries]异常两个entry都为空!\n");
        exit(-1);
    }

    if (!is_the_same_type(a->ptr, b->ptr)) {
        return 0;
    }

    if (a->ref_field_index != b->ref_field_index) {
        if (VECTORP(a->ptr) && VECTORP(b->ptr)) {
            return 1; // 2023.10.19 vectorなら，fieldの一致するかに関心しない
        }
        return 0;
    }

    return 1;
}

static int is_all_entries_the_same(RefPath *path_a, RefPath *path_b, long quantity) {
    for (long i = 0; i < quantity; ++i) {
        RefPathEntry *tmp_path_entry = (RefPathEntry *)utarray_eltptr(path_a->entries, i);
        RefPathEntry *new_path_entry = (RefPathEntry *)utarray_eltptr(path_b->entries, i);
        if (!is_the_same_ref_path_entries(tmp_path_entry, new_path_entry)) {
            return 0;
        }
    }
    return 1;
}

static void try_record_line_numbers_into_ref_path_entry(SCM ptr, SCM ref_ptr, RefPathEntry *entry) {
    if (!is_user_defined_data_type_instance_with_rec_slots(ptr)) {
        return;
    }

    SCM rec_slot_of_field = get_rec_slot_of_field(ptr, entry->ref_field_index);
    if (NULLP(rec_slot_of_field)) {
        return;
    }

    SCM path_entry_class_obj = get_data_type_def_identifier(ref_ptr);

    SCM ln_vector_of_ref_type = EOL;
    for (long i = 1L; i < INUM(vector_length(rec_slot_of_field)); ++i) {
        SCM tmp_ln_vector_of_ref_type = VELTS(rec_slot_of_field)[i];
        if (VELTS(tmp_ln_vector_of_ref_type)[1] != path_entry_class_obj) {
            continue;
        }

        if (INUM(VELTS(tmp_ln_vector_of_ref_type)[2]) <= 3) {
            // e.g. ln_vector_of_ref_type <= [internal-vector-sym, ref-type-def, used-len, EOL, EOL, ...]
            // 即还没有任何的记录
            break;
        }

        ln_vector_of_ref_type = tmp_ln_vector_of_ref_type;
        break;
    }

    if (NULLP(ln_vector_of_ref_type)) {
        return;
    }

    long ln_quantity = INUM(VELTS(ln_vector_of_ref_type)[2]) - 3L;
    for (long i = 0; i < ln_quantity; ++i) {
        long ln = INUM(VELTS(ln_vector_of_ref_type)[i + 3L]);

        CollectedLineNumber *new_ln;
        HASH_FIND(hh, entry->line_numbers, &ln, sizeof(long), new_ln);
        if (NULL != new_ln) {
            continue;
        }

        new_ln = (CollectedLineNumber *) malloc(sizeof(CollectedLineNumber));
        if (NULL == new_ln) {
            fprintf(stderr, "[new_ln]内存分配失败\n");
            exit(1);
        }
        new_ln->line_num = ln;
        HASH_ADD(hh, entry->line_numbers, line_num, sizeof(long), new_ln);
    }
}

static void merge_ref_path_entry_line_nums(RefPathEntry *dest, RefPathEntry *src) {
    CollectedLineNumber *curr, *tmp;
    HASH_ITER(hh, src->line_numbers, curr, tmp) {
        CollectedLineNumber *ln_in_dest;
        HASH_FIND(hh, dest->line_numbers, &(curr->line_num), sizeof(long), ln_in_dest);
        if (NULL != ln_in_dest) {
            continue;
        }
        ln_in_dest = (CollectedLineNumber *) malloc(sizeof(CollectedLineNumber));
        if (NULL == ln_in_dest) {
            fprintf(stderr, "[ln_in_dest]内存分配失败\n");
            exit(1);
        }
        ln_in_dest->line_num = curr->line_num;
        HASH_ADD(hh, dest->line_numbers, line_num, sizeof(long), ln_in_dest);
    }
}

/**
 * TODO 尝试整理path, 现阶段只针对LinkedList的情况, 即只合并相邻的重复entries
 * @param ptr
 * @param last_gc_traced_index
 * @param is_write_ln
 * @return
 */
static RefPath *gen_a_concise_ref_path(SCM ptr, long last_gc_traced_index, int is_write_ln) {
    RefPath *out = (RefPath *) malloc(sizeof(RefPath));
    if (NULL == out) {
        fprintf(stderr, "[new_ref_path]内存分配失败\n");
        exit(1);
    }
    utarray_new(out->entries, ref_path_entry_icd);

    for (long i = 0; i <= last_gc_traced_index; ++i) {
        long tmp_entry_quantity = utarray_len(out->entries);

        RefPathEntry *new_path_entry = (RefPathEntry *) malloc(sizeof(RefPathEntry));
        if (NULL == new_path_entry) {
            fprintf(stderr, "[new_path_entry]内存分配失败\n");
            exit(1);
        }
        new_path_entry->ptr = gc_traced[i].ptr;
        if (i == last_gc_traced_index) {
            new_path_entry->ref_field_index = -1L;
        } else {
            if (is_user_defined_data_type_instance(new_path_entry->ptr)) {
                new_path_entry->ref_field_index = gc_traced[i].ref_field_index - 1L;
            } else {
                new_path_entry->ref_field_index = gc_traced[i].ref_field_index;
            }
        }
        if (i == last_gc_traced_index) {
            new_path_entry->is_active = 0;
        } else {
            if (is_user_defined_data_type_instance(new_path_entry->ptr)) {
                new_path_entry->is_active = 1;
            } else {
                new_path_entry->is_active = 0;
            }
        }
        new_path_entry->is_repeat = 0;
        new_path_entry->line_numbers = NULL;
        if (is_write_ln && i != last_gc_traced_index) {
            try_record_line_numbers_into_ref_path_entry(new_path_entry->ptr, gc_traced[i + 1L].ptr, new_path_entry);
        }
        new_path_entry->gc_count_at_created = current_gc_count;
        new_path_entry->gc_count_at_last_reactivated = current_gc_count;

        // 至少要有两个元素, 也就是需要至少一个link, 才能够开始比较link是否相同
        if (tmp_entry_quantity < 2) {
            utarray_push_back(out->entries, new_path_entry);
            continue;
        }

        RefPathEntry *tmp_entry_n_2 = (RefPathEntry *) utarray_eltptr(out->entries, tmp_entry_quantity - 2);
        RefPathEntry *tmp_entry_n_1 = (RefPathEntry *) utarray_eltptr(out->entries, tmp_entry_quantity - 1);
        if (is_the_same_ref_path_entries(tmp_entry_n_2, tmp_entry_n_1) // 代入元の比較
            && is_the_same_type(tmp_entry_n_1->ptr, new_path_entry->ptr)) // 代入先の比較
        {
            tmp_entry_n_2->is_active = 1;
            tmp_entry_n_2->is_repeat = 1;
            tmp_entry_n_2->gc_count_at_last_reactivated = current_gc_count;
            merge_ref_path_entry_line_nums(tmp_entry_n_2, tmp_entry_n_1);

            utarray_pop_back(out->entries); // remove&free "tmp_entry_n_1" from the arraylist
        }

        utarray_push_back(out->entries, new_path_entry);
    }

    return out;
}

static RefPath *try_find_same_path_in_focusing_list(RefPath *in) {
    RefPath *out = NULL;

    RefPath *tmp_path;
    long input_path_entry_quantity = utarray_len(in->entries);
    DL_FOREACH(focusing_ref_path_list, tmp_path) {
        long tmp_path_entry_quantity = utarray_len(tmp_path->entries);
        if (tmp_path_entry_quantity != input_path_entry_quantity) {
            continue;
        }

        if (!is_all_entries_the_same(tmp_path, in, tmp_path_entry_quantity)) {
            continue;
        }

        out = tmp_path;
        break;
    }

    return out;
}

static void try_free_ref_path_obj(RefPath *ref_path) {
    if (NULL == ref_path) {
        return;
    }
    utarray_free(ref_path->entries);
    free(ref_path);
}

static void print_ref_path_links(RefPath *ref_path, int is_std_err) {
    FILE *io_stream_f = is_std_err ? stderr : stdout;
    long len = utarray_len(ref_path->entries);

    for (long i = 0; i < len; ++i) {
        RefPathEntry *entry = (RefPathEntry *) utarray_eltptr(ref_path->entries, i);
        SCM p = entry->ptr;

        long recorded_lns_quantity = HASH_COUNT(entry->line_numbers);
        if (recorded_lns_quantity <= 0) {
            fprintf(io_stream_f, "%s#f[%ld]", type_str(p), entry->ref_field_index);
        } else {
            fprintf(io_stream_f, "%s#f[%ld]@", type_str(p), entry->ref_field_index);
            CollectedLineNumber *el, *tmp;
            long j = 0;
            HASH_ITER(hh, entry->line_numbers, el, tmp) {
                fprintf(io_stream_f, "%ld", el->line_num);
                if (j != recorded_lns_quantity - 1L) {
                    fprintf(io_stream_f, ",");
                }
                j += 1L;
            }
        }

        if (i != len - 1L) {
            if (entry->is_repeat) {
                fprintf(io_stream_f, " ->+ ");
            } else {
                fprintf(io_stream_f, " -> ");
            }
        } else {
            fprintf(io_stream_f, ";\n");
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

//    // 2023.09.28 即使是标记过的对象, 也让它能够再次触发收集新path的处理
//    if (is_ref_path_info_recorded(ptr)) {
//        return;
//    }

    // 不管存不存在相同的Path, 都新建一个
    RefPath *new_ref_path = gen_a_concise_ref_path(ptr, last_gc_traced_index, 0);

    // 检查是否已经有重复的路径并且将指针记录下来
    RefPath *same_path = try_find_same_path_in_focusing_list(new_ref_path);

    if (NULL == same_path) {
        DL_APPEND(focusing_ref_path_list, new_ref_path);
    } else {
        // 尝试更新原有的path中的子link的信息 (如: 是否重复等)
        long len = utarray_len(new_ref_path->entries);
        for (long i = 0; i < len; ++i) {
            RefPathEntry *tmp_entry_same_path = (RefPathEntry *) utarray_eltptr(same_path->entries, i);
            RefPathEntry  *tmp_entry_new_path = (RefPathEntry *) utarray_eltptr(new_ref_path->entries, i);

            tmp_entry_same_path->ptr = tmp_entry_new_path->ptr; // 2023.10.18 总是更新一下ptr, 防止出现显示freeCell的情况
            if (!is_user_defined_data_type(tmp_entry_same_path->ptr) &&
                !is_user_defined_data_type_instance(tmp_entry_new_path->ptr) &&
                VECTORP(tmp_entry_same_path->ptr) &&
                VECTORP(tmp_entry_new_path->ptr)) {
                // 2023.10.19 vector的话，也更新一下field（只是为了显示，毕竟是不支持记录行号的数据结构）
                tmp_entry_same_path->ref_field_index = -1L;
            }

            if (!(tmp_entry_new_path->is_repeat)) {
                continue;
            }
            if (tmp_entry_same_path->is_repeat) {
                continue;
            }
            tmp_entry_same_path->is_active = 1;
            tmp_entry_same_path->is_repeat = 1;
            tmp_entry_same_path->gc_count_at_last_reactivated = current_gc_count;
        }
    }

    // 尝试清理new_ref_path
    if (NULL != same_path) {
        try_free_ref_path_obj(new_ref_path);
    }

    // print debug info
    if (is_show_ega_debug_info) {
        RefPath *path_to_show;
        if (NULL == same_path) {
            path_to_show = new_ref_path;
            fprintf(stdout, "\n[DebugInfo] New focusing path collected:\n[DebugInfo] ");
        } else {
            path_to_show = same_path;
            fprintf(stdout, "\n[DebugInfo] Still use old path in focusing path list:\n[DebugInfo] ");
        }
        print_ref_path_links(path_to_show, 0);
    }

    // 这个对象不会再触发这个获取新path的处理 (2023.10.04这个记录暂时没什么用)
    set_ref_path_info_recorded(ptr);

    // 尝试去更新WB相关的情报
    try_record_metadata(NULL == same_path ? new_ref_path : same_path);
}

static char s_try_gather_new_line_num[] = "try_gather_new_line_num";
void try_gather_new_line_num(SCM ptr, long last_gc_traced_index) {
    if (last_gc_traced_index < 1L) {
        return;
    }

    if (!is_user_defined_data_type_instance(ptr)) {
        return;
    }

    if (!is_assert_dead_marked(ptr)) {
        return;
    }

    RefPath *curr_path = gen_a_concise_ref_path(ptr, last_gc_traced_index, 1);

    RefPath *same_path = try_find_same_path_in_focusing_list(curr_path);
    if (NULL == same_path) {
        try_free_ref_path_obj(curr_path);
        return;
    }

    long entry_quantity = utarray_len(curr_path->entries);
    for (long i = 0; i < entry_quantity; ++i) {
        RefPathEntry *curr_path_tmp_entry = (RefPathEntry *) utarray_eltptr(curr_path->entries, i);
        RefPathEntry *same_path_tmp_entry = (RefPathEntry *) utarray_eltptr(same_path->entries, i);

        if (!same_path_tmp_entry->is_active) {
            continue;
        }

        CollectedLineNumber *curr, *tmp;
        HASH_ITER(hh, curr_path_tmp_entry->line_numbers, curr, tmp) {
            CollectedLineNumber *ln_info = NULL;
            HASH_FIND(hh, same_path_tmp_entry->line_numbers, &(curr->line_num), sizeof(long), ln_info);
            if (NULL != ln_info) {
                continue;
            }

            ln_info = (CollectedLineNumber *) malloc(sizeof(CollectedLineNumber));
            if (NULL == ln_info) {
                fprintf(stderr, "[new_ln_info]内存分配失败\n");
                exit(1);
            }
            ln_info->line_num = curr->line_num;
            HASH_ADD(hh, same_path_tmp_entry->line_numbers, line_num, sizeof(long), ln_info);
        }
    }

    try_free_ref_path_obj(curr_path);
}

static void print_result(RefPath *ref_path) {
    if (!is_print_result) {
        return;
    }

    long len = utarray_len(ref_path->entries);

    fprintf(stderr, "\nWarning: an object that was asserted dead is reachable.\n"
           "Type: %s;\n"
           "Path to object: ",
           type_str(((RefPathEntry *) utarray_eltptr(ref_path->entries, len - 1L))->ptr));

    print_ref_path_links(ref_path, 1);
}

static void remove_path_info_from_focusing_list(RefPath *ref_path) {
    utarray_free(ref_path->entries);
    DL_DELETE(focusing_ref_path_list, ref_path);
    free(ref_path);
}

static void check_ref_path_list_after_gc() {
    RefPath *curr, *tmp;
    DL_FOREACH_SAFE(focusing_ref_path_list, curr, tmp) {
        long len = utarray_len(curr->entries);

        int ready_to_print_result = 1;
        for (long i = 0; i < len - 1L; ++i) {
            RefPathEntry *tmp_entry = (RefPathEntry *) utarray_eltptr(curr->entries, i);
            if (!tmp_entry->is_active) {
                continue;
            }
            size_t passed_gc_count = current_gc_count - tmp_entry->gc_count_at_last_reactivated;
            if (passed_gc_count >= gc_count_of_a_ref_pattern_at_most
                || HASH_COUNT(tmp_entry->line_numbers) >= line_num_quantity_of_a_ref_pattern_at_least) {
                tmp_entry->is_active = 0;
                continue;
            }
            ready_to_print_result = 0;
        }

        // 尝试记录metadata
        try_record_metadata(curr);

        if (ready_to_print_result) {
            print_result(curr);
//            // 2023.10.04 不删除已经收集完成的path
//            remove_path_info_from_focusing_list(curr);
        }
    }
}

static void plus_current_gc_count() {
    current_gc_count += 1L;
}

static void init_this_gc_time_params();
static void init_tmp_gc_start_time_for_exec_time_recording();

void ega_process_at_gc_start() {
    plus_current_gc_count();

    if (is_show_gc_related_info) {
        fprintf(stdout, "\n[GCRelatedInfo] No.%ld GC Start.\n", current_gc_count);
    }

    if (is_gc_cost_time_recording) {
        init_this_gc_time_params();
    }

    if (is_exec_cost_time_recoding) {
        init_tmp_gc_start_time_for_exec_time_recording();
    }
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
        memset(exec_code, 0, exec_code_len);
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
        memset(exec_code, 0, exec_code_len);
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

    if (is_show_ega_debug_info) {
        fprintf(stdout, "\n[DebugInfo] WriteBarrierChanged: %s\n", exec_code);
    }

    free(exec_code);
}

static void free_wb_update_metadata_hash() {
    WriteBarrierUpdateMetadata *wbim, *wbim_tmp;
    HASH_ITER(hh, wb_update_metadata_hash, wbim, wbim_tmp) {

        MetadataPerField *mpf, *mpf_tmp;
        HASH_ITER(hh, wbim->field_ref_info, mpf, mpf_tmp) {
            UpdateStateByRefType *usbrt, *usbrt_tmp;
            HASH_ITER(hh, mpf->update_state_by_ref_type, usbrt, usbrt_tmp) {
                HASH_DEL(mpf->update_state_by_ref_type, usbrt);
                free(usbrt);
            }
            HASH_CLEAR(hh, mpf->update_state_by_ref_type);

            HASH_DEL(wbim->field_ref_info, mpf);
            free(mpf);
        }
        HASH_CLEAR(hh, wbim->field_ref_info);

        HASH_DEL(wb_update_metadata_hash, wbim);
        free(wbim);
    }
    HASH_CLEAR(hh, wb_update_metadata_hash);
}

static char s_update_type_def_and_wb[] = "update-type-def-and-wb";
static SCM update_type_def_and_wb() {
    WriteBarrierUpdateMetadata *el, *tmp;
    HASH_ITER(hh, wb_update_metadata_hash, el, tmp) {
        // type_def_objの更新
        try_update_data_type_def(el);

        // ライトバリアの更新
        SCM tmp_type_def = el->data_type_def;
        long field_num = field_number(tmp_type_def);
        for (long i = 0; i < field_num; ++i) {
            MetadataPerField *metadata_per_field;
            HASH_FIND(hh, el->field_ref_info, &i, sizeof(long), metadata_per_field);
            if (NULL == metadata_per_field) {
                continue;
            }

            int is_add_final = 0;

            UpdateStateByRefType *e, *t;
            HASH_ITER(hh, metadata_per_field->update_state_by_ref_type, e, t) {
                if (e->is_to_add) {
                    is_add_final = 1;
                    break;
                }
            }

            wb_add_or_rm(tmp_type_def, i, is_add_final);
        }
    }

    // wb_update_metadata_hashのメモリの解放
    free_wb_update_metadata_hash();

    return UNSPECIFIED;
}

static void sum_gc_time_for_exec_time_recoding();
static void calc_this_gc_cost_time();

void ega_process_after_gc() {
    if (is_dynamic_check_mode > 0) {
        check_ref_path_list_after_gc();
        update_type_def_and_wb();
    } else {
        RefPath *el;
        long counter;
        DL_COUNT(focusing_ref_path_list, el, counter);
        if (counter <= 0) {
            return; // 如果没有focusing path的话就不用执行后面的了
        }
        check_ref_path_list_after_gc();
        scm_evstr("(begin"
                  "(display \"System will be exit!\\n\")"
                  "(disk-save)"
                  "(display \"Current executing status has been saved successfully!\\n\")"
                  "(exit))");
    }

    if (is_exec_cost_time_recoding) {
        sum_gc_time_for_exec_time_recoding();
    }

    if (is_gc_cost_time_recording) {
        calc_this_gc_cost_time();
    }

    if (is_show_gc_related_info) {
        fprintf(stdout, "\n[GCRelatedInfo] No.%ld GC End.\n", current_gc_count);
    }
}

static char s_random_0_n[] = "random-0-n";
static SCM random_0_n(SCM n) {
    int number = INUM(n);
    if (number <= 0) {
        wta(n, (char *)ARG1, s_random_0_n);
    }
    return MAKINUM(rand() % number);
}

static void ref_path_entry_dtor(void *elt) {
    RefPathEntry *tmp_elt = (RefPathEntry *) elt;

    CollectedLineNumber *curr, *tmp;
    HASH_ITER(hh, tmp_elt->line_numbers, curr, tmp) {
        HASH_DEL(tmp_elt->line_numbers, curr);
        free(curr);
    }

    HASH_CLEAR(hh, tmp_elt->line_numbers);
}

static void init_gc_traced() {
    gc_traced = (GcTracedInfo *) malloc(heap_cells * sizeof(GcTracedInfo));
    if (NULL == gc_traced) {
        fprintf(stderr, "[gc_traced]内存分配失败\n");
        exit(1);
    }
}

static void init_focusing_ref_path_list() {
    ref_path_entry_icd = (UT_icd *) malloc(sizeof(UT_icd));
    if (NULL == ref_path_entry_icd) {
        fprintf(stderr, "[ref_path_entry_icd]内存分配失败\n");
        exit(1);
    }
    ref_path_entry_icd->sz = sizeof(RefPathEntry);
    ref_path_entry_icd->init = NULL;
    ref_path_entry_icd->copy = NULL;
    ref_path_entry_icd->dtor = ref_path_entry_dtor;

    focusing_ref_path_list = NULL;
}

static void init_wb_update_metadata_hash() {
    wb_update_metadata_hash = NULL;
}

static char s_thread_sleep[] = "thread-sleep";
static SCM thread_sleep(SCM secs) {
    int s = INUM(secs);
    if (s < 0) {
        wta(secs, (char *) ARG1, s_thread_sleep);
    }
    sleep(s);
    return UNSPECIFIED;
}

static char s_show_gc_related_info_on[] = "show-gc-related-info-on";
static SCM show_gc_related_info_on() {
    is_show_gc_related_info = 1;
    return UNSPECIFIED;
}

static char s_show_gc_related_info_off[] = "show-gc-related-info-off";
static SCM show_gc_related_info_off() {
    is_show_gc_related_info = 0;
    return UNSPECIFIED;
}

static char s_int_div[] = "int-div";
static SCM int_div(SCM a, SCM b) {
    return MAKINUM(INUM(a) / INUM(b));
}

static void print_curr_time_str(FILE *fp) {
    time_t now = time(NULL);
    struct tm s_tm;
    s_tm = *localtime(&now);

    fprintf(fp, "\nNEW RECORD AT %d-%02d-%02d %02d:%02d:%02d (%s): \n",
            s_tm.tm_year + 1900,
            s_tm.tm_mon + 1,
            s_tm.tm_mday,
            s_tm.tm_hour,
            s_tm.tm_min,
            s_tm.tm_sec,
            s_tm.tm_zone);
}

static double get_curr_user_mode_time() {
    struct rusage ru;
    getrusage(RUSAGE_SELF, &ru);
    return ru.ru_utime.tv_sec + (double) ru.ru_utime.tv_usec * 1e-6;
}

static char s_start_record_gc_cost_time[] = "start-record-gc-cost-time";
static SCM start_record_gc_cost_time() {
    is_gc_cost_time_recording = 1;
    gc_idx_gc_cost_recording_start_at = 0;
    current_gc_start_time = 0;
    gc_cost_time_sum = 0;

    FILE *fp;
    if ((fp = fopen(s_gc_log_filename, "a+")) == NULL) {
        wta(UNDEFINED, "Can not open or create exec.log", s_start_record_gc_cost_time);
    }
    print_curr_time_str(fp);
    fclose(fp);

    return UNSPECIFIED;
}

static char s_end_record_gc_cost_time[] = "end-record-gc-cost-time";
static SCM end_record_gc_cost_time() {
    is_gc_cost_time_recording = 0;
    gc_idx_gc_cost_recording_start_at = 0;
    current_gc_start_time = 0;
    gc_cost_time_sum = 0;
    return UNSPECIFIED;
}

static void init_this_gc_time_params() {
    if (gc_idx_gc_cost_recording_start_at <= 0) {
        gc_idx_gc_cost_recording_start_at = current_gc_count;
    }
    current_gc_start_time = get_curr_user_mode_time();
}

static char s_calc_this_gc_cost_time[] = "calc_this_gc_cost_time";
static void calc_this_gc_cost_time() {
    double curr_time = get_curr_user_mode_time();
    double past_time = curr_time - current_gc_start_time;
    gc_cost_time_sum += past_time;

    // record info in file
    FILE *fp;
    if ((fp = fopen(s_gc_log_filename, "a+")) == NULL) {
        wta(UNDEFINED, "Can not open or create gc.log", s_calc_this_gc_cost_time);
    }
    fprintf(fp,
            "[GCCostTimeInfo] No.%ld GC end, cost time: %f secs. Accumulated GC cost %f secs.\n",
            current_gc_count,
            past_time,
            gc_cost_time_sum);
    fclose(fp);
}

static char s_start_record_exec_cost_time[] = "start-record-exec-cost-time";
static SCM start_record_exec_cost_time() {
    is_exec_cost_time_recoding = 1;
    exec_recoding_start_time = get_curr_user_mode_time();
    exec_recoding_tmp_gc_start_time = 0;
    exec_recoding_gc_cost_time_sum = 0;
    gc_idx_exec_recoding_start_at = current_gc_count;
    return UNSPECIFIED;
}

static char s_end_record_exec_cost_time[] = "end-record-exec-cost-time";
static SCM end_record_exec_cost_time() {
    double curr_time = get_curr_user_mode_time();

    double total_time = curr_time - exec_recoding_start_time;

    // record info in file
    FILE *fp;
    if ((fp = fopen(s_exec_log_filename, "a+")) == NULL) {
        wta(UNDEFINED, "Can not open or create exec.log", s_end_record_exec_cost_time);
    }
    print_curr_time_str(fp);
    fprintf(fp,
            "[ExecCostTimeInfo] Total cost time: %f secs. "
            "Total exec cost %f secs. "
            "Total GC cost %f secs. "
            "Number of GC times is %ld.\n",
            total_time,
            total_time - exec_recoding_gc_cost_time_sum,
            exec_recoding_gc_cost_time_sum,
            current_gc_count - gc_idx_exec_recoding_start_at);
    fclose(fp);

    is_exec_cost_time_recoding = 0;
    exec_recoding_start_time = 0;
    exec_recoding_tmp_gc_start_time = 0;
    exec_recoding_gc_cost_time_sum = 0;
    gc_idx_exec_recoding_start_at = 0;
    return UNSPECIFIED;
}

static void init_tmp_gc_start_time_for_exec_time_recording() {
    exec_recoding_tmp_gc_start_time = get_curr_user_mode_time();
}

static void sum_gc_time_for_exec_time_recoding() {
    double curr_time = get_curr_user_mode_time();
    exec_recoding_gc_cost_time_sum += curr_time - exec_recoding_tmp_gc_start_time;
}

static iproc subr0s[] = {
        {s_update_type_def_and_wb, update_type_def_and_wb},
        {s_show_gc_related_info_on, show_gc_related_info_on},
        {s_show_gc_related_info_off, show_gc_related_info_off},
        {s_start_record_gc_cost_time, start_record_gc_cost_time},
        {s_end_record_gc_cost_time, end_record_gc_cost_time},
        {s_start_record_exec_cost_time, start_record_exec_cost_time},
        {s_end_record_exec_cost_time, end_record_exec_cost_time},
        {0, 0}
};

static iproc subr1s[] = {
        {s_assert_dead, assert_dead},
        {s_random_0_n, random_0_n},
        {s_thread_sleep, thread_sleep},
        {0, 0}
};

static iproc subr2s[] = {
        {s_int_div, int_div},
        {0, 0}
};

void init_ega() {
    init_gc_traced();
    init_focusing_ref_path_list();
    init_wb_update_metadata_hash();
    init_iprocs(subr0s, tc7_subr_0);
    init_iprocs(subr1s, tc7_subr_1);
    init_iprocs(subr2s, tc7_subr_2);
    add_feature("ega");
}

void init_ega_disk_saved() {
    if (NULL != ref_path_entry_icd) {
        ref_path_entry_icd->sz = sizeof(RefPathEntry);
        ref_path_entry_icd->init = NULL;
        ref_path_entry_icd->copy = NULL;
        ref_path_entry_icd->dtor = ref_path_entry_dtor;
    }

    init_iprocs(subr0s, tc7_subr_0);
    init_iprocs(subr1s, tc7_subr_1);
    init_iprocs(subr2s, tc7_subr_2);
}
