//
// Created by Guang Yang on 2023/05/17.
//

#include "scm.h"
#include "setjump.h"

static char s_disk_save[] = "disk-save";
SCM disk_save() {
    if (!is_disk_save_on) {
        return UNSPECIFIED;
    }

    FILE *fp;
    if ((fp = fopen("saved_image", "wb")) == NULL) {
        wta(UNDEFINED, "Can not open freeze file(save_image)", s_disk_save);
        return UNSPECIFIED;
    }

    disk_saved = 1;

    /* ヒープ関連 mymalloc.c */
    fwrite(&managed_memory_start, sizeof(char *), 1, fp);
    fwrite(&managed_memory_end, sizeof(char *), 1, fp);

    size_t used_bytes = managed_memory_end - managed_memory_start;
    fwrite(managed_memory_start, sizeof(char), used_bytes, fp);

    fwrite(&base, sizeof(HEADER), 1, fp);
    fwrite(&freep, sizeof(HEADER *), 1, fp);

    sizet base_addr = (sizet) &base;
    fwrite(&base_addr, sizeof(sizet), 1, fp);

    /* Cのグローバル変数 */
    // scm.c
    fwrite(&case_sensitize_symbols, sizeof(int), 1, fp);
    fwrite(&disk_saved, sizeof(int), 1, fp);
    fwrite(&dumped, sizeof(int), 1, fp);
    fwrite(&loc_features, sizeof(SCM *), 1, fp);
    fwrite(&setitimer_tab[0].sym, sizeof(SCM), 1, fp);
    fwrite(&setitimer_tab[0].which, sizeof(int), 1, fp);
    fwrite(&setitimer_tab[1].sym, sizeof(SCM), 1, fp);
    fwrite(&setitimer_tab[1].which, sizeof(int), 1, fp);
    fwrite(&setitimer_tab[2].sym, sizeof(SCM), 1, fp);
    fwrite(&setitimer_tab[2].which, sizeof(int), 1, fp);

    // sys.c
    fwrite(&ecache_v, sizeof(cell), 1, fp);
    fwrite(&estk_pool, sizeof(SCM), 1, fp);
    fwrite(&expmem, sizeof(int), 1, fp);
    fwrite(&finals_gra, sizeof(scm_gra), 1, fp);
    fwrite(&freelist, sizeof(SCM), 1, fp);
    fwrite(&gc_finalizers, sizeof(SCM), 1, fp);
    fwrite(&gc_finalizers_pending, sizeof(SCM), 1, fp);
    fwrite(&gc_hook_active, sizeof(int), 1, fp);
    fwrite(&gc_hook_pending, sizeof(int), 1, fp);
    fwrite(&heap_cells, sizeof(long), 1, fp);
    fwrite(&heap_org, sizeof(CELLPTR), 1, fp);
    fwrite(&hplim_ind, sizeof(sizet), 1, fp);
    fwrite(&hplims, sizeof(CELLPTR *), 1, fp);
    fwrite(&loc_gc_hook, sizeof(SCM *), 1, fp);
    fwrite(&loc_open_file, sizeof(SCM *), 1, fp);
    fwrite(&loc_try_create_file, sizeof(SCM *), 1, fp);
    fwrite(&mltrigger, sizeof(long), 1, fp);
    fwrite(&mtrigger, sizeof(long), 1, fp);
    fwrite(&no_symhash_gc, sizeof(int), 1, fp);
    fwrite(&output_deferred, sizeof(int), 1, fp);
    fwrite(&ptobs_gra, sizeof(scm_gra), 1, fp);
    fwrite(&scm_ecache, sizeof(CELLPTR), 1, fp);
    fwrite(&scm_ecache_index, sizeof(long), 1, fp);
    fwrite(&scm_ecache_len, sizeof(long), 1, fp);
    fwrite(&scm_egc_root_index, sizeof(long), 1, fp);
    fwrite(scm_egc_roots, sizeof(SCM), ECACHE_SIZE/20, fp);
    fwrite(&scm_estk, sizeof(SCM), 1, fp);
    fwrite(&scm_estk_ptr, sizeof(SCM *), 1, fp);
    fwrite(&scm_estk_size, sizeof(long), 1, fp);
    fwrite(&scm_port_table, sizeof(port_info *), 1, fp);
    fwrite(&scm_port_table_len, sizeof(sizet), 1, fp);
    fwrite(&smobs_gra, sizeof(scm_gra), 1, fp);
    fwrite(&subrs_gra, sizeof(scm_gra), 1, fp);
    fwrite(&symhash, sizeof(SCM), 1, fp);
    fwrite(&symhash_dim, sizeof(int), 1, fp);
    fwrite(&tc16_clport, sizeof(long), 1, fp);
    fwrite(&tc16_safeport, sizeof(int), 1, fp);
    fwrite(&tc16_sysport, sizeof(int), 1, fp);
    fwrite(&tmp_errp, sizeof(SCM), 1, fp);
    fwrite(tmp_errpbuf, sizeof(cell), 3, fp);

    // subr.c
    // nothing

    // repl.c
    fwrite(upcase, sizeof(unsigned char), CHAR_CODE_LIMIT, fp);
    fwrite(downcase, sizeof(unsigned char), CHAR_CODE_LIMIT, fp);
    fwrite(&loc_broken_pipe, sizeof(SCM *), 1, fp);
    fwrite(&p_read_for_load, sizeof(SCM), 1, fp);
    fwrite(&p_read, sizeof(SCM), 1, fp);
    fwrite(&loc_loadsharp, sizeof(SCM *), 1, fp);
    fwrite(&loc_readsharp, sizeof(SCM *), 1, fp);
    fwrite(&loc_charsharp, sizeof(SCM *), 1, fp);
    fwrite(&tc16_arbiter, sizeof(long), 1, fp);
    fwrite(&ints_disabled, sizeof(int), 1, fp);
    fwrite(&SIG_deferred, sizeof(unsigned long), 1, fp);
    fwrite(&scm_verbose, sizeof(int), 1, fp);
    fwrite(&errjmp_recursive, sizeof(int), 1, fp);
    fwrite(&errobj_codep, sizeof(int), 1, fp);
    fwrite(&err_exp, sizeof(SCM), 1, fp);
    fwrite(&err_env, sizeof(SCM), 1, fp);
    fwrite(&loc_errobj, sizeof(SCM *), 1, fp);
    fwrite(&loc_loadpath, sizeof(SCM *), 1, fp);
    fwrite(&mallocated, sizeof(unsigned long), 1, fp);
    fwrite(&lmallocated, sizeof(unsigned long), 1, fp);
    fwrite(&cells_allocated, sizeof(long), 1, fp);
    fwrite(&lcells_allocated, sizeof(long), 1, fp);
    fwrite(&rt, sizeof(long), 1, fp);
    fwrite(&gc_rt, sizeof(long), 1, fp);
    fwrite(&gc_time_taken, sizeof(long), 1, fp);
    fwrite(&gc_cells_collected, sizeof(long), 1, fp);
    fwrite(&gc_malloc_collected, sizeof(long), 1, fp);
    fwrite(&gc_ports_collected, sizeof(long), 1, fp);
    fwrite(&gc_syms_collected, sizeof(long), 1, fp);
    fwrite(&scm_env_work, sizeof(long), 1, fp);
    fwrite(&scm_gcs, sizeof(long), 1, fp);
    fwrite(&scm_egcs, sizeof(long), 1, fp);
    fwrite(&scm_stk_moved, sizeof(long), 1, fp);
    fwrite(&scm_clo_moved, sizeof(long), 1, fp);
    fwrite(&scm_egc_rt, sizeof(long), 1, fp);
    fwrite(&i_repl, sizeof(SCM), 1, fp);
    fwrite(&i_eval_string, sizeof(SCM), 1, fp);
    fwrite(&i_load_string, sizeof(SCM), 1, fp);

    // eval.c
    fwrite(&scm_env, sizeof(SCM), 1, fp);
    fwrite(&scm_env_tmp, sizeof(SCM), 1, fp);
    fwrite(&tc16_env, sizeof(long), 1, fp);
#ifndef RECKLESS
    fwrite(&scm_trace, sizeof(SCM), 1, fp);
    fwrite(&scm_trace_env, sizeof(SCM), 1, fp);
#endif
    fwrite(&tc16_macro, sizeof(long), 1, fp);
    fwrite(&i_dot, sizeof(SCM), 1, fp);
    fwrite(&i_quote, sizeof(SCM), 1, fp);
    fwrite(&i_quasiquote, sizeof(SCM), 1, fp);
    fwrite(&i_unquote, sizeof(SCM), 1, fp);
    fwrite(&i_uq_splicing, sizeof(SCM), 1, fp);
    fwrite(&i_lambda, sizeof(SCM), 1, fp);
    fwrite(&i_define, sizeof(SCM), 1, fp);
    fwrite(&i_let, sizeof(SCM), 1, fp);
    fwrite(&i_begin, sizeof(SCM), 1, fp);
    fwrite(&i_arrow, sizeof(SCM), 1, fp);
    fwrite(&i_else, sizeof(SCM), 1, fp);
    fwrite(&i_bind, sizeof(SCM), 1, fp);
    fwrite(&i_anon, sizeof(SCM), 1, fp);
    fwrite(&i_side_effect, sizeof(SCM), 1, fp);
    fwrite(&i_test, sizeof(SCM), 1, fp);
    fwrite(&i_procedure, sizeof(SCM), 1, fp);
    fwrite(&i_argument, sizeof(SCM), 1, fp);
    fwrite(&i_check_defines, sizeof(SCM), 1, fp);
    fwrite(&f_begin, sizeof(SCM), 1, fp);
    fwrite(&f_define, sizeof(SCM), 1, fp);
    fwrite(&loc_atcase_aux, sizeof(SCM *), 1, fp);
    fwrite(&in_atcase_aux, sizeof(int), 1, fp);
    fwrite(&tc16_promise, sizeof(long), 1, fp);

    // findexec.c
    // nothing

    // script.c
    // nothing

    // time.c
    //nothing

    // continue.c
#ifdef SHORT_INT
    fwrite(&thrown_value, sizeof(long), 1, fp);
#endif

    // debug.c
    fwrite(&tc16_codeptr, sizeof(long), 1, fp);

    // unif.c
    fwrite(&tc16_array, sizeof(long), 1, fp);

    // rope.c
    fwrite(&scm_protidx, sizeof(long), 1, fp);

    // definedatatype.c
    fwrite(&module_flag_symbol, sizeof(SCM), 1, fp);
    fwrite(&internal_vector_symbol, sizeof(SCM), 1, fp);

    // ega.c
    fwrite(&line_num_quantity_of_a_ref_pattern_at_least, sizeof(size_t), 1, fp);
    fwrite(&gc_count_of_a_ref_pattern_at_most, sizeof(size_t), 1, fp);
    fwrite(&is_print_result, sizeof(char), 1, fp);
    fwrite(&current_gc_count, sizeof(size_t), 1, fp);
    fwrite(&is_dynamic_check_mode, sizeof(char), 1, fp);
    fwrite(&is_show_ega_debug_info, sizeof(char), 1, fp);
    fwrite(&is_disk_save_on, sizeof(char), 1, fp);
    fwrite(&is_show_gc_related_info, sizeof(char), 1, fp);
    fwrite(&is_gc_cost_time_recording, sizeof(char), 1, fp);
    fwrite(&gc_idx_gc_cost_recording_start_at, sizeof(size_t), 1, fp);
    fwrite(&current_gc_start_time, sizeof(double), 1, fp);
    fwrite(&gc_cost_time_sum, sizeof(double), 1, fp);
    fwrite(&is_exec_cost_time_recoding, sizeof(char), 1, fp);
    fwrite(&exec_recoding_start_time, sizeof(double), 1, fp);
    fwrite(&exec_recoding_tmp_gc_start_time, sizeof(double), 1, fp);
    fwrite(&exec_recoding_gc_cost_time_sum, sizeof(double), 1, fp);
    fwrite(&gc_idx_exec_recoding_start_at, sizeof(size_t), 1, fp);
    fwrite(&gc_traced, sizeof(GcTracedInfo *), 1, fp);
    fwrite(&focusing_ref_path_list, sizeof(RefPath *), 1, fp);
    fwrite(&wb_update_metadata_hash, sizeof(WriteBarrierUpdateMetadata *), 1, fp);
    fwrite(&ref_path_entry_icd, sizeof(UT_icd *), 1, fp);

    // disksave.c
    // nothing

    // socket.c
    // 2023.09.19 No usages
//#ifdef COMPILED_INITS
//    fwrite(&tc16_sknm, sizeof(long), 1, fp);
//#endif

    // scl.c
    fwrite(sys_protects, sizeof(SCM), NUM_PROTECTS, fp);
    fwrite(&num_protects, sizeof(sizet), 1, fp);

    fclose(fp);
    return UNSPECIFIED;
}

static iproc subr0s[] = {
        {s_disk_save, disk_save},
        {0, 0}
};

void init_disk_save() {
    init_iprocs(subr0s, tc7_subr_0);
    add_feature("disksave");
}

void init_disk_save_disk_saved() {
    init_iprocs(subr0s, tc7_subr_0);
}
