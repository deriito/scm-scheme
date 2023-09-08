//
// Created by Guang Yang on 2023/05/21.
//

#define PREPARED_MEMORY_BYTES (5 * 1024 * 1024)
#define OFFSET_OF_STARTS (1024 * 1024)

#include <sys/mman.h>
#include "scm.h"
#include "setjump.h"

HEADER base; /* empty list to get started */
HEADER *freep = NULL; /* start of free list */

char *prepared_memory_start;

char *managed_memory_start;
char *managed_memory_end;

void init_my_zone() {
    char *cp = (char *) mmap(NULL, PREPARED_MEMORY_BYTES, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
    if (MAP_FAILED == cp) {
        exit(253);
    }

    prepared_memory_start = cp;

    FILE *fp;
    if ((fp = fopen("saved_image", "rb")) == NULL) { // 正常初始化内存
        managed_memory_start = prepared_memory_start + OFFSET_OF_STARTS;
        managed_memory_end = managed_memory_start;
        return;
    }

    /* ヒープ関連 mymalloc.c */
    fread(&managed_memory_start, sizeof(char *), 1, fp);
    if (managed_memory_start < prepared_memory_start) {
        fclose(fp);
        exit(254);
    }

    fread(&managed_memory_end, sizeof(char *), 1, fp);
    if (managed_memory_end > prepared_memory_start + PREPARED_MEMORY_BYTES) {
        fclose(fp);
        exit(255);
    }

    size_t used_bytes = managed_memory_end - managed_memory_start;
    fread(managed_memory_start, sizeof(char), used_bytes, fp);

    fread(&base, sizeof(HEADER), 1, fp);
    fread(&freep, sizeof(HEADER *), 1, fp);

    sizet old_base_addr;
    fread(&old_base_addr, sizeof(sizet), 1, fp);
    for (HEADER *p = &base; ; p = p->ptr) {
        if (p->ptr == (HEADER *)old_base_addr) {
            p->ptr = &base;
            break;
        }
    }
    if (freep == (HEADER *)old_base_addr) {
        freep = &base;
    }

    /* Cのグローバル変数 */
    // scm.c
    fread(&case_sensitize_symbols, sizeof(int), 1, fp);
    fread(&disk_saved, sizeof(int), 1, fp);
    fread(&dumped, sizeof(int), 1, fp);
    fread(&loc_features, sizeof(SCM *), 1, fp);
    fread(&setitimer_tab[0].sym, sizeof(SCM), 1, fp);
    fread(&setitimer_tab[0].which, sizeof(int), 1, fp);
    fread(&setitimer_tab[1].sym, sizeof(SCM), 1, fp);
    fread(&setitimer_tab[1].which, sizeof(int), 1, fp);
    fread(&setitimer_tab[2].sym, sizeof(SCM), 1, fp);
    fread(&setitimer_tab[2].which, sizeof(int), 1, fp);

    // sys.c
    fread(&ecache_v, sizeof(cell), 1, fp);
    fread(&estk_pool, sizeof(SCM), 1, fp);
    fread(&expmem, sizeof(int), 1, fp);
    fread(&finals_gra, sizeof(scm_gra), 1, fp);
    fread(&freelist, sizeof(SCM), 1, fp);
    fread(&gc_finalizers, sizeof(SCM), 1, fp);
    fread(&gc_finalizers_pending, sizeof(SCM), 1, fp);
    fread(&gc_hook_active, sizeof(int), 1, fp);
    fread(&gc_hook_pending, sizeof(int), 1, fp);
    fread(&heap_cells, sizeof(long), 1, fp);
    fread(&heap_org, sizeof(CELLPTR), 1, fp);
    fread(&hplim_ind, sizeof(sizet), 1, fp);
    fread(&hplims, sizeof(CELLPTR *), 1, fp);
    fread(&loc_gc_hook, sizeof(SCM *), 1, fp);
    fread(&loc_open_file, sizeof(SCM *), 1, fp);
    fread(&loc_try_create_file, sizeof(SCM *), 1, fp);
    fread(&mltrigger, sizeof(long), 1, fp);
    fread(&mtrigger, sizeof(long), 1, fp);
    fread(&no_symhash_gc, sizeof(int), 1, fp);
    fread(&output_deferred, sizeof(int), 1, fp);
    fread(&ptobs_gra, sizeof(scm_gra), 1, fp);
    fread(&scm_ecache, sizeof(CELLPTR), 1, fp);
    fread(&scm_ecache_index, sizeof(long), 1, fp);
    fread(&scm_ecache_len, sizeof(long), 1, fp);
    fread(&scm_egc_root_index, sizeof(long), 1, fp);
    fread(scm_egc_roots, sizeof(SCM), ECACHE_SIZE/20, fp);
    fread(&scm_estk, sizeof(SCM), 1, fp);
    fread(&scm_estk_ptr, sizeof(SCM *), 1, fp);
    fread(&scm_estk_size, sizeof(long), 1, fp);
    fread(&scm_port_table, sizeof(port_info *), 1, fp);
    fread(&scm_port_table_len, sizeof(sizet), 1, fp);
    fread(&smobs_gra, sizeof(scm_gra), 1, fp);
    fread(&subrs_gra, sizeof(scm_gra), 1, fp);
    fread(&symhash, sizeof(SCM), 1, fp);
    fread(&symhash_dim, sizeof(int), 1, fp);
    fread(&tc16_clport, sizeof(long), 1, fp);
    fread(&tc16_safeport, sizeof(int), 1, fp);
    fread(&tc16_sysport, sizeof(int), 1, fp);
    fread(&tmp_errp, sizeof(SCM), 1, fp);
    fread(tmp_errpbuf, sizeof(cell), 3, fp);

    // subr.c
    // nothing

    // repl.c
    fread(upcase, sizeof(unsigned char), CHAR_CODE_LIMIT, fp);
    fread(downcase, sizeof(unsigned char), CHAR_CODE_LIMIT, fp);
    fread(&loc_broken_pipe, sizeof(SCM *), 1, fp);
    fread(&p_read_for_load, sizeof(SCM), 1, fp);
    fread(&p_read, sizeof(SCM), 1, fp);
    fread(&loc_loadsharp, sizeof(SCM *), 1, fp);
    fread(&loc_readsharp, sizeof(SCM *), 1, fp);
    fread(&loc_charsharp, sizeof(SCM *), 1, fp);
    fread(&tc16_arbiter, sizeof(long), 1, fp);
    fread(&ints_disabled, sizeof(int), 1, fp);
    fread(&SIG_deferred, sizeof(unsigned long), 1, fp);
    fread(&scm_verbose, sizeof(int), 1, fp);
    fread(&errjmp_recursive, sizeof(int), 1, fp);
    fread(&errobj_codep, sizeof(int), 1, fp);
    fread(&err_exp, sizeof(SCM), 1, fp);
    fread(&err_env, sizeof(SCM), 1, fp);
    fread(&loc_errobj, sizeof(SCM *), 1, fp);
    fread(&loc_loadpath, sizeof(SCM *), 1, fp);
    fread(&mallocated, sizeof(unsigned long), 1, fp);
    fread(&lmallocated, sizeof(unsigned long), 1, fp);
    fread(&cells_allocated, sizeof(long), 1, fp);
    fread(&lcells_allocated, sizeof(long), 1, fp);
    fread(&rt, sizeof(long), 1, fp);
    fread(&gc_rt, sizeof(long), 1, fp);
    fread(&gc_time_taken, sizeof(long), 1, fp);
    fread(&gc_cells_collected, sizeof(long), 1, fp);
    fread(&gc_malloc_collected, sizeof(long), 1, fp);
    fread(&gc_ports_collected, sizeof(long), 1, fp);
    fread(&gc_syms_collected, sizeof(long), 1, fp);
    fread(&scm_env_work, sizeof(long), 1, fp);
    fread(&scm_gcs, sizeof(long), 1, fp);
    fread(&scm_egcs, sizeof(long), 1, fp);
    fread(&scm_stk_moved, sizeof(long), 1, fp);
    fread(&scm_clo_moved, sizeof(long), 1, fp);
    fread(&scm_egc_rt, sizeof(long), 1, fp);
    fread(&i_repl, sizeof(SCM), 1, fp);
    fread(&i_eval_string, sizeof(SCM), 1, fp);
    fread(&i_load_string, sizeof(SCM), 1, fp);

    // eval.c
    fread(&scm_env, sizeof(SCM), 1, fp);
    fread(&scm_env_tmp, sizeof(SCM), 1, fp);
    fread(&tc16_env, sizeof(long), 1, fp);
#ifndef RECKLESS
    fread(&scm_trace, sizeof(SCM), 1, fp);
    fread(&scm_trace_env, sizeof(SCM), 1, fp);
#endif
    fread(&tc16_macro, sizeof(long), 1, fp);
    fread(&i_dot, sizeof(SCM), 1, fp);
    fread(&i_quote, sizeof(SCM), 1, fp);
    fread(&i_quasiquote, sizeof(SCM), 1, fp);
    fread(&i_unquote, sizeof(SCM), 1, fp);
    fread(&i_uq_splicing, sizeof(SCM), 1, fp);
    fread(&i_lambda, sizeof(SCM), 1, fp);
    fread(&i_define, sizeof(SCM), 1, fp);
    fread(&i_let, sizeof(SCM), 1, fp);
    fread(&i_begin, sizeof(SCM), 1, fp);
    fread(&i_arrow, sizeof(SCM), 1, fp);
    fread(&i_else, sizeof(SCM), 1, fp);
    fread(&i_bind, sizeof(SCM), 1, fp);
    fread(&i_anon, sizeof(SCM), 1, fp);
    fread(&i_side_effect, sizeof(SCM), 1, fp);
    fread(&i_test, sizeof(SCM), 1, fp);
    fread(&i_procedure, sizeof(SCM), 1, fp);
    fread(&i_argument, sizeof(SCM), 1, fp);
    fread(&i_check_defines, sizeof(SCM), 1, fp);
    fread(&f_begin, sizeof(SCM), 1, fp);
    fread(&f_define, sizeof(SCM), 1, fp);
    fread(&loc_atcase_aux, sizeof(SCM *), 1, fp);
    fread(&in_atcase_aux, sizeof(int), 1, fp);
    fread(&tc16_promise, sizeof(long), 1, fp);

    // findexec.c
    // nothing

    // script.c
    // nothing

    // time.c
    //nothing

    // continue.c
#ifdef SHORT_INT
    fread(&thrown_value, sizeof(long), 1, fp);
#endif

    // debug.c
    fread(&tc16_codeptr, sizeof(long), 1, fp);

    // unif.c
    fread(&tc16_array, sizeof(long), 1, fp);

    // rope.c
    fread(&scm_protidx, sizeof(long), 1, fp);

    // definedatatype.c
    fread(&module_flag_symbol, sizeof(SCM), 1, fp);
    fread(&internal_vector_symbol, sizeof(SCM), 1, fp);

    // ega.c
    fread(&is_process_all_ref_paths, sizeof(char), 1, fp);
    fread(&line_num_quantity_of_a_ref_pattern_at_least, sizeof(long), 1, fp);
    fread(&gc_count_of_a_ref_pattern_at_most, sizeof(long), 1, fp);
    fread(&is_print_result, sizeof(char), 1, fp);
    fread(&current_gc_count, sizeof(unsigned long), 1, fp);
    fread(&is_dynamic_check_mode, sizeof(char), 1, fp);
    fread(&gc_traced, sizeof(GcTracedInfo *), 1, fp);
    fread(&focusing_ref_path_list, sizeof(FocusingRefPathList *), 1, fp);
    fread(&collect_info_hash_map, sizeof(CollectedInfoHash **), 1, fp);

    // disksave.c
    // nothing

    // socket.c
#ifdef COMPILED_INITS
    fread(&tc16_sknm, sizeof(long), 1, fp);
#endif

    // scl.c
    fread(sys_protects, sizeof(SCM), NUM_PROTECTS, fp);
    fread(&num_protects, sizeof(sizet), 1, fp);

    fclose(fp);
}

void free(void *ap_arg) {
    HEADER *bp, *p;
    char *ap = ap_arg;

    if (NULL == ap) {
        return;
    }

    bp = (HEADER *) ap - 1;
    for (p = freep; !(bp > p && bp < p->ptr); p = p->ptr) {
        if (p >= p->ptr && (bp > p || bp < p->ptr)) {
            break;
        }
    }

    if (bp + bp->size == p->ptr) {
        bp->size += (p->ptr)->size;
        bp->ptr = (p->ptr)->ptr;
    } else {
        bp->ptr = p->ptr;
    }
    if (p + p->size == bp) {
        p->size += bp->size;
        p->ptr = bp->ptr;
    } else {
        p->ptr = bp;
    }
    freep = p;
}

static HEADER *morecore(unsigned nu) {
    char *cp;
    HEADER *up;

    if (nu < 1024) {
        nu = 1024;
    }

    size_t want_bytes = nu * sizeof(HEADER);
    if (prepared_memory_start + PREPARED_MEMORY_BYTES - managed_memory_end < want_bytes) {
        return (NULL);
    }

    cp = managed_memory_end;
    managed_memory_end += want_bytes;

    up = (HEADER *) cp;
    up->size = nu;
    free((void *) (up + 1));
    return (freep);
}

void *malloc(size_t nbytes) {
    HEADER *p, *prevp;
    unsigned nunits;

    if (0 == nbytes) {
        return (NULL);
    }

    nunits = (nbytes + sizeof(HEADER) - 1) / sizeof(HEADER) + 1;
    if ((prevp = freep) == NULL) {
        base.ptr = freep = prevp = &base;
        base.size = 0;
        prevp = freep;
    }

    for (p = prevp->ptr; ; prevp = p, p = p->ptr) {
        if (p->size >= nunits) {
            if (p->size == nunits)
                prevp->ptr = p->ptr;
            else {
                p->size -= nunits;
                p += p->size;
                p->size = nunits;
            }
            freep = prevp;
            return ((char *)(p + 1));
        }
        if (p == freep) {
            if ((p = morecore(nunits)) == NULL) {
                return (NULL);
            }
        }
    }
}

void *realloc(void *oldptr_arg, size_t size) {
    HEADER *bp;
    unsigned nunits;
    char *p;
    int i;
    char *old_ptr = oldptr_arg;

    if (NULL == old_ptr) {
        return (malloc(size));
    }

    bp = (HEADER *) old_ptr - 1;
    nunits = (size + sizeof(HEADER) - 1) / sizeof(HEADER) + 1;
    if (bp->size == nunits) {
        return (old_ptr);
    } else if (bp->size > nunits) {
        (bp + nunits)->size = bp->size - nunits;
        bp->size = nunits;
        free((char *) (bp + nunits + 1));
        return ((char *) (bp + 1));
    } else {
        p = malloc(size);
        for (i = 0; i < (bp->size - 1) * sizeof(HEADER); i++) {
            p[i] = old_ptr[i];
        }
        return (p);
    }
}

void *calloc(size_t nelem, size_t elsize) {
    char *ptr;
    int i;
    ptr = malloc(i = nelem * elsize);
    while (--i >= 0) {
        ptr[i] = 0;
    }
    return (ptr);
}
