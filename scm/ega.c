// "ega.c" code for Enhanced GC assertions.
//
// Created by Guang Yang on 2023/04/22.
//

#include "scm.h"

static char s_gc_log_filename[] = "gc_cost_time.log";
static char s_exec_log_filename[] = "exec_cost_time.log";

char is_print_result = 1;
size_t current_gc_count = 0;
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

void try_gather_new_ref_path(SCM ptr, long last_gc_traced_index) {
    if (!is_user_defined_data_type_instance(ptr)) {
        return;
    }

    if (!is_assert_dead_marked(ptr)) {
        return;
    }

    if (!is_print_result) {
        return;
    }

    fprintf(stderr, "\nWarning: an object that was asserted dead is reachable.\n"
                    "Type: %s;\n"
                    "Path to object: ",
            type_str(ptr));

    for (long i = 0; i <= last_gc_traced_index; ++i) {
        fprintf(stderr, "%s", type_str(gc_traced[i].ptr));

        if (is_user_defined_data_type_instance(gc_traced[i].ptr)) {
            SCM ln_vector_of_field = get_ln_vector_of_field(gc_traced[i].ptr, gc_traced[i].ref_field_index - 1);
            if (i != last_gc_traced_index && NNULLP(ln_vector_of_field) && INUM(VELTS(ln_vector_of_field)[1]) > 2) {
                fprintf(stderr, "@ln");
                long used_len = INUM(VELTS(ln_vector_of_field)[1]);
                for (long idx = 2; idx < used_len; ++idx) {
                    fprintf(stderr, "%ld", INUM(VELTS(ln_vector_of_field)[idx]));
                    if (idx != used_len - 1) {
                        fprintf(stderr, ", ");
                    }
                }
            }
        }

        if (i != last_gc_traced_index) {
            fprintf(stderr, " -> ");
        } else {
            fprintf(stderr, ";\n");
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

static void sum_gc_time_for_exec_time_recoding();
static void calc_this_gc_cost_time();

void ega_process_after_gc() {
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

static void init_gc_traced() {
    gc_traced = (GcTracedInfo *) malloc(heap_cells * sizeof(GcTracedInfo));
    if (NULL == gc_traced) {
        fprintf(stderr, "[gc_traced]内存分配失败\n");
        exit(1);
    }
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
    init_iprocs(subr0s, tc7_subr_0);
    init_iprocs(subr1s, tc7_subr_1);
    init_iprocs(subr2s, tc7_subr_2);
    add_feature("ega");
}
