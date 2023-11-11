// "ega.c" code for Enhanced GC assertions.
//
// Created by Guang Yang on 2023/04/22.
//

#include "scm.h"

static char s_gc_log_filename[] = "gc_cost_time.log";
static char s_exec_log_filename[] = "exec_cost_time.log";

size_t current_gc_count = 0;
char is_show_gc_related_info = 0;
char is_gc_cost_time_recording = 0;
size_t gc_idx_gc_cost_recording_start_at = 0;
clock_t current_gc_start_time = 0;
clock_t gc_cost_time_sum = 0;
char is_exec_cost_time_recoding = 0;
clock_t exec_recoding_start_time = 0;
clock_t exec_recoding_tmp_gc_start_time = 0;
clock_t exec_recoding_gc_cost_time_sum = 0;
size_t gc_idx_exec_recoding_start_at = 0;

static void plus_current_gc_count() {
    current_gc_count += 1L;
}

void ega_process_at_gc_start() {
    plus_current_gc_count();

    if (is_show_gc_related_info) {
        fprintf(stdout, "\n[GCRelatedInfo] No.%ld GC Start.\n", current_gc_count);
    }

    if (is_gc_cost_time_recording) {
        if (gc_idx_gc_cost_recording_start_at <= 0) {
            gc_idx_gc_cost_recording_start_at = current_gc_count;
        }
        current_gc_start_time = clock();
    }

    if (is_exec_cost_time_recoding) {
        exec_recoding_tmp_gc_start_time = clock();
    }
}

static char s_ega_process_after_gc[] = "ega_process_after_gc";
void ega_process_after_gc() {
    if (is_exec_cost_time_recoding) {
        clock_t curr_clock = clock();
        exec_recoding_gc_cost_time_sum += curr_clock - exec_recoding_tmp_gc_start_time;
    }

    if (is_gc_cost_time_recording) {
        clock_t curr_clock = clock();
        clock_t past_clock = curr_clock - current_gc_start_time; // cpu ticks
        gc_cost_time_sum += past_clock;

        // record info in file
        FILE *fp;
        if ((fp = fopen(s_gc_log_filename, "a+")) == NULL) {
            wta(UNDEFINED, "Can not open or create gc.log", s_ega_process_after_gc);
        }
        fprintf(fp,
                "[GCCostTimeInfo] No.%ld GC end, cost time: %ld ms. Accumulated GC cost %ld ticks.\n",
                current_gc_count,
                past_clock,
                gc_cost_time_sum);
        fclose(fp);
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

static char s_start_record_gc_cost_time[] = "start-record-gc-cost-time";
static SCM start_record_gc_cost_time() {
    is_gc_cost_time_recording = 1;
    gc_idx_gc_cost_recording_start_at = 0;
    current_gc_start_time = 0;
    gc_cost_time_sum = 0;

    time_t now;
    time(&now);
    FILE *fp;
    if ((fp = fopen(s_gc_log_filename, "a+")) == NULL) {
        wta(UNDEFINED, "Can not open or create exec.log", s_start_record_gc_cost_time);
    }
    fprintf(fp, "\nNEW RECORD AT %ld (TIMESTAMP): \n", now);
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

static char s_start_record_exec_cost_time[] = "start-record-exec-cost-time";
static SCM start_record_exec_cost_time() {
    is_exec_cost_time_recoding = 1;
    exec_recoding_start_time = clock();
    exec_recoding_tmp_gc_start_time = 0;
    exec_recoding_gc_cost_time_sum = 0;
    gc_idx_exec_recoding_start_at = current_gc_count;
    return UNSPECIFIED;
}

static char s_end_record_exec_cost_time[] = "end-record-exec-cost-time";
static SCM end_record_exec_cost_time() {
    clock_t curr_time = clock();
    time_t now;
    time(&now);

    clock_t total_time = curr_time - exec_recoding_start_time;

    // record info in file
    FILE *fp;
    if ((fp = fopen(s_exec_log_filename, "a+")) == NULL) {
        wta(UNDEFINED, "Can not open or create exec.log", s_end_record_exec_cost_time);
    }
    fprintf(fp,
            "\nNEW RECORD AT %ld (TIMESTAMP): \n"
            "[ExecCostTimeInfo] Total cost time: %ld ticks. "
            "Total exec cost %ld ticks. "
            "Total GC cost %ld ticks. "
            "Number of GC times is %ld.\n",
            now,
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
        {s_random_0_n, random_0_n},
        {s_thread_sleep, thread_sleep},
        {0, 0}
};

static iproc subr2s[] = {
        {s_int_div, int_div},
        {0, 0}
};

void init_ega() {
    init_iprocs(subr0s, tc7_subr_0);
    init_iprocs(subr1s, tc7_subr_1);
    init_iprocs(subr2s, tc7_subr_2);
    add_feature("ega");
}
