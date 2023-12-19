// "definedatatype.c" code for user definable class-struct.
//
// Created by Guang Yang on 2023/03/29.
//

#include "scm.h"

char s_user_define_datatype[] = "UserDefinedDataType";
char s_internal_vector[] = "InternalVector";

/**
 *
 * 0: DataTypeDef <= [module-flag-sym, internal-code, type-name, [internal-vector-sym, field-name1, ...], null]
 * 1:
 *
 * 2: DataTypeInstance <= [module-flag-sym, internal-code, data-type-def, [internal-vector-sym, field-value1, ...],
 *                         [internal-vector-sym, field1-lns-vector, ...]]]
 * 3:
 */

#define MFS(x) (VELTS(x)[0]) // module-flag-sym
#define IVS(x) (VELTS(x)[0]) // internal-vector-sym

/**
 * internal-code (7bits)
 * low order 3 bits: internal-type
 * low order 3rd bit: assert-dead mark
 * low order 4th bit: ref-path info recorded mark
 * 5th 6th ...
 */
#define IC(x) (VELTS(x)[1])

/**
 * SET_IC_BIT: set the bit of index i to be "1", in internal code in a user defined type obj
 * CLEAR_IC_BIT: set the bit of index i to be "0", in internal code in a user defined type obj
 * GET_IC_BIT: get the bit of index in internal code in a user defined type obj
 */
#define SET_IC_BIT(x, i) (IC(x) = MAKINUM(INUM(IC(x)) | (1 << i)))
#define CLEAR_IC_BIT(x, i) (IC(x) = MAKINUM(INUM(IC(x)) & ~(1 << i)))
#define GET_IC_BIT_INUM(x, i) (((INUM(IC(x)) & (1 << i))) >> i)

#define READ_IT(x) (MAKINUM(7 & INUM(IC(x)))) // get internal-type code (readonly)
#define READ_ASSERT_DEAD_MARK(x) (GET_IC_BIT_INUM(x, 3)) // get the inum of assert-dead-mark (readonly)
#define READ_REF_PATH_INFO_RECORDED_MARK(x) (GET_IC_BIT_INUM(x, 4)) // get the inum of ref-path-info-recorded-mark (readonly)

#define DTD_TN(x) (VELTS(x)[2]) // type-name
#define DTD_FNV(x) (VELTS(x)[3]) // vector of field-names vector
#define DTD_RSD(x) (VELTS(x)[4]) // vector of rec-slot-defs

#define DTI_DTD(x) (VELTS(x)[2]) // data-type-def
#define DTI_FVV(x) (VELTS(x)[3]) // vector of field-values
#define DTI_RSV(x) (VELTS(x)[4]) // vector of line number recorded slots

#define DTD_VECTOR_LEN (MAKINUM(5)) // length of DataTypeDef and DataTypeDefWithRecSlots vector
#define DTI_VECTOR_LEN (MAKINUM(5)) // length of DataTypeInstance and DataTypeInstanceWithRecSlots vector

#define DTD_IT_CODE (MAKINUM(0)) // internal type code of DataTypeDef
#define DTDWRS_IT_CODE (MAKINUM(1)) // internal type code of DataTypeDefWithRecSlots
#define DTI_IT_CODE (MAKINUM(2)) // internal type code of DataTypeInstance
#define DTIWRS_IT_CODE (MAKINUM(3)) // internal type code of DataTypeInstanceWithRecSlots

SCM module_flag_symbol;
SCM internal_vector_symbol;

static char s_c_define_data_type[] = "c-define-data-type";
SCM c_define_data_type(SCM type_name, SCM field_names) {
    long field_names_len = ilength(field_names);

    if (field_names_len < 0) {
        errout: wta(field_names, (char *)ARG2, s_c_define_data_type);
    }

    for (SCM n = field_names; NIMP(n); n = CDR(n)) {
        if (!SYMBOLP(CAR(n))) {
            goto errout;
        }
    }

    // store it as vector in data type definition object
    SCM field_names_vector = vector(cons(internal_vector_symbol, field_names));

    // create a new DataTypeDef
    SCM data_type_def = make_vector(DTD_VECTOR_LEN, EOL);
    MFS(data_type_def) = module_flag_symbol;
    IC(data_type_def) = DTD_IT_CODE; // for initialization
    DTD_TN(data_type_def) = type_name;
    DTD_FNV(data_type_def) = field_names_vector;
    return data_type_def;
}

static char s_c_make_instance[] = "c-make-instance";
SCM c_make_instance(SCM data_type_def, SCM field_values_vector) {
    SCM data_type_instance = make_vector(DTI_VECTOR_LEN, EOL);

    MFS(data_type_instance) = module_flag_symbol;
    IC(data_type_instance) = DTI_IT_CODE; // for initialization
    DTI_DTD(data_type_instance) = data_type_def;

    // field-value vectorの初期化
    long len = INUM(vector_length(DTD_FNV(data_type_def))) - 1L;
    SCM fvv = make_vector(MAKINUM(1L + len), EOL);
    VELTS(fvv)[0] = internal_vector_symbol;
    if (BOOL_T == vectorp(field_values_vector) && INUM(vector_length(field_values_vector)) == len) {
        for (long i = 1L; i < len + 1; ++i) {
            vector_set(fvv, MAKINUM(i), vector_ref(field_values_vector, MAKINUM(i - 1)));
        }
    }
    DTI_FVV(data_type_instance) = fvv;
    DTI_RSV(data_type_instance) = EOL; // SCMのNULLである

    // 行番号記録スロットvectorの初期化
    SCM rsv = make_vector(MAKINUM(1L + len), EOL); // rec_slots_vector
    vector_set(rsv, MAKINUM(0), internal_vector_symbol);
    DTI_RSV(data_type_instance) = rsv;

    return data_type_instance;
}

// a scm obj is the user defined data type's obj or not?
char is_user_defined_data_type(SCM scm_obj) {
    if (BOOL_F == vectorp(scm_obj)) {
        return 0;
    }
    if (INUM(vector_length(scm_obj)) < 1) {
        return 0;
    }
    return BOOL_T == eq(MFS(scm_obj), module_flag_symbol);
}

// a scm obj is the user defined data type's instance or not?
char is_user_defined_data_type_instance(SCM scm_obj) {
    if (!is_user_defined_data_type(scm_obj)) {
        return 0;
    }
    switch (READ_IT(scm_obj)) {
        case DTI_IT_CODE:
        case DTIWRS_IT_CODE:
            return 1;
        default:
            return 0;
    }
}

void set_assert_mark(SCM scm_obj) {
    if (!is_user_defined_data_type_instance(scm_obj)) {
        return;
    }
    SET_IC_BIT(scm_obj, 3);
}

static char s_is_assert_dead_marked[] = "is_assert_dead_marked";
char is_assert_dead_marked(SCM scm_obj) {
    if (!is_user_defined_data_type_instance(scm_obj)) {
        wta(scm_obj, (char *)ARG1, s_is_assert_dead_marked);
    }
    return READ_ASSERT_DEAD_MARK(scm_obj);
}

static char s_instance_type_name[] = "instance_type_name";
char *instance_type_name(SCM ptr) {
    if (!is_user_defined_data_type_instance(ptr)) {
        wta(ptr, (char *)ARG1, s_instance_type_name);
    }
    return CHARS(DTD_TN(DTI_DTD(ptr)));
}

char is_internal_vector(SCM ptr) {
    if (BOOL_F == vectorp(ptr)) {
        return 0;
    }
    if (INUM(vector_length(ptr)) < 1) {
        return 0;
    }
    return BOOL_T == eq(IVS(ptr), internal_vector_symbol);
}

/**
 *
 * @param scm_obj
 * @param field_index
 * @return maybe return EOL
 */
SCM get_ln_vector_of_field(SCM scm_obj, long field_index) {
    if (field_index < 0) {
        return EOL;
    }
    SCM rec_slots_vector = DTI_RSV(scm_obj);
    return (VELTS(rec_slots_vector)[field_index + 1L]);
}

static char s_c_data_type_predicate[] = "c-data-type-predicate";
SCM c_data_type_predicate(SCM data_type_def, SCM obj) {
    if (!is_user_defined_data_type(obj)) {
        return BOOL_F;
    }
    return eq(data_type_def, DTI_DTD(obj));
}

static char s_c_data_type_accessor[] = "c-data-type-accessor";
SCM c_data_type_accessor(SCM obj, SCM index) {
    return vector_ref(DTI_FVV(obj), index);
}

SCM c_data_type_modifier(SCM obj, long index, SCM value) {
    vector_set(DTI_FVV(obj), MAKINUM(index + 1L), value);
    return UNSPECIFIED;
}

SCM c_data_type_modifier_with_wb(SCM obj, long index, SCM value, SCM ln_num) {
    c_data_type_modifier(obj, index, value);

    if (INUM(ln_num) <= 0) {
        return UNSPECIFIED;
    }

    // try process write barrier
    SCM rec_slots_vector = DTI_RSV(obj);
    SCM ln_vector_of_field = VELTS(rec_slots_vector)[index + 1L];
    if (vectorp(ln_vector_of_field) == BOOL_F) {
        ln_vector_of_field = make_vector(MAKINUM(FIELD_REF_INFO_ALLOCATED_LEN + 2L), EOL);
        VELTS(ln_vector_of_field)[0] = internal_vector_symbol;
        VELTS(ln_vector_of_field)[1] = MAKINUM(2); // used_len
        VELTS(rec_slots_vector)[index + 1L] = ln_vector_of_field;
    }

    long used_len = INUM(VELTS(ln_vector_of_field)[1]);

    for (long i = 2; i < used_len; ++i) {
        if (INUM(VELTS(ln_vector_of_field)[i]) == INUM(ln_num)) {
            return UNSPECIFIED;
        }
    }

    // 拡張してみる
    if (used_len >= INUM(vector_length(ln_vector_of_field))) {
        long new_len = used_len + FIELD_REF_INFO_ALLOCATED_LEN; // TODO [dirty] 每次多扩张6个就空位
        SCM new_ln_vector_of_field = make_vector(MAKINUM(new_len), EOL);
        for (long i = 0; i < used_len; ++i) {
            VELTS(new_ln_vector_of_field)[i] = VELTS(ln_vector_of_field)[i];
        }
        VELTS(rec_slots_vector)[index + 1L] = new_ln_vector_of_field;
        ln_vector_of_field = new_ln_vector_of_field;
    }

    // 本当に行番号を記録
    VELTS(ln_vector_of_field)[used_len] = ln_num;
    VELTS(ln_vector_of_field)[1] = MAKINUM(used_len + 1L);
    return UNSPECIFIED;
}

static char s_set_linked_list_node_entry[] = "set-linked-list-node-entry!";
static SCM set_linked_list_node_entry(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_linked_list_node_entry_with_wb[] = "set-linked-list-node-entry!-with-wb";
static SCM set_linked_list_node_entry_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_linked_list_node_next_node[] = "set-linked-list-node-next-node!";
static SCM set_linked_list_node_next_node(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_linked_list_node_next_node_with_wb[] = "set-linked-list-node-next-node!-with-wb";
static SCM set_linked_list_node_next_node_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_linked_list_size[] = "set-linked-list-size!";
static SCM set_linked_list_size(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_linked_list_size_with_wb[] = "set-linked-list-size!-with-wb";
static SCM set_linked_list_size_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_linked_list_pred_proc_sym[] = "set-linked-list-pred-proc-sym!";
static SCM set_linked_list_pred_proc_sym(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_linked_list_pred_proc_sym_with_wb[] = "set-linked-list-pred-proc-sym!-with-wb";
static SCM set_linked_list_pred_proc_sym_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_linked_list_first_node[] = "set-linked-list-first-node!";
static SCM set_linked_list_first_node(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_linked_list_first_node_with_wb[] = "set-linked-list-first-node!-with-wb";
static SCM set_linked_list_first_node_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_linked_list_last_node[] = "set-linked-list-last-node!";
static SCM set_linked_list_last_node(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_linked_list_last_node_with_wb[] = "set-linked-list-last-node!-with-wb";
static SCM set_linked_list_last_node_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_array_list_data_block_data[] = "set-array-list-data-block-data!";
static SCM set_array_list_data_block_data(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_array_list_data_block_data_with_wb[] = "set-array-list-data-block-data!-with-wb";
static SCM set_array_list_data_block_data_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_array_list_size[] = "set-array-list-size!";
static SCM set_array_list_size(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_array_list_size_with_wb[] = "set-array-list-size!-with-wb";
static SCM set_array_list_size_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_array_list_pred_proc_sym[] = "set-array-list-pred-proc-sym!";
static SCM set_array_list_pred_proc_sym(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_array_list_pred_proc_sym_with_wb[] = "set-array-list-pred-proc-sym!-with-wb";
static SCM set_array_list_pred_proc_sym_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_array_list_data_blocks[] = "set-array-list-data-blocks!";
static SCM set_array_list_data_blocks(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_array_list_data_blocks_with_wb[] = "set-array-list-data-blocks!-with-wb";
static SCM set_array_list_data_blocks_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_hash_entry_k[] = "set-hash-entry-k!";
static SCM set_hash_entry_k(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_hash_entry_k_with_wb[] = "set-hash-entry-k!-with-wb";
static SCM set_hash_entry_k_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_hash_entry_v[] = "set-hash-entry-v!";
static SCM set_hash_entry_v(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_hash_entry_v_with_wb[] = "set-hash-entry-v!-with-wb";
static SCM set_hash_entry_v_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_hash_internal_size[] = "set-hash-internal-size!";
static SCM set_hash_internal_size(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_hash_internal_size_with_wb[] = "set-hash-internal-size!-with-wb";
static SCM set_hash_internal_size_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_hash_internal_key_pred_proc_sym[] = "set-hash-internal-key-pred-proc-sym!";
static SCM set_hash_internal_key_pred_proc_sym(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_hash_internal_key_pred_proc_sym_with_wb[] = "set-hash-internal-key-pred-proc-sym!-with-wb";
static SCM set_hash_internal_key_pred_proc_sym_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_hash_internal_value_pred_proc_sym[] = "set-hash-internal-value-pred-proc-sym!";
static SCM set_hash_internal_value_pred_proc_sym(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_hash_internal_value_pred_proc_sym_with_wb[] = "set-hash-internal-value-pred-proc-sym!-with-wb";
static SCM set_hash_internal_value_pred_proc_sym_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_hash_internal_used_bucket_size[] = "set-hash-internal-used-bucket-size!";
static SCM set_hash_internal_used_bucket_size(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_hash_internal_used_bucket_size_with_wb[] = "set-hash-internal-used-bucket-size!-with-wb";
static SCM set_hash_internal_used_bucket_size_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_hash_internal_buckets[] = "set-hash-internal-buckets!";
static SCM set_hash_internal_buckets(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 4, value, call_site);
}

static char s_set_hash_internal_buckets_with_wb[] = "set-hash-internal-buckets!-with-wb";
static SCM set_hash_internal_buckets_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 4, value, call_site);
}

static char s_set_hash_map_hash_internal[] = "set-hash-map-hash-internal!";
static SCM set_hash_map_hash_internal(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_hash_map_hash_internal_with_wb[] = "set-hash-map-hash-internal!-with-wb";
static SCM set_hash_map_hash_internal_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Node_left[] = "set-Node-left!";
static SCM set_Node_left(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Node_left_with_wb[] = "set-Node-left!-with-wb";
static SCM set_Node_left_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Node_right[] = "set-Node-right!";
static SCM set_Node_right(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Node_right_with_wb[] = "set-Node-right!-with-wb";
static SCM set_Node_right_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Node_i[] = "set-Node-i!";
static SCM set_Node_i(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_Node_i_with_wb[] = "set-Node-i!-with-wb";
static SCM set_Node_i_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_Node_j[] = "set-Node-j!";
static SCM set_Node_j(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_Node_j_with_wb[] = "set-Node-j!-with-wb";
static SCM set_Node_j_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_A0_f1[] = "set-A0-f1!";
static SCM set_A0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A0_f1_with_wb[] = "set-A0-f1!-with-wb";
static SCM set_A0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A0_f2[] = "set-A0-f2!";
static SCM set_A0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A0_f2_with_wb[] = "set-A0-f2!-with-wb";
static SCM set_A0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A1_f1[] = "set-A1-f1!";
static SCM set_A1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A1_f1_with_wb[] = "set-A1-f1!-with-wb";
static SCM set_A1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A1_f2[] = "set-A1-f2!";
static SCM set_A1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A1_f2_with_wb[] = "set-A1-f2!-with-wb";
static SCM set_A1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A2_f1[] = "set-A2-f1!";
static SCM set_A2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A2_f1_with_wb[] = "set-A2-f1!-with-wb";
static SCM set_A2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A2_f2[] = "set-A2-f2!";
static SCM set_A2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A2_f2_with_wb[] = "set-A2-f2!-with-wb";
static SCM set_A2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A3_f1[] = "set-A3-f1!";
static SCM set_A3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A3_f1_with_wb[] = "set-A3-f1!-with-wb";
static SCM set_A3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A3_f2[] = "set-A3-f2!";
static SCM set_A3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A3_f2_with_wb[] = "set-A3-f2!-with-wb";
static SCM set_A3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A4_f1[] = "set-A4-f1!";
static SCM set_A4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A4_f1_with_wb[] = "set-A4-f1!-with-wb";
static SCM set_A4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A4_f2[] = "set-A4-f2!";
static SCM set_A4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A4_f2_with_wb[] = "set-A4-f2!-with-wb";
static SCM set_A4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A5_f1[] = "set-A5-f1!";
static SCM set_A5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A5_f1_with_wb[] = "set-A5-f1!-with-wb";
static SCM set_A5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A5_f2[] = "set-A5-f2!";
static SCM set_A5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A5_f2_with_wb[] = "set-A5-f2!-with-wb";
static SCM set_A5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A6_f1[] = "set-A6-f1!";
static SCM set_A6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A6_f1_with_wb[] = "set-A6-f1!-with-wb";
static SCM set_A6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A6_f2[] = "set-A6-f2!";
static SCM set_A6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A6_f2_with_wb[] = "set-A6-f2!-with-wb";
static SCM set_A6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A7_f1[] = "set-A7-f1!";
static SCM set_A7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A7_f1_with_wb[] = "set-A7-f1!-with-wb";
static SCM set_A7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A7_f2[] = "set-A7-f2!";
static SCM set_A7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A7_f2_with_wb[] = "set-A7-f2!-with-wb";
static SCM set_A7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A8_f1[] = "set-A8-f1!";
static SCM set_A8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A8_f1_with_wb[] = "set-A8-f1!-with-wb";
static SCM set_A8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A8_f2[] = "set-A8-f2!";
static SCM set_A8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A8_f2_with_wb[] = "set-A8-f2!-with-wb";
static SCM set_A8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A9_f1[] = "set-A9-f1!";
static SCM set_A9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A9_f1_with_wb[] = "set-A9-f1!-with-wb";
static SCM set_A9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A9_f2[] = "set-A9-f2!";
static SCM set_A9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A9_f2_with_wb[] = "set-A9-f2!-with-wb";
static SCM set_A9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B0_f1[] = "set-B0-f1!";
static SCM set_B0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B0_f1_with_wb[] = "set-B0-f1!-with-wb";
static SCM set_B0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B0_f2[] = "set-B0-f2!";
static SCM set_B0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B0_f2_with_wb[] = "set-B0-f2!-with-wb";
static SCM set_B0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B1_f1[] = "set-B1-f1!";
static SCM set_B1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B1_f1_with_wb[] = "set-B1-f1!-with-wb";
static SCM set_B1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B1_f2[] = "set-B1-f2!";
static SCM set_B1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B1_f2_with_wb[] = "set-B1-f2!-with-wb";
static SCM set_B1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B2_f1[] = "set-B2-f1!";
static SCM set_B2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B2_f1_with_wb[] = "set-B2-f1!-with-wb";
static SCM set_B2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B2_f2[] = "set-B2-f2!";
static SCM set_B2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B2_f2_with_wb[] = "set-B2-f2!-with-wb";
static SCM set_B2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B3_f1[] = "set-B3-f1!";
static SCM set_B3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B3_f1_with_wb[] = "set-B3-f1!-with-wb";
static SCM set_B3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B3_f2[] = "set-B3-f2!";
static SCM set_B3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B3_f2_with_wb[] = "set-B3-f2!-with-wb";
static SCM set_B3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B4_f1[] = "set-B4-f1!";
static SCM set_B4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B4_f1_with_wb[] = "set-B4-f1!-with-wb";
static SCM set_B4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B4_f2[] = "set-B4-f2!";
static SCM set_B4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B4_f2_with_wb[] = "set-B4-f2!-with-wb";
static SCM set_B4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B5_f1[] = "set-B5-f1!";
static SCM set_B5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B5_f1_with_wb[] = "set-B5-f1!-with-wb";
static SCM set_B5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B5_f2[] = "set-B5-f2!";
static SCM set_B5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B5_f2_with_wb[] = "set-B5-f2!-with-wb";
static SCM set_B5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B6_f1[] = "set-B6-f1!";
static SCM set_B6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B6_f1_with_wb[] = "set-B6-f1!-with-wb";
static SCM set_B6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B6_f2[] = "set-B6-f2!";
static SCM set_B6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B6_f2_with_wb[] = "set-B6-f2!-with-wb";
static SCM set_B6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B7_f1[] = "set-B7-f1!";
static SCM set_B7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B7_f1_with_wb[] = "set-B7-f1!-with-wb";
static SCM set_B7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B7_f2[] = "set-B7-f2!";
static SCM set_B7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B7_f2_with_wb[] = "set-B7-f2!-with-wb";
static SCM set_B7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B8_f1[] = "set-B8-f1!";
static SCM set_B8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B8_f1_with_wb[] = "set-B8-f1!-with-wb";
static SCM set_B8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B8_f2[] = "set-B8-f2!";
static SCM set_B8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B8_f2_with_wb[] = "set-B8-f2!-with-wb";
static SCM set_B8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B9_f1[] = "set-B9-f1!";
static SCM set_B9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B9_f1_with_wb[] = "set-B9-f1!-with-wb";
static SCM set_B9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B9_f2[] = "set-B9-f2!";
static SCM set_B9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B9_f2_with_wb[] = "set-B9-f2!-with-wb";
static SCM set_B9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C0_f1[] = "set-C0-f1!";
static SCM set_C0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C0_f1_with_wb[] = "set-C0-f1!-with-wb";
static SCM set_C0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C0_f2[] = "set-C0-f2!";
static SCM set_C0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C0_f2_with_wb[] = "set-C0-f2!-with-wb";
static SCM set_C0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C1_f1[] = "set-C1-f1!";
static SCM set_C1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C1_f1_with_wb[] = "set-C1-f1!-with-wb";
static SCM set_C1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C1_f2[] = "set-C1-f2!";
static SCM set_C1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C1_f2_with_wb[] = "set-C1-f2!-with-wb";
static SCM set_C1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C2_f1[] = "set-C2-f1!";
static SCM set_C2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C2_f1_with_wb[] = "set-C2-f1!-with-wb";
static SCM set_C2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C2_f2[] = "set-C2-f2!";
static SCM set_C2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C2_f2_with_wb[] = "set-C2-f2!-with-wb";
static SCM set_C2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C3_f1[] = "set-C3-f1!";
static SCM set_C3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C3_f1_with_wb[] = "set-C3-f1!-with-wb";
static SCM set_C3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C3_f2[] = "set-C3-f2!";
static SCM set_C3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C3_f2_with_wb[] = "set-C3-f2!-with-wb";
static SCM set_C3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C4_f1[] = "set-C4-f1!";
static SCM set_C4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C4_f1_with_wb[] = "set-C4-f1!-with-wb";
static SCM set_C4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C4_f2[] = "set-C4-f2!";
static SCM set_C4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C4_f2_with_wb[] = "set-C4-f2!-with-wb";
static SCM set_C4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C5_f1[] = "set-C5-f1!";
static SCM set_C5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C5_f1_with_wb[] = "set-C5-f1!-with-wb";
static SCM set_C5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C5_f2[] = "set-C5-f2!";
static SCM set_C5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C5_f2_with_wb[] = "set-C5-f2!-with-wb";
static SCM set_C5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C6_f1[] = "set-C6-f1!";
static SCM set_C6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C6_f1_with_wb[] = "set-C6-f1!-with-wb";
static SCM set_C6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C6_f2[] = "set-C6-f2!";
static SCM set_C6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C6_f2_with_wb[] = "set-C6-f2!-with-wb";
static SCM set_C6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C7_f1[] = "set-C7-f1!";
static SCM set_C7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C7_f1_with_wb[] = "set-C7-f1!-with-wb";
static SCM set_C7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C7_f2[] = "set-C7-f2!";
static SCM set_C7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C7_f2_with_wb[] = "set-C7-f2!-with-wb";
static SCM set_C7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C8_f1[] = "set-C8-f1!";
static SCM set_C8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C8_f1_with_wb[] = "set-C8-f1!-with-wb";
static SCM set_C8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C8_f2[] = "set-C8-f2!";
static SCM set_C8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C8_f2_with_wb[] = "set-C8-f2!-with-wb";
static SCM set_C8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C9_f1[] = "set-C9-f1!";
static SCM set_C9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C9_f1_with_wb[] = "set-C9-f1!-with-wb";
static SCM set_C9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C9_f2[] = "set-C9-f2!";
static SCM set_C9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C9_f2_with_wb[] = "set-C9-f2!-with-wb";
static SCM set_C9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D0_f1[] = "set-D0-f1!";
static SCM set_D0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D0_f1_with_wb[] = "set-D0-f1!-with-wb";
static SCM set_D0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D0_f2[] = "set-D0-f2!";
static SCM set_D0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D0_f2_with_wb[] = "set-D0-f2!-with-wb";
static SCM set_D0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D1_f1[] = "set-D1-f1!";
static SCM set_D1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D1_f1_with_wb[] = "set-D1-f1!-with-wb";
static SCM set_D1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D1_f2[] = "set-D1-f2!";
static SCM set_D1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D1_f2_with_wb[] = "set-D1-f2!-with-wb";
static SCM set_D1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D2_f1[] = "set-D2-f1!";
static SCM set_D2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D2_f1_with_wb[] = "set-D2-f1!-with-wb";
static SCM set_D2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D2_f2[] = "set-D2-f2!";
static SCM set_D2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D2_f2_with_wb[] = "set-D2-f2!-with-wb";
static SCM set_D2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D3_f1[] = "set-D3-f1!";
static SCM set_D3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D3_f1_with_wb[] = "set-D3-f1!-with-wb";
static SCM set_D3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D3_f2[] = "set-D3-f2!";
static SCM set_D3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D3_f2_with_wb[] = "set-D3-f2!-with-wb";
static SCM set_D3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D4_f1[] = "set-D4-f1!";
static SCM set_D4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D4_f1_with_wb[] = "set-D4-f1!-with-wb";
static SCM set_D4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D4_f2[] = "set-D4-f2!";
static SCM set_D4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D4_f2_with_wb[] = "set-D4-f2!-with-wb";
static SCM set_D4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D5_f1[] = "set-D5-f1!";
static SCM set_D5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D5_f1_with_wb[] = "set-D5-f1!-with-wb";
static SCM set_D5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D5_f2[] = "set-D5-f2!";
static SCM set_D5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D5_f2_with_wb[] = "set-D5-f2!-with-wb";
static SCM set_D5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D6_f1[] = "set-D6-f1!";
static SCM set_D6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D6_f1_with_wb[] = "set-D6-f1!-with-wb";
static SCM set_D6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D6_f2[] = "set-D6-f2!";
static SCM set_D6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D6_f2_with_wb[] = "set-D6-f2!-with-wb";
static SCM set_D6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D7_f1[] = "set-D7-f1!";
static SCM set_D7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D7_f1_with_wb[] = "set-D7-f1!-with-wb";
static SCM set_D7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D7_f2[] = "set-D7-f2!";
static SCM set_D7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D7_f2_with_wb[] = "set-D7-f2!-with-wb";
static SCM set_D7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D8_f1[] = "set-D8-f1!";
static SCM set_D8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D8_f1_with_wb[] = "set-D8-f1!-with-wb";
static SCM set_D8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D8_f2[] = "set-D8-f2!";
static SCM set_D8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D8_f2_with_wb[] = "set-D8-f2!-with-wb";
static SCM set_D8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D9_f1[] = "set-D9-f1!";
static SCM set_D9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D9_f1_with_wb[] = "set-D9-f1!-with-wb";
static SCM set_D9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D9_f2[] = "set-D9-f2!";
static SCM set_D9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D9_f2_with_wb[] = "set-D9-f2!-with-wb";
static SCM set_D9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E0_f1[] = "set-E0-f1!";
static SCM set_E0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E0_f1_with_wb[] = "set-E0-f1!-with-wb";
static SCM set_E0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E0_f2[] = "set-E0-f2!";
static SCM set_E0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E0_f2_with_wb[] = "set-E0-f2!-with-wb";
static SCM set_E0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E1_f1[] = "set-E1-f1!";
static SCM set_E1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E1_f1_with_wb[] = "set-E1-f1!-with-wb";
static SCM set_E1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E1_f2[] = "set-E1-f2!";
static SCM set_E1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E1_f2_with_wb[] = "set-E1-f2!-with-wb";
static SCM set_E1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E2_f1[] = "set-E2-f1!";
static SCM set_E2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E2_f1_with_wb[] = "set-E2-f1!-with-wb";
static SCM set_E2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E2_f2[] = "set-E2-f2!";
static SCM set_E2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E2_f2_with_wb[] = "set-E2-f2!-with-wb";
static SCM set_E2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E3_f1[] = "set-E3-f1!";
static SCM set_E3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E3_f1_with_wb[] = "set-E3-f1!-with-wb";
static SCM set_E3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E3_f2[] = "set-E3-f2!";
static SCM set_E3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E3_f2_with_wb[] = "set-E3-f2!-with-wb";
static SCM set_E3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E4_f1[] = "set-E4-f1!";
static SCM set_E4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E4_f1_with_wb[] = "set-E4-f1!-with-wb";
static SCM set_E4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E4_f2[] = "set-E4-f2!";
static SCM set_E4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E4_f2_with_wb[] = "set-E4-f2!-with-wb";
static SCM set_E4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E5_f1[] = "set-E5-f1!";
static SCM set_E5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E5_f1_with_wb[] = "set-E5-f1!-with-wb";
static SCM set_E5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E5_f2[] = "set-E5-f2!";
static SCM set_E5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E5_f2_with_wb[] = "set-E5-f2!-with-wb";
static SCM set_E5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E6_f1[] = "set-E6-f1!";
static SCM set_E6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E6_f1_with_wb[] = "set-E6-f1!-with-wb";
static SCM set_E6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E6_f2[] = "set-E6-f2!";
static SCM set_E6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E6_f2_with_wb[] = "set-E6-f2!-with-wb";
static SCM set_E6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E7_f1[] = "set-E7-f1!";
static SCM set_E7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E7_f1_with_wb[] = "set-E7-f1!-with-wb";
static SCM set_E7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E7_f2[] = "set-E7-f2!";
static SCM set_E7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E7_f2_with_wb[] = "set-E7-f2!-with-wb";
static SCM set_E7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E8_f1[] = "set-E8-f1!";
static SCM set_E8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E8_f1_with_wb[] = "set-E8-f1!-with-wb";
static SCM set_E8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E8_f2[] = "set-E8-f2!";
static SCM set_E8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E8_f2_with_wb[] = "set-E8-f2!-with-wb";
static SCM set_E8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E9_f1[] = "set-E9-f1!";
static SCM set_E9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E9_f1_with_wb[] = "set-E9-f1!-with-wb";
static SCM set_E9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E9_f2[] = "set-E9-f2!";
static SCM set_E9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E9_f2_with_wb[] = "set-E9-f2!-with-wb";
static SCM set_E9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F0_f1[] = "set-F0-f1!";
static SCM set_F0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F0_f1_with_wb[] = "set-F0-f1!-with-wb";
static SCM set_F0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F0_f2[] = "set-F0-f2!";
static SCM set_F0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F0_f2_with_wb[] = "set-F0-f2!-with-wb";
static SCM set_F0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F1_f1[] = "set-F1-f1!";
static SCM set_F1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F1_f1_with_wb[] = "set-F1-f1!-with-wb";
static SCM set_F1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F1_f2[] = "set-F1-f2!";
static SCM set_F1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F1_f2_with_wb[] = "set-F1-f2!-with-wb";
static SCM set_F1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F2_f1[] = "set-F2-f1!";
static SCM set_F2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F2_f1_with_wb[] = "set-F2-f1!-with-wb";
static SCM set_F2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F2_f2[] = "set-F2-f2!";
static SCM set_F2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F2_f2_with_wb[] = "set-F2-f2!-with-wb";
static SCM set_F2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F3_f1[] = "set-F3-f1!";
static SCM set_F3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F3_f1_with_wb[] = "set-F3-f1!-with-wb";
static SCM set_F3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F3_f2[] = "set-F3-f2!";
static SCM set_F3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F3_f2_with_wb[] = "set-F3-f2!-with-wb";
static SCM set_F3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F4_f1[] = "set-F4-f1!";
static SCM set_F4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F4_f1_with_wb[] = "set-F4-f1!-with-wb";
static SCM set_F4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F4_f2[] = "set-F4-f2!";
static SCM set_F4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F4_f2_with_wb[] = "set-F4-f2!-with-wb";
static SCM set_F4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F5_f1[] = "set-F5-f1!";
static SCM set_F5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F5_f1_with_wb[] = "set-F5-f1!-with-wb";
static SCM set_F5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F5_f2[] = "set-F5-f2!";
static SCM set_F5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F5_f2_with_wb[] = "set-F5-f2!-with-wb";
static SCM set_F5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F6_f1[] = "set-F6-f1!";
static SCM set_F6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F6_f1_with_wb[] = "set-F6-f1!-with-wb";
static SCM set_F6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F6_f2[] = "set-F6-f2!";
static SCM set_F6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F6_f2_with_wb[] = "set-F6-f2!-with-wb";
static SCM set_F6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F7_f1[] = "set-F7-f1!";
static SCM set_F7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F7_f1_with_wb[] = "set-F7-f1!-with-wb";
static SCM set_F7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F7_f2[] = "set-F7-f2!";
static SCM set_F7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F7_f2_with_wb[] = "set-F7-f2!-with-wb";
static SCM set_F7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F8_f1[] = "set-F8-f1!";
static SCM set_F8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F8_f1_with_wb[] = "set-F8-f1!-with-wb";
static SCM set_F8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F8_f2[] = "set-F8-f2!";
static SCM set_F8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F8_f2_with_wb[] = "set-F8-f2!-with-wb";
static SCM set_F8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_F9_f1[] = "set-F9-f1!";
static SCM set_F9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_F9_f1_with_wb[] = "set-F9-f1!-with-wb";
static SCM set_F9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_F9_f2[] = "set-F9-f2!";
static SCM set_F9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_F9_f2_with_wb[] = "set-F9-f2!-with-wb";
static SCM set_F9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G0_f1[] = "set-G0-f1!";
static SCM set_G0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G0_f1_with_wb[] = "set-G0-f1!-with-wb";
static SCM set_G0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G0_f2[] = "set-G0-f2!";
static SCM set_G0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G0_f2_with_wb[] = "set-G0-f2!-with-wb";
static SCM set_G0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G1_f1[] = "set-G1-f1!";
static SCM set_G1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G1_f1_with_wb[] = "set-G1-f1!-with-wb";
static SCM set_G1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G1_f2[] = "set-G1-f2!";
static SCM set_G1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G1_f2_with_wb[] = "set-G1-f2!-with-wb";
static SCM set_G1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G2_f1[] = "set-G2-f1!";
static SCM set_G2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G2_f1_with_wb[] = "set-G2-f1!-with-wb";
static SCM set_G2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G2_f2[] = "set-G2-f2!";
static SCM set_G2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G2_f2_with_wb[] = "set-G2-f2!-with-wb";
static SCM set_G2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G3_f1[] = "set-G3-f1!";
static SCM set_G3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G3_f1_with_wb[] = "set-G3-f1!-with-wb";
static SCM set_G3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G3_f2[] = "set-G3-f2!";
static SCM set_G3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G3_f2_with_wb[] = "set-G3-f2!-with-wb";
static SCM set_G3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G4_f1[] = "set-G4-f1!";
static SCM set_G4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G4_f1_with_wb[] = "set-G4-f1!-with-wb";
static SCM set_G4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G4_f2[] = "set-G4-f2!";
static SCM set_G4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G4_f2_with_wb[] = "set-G4-f2!-with-wb";
static SCM set_G4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G5_f1[] = "set-G5-f1!";
static SCM set_G5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G5_f1_with_wb[] = "set-G5-f1!-with-wb";
static SCM set_G5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G5_f2[] = "set-G5-f2!";
static SCM set_G5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G5_f2_with_wb[] = "set-G5-f2!-with-wb";
static SCM set_G5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G6_f1[] = "set-G6-f1!";
static SCM set_G6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G6_f1_with_wb[] = "set-G6-f1!-with-wb";
static SCM set_G6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G6_f2[] = "set-G6-f2!";
static SCM set_G6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G6_f2_with_wb[] = "set-G6-f2!-with-wb";
static SCM set_G6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G7_f1[] = "set-G7-f1!";
static SCM set_G7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G7_f1_with_wb[] = "set-G7-f1!-with-wb";
static SCM set_G7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G7_f2[] = "set-G7-f2!";
static SCM set_G7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G7_f2_with_wb[] = "set-G7-f2!-with-wb";
static SCM set_G7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G8_f1[] = "set-G8-f1!";
static SCM set_G8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G8_f1_with_wb[] = "set-G8-f1!-with-wb";
static SCM set_G8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G8_f2[] = "set-G8-f2!";
static SCM set_G8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G8_f2_with_wb[] = "set-G8-f2!-with-wb";
static SCM set_G8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_G9_f1[] = "set-G9-f1!";
static SCM set_G9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_G9_f1_with_wb[] = "set-G9-f1!-with-wb";
static SCM set_G9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_G9_f2[] = "set-G9-f2!";
static SCM set_G9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_G9_f2_with_wb[] = "set-G9-f2!-with-wb";
static SCM set_G9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H0_f1[] = "set-H0-f1!";
static SCM set_H0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H0_f1_with_wb[] = "set-H0-f1!-with-wb";
static SCM set_H0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H0_f2[] = "set-H0-f2!";
static SCM set_H0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H0_f2_with_wb[] = "set-H0-f2!-with-wb";
static SCM set_H0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H1_f1[] = "set-H1-f1!";
static SCM set_H1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H1_f1_with_wb[] = "set-H1-f1!-with-wb";
static SCM set_H1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H1_f2[] = "set-H1-f2!";
static SCM set_H1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H1_f2_with_wb[] = "set-H1-f2!-with-wb";
static SCM set_H1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H2_f1[] = "set-H2-f1!";
static SCM set_H2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H2_f1_with_wb[] = "set-H2-f1!-with-wb";
static SCM set_H2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H2_f2[] = "set-H2-f2!";
static SCM set_H2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H2_f2_with_wb[] = "set-H2-f2!-with-wb";
static SCM set_H2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H3_f1[] = "set-H3-f1!";
static SCM set_H3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H3_f1_with_wb[] = "set-H3-f1!-with-wb";
static SCM set_H3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H3_f2[] = "set-H3-f2!";
static SCM set_H3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H3_f2_with_wb[] = "set-H3-f2!-with-wb";
static SCM set_H3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H4_f1[] = "set-H4-f1!";
static SCM set_H4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H4_f1_with_wb[] = "set-H4-f1!-with-wb";
static SCM set_H4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H4_f2[] = "set-H4-f2!";
static SCM set_H4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H4_f2_with_wb[] = "set-H4-f2!-with-wb";
static SCM set_H4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H5_f1[] = "set-H5-f1!";
static SCM set_H5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H5_f1_with_wb[] = "set-H5-f1!-with-wb";
static SCM set_H5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H5_f2[] = "set-H5-f2!";
static SCM set_H5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H5_f2_with_wb[] = "set-H5-f2!-with-wb";
static SCM set_H5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H6_f1[] = "set-H6-f1!";
static SCM set_H6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H6_f1_with_wb[] = "set-H6-f1!-with-wb";
static SCM set_H6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H6_f2[] = "set-H6-f2!";
static SCM set_H6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H6_f2_with_wb[] = "set-H6-f2!-with-wb";
static SCM set_H6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H7_f1[] = "set-H7-f1!";
static SCM set_H7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H7_f1_with_wb[] = "set-H7-f1!-with-wb";
static SCM set_H7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H7_f2[] = "set-H7-f2!";
static SCM set_H7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H7_f2_with_wb[] = "set-H7-f2!-with-wb";
static SCM set_H7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H8_f1[] = "set-H8-f1!";
static SCM set_H8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H8_f1_with_wb[] = "set-H8-f1!-with-wb";
static SCM set_H8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H8_f2[] = "set-H8-f2!";
static SCM set_H8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H8_f2_with_wb[] = "set-H8-f2!-with-wb";
static SCM set_H8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_H9_f1[] = "set-H9-f1!";
static SCM set_H9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_H9_f1_with_wb[] = "set-H9-f1!-with-wb";
static SCM set_H9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_H9_f2[] = "set-H9-f2!";
static SCM set_H9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_H9_f2_with_wb[] = "set-H9-f2!-with-wb";
static SCM set_H9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I0_f1[] = "set-I0-f1!";
static SCM set_I0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I0_f1_with_wb[] = "set-I0-f1!-with-wb";
static SCM set_I0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I0_f2[] = "set-I0-f2!";
static SCM set_I0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I0_f2_with_wb[] = "set-I0-f2!-with-wb";
static SCM set_I0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I1_f1[] = "set-I1-f1!";
static SCM set_I1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I1_f1_with_wb[] = "set-I1-f1!-with-wb";
static SCM set_I1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I1_f2[] = "set-I1-f2!";
static SCM set_I1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I1_f2_with_wb[] = "set-I1-f2!-with-wb";
static SCM set_I1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I2_f1[] = "set-I2-f1!";
static SCM set_I2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I2_f1_with_wb[] = "set-I2-f1!-with-wb";
static SCM set_I2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I2_f2[] = "set-I2-f2!";
static SCM set_I2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I2_f2_with_wb[] = "set-I2-f2!-with-wb";
static SCM set_I2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I3_f1[] = "set-I3-f1!";
static SCM set_I3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I3_f1_with_wb[] = "set-I3-f1!-with-wb";
static SCM set_I3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I3_f2[] = "set-I3-f2!";
static SCM set_I3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I3_f2_with_wb[] = "set-I3-f2!-with-wb";
static SCM set_I3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I4_f1[] = "set-I4-f1!";
static SCM set_I4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I4_f1_with_wb[] = "set-I4-f1!-with-wb";
static SCM set_I4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I4_f2[] = "set-I4-f2!";
static SCM set_I4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I4_f2_with_wb[] = "set-I4-f2!-with-wb";
static SCM set_I4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I5_f1[] = "set-I5-f1!";
static SCM set_I5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I5_f1_with_wb[] = "set-I5-f1!-with-wb";
static SCM set_I5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I5_f2[] = "set-I5-f2!";
static SCM set_I5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I5_f2_with_wb[] = "set-I5-f2!-with-wb";
static SCM set_I5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I6_f1[] = "set-I6-f1!";
static SCM set_I6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I6_f1_with_wb[] = "set-I6-f1!-with-wb";
static SCM set_I6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I6_f2[] = "set-I6-f2!";
static SCM set_I6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I6_f2_with_wb[] = "set-I6-f2!-with-wb";
static SCM set_I6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I7_f1[] = "set-I7-f1!";
static SCM set_I7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I7_f1_with_wb[] = "set-I7-f1!-with-wb";
static SCM set_I7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I7_f2[] = "set-I7-f2!";
static SCM set_I7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I7_f2_with_wb[] = "set-I7-f2!-with-wb";
static SCM set_I7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I8_f1[] = "set-I8-f1!";
static SCM set_I8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I8_f1_with_wb[] = "set-I8-f1!-with-wb";
static SCM set_I8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I8_f2[] = "set-I8-f2!";
static SCM set_I8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I8_f2_with_wb[] = "set-I8-f2!-with-wb";
static SCM set_I8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_I9_f1[] = "set-I9-f1!";
static SCM set_I9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_I9_f1_with_wb[] = "set-I9-f1!-with-wb";
static SCM set_I9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_I9_f2[] = "set-I9-f2!";
static SCM set_I9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_I9_f2_with_wb[] = "set-I9-f2!-with-wb";
static SCM set_I9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J0_f1[] = "set-J0-f1!";
static SCM set_J0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J0_f1_with_wb[] = "set-J0-f1!-with-wb";
static SCM set_J0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J0_f2[] = "set-J0-f2!";
static SCM set_J0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J0_f2_with_wb[] = "set-J0-f2!-with-wb";
static SCM set_J0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J1_f1[] = "set-J1-f1!";
static SCM set_J1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J1_f1_with_wb[] = "set-J1-f1!-with-wb";
static SCM set_J1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J1_f2[] = "set-J1-f2!";
static SCM set_J1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J1_f2_with_wb[] = "set-J1-f2!-with-wb";
static SCM set_J1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J2_f1[] = "set-J2-f1!";
static SCM set_J2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J2_f1_with_wb[] = "set-J2-f1!-with-wb";
static SCM set_J2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J2_f2[] = "set-J2-f2!";
static SCM set_J2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J2_f2_with_wb[] = "set-J2-f2!-with-wb";
static SCM set_J2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J3_f1[] = "set-J3-f1!";
static SCM set_J3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J3_f1_with_wb[] = "set-J3-f1!-with-wb";
static SCM set_J3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J3_f2[] = "set-J3-f2!";
static SCM set_J3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J3_f2_with_wb[] = "set-J3-f2!-with-wb";
static SCM set_J3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J4_f1[] = "set-J4-f1!";
static SCM set_J4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J4_f1_with_wb[] = "set-J4-f1!-with-wb";
static SCM set_J4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J4_f2[] = "set-J4-f2!";
static SCM set_J4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J4_f2_with_wb[] = "set-J4-f2!-with-wb";
static SCM set_J4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J5_f1[] = "set-J5-f1!";
static SCM set_J5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J5_f1_with_wb[] = "set-J5-f1!-with-wb";
static SCM set_J5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J5_f2[] = "set-J5-f2!";
static SCM set_J5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J5_f2_with_wb[] = "set-J5-f2!-with-wb";
static SCM set_J5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J6_f1[] = "set-J6-f1!";
static SCM set_J6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J6_f1_with_wb[] = "set-J6-f1!-with-wb";
static SCM set_J6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J6_f2[] = "set-J6-f2!";
static SCM set_J6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J6_f2_with_wb[] = "set-J6-f2!-with-wb";
static SCM set_J6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J7_f1[] = "set-J7-f1!";
static SCM set_J7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J7_f1_with_wb[] = "set-J7-f1!-with-wb";
static SCM set_J7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J7_f2[] = "set-J7-f2!";
static SCM set_J7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J7_f2_with_wb[] = "set-J7-f2!-with-wb";
static SCM set_J7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J8_f1[] = "set-J8-f1!";
static SCM set_J8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J8_f1_with_wb[] = "set-J8-f1!-with-wb";
static SCM set_J8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J8_f2[] = "set-J8-f2!";
static SCM set_J8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J8_f2_with_wb[] = "set-J8-f2!-with-wb";
static SCM set_J8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_J9_f1[] = "set-J9-f1!";
static SCM set_J9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_J9_f1_with_wb[] = "set-J9-f1!-with-wb";
static SCM set_J9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_J9_f2[] = "set-J9-f2!";
static SCM set_J9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_J9_f2_with_wb[] = "set-J9-f2!-with-wb";
static SCM set_J9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K0_f1[] = "set-K0-f1!";
static SCM set_K0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K0_f1_with_wb[] = "set-K0-f1!-with-wb";
static SCM set_K0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K0_f2[] = "set-K0-f2!";
static SCM set_K0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K0_f2_with_wb[] = "set-K0-f2!-with-wb";
static SCM set_K0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K1_f1[] = "set-K1-f1!";
static SCM set_K1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K1_f1_with_wb[] = "set-K1-f1!-with-wb";
static SCM set_K1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K1_f2[] = "set-K1-f2!";
static SCM set_K1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K1_f2_with_wb[] = "set-K1-f2!-with-wb";
static SCM set_K1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K2_f1[] = "set-K2-f1!";
static SCM set_K2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K2_f1_with_wb[] = "set-K2-f1!-with-wb";
static SCM set_K2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K2_f2[] = "set-K2-f2!";
static SCM set_K2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K2_f2_with_wb[] = "set-K2-f2!-with-wb";
static SCM set_K2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K3_f1[] = "set-K3-f1!";
static SCM set_K3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K3_f1_with_wb[] = "set-K3-f1!-with-wb";
static SCM set_K3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K3_f2[] = "set-K3-f2!";
static SCM set_K3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K3_f2_with_wb[] = "set-K3-f2!-with-wb";
static SCM set_K3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K4_f1[] = "set-K4-f1!";
static SCM set_K4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K4_f1_with_wb[] = "set-K4-f1!-with-wb";
static SCM set_K4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K4_f2[] = "set-K4-f2!";
static SCM set_K4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K4_f2_with_wb[] = "set-K4-f2!-with-wb";
static SCM set_K4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K5_f1[] = "set-K5-f1!";
static SCM set_K5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K5_f1_with_wb[] = "set-K5-f1!-with-wb";
static SCM set_K5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K5_f2[] = "set-K5-f2!";
static SCM set_K5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K5_f2_with_wb[] = "set-K5-f2!-with-wb";
static SCM set_K5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K6_f1[] = "set-K6-f1!";
static SCM set_K6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K6_f1_with_wb[] = "set-K6-f1!-with-wb";
static SCM set_K6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K6_f2[] = "set-K6-f2!";
static SCM set_K6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K6_f2_with_wb[] = "set-K6-f2!-with-wb";
static SCM set_K6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K7_f1[] = "set-K7-f1!";
static SCM set_K7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K7_f1_with_wb[] = "set-K7-f1!-with-wb";
static SCM set_K7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K7_f2[] = "set-K7-f2!";
static SCM set_K7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K7_f2_with_wb[] = "set-K7-f2!-with-wb";
static SCM set_K7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K8_f1[] = "set-K8-f1!";
static SCM set_K8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K8_f1_with_wb[] = "set-K8-f1!-with-wb";
static SCM set_K8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K8_f2[] = "set-K8-f2!";
static SCM set_K8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K8_f2_with_wb[] = "set-K8-f2!-with-wb";
static SCM set_K8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_K9_f1[] = "set-K9-f1!";
static SCM set_K9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_K9_f1_with_wb[] = "set-K9-f1!-with-wb";
static SCM set_K9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_K9_f2[] = "set-K9-f2!";
static SCM set_K9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_K9_f2_with_wb[] = "set-K9-f2!-with-wb";
static SCM set_K9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L0_f1[] = "set-L0-f1!";
static SCM set_L0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L0_f1_with_wb[] = "set-L0-f1!-with-wb";
static SCM set_L0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L0_f2[] = "set-L0-f2!";
static SCM set_L0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L0_f2_with_wb[] = "set-L0-f2!-with-wb";
static SCM set_L0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L1_f1[] = "set-L1-f1!";
static SCM set_L1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L1_f1_with_wb[] = "set-L1-f1!-with-wb";
static SCM set_L1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L1_f2[] = "set-L1-f2!";
static SCM set_L1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L1_f2_with_wb[] = "set-L1-f2!-with-wb";
static SCM set_L1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L2_f1[] = "set-L2-f1!";
static SCM set_L2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L2_f1_with_wb[] = "set-L2-f1!-with-wb";
static SCM set_L2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L2_f2[] = "set-L2-f2!";
static SCM set_L2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L2_f2_with_wb[] = "set-L2-f2!-with-wb";
static SCM set_L2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L3_f1[] = "set-L3-f1!";
static SCM set_L3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L3_f1_with_wb[] = "set-L3-f1!-with-wb";
static SCM set_L3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L3_f2[] = "set-L3-f2!";
static SCM set_L3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L3_f2_with_wb[] = "set-L3-f2!-with-wb";
static SCM set_L3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L4_f1[] = "set-L4-f1!";
static SCM set_L4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L4_f1_with_wb[] = "set-L4-f1!-with-wb";
static SCM set_L4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L4_f2[] = "set-L4-f2!";
static SCM set_L4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L4_f2_with_wb[] = "set-L4-f2!-with-wb";
static SCM set_L4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L5_f1[] = "set-L5-f1!";
static SCM set_L5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L5_f1_with_wb[] = "set-L5-f1!-with-wb";
static SCM set_L5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L5_f2[] = "set-L5-f2!";
static SCM set_L5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L5_f2_with_wb[] = "set-L5-f2!-with-wb";
static SCM set_L5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L6_f1[] = "set-L6-f1!";
static SCM set_L6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L6_f1_with_wb[] = "set-L6-f1!-with-wb";
static SCM set_L6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L6_f2[] = "set-L6-f2!";
static SCM set_L6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L6_f2_with_wb[] = "set-L6-f2!-with-wb";
static SCM set_L6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L7_f1[] = "set-L7-f1!";
static SCM set_L7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L7_f1_with_wb[] = "set-L7-f1!-with-wb";
static SCM set_L7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L7_f2[] = "set-L7-f2!";
static SCM set_L7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L7_f2_with_wb[] = "set-L7-f2!-with-wb";
static SCM set_L7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L8_f1[] = "set-L8-f1!";
static SCM set_L8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L8_f1_with_wb[] = "set-L8-f1!-with-wb";
static SCM set_L8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L8_f2[] = "set-L8-f2!";
static SCM set_L8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L8_f2_with_wb[] = "set-L8-f2!-with-wb";
static SCM set_L8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_L9_f1[] = "set-L9-f1!";
static SCM set_L9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_L9_f1_with_wb[] = "set-L9-f1!-with-wb";
static SCM set_L9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_L9_f2[] = "set-L9-f2!";
static SCM set_L9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_L9_f2_with_wb[] = "set-L9-f2!-with-wb";
static SCM set_L9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M0_f1[] = "set-M0-f1!";
static SCM set_M0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M0_f1_with_wb[] = "set-M0-f1!-with-wb";
static SCM set_M0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M0_f2[] = "set-M0-f2!";
static SCM set_M0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M0_f2_with_wb[] = "set-M0-f2!-with-wb";
static SCM set_M0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M1_f1[] = "set-M1-f1!";
static SCM set_M1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M1_f1_with_wb[] = "set-M1-f1!-with-wb";
static SCM set_M1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M1_f2[] = "set-M1-f2!";
static SCM set_M1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M1_f2_with_wb[] = "set-M1-f2!-with-wb";
static SCM set_M1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M2_f1[] = "set-M2-f1!";
static SCM set_M2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M2_f1_with_wb[] = "set-M2-f1!-with-wb";
static SCM set_M2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M2_f2[] = "set-M2-f2!";
static SCM set_M2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M2_f2_with_wb[] = "set-M2-f2!-with-wb";
static SCM set_M2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M3_f1[] = "set-M3-f1!";
static SCM set_M3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M3_f1_with_wb[] = "set-M3-f1!-with-wb";
static SCM set_M3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M3_f2[] = "set-M3-f2!";
static SCM set_M3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M3_f2_with_wb[] = "set-M3-f2!-with-wb";
static SCM set_M3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M4_f1[] = "set-M4-f1!";
static SCM set_M4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M4_f1_with_wb[] = "set-M4-f1!-with-wb";
static SCM set_M4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M4_f2[] = "set-M4-f2!";
static SCM set_M4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M4_f2_with_wb[] = "set-M4-f2!-with-wb";
static SCM set_M4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M5_f1[] = "set-M5-f1!";
static SCM set_M5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M5_f1_with_wb[] = "set-M5-f1!-with-wb";
static SCM set_M5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M5_f2[] = "set-M5-f2!";
static SCM set_M5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M5_f2_with_wb[] = "set-M5-f2!-with-wb";
static SCM set_M5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M6_f1[] = "set-M6-f1!";
static SCM set_M6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M6_f1_with_wb[] = "set-M6-f1!-with-wb";
static SCM set_M6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M6_f2[] = "set-M6-f2!";
static SCM set_M6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M6_f2_with_wb[] = "set-M6-f2!-with-wb";
static SCM set_M6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M7_f1[] = "set-M7-f1!";
static SCM set_M7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M7_f1_with_wb[] = "set-M7-f1!-with-wb";
static SCM set_M7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M7_f2[] = "set-M7-f2!";
static SCM set_M7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M7_f2_with_wb[] = "set-M7-f2!-with-wb";
static SCM set_M7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M8_f1[] = "set-M8-f1!";
static SCM set_M8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M8_f1_with_wb[] = "set-M8-f1!-with-wb";
static SCM set_M8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M8_f2[] = "set-M8-f2!";
static SCM set_M8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M8_f2_with_wb[] = "set-M8-f2!-with-wb";
static SCM set_M8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_M9_f1[] = "set-M9-f1!";
static SCM set_M9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_M9_f1_with_wb[] = "set-M9-f1!-with-wb";
static SCM set_M9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_M9_f2[] = "set-M9-f2!";
static SCM set_M9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_M9_f2_with_wb[] = "set-M9-f2!-with-wb";
static SCM set_M9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N0_f1[] = "set-N0-f1!";
static SCM set_N0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N0_f1_with_wb[] = "set-N0-f1!-with-wb";
static SCM set_N0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N0_f2[] = "set-N0-f2!";
static SCM set_N0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N0_f2_with_wb[] = "set-N0-f2!-with-wb";
static SCM set_N0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N1_f1[] = "set-N1-f1!";
static SCM set_N1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N1_f1_with_wb[] = "set-N1-f1!-with-wb";
static SCM set_N1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N1_f2[] = "set-N1-f2!";
static SCM set_N1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N1_f2_with_wb[] = "set-N1-f2!-with-wb";
static SCM set_N1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N2_f1[] = "set-N2-f1!";
static SCM set_N2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N2_f1_with_wb[] = "set-N2-f1!-with-wb";
static SCM set_N2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N2_f2[] = "set-N2-f2!";
static SCM set_N2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N2_f2_with_wb[] = "set-N2-f2!-with-wb";
static SCM set_N2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N3_f1[] = "set-N3-f1!";
static SCM set_N3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N3_f1_with_wb[] = "set-N3-f1!-with-wb";
static SCM set_N3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N3_f2[] = "set-N3-f2!";
static SCM set_N3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N3_f2_with_wb[] = "set-N3-f2!-with-wb";
static SCM set_N3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N4_f1[] = "set-N4-f1!";
static SCM set_N4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N4_f1_with_wb[] = "set-N4-f1!-with-wb";
static SCM set_N4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N4_f2[] = "set-N4-f2!";
static SCM set_N4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N4_f2_with_wb[] = "set-N4-f2!-with-wb";
static SCM set_N4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N5_f1[] = "set-N5-f1!";
static SCM set_N5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N5_f1_with_wb[] = "set-N5-f1!-with-wb";
static SCM set_N5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N5_f2[] = "set-N5-f2!";
static SCM set_N5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N5_f2_with_wb[] = "set-N5-f2!-with-wb";
static SCM set_N5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N6_f1[] = "set-N6-f1!";
static SCM set_N6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N6_f1_with_wb[] = "set-N6-f1!-with-wb";
static SCM set_N6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N6_f2[] = "set-N6-f2!";
static SCM set_N6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N6_f2_with_wb[] = "set-N6-f2!-with-wb";
static SCM set_N6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N7_f1[] = "set-N7-f1!";
static SCM set_N7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N7_f1_with_wb[] = "set-N7-f1!-with-wb";
static SCM set_N7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N7_f2[] = "set-N7-f2!";
static SCM set_N7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N7_f2_with_wb[] = "set-N7-f2!-with-wb";
static SCM set_N7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N8_f1[] = "set-N8-f1!";
static SCM set_N8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N8_f1_with_wb[] = "set-N8-f1!-with-wb";
static SCM set_N8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N8_f2[] = "set-N8-f2!";
static SCM set_N8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N8_f2_with_wb[] = "set-N8-f2!-with-wb";
static SCM set_N8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_N9_f1[] = "set-N9-f1!";
static SCM set_N9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_N9_f1_with_wb[] = "set-N9-f1!-with-wb";
static SCM set_N9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_N9_f2[] = "set-N9-f2!";
static SCM set_N9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_N9_f2_with_wb[] = "set-N9-f2!-with-wb";
static SCM set_N9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O0_f1[] = "set-O0-f1!";
static SCM set_O0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O0_f1_with_wb[] = "set-O0-f1!-with-wb";
static SCM set_O0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O0_f2[] = "set-O0-f2!";
static SCM set_O0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O0_f2_with_wb[] = "set-O0-f2!-with-wb";
static SCM set_O0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O1_f1[] = "set-O1-f1!";
static SCM set_O1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O1_f1_with_wb[] = "set-O1-f1!-with-wb";
static SCM set_O1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O1_f2[] = "set-O1-f2!";
static SCM set_O1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O1_f2_with_wb[] = "set-O1-f2!-with-wb";
static SCM set_O1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O2_f1[] = "set-O2-f1!";
static SCM set_O2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O2_f1_with_wb[] = "set-O2-f1!-with-wb";
static SCM set_O2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O2_f2[] = "set-O2-f2!";
static SCM set_O2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O2_f2_with_wb[] = "set-O2-f2!-with-wb";
static SCM set_O2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O3_f1[] = "set-O3-f1!";
static SCM set_O3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O3_f1_with_wb[] = "set-O3-f1!-with-wb";
static SCM set_O3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O3_f2[] = "set-O3-f2!";
static SCM set_O3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O3_f2_with_wb[] = "set-O3-f2!-with-wb";
static SCM set_O3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O4_f1[] = "set-O4-f1!";
static SCM set_O4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O4_f1_with_wb[] = "set-O4-f1!-with-wb";
static SCM set_O4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O4_f2[] = "set-O4-f2!";
static SCM set_O4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O4_f2_with_wb[] = "set-O4-f2!-with-wb";
static SCM set_O4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O5_f1[] = "set-O5-f1!";
static SCM set_O5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O5_f1_with_wb[] = "set-O5-f1!-with-wb";
static SCM set_O5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O5_f2[] = "set-O5-f2!";
static SCM set_O5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O5_f2_with_wb[] = "set-O5-f2!-with-wb";
static SCM set_O5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O6_f1[] = "set-O6-f1!";
static SCM set_O6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O6_f1_with_wb[] = "set-O6-f1!-with-wb";
static SCM set_O6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O6_f2[] = "set-O6-f2!";
static SCM set_O6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O6_f2_with_wb[] = "set-O6-f2!-with-wb";
static SCM set_O6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O7_f1[] = "set-O7-f1!";
static SCM set_O7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O7_f1_with_wb[] = "set-O7-f1!-with-wb";
static SCM set_O7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O7_f2[] = "set-O7-f2!";
static SCM set_O7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O7_f2_with_wb[] = "set-O7-f2!-with-wb";
static SCM set_O7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O8_f1[] = "set-O8-f1!";
static SCM set_O8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O8_f1_with_wb[] = "set-O8-f1!-with-wb";
static SCM set_O8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O8_f2[] = "set-O8-f2!";
static SCM set_O8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O8_f2_with_wb[] = "set-O8-f2!-with-wb";
static SCM set_O8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_O9_f1[] = "set-O9-f1!";
static SCM set_O9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_O9_f1_with_wb[] = "set-O9-f1!-with-wb";
static SCM set_O9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_O9_f2[] = "set-O9-f2!";
static SCM set_O9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_O9_f2_with_wb[] = "set-O9-f2!-with-wb";
static SCM set_O9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P0_f1[] = "set-P0-f1!";
static SCM set_P0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P0_f1_with_wb[] = "set-P0-f1!-with-wb";
static SCM set_P0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P0_f2[] = "set-P0-f2!";
static SCM set_P0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P0_f2_with_wb[] = "set-P0-f2!-with-wb";
static SCM set_P0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P1_f1[] = "set-P1-f1!";
static SCM set_P1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P1_f1_with_wb[] = "set-P1-f1!-with-wb";
static SCM set_P1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P1_f2[] = "set-P1-f2!";
static SCM set_P1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P1_f2_with_wb[] = "set-P1-f2!-with-wb";
static SCM set_P1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P2_f1[] = "set-P2-f1!";
static SCM set_P2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P2_f1_with_wb[] = "set-P2-f1!-with-wb";
static SCM set_P2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P2_f2[] = "set-P2-f2!";
static SCM set_P2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P2_f2_with_wb[] = "set-P2-f2!-with-wb";
static SCM set_P2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P3_f1[] = "set-P3-f1!";
static SCM set_P3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P3_f1_with_wb[] = "set-P3-f1!-with-wb";
static SCM set_P3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P3_f2[] = "set-P3-f2!";
static SCM set_P3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P3_f2_with_wb[] = "set-P3-f2!-with-wb";
static SCM set_P3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P4_f1[] = "set-P4-f1!";
static SCM set_P4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P4_f1_with_wb[] = "set-P4-f1!-with-wb";
static SCM set_P4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P4_f2[] = "set-P4-f2!";
static SCM set_P4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P4_f2_with_wb[] = "set-P4-f2!-with-wb";
static SCM set_P4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P5_f1[] = "set-P5-f1!";
static SCM set_P5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P5_f1_with_wb[] = "set-P5-f1!-with-wb";
static SCM set_P5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P5_f2[] = "set-P5-f2!";
static SCM set_P5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P5_f2_with_wb[] = "set-P5-f2!-with-wb";
static SCM set_P5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P6_f1[] = "set-P6-f1!";
static SCM set_P6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P6_f1_with_wb[] = "set-P6-f1!-with-wb";
static SCM set_P6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P6_f2[] = "set-P6-f2!";
static SCM set_P6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P6_f2_with_wb[] = "set-P6-f2!-with-wb";
static SCM set_P6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P7_f1[] = "set-P7-f1!";
static SCM set_P7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P7_f1_with_wb[] = "set-P7-f1!-with-wb";
static SCM set_P7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P7_f2[] = "set-P7-f2!";
static SCM set_P7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P7_f2_with_wb[] = "set-P7-f2!-with-wb";
static SCM set_P7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P8_f1[] = "set-P8-f1!";
static SCM set_P8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P8_f1_with_wb[] = "set-P8-f1!-with-wb";
static SCM set_P8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P8_f2[] = "set-P8-f2!";
static SCM set_P8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P8_f2_with_wb[] = "set-P8-f2!-with-wb";
static SCM set_P8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_P9_f1[] = "set-P9-f1!";
static SCM set_P9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_P9_f1_with_wb[] = "set-P9-f1!-with-wb";
static SCM set_P9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_P9_f2[] = "set-P9-f2!";
static SCM set_P9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_P9_f2_with_wb[] = "set-P9-f2!-with-wb";
static SCM set_P9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q0_f1[] = "set-Q0-f1!";
static SCM set_Q0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q0_f1_with_wb[] = "set-Q0-f1!-with-wb";
static SCM set_Q0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q0_f2[] = "set-Q0-f2!";
static SCM set_Q0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q0_f2_with_wb[] = "set-Q0-f2!-with-wb";
static SCM set_Q0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q1_f1[] = "set-Q1-f1!";
static SCM set_Q1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q1_f1_with_wb[] = "set-Q1-f1!-with-wb";
static SCM set_Q1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q1_f2[] = "set-Q1-f2!";
static SCM set_Q1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q1_f2_with_wb[] = "set-Q1-f2!-with-wb";
static SCM set_Q1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q2_f1[] = "set-Q2-f1!";
static SCM set_Q2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q2_f1_with_wb[] = "set-Q2-f1!-with-wb";
static SCM set_Q2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q2_f2[] = "set-Q2-f2!";
static SCM set_Q2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q2_f2_with_wb[] = "set-Q2-f2!-with-wb";
static SCM set_Q2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q3_f1[] = "set-Q3-f1!";
static SCM set_Q3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q3_f1_with_wb[] = "set-Q3-f1!-with-wb";
static SCM set_Q3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q3_f2[] = "set-Q3-f2!";
static SCM set_Q3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q3_f2_with_wb[] = "set-Q3-f2!-with-wb";
static SCM set_Q3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q4_f1[] = "set-Q4-f1!";
static SCM set_Q4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q4_f1_with_wb[] = "set-Q4-f1!-with-wb";
static SCM set_Q4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q4_f2[] = "set-Q4-f2!";
static SCM set_Q4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q4_f2_with_wb[] = "set-Q4-f2!-with-wb";
static SCM set_Q4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q5_f1[] = "set-Q5-f1!";
static SCM set_Q5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q5_f1_with_wb[] = "set-Q5-f1!-with-wb";
static SCM set_Q5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q5_f2[] = "set-Q5-f2!";
static SCM set_Q5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q5_f2_with_wb[] = "set-Q5-f2!-with-wb";
static SCM set_Q5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q6_f1[] = "set-Q6-f1!";
static SCM set_Q6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q6_f1_with_wb[] = "set-Q6-f1!-with-wb";
static SCM set_Q6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q6_f2[] = "set-Q6-f2!";
static SCM set_Q6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q6_f2_with_wb[] = "set-Q6-f2!-with-wb";
static SCM set_Q6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q7_f1[] = "set-Q7-f1!";
static SCM set_Q7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q7_f1_with_wb[] = "set-Q7-f1!-with-wb";
static SCM set_Q7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q7_f2[] = "set-Q7-f2!";
static SCM set_Q7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q7_f2_with_wb[] = "set-Q7-f2!-with-wb";
static SCM set_Q7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q8_f1[] = "set-Q8-f1!";
static SCM set_Q8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q8_f1_with_wb[] = "set-Q8-f1!-with-wb";
static SCM set_Q8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q8_f2[] = "set-Q8-f2!";
static SCM set_Q8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q8_f2_with_wb[] = "set-Q8-f2!-with-wb";
static SCM set_Q8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Q9_f1[] = "set-Q9-f1!";
static SCM set_Q9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Q9_f1_with_wb[] = "set-Q9-f1!-with-wb";
static SCM set_Q9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Q9_f2[] = "set-Q9-f2!";
static SCM set_Q9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Q9_f2_with_wb[] = "set-Q9-f2!-with-wb";
static SCM set_Q9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R0_f1[] = "set-R0-f1!";
static SCM set_R0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R0_f1_with_wb[] = "set-R0-f1!-with-wb";
static SCM set_R0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R0_f2[] = "set-R0-f2!";
static SCM set_R0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R0_f2_with_wb[] = "set-R0-f2!-with-wb";
static SCM set_R0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R1_f1[] = "set-R1-f1!";
static SCM set_R1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R1_f1_with_wb[] = "set-R1-f1!-with-wb";
static SCM set_R1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R1_f2[] = "set-R1-f2!";
static SCM set_R1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R1_f2_with_wb[] = "set-R1-f2!-with-wb";
static SCM set_R1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R2_f1[] = "set-R2-f1!";
static SCM set_R2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R2_f1_with_wb[] = "set-R2-f1!-with-wb";
static SCM set_R2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R2_f2[] = "set-R2-f2!";
static SCM set_R2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R2_f2_with_wb[] = "set-R2-f2!-with-wb";
static SCM set_R2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R3_f1[] = "set-R3-f1!";
static SCM set_R3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R3_f1_with_wb[] = "set-R3-f1!-with-wb";
static SCM set_R3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R3_f2[] = "set-R3-f2!";
static SCM set_R3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R3_f2_with_wb[] = "set-R3-f2!-with-wb";
static SCM set_R3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R4_f1[] = "set-R4-f1!";
static SCM set_R4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R4_f1_with_wb[] = "set-R4-f1!-with-wb";
static SCM set_R4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R4_f2[] = "set-R4-f2!";
static SCM set_R4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R4_f2_with_wb[] = "set-R4-f2!-with-wb";
static SCM set_R4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R5_f1[] = "set-R5-f1!";
static SCM set_R5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R5_f1_with_wb[] = "set-R5-f1!-with-wb";
static SCM set_R5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R5_f2[] = "set-R5-f2!";
static SCM set_R5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R5_f2_with_wb[] = "set-R5-f2!-with-wb";
static SCM set_R5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R6_f1[] = "set-R6-f1!";
static SCM set_R6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R6_f1_with_wb[] = "set-R6-f1!-with-wb";
static SCM set_R6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R6_f2[] = "set-R6-f2!";
static SCM set_R6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R6_f2_with_wb[] = "set-R6-f2!-with-wb";
static SCM set_R6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R7_f1[] = "set-R7-f1!";
static SCM set_R7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R7_f1_with_wb[] = "set-R7-f1!-with-wb";
static SCM set_R7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R7_f2[] = "set-R7-f2!";
static SCM set_R7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R7_f2_with_wb[] = "set-R7-f2!-with-wb";
static SCM set_R7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R8_f1[] = "set-R8-f1!";
static SCM set_R8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R8_f1_with_wb[] = "set-R8-f1!-with-wb";
static SCM set_R8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R8_f2[] = "set-R8-f2!";
static SCM set_R8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R8_f2_with_wb[] = "set-R8-f2!-with-wb";
static SCM set_R8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_R9_f1[] = "set-R9-f1!";
static SCM set_R9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_R9_f1_with_wb[] = "set-R9-f1!-with-wb";
static SCM set_R9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_R9_f2[] = "set-R9-f2!";
static SCM set_R9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_R9_f2_with_wb[] = "set-R9-f2!-with-wb";
static SCM set_R9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S0_f1[] = "set-S0-f1!";
static SCM set_S0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S0_f1_with_wb[] = "set-S0-f1!-with-wb";
static SCM set_S0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S0_f2[] = "set-S0-f2!";
static SCM set_S0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S0_f2_with_wb[] = "set-S0-f2!-with-wb";
static SCM set_S0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S1_f1[] = "set-S1-f1!";
static SCM set_S1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S1_f1_with_wb[] = "set-S1-f1!-with-wb";
static SCM set_S1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S1_f2[] = "set-S1-f2!";
static SCM set_S1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S1_f2_with_wb[] = "set-S1-f2!-with-wb";
static SCM set_S1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S2_f1[] = "set-S2-f1!";
static SCM set_S2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S2_f1_with_wb[] = "set-S2-f1!-with-wb";
static SCM set_S2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S2_f2[] = "set-S2-f2!";
static SCM set_S2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S2_f2_with_wb[] = "set-S2-f2!-with-wb";
static SCM set_S2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S3_f1[] = "set-S3-f1!";
static SCM set_S3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S3_f1_with_wb[] = "set-S3-f1!-with-wb";
static SCM set_S3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S3_f2[] = "set-S3-f2!";
static SCM set_S3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S3_f2_with_wb[] = "set-S3-f2!-with-wb";
static SCM set_S3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S4_f1[] = "set-S4-f1!";
static SCM set_S4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S4_f1_with_wb[] = "set-S4-f1!-with-wb";
static SCM set_S4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S4_f2[] = "set-S4-f2!";
static SCM set_S4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S4_f2_with_wb[] = "set-S4-f2!-with-wb";
static SCM set_S4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S5_f1[] = "set-S5-f1!";
static SCM set_S5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S5_f1_with_wb[] = "set-S5-f1!-with-wb";
static SCM set_S5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S5_f2[] = "set-S5-f2!";
static SCM set_S5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S5_f2_with_wb[] = "set-S5-f2!-with-wb";
static SCM set_S5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S6_f1[] = "set-S6-f1!";
static SCM set_S6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S6_f1_with_wb[] = "set-S6-f1!-with-wb";
static SCM set_S6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S6_f2[] = "set-S6-f2!";
static SCM set_S6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S6_f2_with_wb[] = "set-S6-f2!-with-wb";
static SCM set_S6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S7_f1[] = "set-S7-f1!";
static SCM set_S7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S7_f1_with_wb[] = "set-S7-f1!-with-wb";
static SCM set_S7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S7_f2[] = "set-S7-f2!";
static SCM set_S7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S7_f2_with_wb[] = "set-S7-f2!-with-wb";
static SCM set_S7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S8_f1[] = "set-S8-f1!";
static SCM set_S8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S8_f1_with_wb[] = "set-S8-f1!-with-wb";
static SCM set_S8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S8_f2[] = "set-S8-f2!";
static SCM set_S8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S8_f2_with_wb[] = "set-S8-f2!-with-wb";
static SCM set_S8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_S9_f1[] = "set-S9-f1!";
static SCM set_S9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_S9_f1_with_wb[] = "set-S9-f1!-with-wb";
static SCM set_S9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_S9_f2[] = "set-S9-f2!";
static SCM set_S9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_S9_f2_with_wb[] = "set-S9-f2!-with-wb";
static SCM set_S9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T0_f1[] = "set-T0-f1!";
static SCM set_T0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T0_f1_with_wb[] = "set-T0-f1!-with-wb";
static SCM set_T0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T0_f2[] = "set-T0-f2!";
static SCM set_T0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T0_f2_with_wb[] = "set-T0-f2!-with-wb";
static SCM set_T0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T1_f1[] = "set-T1-f1!";
static SCM set_T1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T1_f1_with_wb[] = "set-T1-f1!-with-wb";
static SCM set_T1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T1_f2[] = "set-T1-f2!";
static SCM set_T1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T1_f2_with_wb[] = "set-T1-f2!-with-wb";
static SCM set_T1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T2_f1[] = "set-T2-f1!";
static SCM set_T2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T2_f1_with_wb[] = "set-T2-f1!-with-wb";
static SCM set_T2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T2_f2[] = "set-T2-f2!";
static SCM set_T2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T2_f2_with_wb[] = "set-T2-f2!-with-wb";
static SCM set_T2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T3_f1[] = "set-T3-f1!";
static SCM set_T3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T3_f1_with_wb[] = "set-T3-f1!-with-wb";
static SCM set_T3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T3_f2[] = "set-T3-f2!";
static SCM set_T3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T3_f2_with_wb[] = "set-T3-f2!-with-wb";
static SCM set_T3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T4_f1[] = "set-T4-f1!";
static SCM set_T4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T4_f1_with_wb[] = "set-T4-f1!-with-wb";
static SCM set_T4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T4_f2[] = "set-T4-f2!";
static SCM set_T4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T4_f2_with_wb[] = "set-T4-f2!-with-wb";
static SCM set_T4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T5_f1[] = "set-T5-f1!";
static SCM set_T5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T5_f1_with_wb[] = "set-T5-f1!-with-wb";
static SCM set_T5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T5_f2[] = "set-T5-f2!";
static SCM set_T5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T5_f2_with_wb[] = "set-T5-f2!-with-wb";
static SCM set_T5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T6_f1[] = "set-T6-f1!";
static SCM set_T6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T6_f1_with_wb[] = "set-T6-f1!-with-wb";
static SCM set_T6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T6_f2[] = "set-T6-f2!";
static SCM set_T6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T6_f2_with_wb[] = "set-T6-f2!-with-wb";
static SCM set_T6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T7_f1[] = "set-T7-f1!";
static SCM set_T7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T7_f1_with_wb[] = "set-T7-f1!-with-wb";
static SCM set_T7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T7_f2[] = "set-T7-f2!";
static SCM set_T7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T7_f2_with_wb[] = "set-T7-f2!-with-wb";
static SCM set_T7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T8_f1[] = "set-T8-f1!";
static SCM set_T8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T8_f1_with_wb[] = "set-T8-f1!-with-wb";
static SCM set_T8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T8_f2[] = "set-T8-f2!";
static SCM set_T8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T8_f2_with_wb[] = "set-T8-f2!-with-wb";
static SCM set_T8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_T9_f1[] = "set-T9-f1!";
static SCM set_T9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_T9_f1_with_wb[] = "set-T9-f1!-with-wb";
static SCM set_T9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_T9_f2[] = "set-T9-f2!";
static SCM set_T9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_T9_f2_with_wb[] = "set-T9-f2!-with-wb";
static SCM set_T9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_LeakPath_first[] = "set-LeakPath-first!";
static SCM set_LeakPath_first(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_LeakPath_first_with_wb[] = "set-LeakPath-first!-with-wb";
static SCM set_LeakPath_first_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_LeakPath_last[] = "set-LeakPath-last!";
static SCM set_LeakPath_last(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_LeakPath_last_with_wb[] = "set-LeakPath-last!-with-wb";
static SCM set_LeakPath_last_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_LeakPath_len[] = "set-LeakPath-len!";
static SCM set_LeakPath_len(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_LeakPath_len_with_wb[] = "set-LeakPath-len!-with-wb";
static SCM set_LeakPath_len_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_make_internal_vector[] = "make-internal-vector";
static SCM make_internal_vector(SCM k, SCM fill) {
    SCM new_v = make_vector(MAKINUM(INUM(k) + 1L), fill);
    VELTS(new_v)[0] = internal_vector_symbol;
    return new_v;
}

static char s_internal_vector_set[] = "internal-vector-set!";
static SCM internal_vector_set(SCM v, SCM k, SCM obj) {
    if (!is_internal_vector(v)) {
        wta(v, (char *)ARG1, s_internal_vector_set);
    }
    return vector_set(v, MAKINUM(INUM(k) + 1L), obj);
}

static char s_internal_vector_ref[] = "internal-vector-ref";
static SCM internal_vector_ref(SCM v, SCM k) {
    if (!is_internal_vector(v)) {
        wta(v, (char *)ARG1, s_internal_vector_ref);
    }
    return vector_ref(v, MAKINUM(INUM(k) + 1L));
}

static char s_internal_vector_length[] = "internal-vector-length";
static SCM internal_vector_length(SCM v) {
    if (!is_internal_vector(v)) {
        wta(v, (char *)ARG1, s_internal_vector_length);
    }
    return MAKINUM(INUM(vector_length(v)) - 1L);
}

static iproc subr1s[] = {
        {s_internal_vector_length, internal_vector_length},
        {0, 0}
};

static iproc subr2s[] = {
        {s_c_define_data_type, c_define_data_type},
        {s_c_make_instance, c_make_instance},
        {s_c_data_type_predicate, c_data_type_predicate},
        {s_c_data_type_accessor, c_data_type_accessor},
        {s_internal_vector_ref, internal_vector_ref},
        {0, 0}
};

static iproc subr2os[] = {
        {s_make_internal_vector, make_internal_vector},
        {0, 0}
};

static iproc subr3s[] = {
        {s_set_linked_list_node_entry, set_linked_list_node_entry},
        {s_set_linked_list_node_entry_with_wb, set_linked_list_node_entry_with_wb},
        {s_set_linked_list_node_next_node, set_linked_list_node_next_node},
        {s_set_linked_list_node_next_node_with_wb, set_linked_list_node_next_node_with_wb},
        {s_set_linked_list_size, set_linked_list_size},
        {s_set_linked_list_size_with_wb, set_linked_list_size_with_wb},
        {s_set_linked_list_pred_proc_sym, set_linked_list_pred_proc_sym},
        {s_set_linked_list_pred_proc_sym_with_wb, set_linked_list_pred_proc_sym_with_wb},
        {s_set_linked_list_first_node, set_linked_list_first_node},
        {s_set_linked_list_first_node_with_wb, set_linked_list_first_node_with_wb},
        {s_set_linked_list_last_node, set_linked_list_last_node},
        {s_set_linked_list_last_node_with_wb, set_linked_list_last_node_with_wb},
        {s_set_array_list_data_block_data, set_array_list_data_block_data},
        {s_set_array_list_data_block_data_with_wb, set_array_list_data_block_data_with_wb},
        {s_set_array_list_size, set_array_list_size},
        {s_set_array_list_size_with_wb, set_array_list_size_with_wb},
        {s_set_array_list_pred_proc_sym, set_array_list_pred_proc_sym},
        {s_set_array_list_pred_proc_sym_with_wb, set_array_list_pred_proc_sym_with_wb},
        {s_set_array_list_data_blocks, set_array_list_data_blocks},
        {s_set_array_list_data_blocks_with_wb, set_array_list_data_blocks_with_wb},
        {s_set_hash_entry_k, set_hash_entry_k},
        {s_set_hash_entry_k_with_wb, set_hash_entry_k_with_wb},
        {s_set_hash_entry_v, set_hash_entry_v},
        {s_set_hash_entry_v_with_wb, set_hash_entry_v_with_wb},
        {s_set_hash_internal_size, set_hash_internal_size},
        {s_set_hash_internal_size_with_wb, set_hash_internal_size_with_wb},
        {s_set_hash_internal_key_pred_proc_sym, set_hash_internal_key_pred_proc_sym},
        {s_set_hash_internal_key_pred_proc_sym_with_wb, set_hash_internal_key_pred_proc_sym_with_wb},
        {s_set_hash_internal_value_pred_proc_sym, set_hash_internal_value_pred_proc_sym},
        {s_set_hash_internal_value_pred_proc_sym_with_wb, set_hash_internal_value_pred_proc_sym_with_wb},
        {s_set_hash_internal_used_bucket_size, set_hash_internal_used_bucket_size},
        {s_set_hash_internal_used_bucket_size_with_wb, set_hash_internal_used_bucket_size_with_wb},
        {s_set_hash_internal_buckets, set_hash_internal_buckets},
        {s_set_hash_internal_buckets_with_wb, set_hash_internal_buckets_with_wb},
        {s_set_hash_map_hash_internal, set_hash_map_hash_internal},
        {s_set_hash_map_hash_internal_with_wb, set_hash_map_hash_internal_with_wb},
        {s_set_Node_left, set_Node_left},
        {s_set_Node_left_with_wb, set_Node_left_with_wb},
        {s_set_Node_right, set_Node_right},
        {s_set_Node_right_with_wb, set_Node_right_with_wb},
        {s_set_Node_i, set_Node_i},
        {s_set_Node_i_with_wb, set_Node_i_with_wb},
        {s_set_Node_j, set_Node_j},
        {s_set_Node_j_with_wb, set_Node_j_with_wb},
        {s_set_A0_f1, set_A0_f1},
        {s_set_A0_f1_with_wb, set_A0_f1_with_wb},
        {s_set_A0_f2, set_A0_f2},
        {s_set_A0_f2_with_wb, set_A0_f2_with_wb},
        {s_set_A1_f1, set_A1_f1},
        {s_set_A1_f1_with_wb, set_A1_f1_with_wb},
        {s_set_A1_f2, set_A1_f2},
        {s_set_A1_f2_with_wb, set_A1_f2_with_wb},
        {s_set_A2_f1, set_A2_f1},
        {s_set_A2_f1_with_wb, set_A2_f1_with_wb},
        {s_set_A2_f2, set_A2_f2},
        {s_set_A2_f2_with_wb, set_A2_f2_with_wb},
        {s_set_A3_f1, set_A3_f1},
        {s_set_A3_f1_with_wb, set_A3_f1_with_wb},
        {s_set_A3_f2, set_A3_f2},
        {s_set_A3_f2_with_wb, set_A3_f2_with_wb},
        {s_set_A4_f1, set_A4_f1},
        {s_set_A4_f1_with_wb, set_A4_f1_with_wb},
        {s_set_A4_f2, set_A4_f2},
        {s_set_A4_f2_with_wb, set_A4_f2_with_wb},
        {s_set_A5_f1, set_A5_f1},
        {s_set_A5_f1_with_wb, set_A5_f1_with_wb},
        {s_set_A5_f2, set_A5_f2},
        {s_set_A5_f2_with_wb, set_A5_f2_with_wb},
        {s_set_A6_f1, set_A6_f1},
        {s_set_A6_f1_with_wb, set_A6_f1_with_wb},
        {s_set_A6_f2, set_A6_f2},
        {s_set_A6_f2_with_wb, set_A6_f2_with_wb},
        {s_set_A7_f1, set_A7_f1},
        {s_set_A7_f1_with_wb, set_A7_f1_with_wb},
        {s_set_A7_f2, set_A7_f2},
        {s_set_A7_f2_with_wb, set_A7_f2_with_wb},
        {s_set_A8_f1, set_A8_f1},
        {s_set_A8_f1_with_wb, set_A8_f1_with_wb},
        {s_set_A8_f2, set_A8_f2},
        {s_set_A8_f2_with_wb, set_A8_f2_with_wb},
        {s_set_A9_f1, set_A9_f1},
        {s_set_A9_f1_with_wb, set_A9_f1_with_wb},
        {s_set_A9_f2, set_A9_f2},
        {s_set_A9_f2_with_wb, set_A9_f2_with_wb},
        {s_set_B0_f1, set_B0_f1},
        {s_set_B0_f1_with_wb, set_B0_f1_with_wb},
        {s_set_B0_f2, set_B0_f2},
        {s_set_B0_f2_with_wb, set_B0_f2_with_wb},
        {s_set_B1_f1, set_B1_f1},
        {s_set_B1_f1_with_wb, set_B1_f1_with_wb},
        {s_set_B1_f2, set_B1_f2},
        {s_set_B1_f2_with_wb, set_B1_f2_with_wb},
        {s_set_B2_f1, set_B2_f1},
        {s_set_B2_f1_with_wb, set_B2_f1_with_wb},
        {s_set_B2_f2, set_B2_f2},
        {s_set_B2_f2_with_wb, set_B2_f2_with_wb},
        {s_set_B3_f1, set_B3_f1},
        {s_set_B3_f1_with_wb, set_B3_f1_with_wb},
        {s_set_B3_f2, set_B3_f2},
        {s_set_B3_f2_with_wb, set_B3_f2_with_wb},
        {s_set_B4_f1, set_B4_f1},
        {s_set_B4_f1_with_wb, set_B4_f1_with_wb},
        {s_set_B4_f2, set_B4_f2},
        {s_set_B4_f2_with_wb, set_B4_f2_with_wb},
        {s_set_B5_f1, set_B5_f1},
        {s_set_B5_f1_with_wb, set_B5_f1_with_wb},
        {s_set_B5_f2, set_B5_f2},
        {s_set_B5_f2_with_wb, set_B5_f2_with_wb},
        {s_set_B6_f1, set_B6_f1},
        {s_set_B6_f1_with_wb, set_B6_f1_with_wb},
        {s_set_B6_f2, set_B6_f2},
        {s_set_B6_f2_with_wb, set_B6_f2_with_wb},
        {s_set_B7_f1, set_B7_f1},
        {s_set_B7_f1_with_wb, set_B7_f1_with_wb},
        {s_set_B7_f2, set_B7_f2},
        {s_set_B7_f2_with_wb, set_B7_f2_with_wb},
        {s_set_B8_f1, set_B8_f1},
        {s_set_B8_f1_with_wb, set_B8_f1_with_wb},
        {s_set_B8_f2, set_B8_f2},
        {s_set_B8_f2_with_wb, set_B8_f2_with_wb},
        {s_set_B9_f1, set_B9_f1},
        {s_set_B9_f1_with_wb, set_B9_f1_with_wb},
        {s_set_B9_f2, set_B9_f2},
        {s_set_B9_f2_with_wb, set_B9_f2_with_wb},
        {s_set_C0_f1, set_C0_f1},
        {s_set_C0_f1_with_wb, set_C0_f1_with_wb},
        {s_set_C0_f2, set_C0_f2},
        {s_set_C0_f2_with_wb, set_C0_f2_with_wb},
        {s_set_C1_f1, set_C1_f1},
        {s_set_C1_f1_with_wb, set_C1_f1_with_wb},
        {s_set_C1_f2, set_C1_f2},
        {s_set_C1_f2_with_wb, set_C1_f2_with_wb},
        {s_set_C2_f1, set_C2_f1},
        {s_set_C2_f1_with_wb, set_C2_f1_with_wb},
        {s_set_C2_f2, set_C2_f2},
        {s_set_C2_f2_with_wb, set_C2_f2_with_wb},
        {s_set_C3_f1, set_C3_f1},
        {s_set_C3_f1_with_wb, set_C3_f1_with_wb},
        {s_set_C3_f2, set_C3_f2},
        {s_set_C3_f2_with_wb, set_C3_f2_with_wb},
        {s_set_C4_f1, set_C4_f1},
        {s_set_C4_f1_with_wb, set_C4_f1_with_wb},
        {s_set_C4_f2, set_C4_f2},
        {s_set_C4_f2_with_wb, set_C4_f2_with_wb},
        {s_set_C5_f1, set_C5_f1},
        {s_set_C5_f1_with_wb, set_C5_f1_with_wb},
        {s_set_C5_f2, set_C5_f2},
        {s_set_C5_f2_with_wb, set_C5_f2_with_wb},
        {s_set_C6_f1, set_C6_f1},
        {s_set_C6_f1_with_wb, set_C6_f1_with_wb},
        {s_set_C6_f2, set_C6_f2},
        {s_set_C6_f2_with_wb, set_C6_f2_with_wb},
        {s_set_C7_f1, set_C7_f1},
        {s_set_C7_f1_with_wb, set_C7_f1_with_wb},
        {s_set_C7_f2, set_C7_f2},
        {s_set_C7_f2_with_wb, set_C7_f2_with_wb},
        {s_set_C8_f1, set_C8_f1},
        {s_set_C8_f1_with_wb, set_C8_f1_with_wb},
        {s_set_C8_f2, set_C8_f2},
        {s_set_C8_f2_with_wb, set_C8_f2_with_wb},
        {s_set_C9_f1, set_C9_f1},
        {s_set_C9_f1_with_wb, set_C9_f1_with_wb},
        {s_set_C9_f2, set_C9_f2},
        {s_set_C9_f2_with_wb, set_C9_f2_with_wb},
        {s_set_D0_f1, set_D0_f1},
        {s_set_D0_f1_with_wb, set_D0_f1_with_wb},
        {s_set_D0_f2, set_D0_f2},
        {s_set_D0_f2_with_wb, set_D0_f2_with_wb},
        {s_set_D1_f1, set_D1_f1},
        {s_set_D1_f1_with_wb, set_D1_f1_with_wb},
        {s_set_D1_f2, set_D1_f2},
        {s_set_D1_f2_with_wb, set_D1_f2_with_wb},
        {s_set_D2_f1, set_D2_f1},
        {s_set_D2_f1_with_wb, set_D2_f1_with_wb},
        {s_set_D2_f2, set_D2_f2},
        {s_set_D2_f2_with_wb, set_D2_f2_with_wb},
        {s_set_D3_f1, set_D3_f1},
        {s_set_D3_f1_with_wb, set_D3_f1_with_wb},
        {s_set_D3_f2, set_D3_f2},
        {s_set_D3_f2_with_wb, set_D3_f2_with_wb},
        {s_set_D4_f1, set_D4_f1},
        {s_set_D4_f1_with_wb, set_D4_f1_with_wb},
        {s_set_D4_f2, set_D4_f2},
        {s_set_D4_f2_with_wb, set_D4_f2_with_wb},
        {s_set_D5_f1, set_D5_f1},
        {s_set_D5_f1_with_wb, set_D5_f1_with_wb},
        {s_set_D5_f2, set_D5_f2},
        {s_set_D5_f2_with_wb, set_D5_f2_with_wb},
        {s_set_D6_f1, set_D6_f1},
        {s_set_D6_f1_with_wb, set_D6_f1_with_wb},
        {s_set_D6_f2, set_D6_f2},
        {s_set_D6_f2_with_wb, set_D6_f2_with_wb},
        {s_set_D7_f1, set_D7_f1},
        {s_set_D7_f1_with_wb, set_D7_f1_with_wb},
        {s_set_D7_f2, set_D7_f2},
        {s_set_D7_f2_with_wb, set_D7_f2_with_wb},
        {s_set_D8_f1, set_D8_f1},
        {s_set_D8_f1_with_wb, set_D8_f1_with_wb},
        {s_set_D8_f2, set_D8_f2},
        {s_set_D8_f2_with_wb, set_D8_f2_with_wb},
        {s_set_D9_f1, set_D9_f1},
        {s_set_D9_f1_with_wb, set_D9_f1_with_wb},
        {s_set_D9_f2, set_D9_f2},
        {s_set_D9_f2_with_wb, set_D9_f2_with_wb},
        {s_set_E0_f1, set_E0_f1},
        {s_set_E0_f1_with_wb, set_E0_f1_with_wb},
        {s_set_E0_f2, set_E0_f2},
        {s_set_E0_f2_with_wb, set_E0_f2_with_wb},
        {s_set_E1_f1, set_E1_f1},
        {s_set_E1_f1_with_wb, set_E1_f1_with_wb},
        {s_set_E1_f2, set_E1_f2},
        {s_set_E1_f2_with_wb, set_E1_f2_with_wb},
        {s_set_E2_f1, set_E2_f1},
        {s_set_E2_f1_with_wb, set_E2_f1_with_wb},
        {s_set_E2_f2, set_E2_f2},
        {s_set_E2_f2_with_wb, set_E2_f2_with_wb},
        {s_set_E3_f1, set_E3_f1},
        {s_set_E3_f1_with_wb, set_E3_f1_with_wb},
        {s_set_E3_f2, set_E3_f2},
        {s_set_E3_f2_with_wb, set_E3_f2_with_wb},
        {s_set_E4_f1, set_E4_f1},
        {s_set_E4_f1_with_wb, set_E4_f1_with_wb},
        {s_set_E4_f2, set_E4_f2},
        {s_set_E4_f2_with_wb, set_E4_f2_with_wb},
        {s_set_E5_f1, set_E5_f1},
        {s_set_E5_f1_with_wb, set_E5_f1_with_wb},
        {s_set_E5_f2, set_E5_f2},
        {s_set_E5_f2_with_wb, set_E5_f2_with_wb},
        {s_set_E6_f1, set_E6_f1},
        {s_set_E6_f1_with_wb, set_E6_f1_with_wb},
        {s_set_E6_f2, set_E6_f2},
        {s_set_E6_f2_with_wb, set_E6_f2_with_wb},
        {s_set_E7_f1, set_E7_f1},
        {s_set_E7_f1_with_wb, set_E7_f1_with_wb},
        {s_set_E7_f2, set_E7_f2},
        {s_set_E7_f2_with_wb, set_E7_f2_with_wb},
        {s_set_E8_f1, set_E8_f1},
        {s_set_E8_f1_with_wb, set_E8_f1_with_wb},
        {s_set_E8_f2, set_E8_f2},
        {s_set_E8_f2_with_wb, set_E8_f2_with_wb},
        {s_set_E9_f1, set_E9_f1},
        {s_set_E9_f1_with_wb, set_E9_f1_with_wb},
        {s_set_E9_f2, set_E9_f2},
        {s_set_E9_f2_with_wb, set_E9_f2_with_wb},
        {s_set_F0_f1, set_F0_f1},
        {s_set_F0_f1_with_wb, set_F0_f1_with_wb},
        {s_set_F0_f2, set_F0_f2},
        {s_set_F0_f2_with_wb, set_F0_f2_with_wb},
        {s_set_F1_f1, set_F1_f1},
        {s_set_F1_f1_with_wb, set_F1_f1_with_wb},
        {s_set_F1_f2, set_F1_f2},
        {s_set_F1_f2_with_wb, set_F1_f2_with_wb},
        {s_set_F2_f1, set_F2_f1},
        {s_set_F2_f1_with_wb, set_F2_f1_with_wb},
        {s_set_F2_f2, set_F2_f2},
        {s_set_F2_f2_with_wb, set_F2_f2_with_wb},
        {s_set_F3_f1, set_F3_f1},
        {s_set_F3_f1_with_wb, set_F3_f1_with_wb},
        {s_set_F3_f2, set_F3_f2},
        {s_set_F3_f2_with_wb, set_F3_f2_with_wb},
        {s_set_F4_f1, set_F4_f1},
        {s_set_F4_f1_with_wb, set_F4_f1_with_wb},
        {s_set_F4_f2, set_F4_f2},
        {s_set_F4_f2_with_wb, set_F4_f2_with_wb},
        {s_set_F5_f1, set_F5_f1},
        {s_set_F5_f1_with_wb, set_F5_f1_with_wb},
        {s_set_F5_f2, set_F5_f2},
        {s_set_F5_f2_with_wb, set_F5_f2_with_wb},
        {s_set_F6_f1, set_F6_f1},
        {s_set_F6_f1_with_wb, set_F6_f1_with_wb},
        {s_set_F6_f2, set_F6_f2},
        {s_set_F6_f2_with_wb, set_F6_f2_with_wb},
        {s_set_F7_f1, set_F7_f1},
        {s_set_F7_f1_with_wb, set_F7_f1_with_wb},
        {s_set_F7_f2, set_F7_f2},
        {s_set_F7_f2_with_wb, set_F7_f2_with_wb},
        {s_set_F8_f1, set_F8_f1},
        {s_set_F8_f1_with_wb, set_F8_f1_with_wb},
        {s_set_F8_f2, set_F8_f2},
        {s_set_F8_f2_with_wb, set_F8_f2_with_wb},
        {s_set_F9_f1, set_F9_f1},
        {s_set_F9_f1_with_wb, set_F9_f1_with_wb},
        {s_set_F9_f2, set_F9_f2},
        {s_set_F9_f2_with_wb, set_F9_f2_with_wb},
        {s_set_G0_f1, set_G0_f1},
        {s_set_G0_f1_with_wb, set_G0_f1_with_wb},
        {s_set_G0_f2, set_G0_f2},
        {s_set_G0_f2_with_wb, set_G0_f2_with_wb},
        {s_set_G1_f1, set_G1_f1},
        {s_set_G1_f1_with_wb, set_G1_f1_with_wb},
        {s_set_G1_f2, set_G1_f2},
        {s_set_G1_f2_with_wb, set_G1_f2_with_wb},
        {s_set_G2_f1, set_G2_f1},
        {s_set_G2_f1_with_wb, set_G2_f1_with_wb},
        {s_set_G2_f2, set_G2_f2},
        {s_set_G2_f2_with_wb, set_G2_f2_with_wb},
        {s_set_G3_f1, set_G3_f1},
        {s_set_G3_f1_with_wb, set_G3_f1_with_wb},
        {s_set_G3_f2, set_G3_f2},
        {s_set_G3_f2_with_wb, set_G3_f2_with_wb},
        {s_set_G4_f1, set_G4_f1},
        {s_set_G4_f1_with_wb, set_G4_f1_with_wb},
        {s_set_G4_f2, set_G4_f2},
        {s_set_G4_f2_with_wb, set_G4_f2_with_wb},
        {s_set_G5_f1, set_G5_f1},
        {s_set_G5_f1_with_wb, set_G5_f1_with_wb},
        {s_set_G5_f2, set_G5_f2},
        {s_set_G5_f2_with_wb, set_G5_f2_with_wb},
        {s_set_G6_f1, set_G6_f1},
        {s_set_G6_f1_with_wb, set_G6_f1_with_wb},
        {s_set_G6_f2, set_G6_f2},
        {s_set_G6_f2_with_wb, set_G6_f2_with_wb},
        {s_set_G7_f1, set_G7_f1},
        {s_set_G7_f1_with_wb, set_G7_f1_with_wb},
        {s_set_G7_f2, set_G7_f2},
        {s_set_G7_f2_with_wb, set_G7_f2_with_wb},
        {s_set_G8_f1, set_G8_f1},
        {s_set_G8_f1_with_wb, set_G8_f1_with_wb},
        {s_set_G8_f2, set_G8_f2},
        {s_set_G8_f2_with_wb, set_G8_f2_with_wb},
        {s_set_G9_f1, set_G9_f1},
        {s_set_G9_f1_with_wb, set_G9_f1_with_wb},
        {s_set_G9_f2, set_G9_f2},
        {s_set_G9_f2_with_wb, set_G9_f2_with_wb},
        {s_set_H0_f1, set_H0_f1},
        {s_set_H0_f1_with_wb, set_H0_f1_with_wb},
        {s_set_H0_f2, set_H0_f2},
        {s_set_H0_f2_with_wb, set_H0_f2_with_wb},
        {s_set_H1_f1, set_H1_f1},
        {s_set_H1_f1_with_wb, set_H1_f1_with_wb},
        {s_set_H1_f2, set_H1_f2},
        {s_set_H1_f2_with_wb, set_H1_f2_with_wb},
        {s_set_H2_f1, set_H2_f1},
        {s_set_H2_f1_with_wb, set_H2_f1_with_wb},
        {s_set_H2_f2, set_H2_f2},
        {s_set_H2_f2_with_wb, set_H2_f2_with_wb},
        {s_set_H3_f1, set_H3_f1},
        {s_set_H3_f1_with_wb, set_H3_f1_with_wb},
        {s_set_H3_f2, set_H3_f2},
        {s_set_H3_f2_with_wb, set_H3_f2_with_wb},
        {s_set_H4_f1, set_H4_f1},
        {s_set_H4_f1_with_wb, set_H4_f1_with_wb},
        {s_set_H4_f2, set_H4_f2},
        {s_set_H4_f2_with_wb, set_H4_f2_with_wb},
        {s_set_H5_f1, set_H5_f1},
        {s_set_H5_f1_with_wb, set_H5_f1_with_wb},
        {s_set_H5_f2, set_H5_f2},
        {s_set_H5_f2_with_wb, set_H5_f2_with_wb},
        {s_set_H6_f1, set_H6_f1},
        {s_set_H6_f1_with_wb, set_H6_f1_with_wb},
        {s_set_H6_f2, set_H6_f2},
        {s_set_H6_f2_with_wb, set_H6_f2_with_wb},
        {s_set_H7_f1, set_H7_f1},
        {s_set_H7_f1_with_wb, set_H7_f1_with_wb},
        {s_set_H7_f2, set_H7_f2},
        {s_set_H7_f2_with_wb, set_H7_f2_with_wb},
        {s_set_H8_f1, set_H8_f1},
        {s_set_H8_f1_with_wb, set_H8_f1_with_wb},
        {s_set_H8_f2, set_H8_f2},
        {s_set_H8_f2_with_wb, set_H8_f2_with_wb},
        {s_set_H9_f1, set_H9_f1},
        {s_set_H9_f1_with_wb, set_H9_f1_with_wb},
        {s_set_H9_f2, set_H9_f2},
        {s_set_H9_f2_with_wb, set_H9_f2_with_wb},
        {s_set_I0_f1, set_I0_f1},
        {s_set_I0_f1_with_wb, set_I0_f1_with_wb},
        {s_set_I0_f2, set_I0_f2},
        {s_set_I0_f2_with_wb, set_I0_f2_with_wb},
        {s_set_I1_f1, set_I1_f1},
        {s_set_I1_f1_with_wb, set_I1_f1_with_wb},
        {s_set_I1_f2, set_I1_f2},
        {s_set_I1_f2_with_wb, set_I1_f2_with_wb},
        {s_set_I2_f1, set_I2_f1},
        {s_set_I2_f1_with_wb, set_I2_f1_with_wb},
        {s_set_I2_f2, set_I2_f2},
        {s_set_I2_f2_with_wb, set_I2_f2_with_wb},
        {s_set_I3_f1, set_I3_f1},
        {s_set_I3_f1_with_wb, set_I3_f1_with_wb},
        {s_set_I3_f2, set_I3_f2},
        {s_set_I3_f2_with_wb, set_I3_f2_with_wb},
        {s_set_I4_f1, set_I4_f1},
        {s_set_I4_f1_with_wb, set_I4_f1_with_wb},
        {s_set_I4_f2, set_I4_f2},
        {s_set_I4_f2_with_wb, set_I4_f2_with_wb},
        {s_set_I5_f1, set_I5_f1},
        {s_set_I5_f1_with_wb, set_I5_f1_with_wb},
        {s_set_I5_f2, set_I5_f2},
        {s_set_I5_f2_with_wb, set_I5_f2_with_wb},
        {s_set_I6_f1, set_I6_f1},
        {s_set_I6_f1_with_wb, set_I6_f1_with_wb},
        {s_set_I6_f2, set_I6_f2},
        {s_set_I6_f2_with_wb, set_I6_f2_with_wb},
        {s_set_I7_f1, set_I7_f1},
        {s_set_I7_f1_with_wb, set_I7_f1_with_wb},
        {s_set_I7_f2, set_I7_f2},
        {s_set_I7_f2_with_wb, set_I7_f2_with_wb},
        {s_set_I8_f1, set_I8_f1},
        {s_set_I8_f1_with_wb, set_I8_f1_with_wb},
        {s_set_I8_f2, set_I8_f2},
        {s_set_I8_f2_with_wb, set_I8_f2_with_wb},
        {s_set_I9_f1, set_I9_f1},
        {s_set_I9_f1_with_wb, set_I9_f1_with_wb},
        {s_set_I9_f2, set_I9_f2},
        {s_set_I9_f2_with_wb, set_I9_f2_with_wb},
        {s_set_J0_f1, set_J0_f1},
        {s_set_J0_f1_with_wb, set_J0_f1_with_wb},
        {s_set_J0_f2, set_J0_f2},
        {s_set_J0_f2_with_wb, set_J0_f2_with_wb},
        {s_set_J1_f1, set_J1_f1},
        {s_set_J1_f1_with_wb, set_J1_f1_with_wb},
        {s_set_J1_f2, set_J1_f2},
        {s_set_J1_f2_with_wb, set_J1_f2_with_wb},
        {s_set_J2_f1, set_J2_f1},
        {s_set_J2_f1_with_wb, set_J2_f1_with_wb},
        {s_set_J2_f2, set_J2_f2},
        {s_set_J2_f2_with_wb, set_J2_f2_with_wb},
        {s_set_J3_f1, set_J3_f1},
        {s_set_J3_f1_with_wb, set_J3_f1_with_wb},
        {s_set_J3_f2, set_J3_f2},
        {s_set_J3_f2_with_wb, set_J3_f2_with_wb},
        {s_set_J4_f1, set_J4_f1},
        {s_set_J4_f1_with_wb, set_J4_f1_with_wb},
        {s_set_J4_f2, set_J4_f2},
        {s_set_J4_f2_with_wb, set_J4_f2_with_wb},
        {s_set_J5_f1, set_J5_f1},
        {s_set_J5_f1_with_wb, set_J5_f1_with_wb},
        {s_set_J5_f2, set_J5_f2},
        {s_set_J5_f2_with_wb, set_J5_f2_with_wb},
        {s_set_J6_f1, set_J6_f1},
        {s_set_J6_f1_with_wb, set_J6_f1_with_wb},
        {s_set_J6_f2, set_J6_f2},
        {s_set_J6_f2_with_wb, set_J6_f2_with_wb},
        {s_set_J7_f1, set_J7_f1},
        {s_set_J7_f1_with_wb, set_J7_f1_with_wb},
        {s_set_J7_f2, set_J7_f2},
        {s_set_J7_f2_with_wb, set_J7_f2_with_wb},
        {s_set_J8_f1, set_J8_f1},
        {s_set_J8_f1_with_wb, set_J8_f1_with_wb},
        {s_set_J8_f2, set_J8_f2},
        {s_set_J8_f2_with_wb, set_J8_f2_with_wb},
        {s_set_J9_f1, set_J9_f1},
        {s_set_J9_f1_with_wb, set_J9_f1_with_wb},
        {s_set_J9_f2, set_J9_f2},
        {s_set_J9_f2_with_wb, set_J9_f2_with_wb},
        {s_set_K0_f1, set_K0_f1},
        {s_set_K0_f1_with_wb, set_K0_f1_with_wb},
        {s_set_K0_f2, set_K0_f2},
        {s_set_K0_f2_with_wb, set_K0_f2_with_wb},
        {s_set_K1_f1, set_K1_f1},
        {s_set_K1_f1_with_wb, set_K1_f1_with_wb},
        {s_set_K1_f2, set_K1_f2},
        {s_set_K1_f2_with_wb, set_K1_f2_with_wb},
        {s_set_K2_f1, set_K2_f1},
        {s_set_K2_f1_with_wb, set_K2_f1_with_wb},
        {s_set_K2_f2, set_K2_f2},
        {s_set_K2_f2_with_wb, set_K2_f2_with_wb},
        {s_set_K3_f1, set_K3_f1},
        {s_set_K3_f1_with_wb, set_K3_f1_with_wb},
        {s_set_K3_f2, set_K3_f2},
        {s_set_K3_f2_with_wb, set_K3_f2_with_wb},
        {s_set_K4_f1, set_K4_f1},
        {s_set_K4_f1_with_wb, set_K4_f1_with_wb},
        {s_set_K4_f2, set_K4_f2},
        {s_set_K4_f2_with_wb, set_K4_f2_with_wb},
        {s_set_K5_f1, set_K5_f1},
        {s_set_K5_f1_with_wb, set_K5_f1_with_wb},
        {s_set_K5_f2, set_K5_f2},
        {s_set_K5_f2_with_wb, set_K5_f2_with_wb},
        {s_set_K6_f1, set_K6_f1},
        {s_set_K6_f1_with_wb, set_K6_f1_with_wb},
        {s_set_K6_f2, set_K6_f2},
        {s_set_K6_f2_with_wb, set_K6_f2_with_wb},
        {s_set_K7_f1, set_K7_f1},
        {s_set_K7_f1_with_wb, set_K7_f1_with_wb},
        {s_set_K7_f2, set_K7_f2},
        {s_set_K7_f2_with_wb, set_K7_f2_with_wb},
        {s_set_K8_f1, set_K8_f1},
        {s_set_K8_f1_with_wb, set_K8_f1_with_wb},
        {s_set_K8_f2, set_K8_f2},
        {s_set_K8_f2_with_wb, set_K8_f2_with_wb},
        {s_set_K9_f1, set_K9_f1},
        {s_set_K9_f1_with_wb, set_K9_f1_with_wb},
        {s_set_K9_f2, set_K9_f2},
        {s_set_K9_f2_with_wb, set_K9_f2_with_wb},
        {s_set_L0_f1, set_L0_f1},
        {s_set_L0_f1_with_wb, set_L0_f1_with_wb},
        {s_set_L0_f2, set_L0_f2},
        {s_set_L0_f2_with_wb, set_L0_f2_with_wb},
        {s_set_L1_f1, set_L1_f1},
        {s_set_L1_f1_with_wb, set_L1_f1_with_wb},
        {s_set_L1_f2, set_L1_f2},
        {s_set_L1_f2_with_wb, set_L1_f2_with_wb},
        {s_set_L2_f1, set_L2_f1},
        {s_set_L2_f1_with_wb, set_L2_f1_with_wb},
        {s_set_L2_f2, set_L2_f2},
        {s_set_L2_f2_with_wb, set_L2_f2_with_wb},
        {s_set_L3_f1, set_L3_f1},
        {s_set_L3_f1_with_wb, set_L3_f1_with_wb},
        {s_set_L3_f2, set_L3_f2},
        {s_set_L3_f2_with_wb, set_L3_f2_with_wb},
        {s_set_L4_f1, set_L4_f1},
        {s_set_L4_f1_with_wb, set_L4_f1_with_wb},
        {s_set_L4_f2, set_L4_f2},
        {s_set_L4_f2_with_wb, set_L4_f2_with_wb},
        {s_set_L5_f1, set_L5_f1},
        {s_set_L5_f1_with_wb, set_L5_f1_with_wb},
        {s_set_L5_f2, set_L5_f2},
        {s_set_L5_f2_with_wb, set_L5_f2_with_wb},
        {s_set_L6_f1, set_L6_f1},
        {s_set_L6_f1_with_wb, set_L6_f1_with_wb},
        {s_set_L6_f2, set_L6_f2},
        {s_set_L6_f2_with_wb, set_L6_f2_with_wb},
        {s_set_L7_f1, set_L7_f1},
        {s_set_L7_f1_with_wb, set_L7_f1_with_wb},
        {s_set_L7_f2, set_L7_f2},
        {s_set_L7_f2_with_wb, set_L7_f2_with_wb},
        {s_set_L8_f1, set_L8_f1},
        {s_set_L8_f1_with_wb, set_L8_f1_with_wb},
        {s_set_L8_f2, set_L8_f2},
        {s_set_L8_f2_with_wb, set_L8_f2_with_wb},
        {s_set_L9_f1, set_L9_f1},
        {s_set_L9_f1_with_wb, set_L9_f1_with_wb},
        {s_set_L9_f2, set_L9_f2},
        {s_set_L9_f2_with_wb, set_L9_f2_with_wb},
        {s_set_M0_f1, set_M0_f1},
        {s_set_M0_f1_with_wb, set_M0_f1_with_wb},
        {s_set_M0_f2, set_M0_f2},
        {s_set_M0_f2_with_wb, set_M0_f2_with_wb},
        {s_set_M1_f1, set_M1_f1},
        {s_set_M1_f1_with_wb, set_M1_f1_with_wb},
        {s_set_M1_f2, set_M1_f2},
        {s_set_M1_f2_with_wb, set_M1_f2_with_wb},
        {s_set_M2_f1, set_M2_f1},
        {s_set_M2_f1_with_wb, set_M2_f1_with_wb},
        {s_set_M2_f2, set_M2_f2},
        {s_set_M2_f2_with_wb, set_M2_f2_with_wb},
        {s_set_M3_f1, set_M3_f1},
        {s_set_M3_f1_with_wb, set_M3_f1_with_wb},
        {s_set_M3_f2, set_M3_f2},
        {s_set_M3_f2_with_wb, set_M3_f2_with_wb},
        {s_set_M4_f1, set_M4_f1},
        {s_set_M4_f1_with_wb, set_M4_f1_with_wb},
        {s_set_M4_f2, set_M4_f2},
        {s_set_M4_f2_with_wb, set_M4_f2_with_wb},
        {s_set_M5_f1, set_M5_f1},
        {s_set_M5_f1_with_wb, set_M5_f1_with_wb},
        {s_set_M5_f2, set_M5_f2},
        {s_set_M5_f2_with_wb, set_M5_f2_with_wb},
        {s_set_M6_f1, set_M6_f1},
        {s_set_M6_f1_with_wb, set_M6_f1_with_wb},
        {s_set_M6_f2, set_M6_f2},
        {s_set_M6_f2_with_wb, set_M6_f2_with_wb},
        {s_set_M7_f1, set_M7_f1},
        {s_set_M7_f1_with_wb, set_M7_f1_with_wb},
        {s_set_M7_f2, set_M7_f2},
        {s_set_M7_f2_with_wb, set_M7_f2_with_wb},
        {s_set_M8_f1, set_M8_f1},
        {s_set_M8_f1_with_wb, set_M8_f1_with_wb},
        {s_set_M8_f2, set_M8_f2},
        {s_set_M8_f2_with_wb, set_M8_f2_with_wb},
        {s_set_M9_f1, set_M9_f1},
        {s_set_M9_f1_with_wb, set_M9_f1_with_wb},
        {s_set_M9_f2, set_M9_f2},
        {s_set_M9_f2_with_wb, set_M9_f2_with_wb},
        {s_set_N0_f1, set_N0_f1},
        {s_set_N0_f1_with_wb, set_N0_f1_with_wb},
        {s_set_N0_f2, set_N0_f2},
        {s_set_N0_f2_with_wb, set_N0_f2_with_wb},
        {s_set_N1_f1, set_N1_f1},
        {s_set_N1_f1_with_wb, set_N1_f1_with_wb},
        {s_set_N1_f2, set_N1_f2},
        {s_set_N1_f2_with_wb, set_N1_f2_with_wb},
        {s_set_N2_f1, set_N2_f1},
        {s_set_N2_f1_with_wb, set_N2_f1_with_wb},
        {s_set_N2_f2, set_N2_f2},
        {s_set_N2_f2_with_wb, set_N2_f2_with_wb},
        {s_set_N3_f1, set_N3_f1},
        {s_set_N3_f1_with_wb, set_N3_f1_with_wb},
        {s_set_N3_f2, set_N3_f2},
        {s_set_N3_f2_with_wb, set_N3_f2_with_wb},
        {s_set_N4_f1, set_N4_f1},
        {s_set_N4_f1_with_wb, set_N4_f1_with_wb},
        {s_set_N4_f2, set_N4_f2},
        {s_set_N4_f2_with_wb, set_N4_f2_with_wb},
        {s_set_N5_f1, set_N5_f1},
        {s_set_N5_f1_with_wb, set_N5_f1_with_wb},
        {s_set_N5_f2, set_N5_f2},
        {s_set_N5_f2_with_wb, set_N5_f2_with_wb},
        {s_set_N6_f1, set_N6_f1},
        {s_set_N6_f1_with_wb, set_N6_f1_with_wb},
        {s_set_N6_f2, set_N6_f2},
        {s_set_N6_f2_with_wb, set_N6_f2_with_wb},
        {s_set_N7_f1, set_N7_f1},
        {s_set_N7_f1_with_wb, set_N7_f1_with_wb},
        {s_set_N7_f2, set_N7_f2},
        {s_set_N7_f2_with_wb, set_N7_f2_with_wb},
        {s_set_N8_f1, set_N8_f1},
        {s_set_N8_f1_with_wb, set_N8_f1_with_wb},
        {s_set_N8_f2, set_N8_f2},
        {s_set_N8_f2_with_wb, set_N8_f2_with_wb},
        {s_set_N9_f1, set_N9_f1},
        {s_set_N9_f1_with_wb, set_N9_f1_with_wb},
        {s_set_N9_f2, set_N9_f2},
        {s_set_N9_f2_with_wb, set_N9_f2_with_wb},
        {s_set_O0_f1, set_O0_f1},
        {s_set_O0_f1_with_wb, set_O0_f1_with_wb},
        {s_set_O0_f2, set_O0_f2},
        {s_set_O0_f2_with_wb, set_O0_f2_with_wb},
        {s_set_O1_f1, set_O1_f1},
        {s_set_O1_f1_with_wb, set_O1_f1_with_wb},
        {s_set_O1_f2, set_O1_f2},
        {s_set_O1_f2_with_wb, set_O1_f2_with_wb},
        {s_set_O2_f1, set_O2_f1},
        {s_set_O2_f1_with_wb, set_O2_f1_with_wb},
        {s_set_O2_f2, set_O2_f2},
        {s_set_O2_f2_with_wb, set_O2_f2_with_wb},
        {s_set_O3_f1, set_O3_f1},
        {s_set_O3_f1_with_wb, set_O3_f1_with_wb},
        {s_set_O3_f2, set_O3_f2},
        {s_set_O3_f2_with_wb, set_O3_f2_with_wb},
        {s_set_O4_f1, set_O4_f1},
        {s_set_O4_f1_with_wb, set_O4_f1_with_wb},
        {s_set_O4_f2, set_O4_f2},
        {s_set_O4_f2_with_wb, set_O4_f2_with_wb},
        {s_set_O5_f1, set_O5_f1},
        {s_set_O5_f1_with_wb, set_O5_f1_with_wb},
        {s_set_O5_f2, set_O5_f2},
        {s_set_O5_f2_with_wb, set_O5_f2_with_wb},
        {s_set_O6_f1, set_O6_f1},
        {s_set_O6_f1_with_wb, set_O6_f1_with_wb},
        {s_set_O6_f2, set_O6_f2},
        {s_set_O6_f2_with_wb, set_O6_f2_with_wb},
        {s_set_O7_f1, set_O7_f1},
        {s_set_O7_f1_with_wb, set_O7_f1_with_wb},
        {s_set_O7_f2, set_O7_f2},
        {s_set_O7_f2_with_wb, set_O7_f2_with_wb},
        {s_set_O8_f1, set_O8_f1},
        {s_set_O8_f1_with_wb, set_O8_f1_with_wb},
        {s_set_O8_f2, set_O8_f2},
        {s_set_O8_f2_with_wb, set_O8_f2_with_wb},
        {s_set_O9_f1, set_O9_f1},
        {s_set_O9_f1_with_wb, set_O9_f1_with_wb},
        {s_set_O9_f2, set_O9_f2},
        {s_set_O9_f2_with_wb, set_O9_f2_with_wb},
        {s_set_P0_f1, set_P0_f1},
        {s_set_P0_f1_with_wb, set_P0_f1_with_wb},
        {s_set_P0_f2, set_P0_f2},
        {s_set_P0_f2_with_wb, set_P0_f2_with_wb},
        {s_set_P1_f1, set_P1_f1},
        {s_set_P1_f1_with_wb, set_P1_f1_with_wb},
        {s_set_P1_f2, set_P1_f2},
        {s_set_P1_f2_with_wb, set_P1_f2_with_wb},
        {s_set_P2_f1, set_P2_f1},
        {s_set_P2_f1_with_wb, set_P2_f1_with_wb},
        {s_set_P2_f2, set_P2_f2},
        {s_set_P2_f2_with_wb, set_P2_f2_with_wb},
        {s_set_P3_f1, set_P3_f1},
        {s_set_P3_f1_with_wb, set_P3_f1_with_wb},
        {s_set_P3_f2, set_P3_f2},
        {s_set_P3_f2_with_wb, set_P3_f2_with_wb},
        {s_set_P4_f1, set_P4_f1},
        {s_set_P4_f1_with_wb, set_P4_f1_with_wb},
        {s_set_P4_f2, set_P4_f2},
        {s_set_P4_f2_with_wb, set_P4_f2_with_wb},
        {s_set_P5_f1, set_P5_f1},
        {s_set_P5_f1_with_wb, set_P5_f1_with_wb},
        {s_set_P5_f2, set_P5_f2},
        {s_set_P5_f2_with_wb, set_P5_f2_with_wb},
        {s_set_P6_f1, set_P6_f1},
        {s_set_P6_f1_with_wb, set_P6_f1_with_wb},
        {s_set_P6_f2, set_P6_f2},
        {s_set_P6_f2_with_wb, set_P6_f2_with_wb},
        {s_set_P7_f1, set_P7_f1},
        {s_set_P7_f1_with_wb, set_P7_f1_with_wb},
        {s_set_P7_f2, set_P7_f2},
        {s_set_P7_f2_with_wb, set_P7_f2_with_wb},
        {s_set_P8_f1, set_P8_f1},
        {s_set_P8_f1_with_wb, set_P8_f1_with_wb},
        {s_set_P8_f2, set_P8_f2},
        {s_set_P8_f2_with_wb, set_P8_f2_with_wb},
        {s_set_P9_f1, set_P9_f1},
        {s_set_P9_f1_with_wb, set_P9_f1_with_wb},
        {s_set_P9_f2, set_P9_f2},
        {s_set_P9_f2_with_wb, set_P9_f2_with_wb},
        {s_set_Q0_f1, set_Q0_f1},
        {s_set_Q0_f1_with_wb, set_Q0_f1_with_wb},
        {s_set_Q0_f2, set_Q0_f2},
        {s_set_Q0_f2_with_wb, set_Q0_f2_with_wb},
        {s_set_Q1_f1, set_Q1_f1},
        {s_set_Q1_f1_with_wb, set_Q1_f1_with_wb},
        {s_set_Q1_f2, set_Q1_f2},
        {s_set_Q1_f2_with_wb, set_Q1_f2_with_wb},
        {s_set_Q2_f1, set_Q2_f1},
        {s_set_Q2_f1_with_wb, set_Q2_f1_with_wb},
        {s_set_Q2_f2, set_Q2_f2},
        {s_set_Q2_f2_with_wb, set_Q2_f2_with_wb},
        {s_set_Q3_f1, set_Q3_f1},
        {s_set_Q3_f1_with_wb, set_Q3_f1_with_wb},
        {s_set_Q3_f2, set_Q3_f2},
        {s_set_Q3_f2_with_wb, set_Q3_f2_with_wb},
        {s_set_Q4_f1, set_Q4_f1},
        {s_set_Q4_f1_with_wb, set_Q4_f1_with_wb},
        {s_set_Q4_f2, set_Q4_f2},
        {s_set_Q4_f2_with_wb, set_Q4_f2_with_wb},
        {s_set_Q5_f1, set_Q5_f1},
        {s_set_Q5_f1_with_wb, set_Q5_f1_with_wb},
        {s_set_Q5_f2, set_Q5_f2},
        {s_set_Q5_f2_with_wb, set_Q5_f2_with_wb},
        {s_set_Q6_f1, set_Q6_f1},
        {s_set_Q6_f1_with_wb, set_Q6_f1_with_wb},
        {s_set_Q6_f2, set_Q6_f2},
        {s_set_Q6_f2_with_wb, set_Q6_f2_with_wb},
        {s_set_Q7_f1, set_Q7_f1},
        {s_set_Q7_f1_with_wb, set_Q7_f1_with_wb},
        {s_set_Q7_f2, set_Q7_f2},
        {s_set_Q7_f2_with_wb, set_Q7_f2_with_wb},
        {s_set_Q8_f1, set_Q8_f1},
        {s_set_Q8_f1_with_wb, set_Q8_f1_with_wb},
        {s_set_Q8_f2, set_Q8_f2},
        {s_set_Q8_f2_with_wb, set_Q8_f2_with_wb},
        {s_set_Q9_f1, set_Q9_f1},
        {s_set_Q9_f1_with_wb, set_Q9_f1_with_wb},
        {s_set_Q9_f2, set_Q9_f2},
        {s_set_Q9_f2_with_wb, set_Q9_f2_with_wb},
        {s_set_R0_f1, set_R0_f1},
        {s_set_R0_f1_with_wb, set_R0_f1_with_wb},
        {s_set_R0_f2, set_R0_f2},
        {s_set_R0_f2_with_wb, set_R0_f2_with_wb},
        {s_set_R1_f1, set_R1_f1},
        {s_set_R1_f1_with_wb, set_R1_f1_with_wb},
        {s_set_R1_f2, set_R1_f2},
        {s_set_R1_f2_with_wb, set_R1_f2_with_wb},
        {s_set_R2_f1, set_R2_f1},
        {s_set_R2_f1_with_wb, set_R2_f1_with_wb},
        {s_set_R2_f2, set_R2_f2},
        {s_set_R2_f2_with_wb, set_R2_f2_with_wb},
        {s_set_R3_f1, set_R3_f1},
        {s_set_R3_f1_with_wb, set_R3_f1_with_wb},
        {s_set_R3_f2, set_R3_f2},
        {s_set_R3_f2_with_wb, set_R3_f2_with_wb},
        {s_set_R4_f1, set_R4_f1},
        {s_set_R4_f1_with_wb, set_R4_f1_with_wb},
        {s_set_R4_f2, set_R4_f2},
        {s_set_R4_f2_with_wb, set_R4_f2_with_wb},
        {s_set_R5_f1, set_R5_f1},
        {s_set_R5_f1_with_wb, set_R5_f1_with_wb},
        {s_set_R5_f2, set_R5_f2},
        {s_set_R5_f2_with_wb, set_R5_f2_with_wb},
        {s_set_R6_f1, set_R6_f1},
        {s_set_R6_f1_with_wb, set_R6_f1_with_wb},
        {s_set_R6_f2, set_R6_f2},
        {s_set_R6_f2_with_wb, set_R6_f2_with_wb},
        {s_set_R7_f1, set_R7_f1},
        {s_set_R7_f1_with_wb, set_R7_f1_with_wb},
        {s_set_R7_f2, set_R7_f2},
        {s_set_R7_f2_with_wb, set_R7_f2_with_wb},
        {s_set_R8_f1, set_R8_f1},
        {s_set_R8_f1_with_wb, set_R8_f1_with_wb},
        {s_set_R8_f2, set_R8_f2},
        {s_set_R8_f2_with_wb, set_R8_f2_with_wb},
        {s_set_R9_f1, set_R9_f1},
        {s_set_R9_f1_with_wb, set_R9_f1_with_wb},
        {s_set_R9_f2, set_R9_f2},
        {s_set_R9_f2_with_wb, set_R9_f2_with_wb},
        {s_set_S0_f1, set_S0_f1},
        {s_set_S0_f1_with_wb, set_S0_f1_with_wb},
        {s_set_S0_f2, set_S0_f2},
        {s_set_S0_f2_with_wb, set_S0_f2_with_wb},
        {s_set_S1_f1, set_S1_f1},
        {s_set_S1_f1_with_wb, set_S1_f1_with_wb},
        {s_set_S1_f2, set_S1_f2},
        {s_set_S1_f2_with_wb, set_S1_f2_with_wb},
        {s_set_S2_f1, set_S2_f1},
        {s_set_S2_f1_with_wb, set_S2_f1_with_wb},
        {s_set_S2_f2, set_S2_f2},
        {s_set_S2_f2_with_wb, set_S2_f2_with_wb},
        {s_set_S3_f1, set_S3_f1},
        {s_set_S3_f1_with_wb, set_S3_f1_with_wb},
        {s_set_S3_f2, set_S3_f2},
        {s_set_S3_f2_with_wb, set_S3_f2_with_wb},
        {s_set_S4_f1, set_S4_f1},
        {s_set_S4_f1_with_wb, set_S4_f1_with_wb},
        {s_set_S4_f2, set_S4_f2},
        {s_set_S4_f2_with_wb, set_S4_f2_with_wb},
        {s_set_S5_f1, set_S5_f1},
        {s_set_S5_f1_with_wb, set_S5_f1_with_wb},
        {s_set_S5_f2, set_S5_f2},
        {s_set_S5_f2_with_wb, set_S5_f2_with_wb},
        {s_set_S6_f1, set_S6_f1},
        {s_set_S6_f1_with_wb, set_S6_f1_with_wb},
        {s_set_S6_f2, set_S6_f2},
        {s_set_S6_f2_with_wb, set_S6_f2_with_wb},
        {s_set_S7_f1, set_S7_f1},
        {s_set_S7_f1_with_wb, set_S7_f1_with_wb},
        {s_set_S7_f2, set_S7_f2},
        {s_set_S7_f2_with_wb, set_S7_f2_with_wb},
        {s_set_S8_f1, set_S8_f1},
        {s_set_S8_f1_with_wb, set_S8_f1_with_wb},
        {s_set_S8_f2, set_S8_f2},
        {s_set_S8_f2_with_wb, set_S8_f2_with_wb},
        {s_set_S9_f1, set_S9_f1},
        {s_set_S9_f1_with_wb, set_S9_f1_with_wb},
        {s_set_S9_f2, set_S9_f2},
        {s_set_S9_f2_with_wb, set_S9_f2_with_wb},
        {s_set_T0_f1, set_T0_f1},
        {s_set_T0_f1_with_wb, set_T0_f1_with_wb},
        {s_set_T0_f2, set_T0_f2},
        {s_set_T0_f2_with_wb, set_T0_f2_with_wb},
        {s_set_T1_f1, set_T1_f1},
        {s_set_T1_f1_with_wb, set_T1_f1_with_wb},
        {s_set_T1_f2, set_T1_f2},
        {s_set_T1_f2_with_wb, set_T1_f2_with_wb},
        {s_set_T2_f1, set_T2_f1},
        {s_set_T2_f1_with_wb, set_T2_f1_with_wb},
        {s_set_T2_f2, set_T2_f2},
        {s_set_T2_f2_with_wb, set_T2_f2_with_wb},
        {s_set_T3_f1, set_T3_f1},
        {s_set_T3_f1_with_wb, set_T3_f1_with_wb},
        {s_set_T3_f2, set_T3_f2},
        {s_set_T3_f2_with_wb, set_T3_f2_with_wb},
        {s_set_T4_f1, set_T4_f1},
        {s_set_T4_f1_with_wb, set_T4_f1_with_wb},
        {s_set_T4_f2, set_T4_f2},
        {s_set_T4_f2_with_wb, set_T4_f2_with_wb},
        {s_set_T5_f1, set_T5_f1},
        {s_set_T5_f1_with_wb, set_T5_f1_with_wb},
        {s_set_T5_f2, set_T5_f2},
        {s_set_T5_f2_with_wb, set_T5_f2_with_wb},
        {s_set_T6_f1, set_T6_f1},
        {s_set_T6_f1_with_wb, set_T6_f1_with_wb},
        {s_set_T6_f2, set_T6_f2},
        {s_set_T6_f2_with_wb, set_T6_f2_with_wb},
        {s_set_T7_f1, set_T7_f1},
        {s_set_T7_f1_with_wb, set_T7_f1_with_wb},
        {s_set_T7_f2, set_T7_f2},
        {s_set_T7_f2_with_wb, set_T7_f2_with_wb},
        {s_set_T8_f1, set_T8_f1},
        {s_set_T8_f1_with_wb, set_T8_f1_with_wb},
        {s_set_T8_f2, set_T8_f2},
        {s_set_T8_f2_with_wb, set_T8_f2_with_wb},
        {s_set_T9_f1, set_T9_f1},
        {s_set_T9_f1_with_wb, set_T9_f1_with_wb},
        {s_set_T9_f2, set_T9_f2},
        {s_set_T9_f2_with_wb, set_T9_f2_with_wb},
        {s_set_LeakPath_first, set_LeakPath_first},
        {s_set_LeakPath_first_with_wb, set_LeakPath_first_with_wb},
        {s_set_LeakPath_last, set_LeakPath_last},
        {s_set_LeakPath_last_with_wb, set_LeakPath_last_with_wb},
        {s_set_LeakPath_len, set_LeakPath_len},
        {s_set_LeakPath_len_with_wb, set_LeakPath_len_with_wb},
        {s_internal_vector_set, internal_vector_set},
        {0, 0}
};

void init_define_data_type() {
    module_flag_symbol = CAR(sysintern(s_user_define_datatype, makfrom0str(s_user_define_datatype)));
    internal_vector_symbol = CAR(sysintern(s_internal_vector, makfrom0str(s_internal_vector)));
    init_iprocs(subr1s, tc7_subr_1);
    init_iprocs(subr2s, tc7_subr_2);
    init_iprocs(subr2os, tc7_subr_2o);
    init_iprocs(subr3s, tc7_subr_3);
    add_feature("definedatatype");
}
