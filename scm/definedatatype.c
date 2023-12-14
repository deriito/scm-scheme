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
 * 1: DataTypeDefWithRecSlots <= [module-flag-sym, internal-code, type-name, [internal-vector-sym, field-name1, ...],
 *                                 [internal-vector-sym, [rtf1], [rtf2], ..., [rtfn]]] // rec_slot_defs
 *
 *    [rtfn] <= [internal-vector-sym, ref-type-def1, ref-type-def2, ..., ref-type-defn] // ref_types_of_field
 *
 * 2: DataTypeInstance <= [module-flag-sym, internal-code, data-type-def, [internal-vector-sym, field-value1, ...], null]
 * 3: DataTypeInstanceWithRecSlots <= [module-flag-sym, internal-code, data-type-def, [internal-vector-sym, field-value1, ...],
 *                                      [internal-vector-sym, [rsf1], [rsf2], ..., [rsfn]]] // rec_slots_vector
 *
 *    [rsfn] <= [internal-vector-sym, [lnvrt1], ..., [lnvrtn]] // rec_slot_of_field_n
 *    [lnvrtn] <= [internal-vector-sym, ref-type-defn, used-len, ln1, ln2, null, ...] // line_num_vector_of_ref_type_n
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
    SCM data_type_instance = make_vector(DTI_VECTOR_LEN, UNDEFINED);

    MFS(data_type_instance) = module_flag_symbol;
    IC(data_type_instance) = (IC(data_type_def) == DTDWRS_IT_CODE) ? DTIWRS_IT_CODE : DTI_IT_CODE; // for initialization
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

    // 行番号記録スロットの初期化
    if (IC(data_type_def) == DTDWRS_IT_CODE) {
        SCM rsv = make_vector(MAKINUM(1L + len), EOL); // rec_slots_vector
        vector_set(rsv, MAKINUM(0), internal_vector_symbol);

        SCM rec_slot_defs = DTD_RSD(data_type_def);
        for (long idx = 0; idx < len; ++idx) {
            SCM ref_types_of_field = VELTS(rec_slot_defs)[idx + 1L];
            SCM len_of_ref_types_of_field = NULLP(ref_types_of_field) ? 0 : vector_length(ref_types_of_field);

            if (INUM(len_of_ref_types_of_field) > 1) {
                SCM rec_slot_of_field = make_vector(len_of_ref_types_of_field, internal_vector_symbol);
                for (long j = 1; j < INUM(len_of_ref_types_of_field); ++j) {
                    SCM line_num_vector_of_ref_type = make_vector(MAKINUM(FIELD_REF_INFO_ALLOCATED_LEN), EOL);
                    vector_set(line_num_vector_of_ref_type, MAKINUM(0), internal_vector_symbol);
                    vector_set(line_num_vector_of_ref_type, MAKINUM(1), VELTS(ref_types_of_field)[j]);
                    vector_set(line_num_vector_of_ref_type, MAKINUM(2), MAKINUM(3)); // used-len
                    vector_set(rec_slot_of_field, MAKINUM(j), line_num_vector_of_ref_type);
                }
                vector_set(rsv, MAKINUM(idx + 1L), rec_slot_of_field);
            }
        }

        DTI_RSV(data_type_instance) = rsv;
    }

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

void set_ref_path_info_recorded(SCM scm_obj) {
    if (!is_user_defined_data_type_instance(scm_obj)) {
        return;
    }
    SET_IC_BIT(scm_obj, 4);
}

static char s_is_ref_path_info_recorded[] = "is_ref_path_info_recorded";
char is_ref_path_info_recorded(SCM scm_obj) {
    if (!is_user_defined_data_type_instance(scm_obj)) {
        wta(scm_obj, (char *)ARG1, s_is_ref_path_info_recorded);
    }
    return READ_REF_PATH_INFO_RECORDED_MARK(scm_obj);
}

static char s_get_data_type_def_identifier[] = "get_data_type_def_identifier";
SCM get_data_type_def_identifier(SCM scm_obj) {
    if (!is_user_defined_data_type_instance(scm_obj)) {
        wta(scm_obj, (char *)ARG1, s_get_data_type_def_identifier);
    }

    return DTI_DTD(scm_obj);
}

static char s_field_number[] = "field_number";
long field_number(SCM data_type_def) {
    if (!is_user_defined_data_type(data_type_def)) {
        wta(data_type_def, (char *)ARG1, s_field_number);
    }

    if (IC(data_type_def) != DTD_IT_CODE && IC(data_type_def) != DTDWRS_IT_CODE) {
        wta(data_type_def, (char *)ARG1, s_field_number);
    }

    return (INUM(vector_length(DTD_FNV(data_type_def))) - 1L);
}

static char s_data_type_field_name[] = "data_type_field_name";
char *data_type_field_name(SCM data_type_def, long field_index) {
    if (!is_user_defined_data_type(data_type_def)) {
        wta(data_type_def, (char *)ARG1, s_data_type_field_name);
    }

    SCM field_names_vector = DTD_FNV(data_type_def);
    long field_num = INUM(vector_length(field_names_vector)) - 1L;
    if (field_index < 0 || field_num <= field_index) {
        wta(field_index, (char *)ARG2, s_data_type_field_name);
    }

    SCM field_name = VELTS(field_names_vector)[field_index + 1L];
    return CHARS(field_name);
}

static char s_data_type_name[] = "data_type_name";
char *data_type_name(SCM data_type_def) {
    if (!is_user_defined_data_type(data_type_def)) {
        wta(data_type_def, (char *)ARG1, s_data_type_name);
    }

    if (IC(data_type_def) != DTD_IT_CODE && IC(data_type_def) != DTDWRS_IT_CODE) {
        wta(data_type_def, (char *)ARG1, s_data_type_name);
    }

    return CHARS(DTD_TN(data_type_def));
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

char is_user_defined_data_type_instance_with_rec_slots(SCM scm_obj) {
    if (!is_user_defined_data_type_instance(scm_obj)) {
        return 0;
    }
    return READ_IT(scm_obj) == DTIWRS_IT_CODE;
}

/**
 *
 * @param scm_obj
 * @param field_index
 * @return maybe return EOL
 */
SCM get_rec_slot_of_field(SCM scm_obj, long field_index) {
    if (!is_user_defined_data_type_instance_with_rec_slots(scm_obj)) {
        return EOL;
    }

    SCM rec_slots_vector = DTI_RSV(scm_obj);

    return VELTS(rec_slots_vector)[field_index + 1L];
}

void try_update_data_type_def(WriteBarrierUpdateMetadata *metadata) {
    if (HASH_COUNT(metadata->field_ref_info) <= 0) {
        return;
    }

    SCM class_obj = metadata->data_type_def;
    long field_num = field_number(class_obj);

    SCM new_rec_slot_defs = EOL;
    for (long field_idx = 0; field_idx < field_num; ++field_idx) {
        MetadataPerField *field_ref_data_type_info;
        HASH_FIND(hh, metadata->field_ref_info, &field_idx, sizeof(long), field_ref_data_type_info);

        long ref_num_to_add = 0;
        if (NULL != field_ref_data_type_info) {
            UpdateStateByRefType *e, *t;
            HASH_ITER(hh, field_ref_data_type_info->update_state_by_ref_type, e, t) {
                if (e->is_to_add) ref_num_to_add += 1L;
            }
        }

        if (ref_num_to_add <= 0) {
            continue;
        }

        if (NULLP(new_rec_slot_defs)) {
            new_rec_slot_defs = make_vector(MAKINUM(field_num + 1L), EOL);
            vector_set(new_rec_slot_defs, MAKINUM(0), internal_vector_symbol);
        }

        SCM new_ref_types_of_field = make_vector(MAKINUM(ref_num_to_add + 1L), internal_vector_symbol);
        UpdateStateByRefType *element, *temp;
        long prev_set_index = 0;
        HASH_ITER(hh, field_ref_data_type_info->update_state_by_ref_type, element, temp) {
            if (element->is_to_add) {
                VELTS(new_ref_types_of_field)[prev_set_index + 1L] = element->ref_data_type;
                prev_set_index += 1L;
            }
        }
        VELTS(new_rec_slot_defs)[field_idx + 1L] = new_ref_types_of_field;
    }

    DTD_RSD(class_obj) = new_rec_slot_defs;
    IC(class_obj) = NULLP(new_rec_slot_defs) ? DTD_IT_CODE : DTDWRS_IT_CODE;
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

    // try process write barrier
    if (!is_user_defined_data_type_instance_with_rec_slots(obj)) {
        return UNSPECIFIED;
    }

    SCM rec_slot_of_field = get_rec_slot_of_field(obj, index);
    if (NULLP(rec_slot_of_field)) {
        return UNSPECIFIED;
    }

    long len = INUM(vector_length(rec_slot_of_field));

    SCM line_num_vector_of_ref_type = EOL;
    long idx = -1;
    for (long i = 1; i < len; ++i) {
        SCM tmp_ln_v_of_ref_type = VELTS(rec_slot_of_field)[i];
        SCM classobj_or_fixed_type_code_of_value;
        if (is_user_defined_data_type_instance(value)) {
            classobj_or_fixed_type_code_of_value = DTI_DTD(value);
        } else {
            classobj_or_fixed_type_code_of_value = IMP(value) ? -1L : TYP7(value);
        }
        if (VELTS(tmp_ln_v_of_ref_type)[1] == classobj_or_fixed_type_code_of_value) {
            line_num_vector_of_ref_type = tmp_ln_v_of_ref_type;
            idx = i;
            break;
        }
    }

    if (NULLP(line_num_vector_of_ref_type)) {
        return UNSPECIFIED;
    }

    long used_len = INUM(VELTS(line_num_vector_of_ref_type)[2]);

    for (long i = 3; i < used_len; ++i) {
        if (INUM(VELTS(line_num_vector_of_ref_type)[i]) == INUM(ln_num)) {
            return UNSPECIFIED;
        }
    }

    // 拡張してみる
    if (used_len >= INUM(vector_length(line_num_vector_of_ref_type))) {
        long new_len = used_len + FIELD_REF_INFO_ALLOCATED_LEN; // TODO [dirty] 每次多扩张6个就空位
        SCM new_ln_v_of_ref_type = make_vector(MAKINUM(new_len), EOL);
        for (long i = 0; i < used_len; ++i) {
            VELTS(new_ln_v_of_ref_type)[i] = VELTS(line_num_vector_of_ref_type)[i];
        }
        VELTS(rec_slot_of_field)[idx] = new_ln_v_of_ref_type;
        line_num_vector_of_ref_type = new_ln_v_of_ref_type;
    }

    // 本当に行番号を記録
    VELTS(line_num_vector_of_ref_type)[used_len] = ln_num;
    VELTS(line_num_vector_of_ref_type)[2] = MAKINUM(used_len + 1L);

    if (is_show_ega_debug_info) {
        fprintf(stdout, "\n[DebugInfo] A new site_info@%ld is recorded in \"%s#%s <= %s\"\n",
                INUM(ln_num),
                data_type_name(get_data_type_def_identifier(obj)),
                data_type_field_name(get_data_type_def_identifier(obj), index),
                data_type_name(get_data_type_def_identifier(value)));
    }

    return UNSPECIFIED;
}

static char s_set_linked_list_node_entry[] = "set-linked-list-node-entry!";
static SCM set_linked_list_node_entry(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_linked_list_node_entry_with_wb[] = "set-linked-list-node-entry!-with-wb";
static SCM set_linked_list_node_entry_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_linked_list_node_next_node[] = "set-linked-list-node-next-node!";
static SCM set_linked_list_node_next_node(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_linked_list_node_next_node_with_wb[] = "set-linked-list-node-next-node!-with-wb";
static SCM set_linked_list_node_next_node_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_linked_list_size[] = "set-linked-list-size!";
static SCM set_linked_list_size(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_linked_list_size_with_wb[] = "set-linked-list-size!-with-wb";
static SCM set_linked_list_size_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_linked_list_pred_proc_sym[] = "set-linked-list-pred-proc-sym!";
static SCM set_linked_list_pred_proc_sym(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_linked_list_pred_proc_sym_with_wb[] = "set-linked-list-pred-proc-sym!-with-wb";
static SCM set_linked_list_pred_proc_sym_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_linked_list_first_node[] = "set-linked-list-first-node!";
static SCM set_linked_list_first_node(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 2, value);
}

static char s_set_linked_list_first_node_with_wb[] = "set-linked-list-first-node!-with-wb";
static SCM set_linked_list_first_node_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_linked_list_last_node[] = "set-linked-list-last-node!";
static SCM set_linked_list_last_node(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 3, value);
}

static char s_set_linked_list_last_node_with_wb[] = "set-linked-list-last-node!-with-wb";
static SCM set_linked_list_last_node_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_array_list_data_block_data[] = "set-array-list-data-block-data!";
static SCM set_array_list_data_block_data(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_array_list_data_block_data_with_wb[] = "set-array-list-data-block-data!-with-wb";
static SCM set_array_list_data_block_data_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_array_list_size[] = "set-array-list-size!";
static SCM set_array_list_size(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_array_list_size_with_wb[] = "set-array-list-size!-with-wb";
static SCM set_array_list_size_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_array_list_pred_proc_sym[] = "set-array-list-pred-proc-sym!";
static SCM set_array_list_pred_proc_sym(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_array_list_pred_proc_sym_with_wb[] = "set-array-list-pred-proc-sym!-with-wb";
static SCM set_array_list_pred_proc_sym_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_array_list_data_blocks[] = "set-array-list-data-blocks!";
static SCM set_array_list_data_blocks(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 2, value);
}

static char s_set_array_list_data_blocks_with_wb[] = "set-array-list-data-blocks!-with-wb";
static SCM set_array_list_data_blocks_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_hash_entry_k[] = "set-hash-entry-k!";
static SCM set_hash_entry_k(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_hash_entry_k_with_wb[] = "set-hash-entry-k!-with-wb";
static SCM set_hash_entry_k_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_hash_entry_v[] = "set-hash-entry-v!";
static SCM set_hash_entry_v(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_hash_entry_v_with_wb[] = "set-hash-entry-v!-with-wb";
static SCM set_hash_entry_v_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_hash_internal_size[] = "set-hash-internal-size!";
static SCM set_hash_internal_size(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_hash_internal_size_with_wb[] = "set-hash-internal-size!-with-wb";
static SCM set_hash_internal_size_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_hash_internal_key_pred_proc_sym[] = "set-hash-internal-key-pred-proc-sym!";
static SCM set_hash_internal_key_pred_proc_sym(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_hash_internal_key_pred_proc_sym_with_wb[] = "set-hash-internal-key-pred-proc-sym!-with-wb";
static SCM set_hash_internal_key_pred_proc_sym_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_hash_internal_value_pred_proc_sym[] = "set-hash-internal-value-pred-proc-sym!";
static SCM set_hash_internal_value_pred_proc_sym(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 2, value);
}

static char s_set_hash_internal_value_pred_proc_sym_with_wb[] = "set-hash-internal-value-pred-proc-sym!-with-wb";
static SCM set_hash_internal_value_pred_proc_sym_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_hash_internal_used_bucket_size[] = "set-hash-internal-used-bucket-size!";
static SCM set_hash_internal_used_bucket_size(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 3, value);
}

static char s_set_hash_internal_used_bucket_size_with_wb[] = "set-hash-internal-used-bucket-size!-with-wb";
static SCM set_hash_internal_used_bucket_size_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_hash_internal_buckets[] = "set-hash-internal-buckets!";
static SCM set_hash_internal_buckets(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 4, value);
}

static char s_set_hash_internal_buckets_with_wb[] = "set-hash-internal-buckets!-with-wb";
static SCM set_hash_internal_buckets_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 4, value, call_site);
}

static char s_set_hash_map_hash_internal[] = "set-hash-map-hash-internal!";
static SCM set_hash_map_hash_internal(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_hash_map_hash_internal_with_wb[] = "set-hash-map-hash-internal!-with-wb";
static SCM set_hash_map_hash_internal_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Node_left[] = "set-Node-left!";
static SCM set_Node_left(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_Node_left_with_wb[] = "set-Node-left!-with-wb";
static SCM set_Node_left_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_Node_right[] = "set-Node-right!";
static SCM set_Node_right(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_Node_right_with_wb[] = "set-Node-right!-with-wb";
static SCM set_Node_right_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_Node_i[] = "set-Node-i!";
static SCM set_Node_i(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 2, value);
}

static char s_set_Node_i_with_wb[] = "set-Node-i!-with-wb";
static SCM set_Node_i_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 2, value, call_site);
}

static char s_set_Node_j[] = "set-Node-j!";
static SCM set_Node_j(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 3, value);
}

static char s_set_Node_j_with_wb[] = "set-Node-j!-with-wb";
static SCM set_Node_j_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 3, value, call_site);
}

static char s_set_A0_f1[] = "set-A0-f1!";
static SCM set_A0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A0_f1_with_wb[] = "set-A0-f1!-with-wb";
static SCM set_A0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A0_f2[] = "set-A0-f2!";
static SCM set_A0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A0_f2_with_wb[] = "set-A0-f2!-with-wb";
static SCM set_A0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A1_f1[] = "set-A1-f1!";
static SCM set_A1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A1_f1_with_wb[] = "set-A1-f1!-with-wb";
static SCM set_A1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A1_f2[] = "set-A1-f2!";
static SCM set_A1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A1_f2_with_wb[] = "set-A1-f2!-with-wb";
static SCM set_A1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A2_f1[] = "set-A2-f1!";
static SCM set_A2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A2_f1_with_wb[] = "set-A2-f1!-with-wb";
static SCM set_A2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A2_f2[] = "set-A2-f2!";
static SCM set_A2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A2_f2_with_wb[] = "set-A2-f2!-with-wb";
static SCM set_A2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A3_f1[] = "set-A3-f1!";
static SCM set_A3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A3_f1_with_wb[] = "set-A3-f1!-with-wb";
static SCM set_A3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A3_f2[] = "set-A3-f2!";
static SCM set_A3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A3_f2_with_wb[] = "set-A3-f2!-with-wb";
static SCM set_A3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A4_f1[] = "set-A4-f1!";
static SCM set_A4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A4_f1_with_wb[] = "set-A4-f1!-with-wb";
static SCM set_A4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A4_f2[] = "set-A4-f2!";
static SCM set_A4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A4_f2_with_wb[] = "set-A4-f2!-with-wb";
static SCM set_A4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A5_f1[] = "set-A5-f1!";
static SCM set_A5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A5_f1_with_wb[] = "set-A5-f1!-with-wb";
static SCM set_A5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A5_f2[] = "set-A5-f2!";
static SCM set_A5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A5_f2_with_wb[] = "set-A5-f2!-with-wb";
static SCM set_A5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A6_f1[] = "set-A6-f1!";
static SCM set_A6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A6_f1_with_wb[] = "set-A6-f1!-with-wb";
static SCM set_A6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A6_f2[] = "set-A6-f2!";
static SCM set_A6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A6_f2_with_wb[] = "set-A6-f2!-with-wb";
static SCM set_A6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A7_f1[] = "set-A7-f1!";
static SCM set_A7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A7_f1_with_wb[] = "set-A7-f1!-with-wb";
static SCM set_A7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A7_f2[] = "set-A7-f2!";
static SCM set_A7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A7_f2_with_wb[] = "set-A7-f2!-with-wb";
static SCM set_A7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A8_f1[] = "set-A8-f1!";
static SCM set_A8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A8_f1_with_wb[] = "set-A8-f1!-with-wb";
static SCM set_A8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A8_f2[] = "set-A8-f2!";
static SCM set_A8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A8_f2_with_wb[] = "set-A8-f2!-with-wb";
static SCM set_A8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_A9_f1[] = "set-A9-f1!";
static SCM set_A9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_A9_f1_with_wb[] = "set-A9-f1!-with-wb";
static SCM set_A9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_A9_f2[] = "set-A9-f2!";
static SCM set_A9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_A9_f2_with_wb[] = "set-A9-f2!-with-wb";
static SCM set_A9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B0_f1[] = "set-B0-f1!";
static SCM set_B0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B0_f1_with_wb[] = "set-B0-f1!-with-wb";
static SCM set_B0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B0_f2[] = "set-B0-f2!";
static SCM set_B0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B0_f2_with_wb[] = "set-B0-f2!-with-wb";
static SCM set_B0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B1_f1[] = "set-B1-f1!";
static SCM set_B1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B1_f1_with_wb[] = "set-B1-f1!-with-wb";
static SCM set_B1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B1_f2[] = "set-B1-f2!";
static SCM set_B1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B1_f2_with_wb[] = "set-B1-f2!-with-wb";
static SCM set_B1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B2_f1[] = "set-B2-f1!";
static SCM set_B2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B2_f1_with_wb[] = "set-B2-f1!-with-wb";
static SCM set_B2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B2_f2[] = "set-B2-f2!";
static SCM set_B2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B2_f2_with_wb[] = "set-B2-f2!-with-wb";
static SCM set_B2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B3_f1[] = "set-B3-f1!";
static SCM set_B3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B3_f1_with_wb[] = "set-B3-f1!-with-wb";
static SCM set_B3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B3_f2[] = "set-B3-f2!";
static SCM set_B3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B3_f2_with_wb[] = "set-B3-f2!-with-wb";
static SCM set_B3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B4_f1[] = "set-B4-f1!";
static SCM set_B4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B4_f1_with_wb[] = "set-B4-f1!-with-wb";
static SCM set_B4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B4_f2[] = "set-B4-f2!";
static SCM set_B4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B4_f2_with_wb[] = "set-B4-f2!-with-wb";
static SCM set_B4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B5_f1[] = "set-B5-f1!";
static SCM set_B5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B5_f1_with_wb[] = "set-B5-f1!-with-wb";
static SCM set_B5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B5_f2[] = "set-B5-f2!";
static SCM set_B5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B5_f2_with_wb[] = "set-B5-f2!-with-wb";
static SCM set_B5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B6_f1[] = "set-B6-f1!";
static SCM set_B6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B6_f1_with_wb[] = "set-B6-f1!-with-wb";
static SCM set_B6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B6_f2[] = "set-B6-f2!";
static SCM set_B6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B6_f2_with_wb[] = "set-B6-f2!-with-wb";
static SCM set_B6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B7_f1[] = "set-B7-f1!";
static SCM set_B7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B7_f1_with_wb[] = "set-B7-f1!-with-wb";
static SCM set_B7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B7_f2[] = "set-B7-f2!";
static SCM set_B7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B7_f2_with_wb[] = "set-B7-f2!-with-wb";
static SCM set_B7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B8_f1[] = "set-B8-f1!";
static SCM set_B8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B8_f1_with_wb[] = "set-B8-f1!-with-wb";
static SCM set_B8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B8_f2[] = "set-B8-f2!";
static SCM set_B8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B8_f2_with_wb[] = "set-B8-f2!-with-wb";
static SCM set_B8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_B9_f1[] = "set-B9-f1!";
static SCM set_B9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_B9_f1_with_wb[] = "set-B9-f1!-with-wb";
static SCM set_B9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_B9_f2[] = "set-B9-f2!";
static SCM set_B9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_B9_f2_with_wb[] = "set-B9-f2!-with-wb";
static SCM set_B9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C0_f1[] = "set-C0-f1!";
static SCM set_C0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C0_f1_with_wb[] = "set-C0-f1!-with-wb";
static SCM set_C0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C0_f2[] = "set-C0-f2!";
static SCM set_C0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C0_f2_with_wb[] = "set-C0-f2!-with-wb";
static SCM set_C0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C1_f1[] = "set-C1-f1!";
static SCM set_C1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C1_f1_with_wb[] = "set-C1-f1!-with-wb";
static SCM set_C1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C1_f2[] = "set-C1-f2!";
static SCM set_C1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C1_f2_with_wb[] = "set-C1-f2!-with-wb";
static SCM set_C1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C2_f1[] = "set-C2-f1!";
static SCM set_C2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C2_f1_with_wb[] = "set-C2-f1!-with-wb";
static SCM set_C2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C2_f2[] = "set-C2-f2!";
static SCM set_C2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C2_f2_with_wb[] = "set-C2-f2!-with-wb";
static SCM set_C2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C3_f1[] = "set-C3-f1!";
static SCM set_C3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C3_f1_with_wb[] = "set-C3-f1!-with-wb";
static SCM set_C3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C3_f2[] = "set-C3-f2!";
static SCM set_C3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C3_f2_with_wb[] = "set-C3-f2!-with-wb";
static SCM set_C3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C4_f1[] = "set-C4-f1!";
static SCM set_C4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C4_f1_with_wb[] = "set-C4-f1!-with-wb";
static SCM set_C4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C4_f2[] = "set-C4-f2!";
static SCM set_C4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C4_f2_with_wb[] = "set-C4-f2!-with-wb";
static SCM set_C4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C5_f1[] = "set-C5-f1!";
static SCM set_C5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C5_f1_with_wb[] = "set-C5-f1!-with-wb";
static SCM set_C5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C5_f2[] = "set-C5-f2!";
static SCM set_C5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C5_f2_with_wb[] = "set-C5-f2!-with-wb";
static SCM set_C5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C6_f1[] = "set-C6-f1!";
static SCM set_C6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C6_f1_with_wb[] = "set-C6-f1!-with-wb";
static SCM set_C6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C6_f2[] = "set-C6-f2!";
static SCM set_C6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C6_f2_with_wb[] = "set-C6-f2!-with-wb";
static SCM set_C6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C7_f1[] = "set-C7-f1!";
static SCM set_C7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C7_f1_with_wb[] = "set-C7-f1!-with-wb";
static SCM set_C7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C7_f2[] = "set-C7-f2!";
static SCM set_C7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C7_f2_with_wb[] = "set-C7-f2!-with-wb";
static SCM set_C7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C8_f1[] = "set-C8-f1!";
static SCM set_C8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C8_f1_with_wb[] = "set-C8-f1!-with-wb";
static SCM set_C8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C8_f2[] = "set-C8-f2!";
static SCM set_C8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C8_f2_with_wb[] = "set-C8-f2!-with-wb";
static SCM set_C8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_C9_f1[] = "set-C9-f1!";
static SCM set_C9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_C9_f1_with_wb[] = "set-C9-f1!-with-wb";
static SCM set_C9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_C9_f2[] = "set-C9-f2!";
static SCM set_C9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_C9_f2_with_wb[] = "set-C9-f2!-with-wb";
static SCM set_C9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D0_f1[] = "set-D0-f1!";
static SCM set_D0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D0_f1_with_wb[] = "set-D0-f1!-with-wb";
static SCM set_D0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D0_f2[] = "set-D0-f2!";
static SCM set_D0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D0_f2_with_wb[] = "set-D0-f2!-with-wb";
static SCM set_D0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D1_f1[] = "set-D1-f1!";
static SCM set_D1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D1_f1_with_wb[] = "set-D1-f1!-with-wb";
static SCM set_D1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D1_f2[] = "set-D1-f2!";
static SCM set_D1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D1_f2_with_wb[] = "set-D1-f2!-with-wb";
static SCM set_D1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D2_f1[] = "set-D2-f1!";
static SCM set_D2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D2_f1_with_wb[] = "set-D2-f1!-with-wb";
static SCM set_D2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D2_f2[] = "set-D2-f2!";
static SCM set_D2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D2_f2_with_wb[] = "set-D2-f2!-with-wb";
static SCM set_D2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D3_f1[] = "set-D3-f1!";
static SCM set_D3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D3_f1_with_wb[] = "set-D3-f1!-with-wb";
static SCM set_D3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D3_f2[] = "set-D3-f2!";
static SCM set_D3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D3_f2_with_wb[] = "set-D3-f2!-with-wb";
static SCM set_D3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D4_f1[] = "set-D4-f1!";
static SCM set_D4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D4_f1_with_wb[] = "set-D4-f1!-with-wb";
static SCM set_D4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D4_f2[] = "set-D4-f2!";
static SCM set_D4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D4_f2_with_wb[] = "set-D4-f2!-with-wb";
static SCM set_D4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D5_f1[] = "set-D5-f1!";
static SCM set_D5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D5_f1_with_wb[] = "set-D5-f1!-with-wb";
static SCM set_D5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D5_f2[] = "set-D5-f2!";
static SCM set_D5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D5_f2_with_wb[] = "set-D5-f2!-with-wb";
static SCM set_D5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D6_f1[] = "set-D6-f1!";
static SCM set_D6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D6_f1_with_wb[] = "set-D6-f1!-with-wb";
static SCM set_D6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D6_f2[] = "set-D6-f2!";
static SCM set_D6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D6_f2_with_wb[] = "set-D6-f2!-with-wb";
static SCM set_D6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D7_f1[] = "set-D7-f1!";
static SCM set_D7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D7_f1_with_wb[] = "set-D7-f1!-with-wb";
static SCM set_D7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D7_f2[] = "set-D7-f2!";
static SCM set_D7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D7_f2_with_wb[] = "set-D7-f2!-with-wb";
static SCM set_D7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D8_f1[] = "set-D8-f1!";
static SCM set_D8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D8_f1_with_wb[] = "set-D8-f1!-with-wb";
static SCM set_D8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D8_f2[] = "set-D8-f2!";
static SCM set_D8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D8_f2_with_wb[] = "set-D8-f2!-with-wb";
static SCM set_D8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_D9_f1[] = "set-D9-f1!";
static SCM set_D9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_D9_f1_with_wb[] = "set-D9-f1!-with-wb";
static SCM set_D9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_D9_f2[] = "set-D9-f2!";
static SCM set_D9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_D9_f2_with_wb[] = "set-D9-f2!-with-wb";
static SCM set_D9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E0_f1[] = "set-E0-f1!";
static SCM set_E0_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E0_f1_with_wb[] = "set-E0-f1!-with-wb";
static SCM set_E0_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E0_f2[] = "set-E0-f2!";
static SCM set_E0_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E0_f2_with_wb[] = "set-E0-f2!-with-wb";
static SCM set_E0_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E1_f1[] = "set-E1-f1!";
static SCM set_E1_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E1_f1_with_wb[] = "set-E1-f1!-with-wb";
static SCM set_E1_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E1_f2[] = "set-E1-f2!";
static SCM set_E1_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E1_f2_with_wb[] = "set-E1-f2!-with-wb";
static SCM set_E1_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E2_f1[] = "set-E2-f1!";
static SCM set_E2_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E2_f1_with_wb[] = "set-E2-f1!-with-wb";
static SCM set_E2_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E2_f2[] = "set-E2-f2!";
static SCM set_E2_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E2_f2_with_wb[] = "set-E2-f2!-with-wb";
static SCM set_E2_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E3_f1[] = "set-E3-f1!";
static SCM set_E3_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E3_f1_with_wb[] = "set-E3-f1!-with-wb";
static SCM set_E3_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E3_f2[] = "set-E3-f2!";
static SCM set_E3_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E3_f2_with_wb[] = "set-E3-f2!-with-wb";
static SCM set_E3_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E4_f1[] = "set-E4-f1!";
static SCM set_E4_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E4_f1_with_wb[] = "set-E4-f1!-with-wb";
static SCM set_E4_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E4_f2[] = "set-E4-f2!";
static SCM set_E4_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E4_f2_with_wb[] = "set-E4-f2!-with-wb";
static SCM set_E4_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E5_f1[] = "set-E5-f1!";
static SCM set_E5_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E5_f1_with_wb[] = "set-E5-f1!-with-wb";
static SCM set_E5_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E5_f2[] = "set-E5-f2!";
static SCM set_E5_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E5_f2_with_wb[] = "set-E5-f2!-with-wb";
static SCM set_E5_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E6_f1[] = "set-E6-f1!";
static SCM set_E6_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E6_f1_with_wb[] = "set-E6-f1!-with-wb";
static SCM set_E6_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E6_f2[] = "set-E6-f2!";
static SCM set_E6_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E6_f2_with_wb[] = "set-E6-f2!-with-wb";
static SCM set_E6_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E7_f1[] = "set-E7-f1!";
static SCM set_E7_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E7_f1_with_wb[] = "set-E7-f1!-with-wb";
static SCM set_E7_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E7_f2[] = "set-E7-f2!";
static SCM set_E7_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E7_f2_with_wb[] = "set-E7-f2!-with-wb";
static SCM set_E7_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E8_f1[] = "set-E8-f1!";
static SCM set_E8_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E8_f1_with_wb[] = "set-E8-f1!-with-wb";
static SCM set_E8_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E8_f2[] = "set-E8-f2!";
static SCM set_E8_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E8_f2_with_wb[] = "set-E8-f2!-with-wb";
static SCM set_E8_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_E9_f1[] = "set-E9-f1!";
static SCM set_E9_f1(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_E9_f1_with_wb[] = "set-E9-f1!-with-wb";
static SCM set_E9_f1_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_E9_f2[] = "set-E9-f2!";
static SCM set_E9_f2(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_E9_f2_with_wb[] = "set-E9-f2!-with-wb";
static SCM set_E9_f2_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_LeakPath_first[] = "set-LeakPath-first!";
static SCM set_LeakPath_first(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 0, value);
}

static char s_set_LeakPath_first_with_wb[] = "set-LeakPath-first!-with-wb";
static SCM set_LeakPath_first_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 0, value, call_site);
}

static char s_set_LeakPath_last[] = "set-LeakPath-last!";
static SCM set_LeakPath_last(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 1, value);
}

static char s_set_LeakPath_last_with_wb[] = "set-LeakPath-last!-with-wb";
static SCM set_LeakPath_last_with_wb(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier_with_wb(obj, 1, value, call_site);
}

static char s_set_LeakPath_len[] = "set-LeakPath-len!";
static SCM set_LeakPath_len(SCM obj, SCM value, SCM call_site) {
    return c_data_type_modifier(obj, 2, value);
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

void init_define_data_type_disk_saved() {
    init_iprocs(subr1s, tc7_subr_1);
    init_iprocs(subr2s, tc7_subr_2);
    init_iprocs(subr2os, tc7_subr_2o);
    init_iprocs(subr3s, tc7_subr_3);
}
