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

static char s_c_data_type_modifier[] = "c-data-type-modifier";
SCM c_data_type_modifier(SCM obj, SCM index, SCM value) {
    vector_set(DTI_FVV(obj), MAKINUM(INUM(index) + 1L), value);
    return UNSPECIFIED;
}

SCM c_data_type_modifier_with_wb(SCM obj, SCM index, SCM value, SCM ln_num) {
    c_data_type_modifier(obj, index, value);

    // try process write barrier
    if (!is_user_defined_data_type_instance_with_rec_slots(obj)) {
        return UNSPECIFIED;
    }

    SCM rec_slot_of_field = get_rec_slot_of_field(obj, INUM(index));
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
                data_type_field_name(get_data_type_def_identifier(obj), INUM(index)),
                data_type_name(get_data_type_def_identifier(value)));
    }

    return UNSPECIFIED;
}

static char s_c_data_type_modifier_with_wb_of_field_0[] = "c-data-type-modifier-with-wb-of-field-0";
static SCM c_data_type_modifier_with_wb_of_field_0(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(0L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_1[] = "c-data-type-modifier-with-wb-of-field-1";
static SCM c_data_type_modifier_with_wb_of_field_1(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(1L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_2[] = "c-data-type-modifier-with-wb-of-field-2";
static SCM c_data_type_modifier_with_wb_of_field_2(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(2L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_3[] = "c-data-type-modifier-with-wb-of-field-3";
static SCM c_data_type_modifier_with_wb_of_field_3(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(3L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_4[] = "c-data-type-modifier-with-wb-of-field-4";
static SCM c_data_type_modifier_with_wb_of_field_4(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(4L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_5[] = "c-data-type-modifier-with-wb-of-field-5";
static SCM c_data_type_modifier_with_wb_of_field_5(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(5L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_6[] = "c-data-type-modifier-with-wb-of-field-6";
static SCM c_data_type_modifier_with_wb_of_field_6(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(6L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_7[] = "c-data-type-modifier-with-wb-of-field-7";
static SCM c_data_type_modifier_with_wb_of_field_7(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(7L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_8[] = "c-data-type-modifier-with-wb-of-field-8";
static SCM c_data_type_modifier_with_wb_of_field_8(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(8L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_9[] = "c-data-type-modifier-with-wb-of-field-9";
static SCM c_data_type_modifier_with_wb_of_field_9(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(9L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_10[] = "c-data-type-modifier-with-wb-of-field-10";
static SCM c_data_type_modifier_with_wb_of_field_10(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(10L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_11[] = "c-data-type-modifier-with-wb-of-field-11";
static SCM c_data_type_modifier_with_wb_of_field_11(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(11L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_12[] = "c-data-type-modifier-with-wb-of-field-12";
static SCM c_data_type_modifier_with_wb_of_field_12(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(12L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_13[] = "c-data-type-modifier-with-wb-of-field-13";
static SCM c_data_type_modifier_with_wb_of_field_13(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(13L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_14[] = "c-data-type-modifier-with-wb-of-field-14";
static SCM c_data_type_modifier_with_wb_of_field_14(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(14L), value, ln_num);
}

static char s_c_data_type_modifier_with_wb_of_field_15[] = "c-data-type-modifier-with-wb-of-field-15";
static SCM c_data_type_modifier_with_wb_of_field_15(SCM obj, SCM value, SCM ln_num) {
    return c_data_type_modifier_with_wb(obj, MAKINUM(15L), value, ln_num);
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
        {s_c_data_type_modifier, c_data_type_modifier},
        {s_c_data_type_modifier_with_wb_of_field_0, c_data_type_modifier_with_wb_of_field_0},
        {s_c_data_type_modifier_with_wb_of_field_1, c_data_type_modifier_with_wb_of_field_1},
        {s_c_data_type_modifier_with_wb_of_field_2, c_data_type_modifier_with_wb_of_field_2},
        {s_c_data_type_modifier_with_wb_of_field_3, c_data_type_modifier_with_wb_of_field_3},
        {s_c_data_type_modifier_with_wb_of_field_4, c_data_type_modifier_with_wb_of_field_4},
        {s_c_data_type_modifier_with_wb_of_field_5, c_data_type_modifier_with_wb_of_field_5},
        {s_c_data_type_modifier_with_wb_of_field_6, c_data_type_modifier_with_wb_of_field_6},
        {s_c_data_type_modifier_with_wb_of_field_7, c_data_type_modifier_with_wb_of_field_7},
        {s_c_data_type_modifier_with_wb_of_field_8, c_data_type_modifier_with_wb_of_field_8},
        {s_c_data_type_modifier_with_wb_of_field_9, c_data_type_modifier_with_wb_of_field_9},
        {s_c_data_type_modifier_with_wb_of_field_10, c_data_type_modifier_with_wb_of_field_10},
        {s_c_data_type_modifier_with_wb_of_field_11, c_data_type_modifier_with_wb_of_field_11},
        {s_c_data_type_modifier_with_wb_of_field_12, c_data_type_modifier_with_wb_of_field_12},
        {s_c_data_type_modifier_with_wb_of_field_13, c_data_type_modifier_with_wb_of_field_13},
        {s_c_data_type_modifier_with_wb_of_field_14, c_data_type_modifier_with_wb_of_field_14},
        {s_c_data_type_modifier_with_wb_of_field_15, c_data_type_modifier_with_wb_of_field_15},
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
