// "definedatatype.c" code for user definable class-struct.
//
// Created by Guang Yang on 2023/03/29.
//

#include "scm.h"

// 0: DataTypeDef <= [module-flag-sym, internal-code, type-name, [field-name ...]]
// 1: undefined
// 2: DataTypeInstance <= [module-flag-sym, internal-code, data-type-def, [field-value ...]]

#define MFS(x) (VELTS(x)[0]) // module-flag-sym

/*
 * internal-code (7bits)
 * low order 3 bits: internal-type
 * low order 4th bit: assert-dead mark
 * TODO 5th 6th 7th...
 */
#define IC(x) (VELTS(x)[1])

/*
 * SET_IC_BIT: set the bit of index i to be "1", in internal code in a user defined type obj
 * CLEAR_IC_BIT: set the bit of index i to be "0", in internal code in a user defined type obj
 * GET_IC_BIT: get the bit of index in internal code in a user defined type obj
 */
#define SET_IC_BIT(x, i) (IC(x) = MAKINUM(INUM(IC(x)) | (1 << i)))
#define CLEAR_IC_BIT(x, i) (IC(x) = MAKINUM(INUM(IC(x)) & ~(1 << i)))
#define GET_IC_BIT_INUM(x, i) (((INUM(IC(x)) & (1 << i))) >> i)

#define READ_IT(x) (MAKINUM(7 & INUM(IC(x)))) // get internal-type code (readonly)
#define READ_ASSERT_DEAD_MARK(x) (GET_IC_BIT_INUM(x, 3)) // get the inum of assert-dead-mark (readonly)

#define DTD_TN(x) (VELTS(x)[2]) // type-name
#define DTD_FNV(x) (VELTS(x)[3]) // vector of field-names vector

#define DTI_DTD(x) (VELTS(x)[2]) // data-type-def
#define DTI_FVV(x) (VELTS(x)[3]) // vector of field-values

#define DTD_VECTOR_LEN (MAKINUM(4)) // length of DataTypeDef vector
#define DTI_VECTOR_LEN (MAKINUM(4)) // length of DataTypeInstance vector

#define DTD_IT_CODE (MAKINUM(0)) // internal type code of DataTypeDef
#define UNDEFINED_IT_CODE (MAKINUM(1)) // internal type code of undefined item
#define DTI_IT_CODE (MAKINUM(2)) // internal type code of DataTypeInstance

static SCM module_flag_symbol;

static char s_c_define_data_type[] = "c-define-data-type";
SCM c_define_data_type(SCM type_name, SCM field_names) {
    if (ilength(field_names) < 0) {
        errout: wta(field_names, (char *)ARG2, s_c_define_data_type);
    }
    for (SCM n = field_names; NIMP(n); n = CDR(n)) {
        if (!SYMBOLP(CAR(n))) {
            goto errout;
        }
    }

    // store it as vector in data type definition object
    SCM field_names_vector = vector(field_names);

    // create a new DataTypeDef
    SCM data_type_def = make_vector(DTD_VECTOR_LEN, UNDEFINED);
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
    IC(data_type_instance) = DTI_IT_CODE; // for initialization
    DTI_DTD(data_type_instance) = data_type_def;
    DTI_FVV(data_type_instance) = field_values_vector;
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
    return READ_IT(scm_obj) != DTD_IT_CODE;
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
    vector_set(DTI_FVV(obj), index, value);
    return UNSPECIFIED;
}

static iproc subr2s[] = {
        {s_c_define_data_type, c_define_data_type},
        {s_c_make_instance, c_make_instance},
        {s_c_data_type_predicate, c_data_type_predicate},
        {s_c_data_type_accessor, c_data_type_accessor},
        {0, 0}
};
static iproc subr3s[] = {
        {s_c_data_type_modifier, c_data_type_modifier},
        {0, 0}
};

void init_define_data_type() {
    module_flag_symbol = string2symbol(makfrom0str("UserDefinedDataType"));
    init_iprocs(subr2s, tc7_subr_2);
    init_iprocs(subr3s, tc7_subr_3);
    add_feature("definedatatype");
}