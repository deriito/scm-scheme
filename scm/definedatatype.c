// definedatatype.c" code for user definable class-struct.
//
// Created by Guang Yang on 2023/03/29.
//

#include "scm.h"

// 0: DataTypeDef <= ["UserDefinedDataType", internal-type, type-name, [field-name ...]]
// 1: undefined
// 2: DataTypeInstance <= ["UserDefinedDataType", internal-type, data-type-def, [field-value ...]]

static int i_v_least_length_uddt = 4; // UserDefinedDataTypeのvectorの最小長さ
static char s_user_defined_data_type[] = "UserDefinedDataType";
static int i_internal_type_dtd = 0;
static int i_internal_type_undefined = 1;
static int i_internal_type_dti = 2;

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
    SCM data_type_def = make_vector(MAKINUM(4), UNDEFINED);
    vector_set(data_type_def, MAKINUM(0), makfrom0str(s_user_defined_data_type));
    vector_set(data_type_def, MAKINUM(1), MAKINUM(i_internal_type_dtd));
    vector_set(data_type_def, MAKINUM(2), type_name);
    vector_set(data_type_def, MAKINUM(3), field_names_vector);
    return data_type_def;
}

static char s_c_make_instance[] = "c-make-instance";
SCM c_make_instance(SCM data_type_def, SCM field_values_vector) {
    SCM data_type_instance = make_vector(MAKINUM(4), UNDEFINED);
    vector_set(data_type_instance, MAKINUM(0), makfrom0str(s_user_defined_data_type));
    vector_set(data_type_instance, MAKINUM(1), MAKINUM(i_internal_type_dti));
    vector_set(data_type_instance, MAKINUM(2), data_type_def);
    vector_set(data_type_instance, MAKINUM(3), field_values_vector);
    return data_type_instance;
}

static char s_c_data_type_predicate[] = "c-data-type-predicate";
SCM c_data_type_predicate(SCM data_type_def, SCM obj) {
    if (BOOL_F == vectorp(data_type_def)) {
        errout: wta(data_type_def, (char *)ARG2, s_c_data_type_predicate);
    }
    if (INUM(vector_length(data_type_def)) < i_v_least_length_uddt) {
        goto errout;
    }
    if (BOOL_F == equal(vector_ref(data_type_def, MAKINUM(0)), makfrom0str(s_user_defined_data_type))) {
        goto errout;
    }
    if (BOOL_F == vectorp(obj)) {
        return BOOL_F;
    }
    if (INUM(vector_length(obj)) < i_v_least_length_uddt) {
        return BOOL_F;
    }
    if (BOOL_F == equal(vector_ref(obj, MAKINUM(0)), makfrom0str(s_user_defined_data_type))) {
        return BOOL_F;
    }
    if (BOOL_F == equal(data_type_def, vector_ref(obj, MAKINUM(2)))) {
        return BOOL_F;
    }
    return BOOL_T;
}

static char s_c_data_type_accessor[] = "c-data-type-accessor";
SCM c_data_type_accessor(SCM obj, SCM index) {
    SCM field_values = vector_ref(obj, MAKINUM(3));
    return vector_ref(field_values, index);
}

static char s_c_data_type_modifier[] = "c-data-type-modifier";
SCM c_data_type_modifier(SCM obj, SCM index, SCM value) {
    SCM field_values = vector_ref(obj, MAKINUM(3));
    vector_set(field_values, index, value);
    return BOOL_T;
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
    init_iprocs(subr2s, tc7_subr_2);
    init_iprocs(subr3s, tc7_subr_3);
    add_feature("definedatatype");
}