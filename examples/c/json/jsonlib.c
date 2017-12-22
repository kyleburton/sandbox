#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "jsonlib.h"

static json_object* JSON_NULL = NULL;
static json_object* JSON_TRUE = NULL;
static json_object* JSON_FALSE = NULL;

json_object* jsonlib_alloc_object() {
    json_object* rv = malloc(1 * sizeof(json_object));
    bzero(rv, sizeof(json_object));
    return rv;
}

int jsonlib_char_is_special (char c) {
    switch (c) {
        case '"':   return 1; break;
        case '\\':  return 1; break;
        case '/':   return 1; break;
        case '\b':  return 1; break;
        case '\f':  return 1; break;
        case '\n':  return 1; break;
        case '\r':  return 1; break;
        case '\t':  return 1; break;
    }
    return 0;
}

// TODO: utf-8
json_object* jsonlib_encode_string(char * s, size_t slen) {
    json_object* rv = jsonlib_alloc_object();
    rv->object_type = T_STRING;
    rv->slen = 1+slen;

    for (size_t ii = 0; ii < slen; ++ii) {
        if (jsonlib_char_is_special(s[ii])) {
            rv->slen++;
        }
    }


    rv->sval = (char*)malloc(rv->slen * sizeof(char));
    if (!rv->sval) {
        return NULL;
    }

    size_t jj = 0;
    for (size_t ii = 0; ii < rv->slen; ++ii) {
        if (jsonlib_char_is_special(s[ii])) {
            rv->sval[jj] = '\\';
            jj++;
        }
        rv->sval[jj] = s[ii];
        jj++;
    }
    rv->sval[rv->slen-1] = '\0';

    return rv;
}

int jsonlib_init() {
    JSON_NULL = jsonlib_alloc_object();
    JSON_NULL->object_type = T_NUMBER;
    JSON_NULL->isnull = 1;

    JSON_TRUE = NULL;
    JSON_NULL->object_type = T_TRUE;
    JSON_NULL->isnull = 0;
    JSON_NULL->istrue = 1;

    JSON_FALSE = NULL;
    JSON_NULL->object_type = T_FALSE;
    JSON_NULL->isfalse = 1;
    return 0;
}

json_object* jsonlib_new(json_value_t t, ...) {
    va_list args;
    json_object* rv = NULL;

    va_start(args, t);

    // TODO: if the type is T_OBJECT, assert there are an even number of args
    switch (t) {
        case T_STRING:
            return jsonlib_encode_string(va_arg(args, char*), va_arg(args, int));
            break;
        case T_NUMBER:
            break;
        case T_OBJECT:
            break;
        case T_ARRAY:
            break;
        case T_TRUE:
            rv = NULL;
            break;
        case T_FALSE:
            rv = NULL;
            break;
        case T_NULL:
            rv = NULL;
            break;
        default:
            rv = NULL;
            break;
    }

    va_end(args);

    return rv;
}

json_object* jsonlib_true() {
    return JSON_TRUE;
}

json_object* jsonlib_false() {
    return JSON_FALSE;
}

json_object* jsonlib_null() {
    return JSON_NULL;
}

char* jsonlib_astrcat(char *left, size_t llen, const char* right, char rlen) {
    size_t newlen = (llen + rlen + 1);
    left = realloc(left, newlen);

    if (!left) {
        return NULL;
    }

    memcpy(left + (llen + rlen), right, rlen);

    left[newlen-1] = '\0';

    return left;
}

char* jsonlib_obj_to_new_string (json_object *obj) {
    char* rv;
    switch (obj->object_type) {
        case T_STRING:
            rv = (char*)malloc(obj->slen * sizeof(char));
            memcpy(rv, obj->sval, obj->slen);
            return rv;
            break;
        case T_NUMBER:
            if (0 != asprintf(&rv, "%lf", obj->nval) ) {
                return NULL;
            }
            return rv;
            break;
        case T_OBJECT:
            rv = strdup("{");
            size_t rvlen = 1;
            for (size_t ii = 0; ii < obj->klen; ii++) {
                char* kstr = jsonlib_obj_to_new_string(obj->keys[ii]);
                rv = jsonlib_astrcat(rv, rvlen, kstr, strlen(kstr));
                rv = jsonlib_astrcat(rv, ":");

                char* vstr = jsonlib_obj_to_new_string(obj->values[ii]);
                rv = jsonlib_astrcat(rv, rvlen, vstr, strlen(vstr));
                free(vstr);
            }
            rv = jsonlib_astrcat(rv, "}");
            break;
        case T_ARRAY:
            // "[" + join(",", to_string(elt) + "]"
            printf("ERROR[jsonlib_obj_to_new_string]: T_ARRAY: implement this!!!\n");
            break;
        case T_TRUE:
            return strdup("true");
            break;
        case T_FALSE:
            return strdup("false");
            break;
        case T_NULL:
            return strdup("null");
            break;
        default:
            rv = NULL;
            break;
    }
    return NULL;
}

void jsonlib_free_object(json_object *obj) {
    switch(obj->object_type) {
        case T_STRING:
            free(obj->sval);
            break;
        case T_NUMBER:
            break;
        case T_OBJECT:
            jsonlib_free_object(obj->oval)
                break;
        case T_ARRAY:
            for (size_t ii = 0; ii < obj->alen; ii++) {
                jsonlib_free_object(obj->aval[ii]);
            }
            free(obj->aval);
            break;
        default:
            break;
    }
}
void test_json_to_string () {
    /*
       json_object *obj = JSOBJ(
       JSSTR("name"), JSSTR("Kyle"),
       JSSTR("info"), JSOBJ(
       JSSTR("email"), JSSTR("kyle.burton@gmail.com"),
       JSSTR("address"), JSOBJ(
       JSSTR("street1"), JSSTR("123 Main St."),
       JSSTR("city"),  JSSTR("Los Angeles"),
       JSSTR("state"), JSSTR("CA"),
       JSSTR("zip"),   JSSTR("90333")
       )
       ),
       JSSTR("age"), JSNUM(42)
       );
       */
    json_object *obj = JSOBJ(
            JSSTR("name"), JSSTR("Kyle"),
            JSSTR("age"),  JSNUM(42)
            );

    char *jstext = jsonlib_obj_to_new_string(obj);
    jsonlib_free_object(jstext);
}

int main (int argc, char **argv ) {
    printf("Implement something here ok: argc=%d argv[0]=%s\n",
            argc,
            argv[0]);
    test_json_to_string();
    return 0;
}
