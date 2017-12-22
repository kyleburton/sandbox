#ifndef JSONLIB_H
#define JSONLIB_H

typedef enum {
    T_STRING = 1,
    T_NUMBER,
    T_OBJECT,
    T_ARRAY,
    T_TRUE,
    T_FALSE,
    T_NULL,
} json_value_t;

typedef struct json_object {
  json_value_t          object_type;
  struct json_object**  keys;
  struct json_object**  values;
  size_t                klen;

  int                   isnull;
  int                   istrue;
  int                   isfalse;
  double                nval;
  struct json_object*   oval;

  char*                 sval;
  size_t                slen;

  struct json_object*   aval;
  size_t                alen;
} json_object;

#define JSSTR(s)         jsonlib_new(T_STRING, s, strlen(s))
#define JSNUM(n)         jsonlib_new(T_NUMBER, n)
#define JSOBJ(...)       jsonlib_new(T_OBJECT, __VA_ARGS__)
#define JSARR(...)       jsonlib_new(T_ARRAY, __VA_ARGS__)
#define JSTRUE()         jsonlib_true()
#define JSFALSE()        jsonlib_false()
#define JSNULL()         jsonlib_null()

int jsonlib_init();

json_object* jsonlib_new(json_value_t t, ...);
json_object* jsonlib_new(json_value_t t, ...);
json_object* jsonlib_new(json_value_t t, ...);
json_object* jsonlib_new(json_value_t t, ...);
json_object* jsonlib_true();
json_object* jsonlib_false();
json_object* jsonlib_null();

#endif
