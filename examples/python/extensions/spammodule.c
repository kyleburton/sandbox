/*
 * https://docs.python.org/2/extending/extending.html
 * https://docs.python.org/2/extending/building.html#building
 *
 **/

#include <Python.h>

static PyObject *SpamError;

static PyObject*
spam_system(PyObject *self, PyObject *args)
{
  const char *command;
  int cmdlen;
  int res;

  if(!PyArg_ParseTuple(args, "s#", &command, &cmdlen)) {
    return NULL;
  }

  res = system(command);
  if (res < 0) {
    PyErr_SetString(SpamError,"This is the error message");
    return NULL;
  }

  return Py_BuildValue("i", res);
}

static PyObject*
spam_exception1(PyObject *self, PyObject *args)
{
  PyErr_SetString(SpamError,"This is the error message");
  return NULL;
}

static PyObject*
spam_noneresult(PyObject *self, PyObject *args)
{
  Py_RETURN_NONE;
}

static PyMethodDef SpamMethods[] = {
  {"system",      spam_system,     METH_VARARGS, "Execute a shell command."},
  {"exception1",  spam_exception1, METH_VARARGS, "Test throwing an exception"},
  {"noneresult",  spam_noneresult, METH_VARARGS, "Return None as a value"},
  {NULL, NULL, 0, NULL}        /* Sentinel */
};

PyMODINIT_FUNC
initspam(void) 
{
  PyObject *m;
  m = Py_InitModule("spam", SpamMethods);

  if (m == NULL) {
    // NB: error during initialization?
    return;
  }

  SpamError = PyErr_NewException("spam.error", NULL, NULL);
  Py_INCREF(SpamError);
  PyModule_AddObject(m, "error", SpamError);
}

