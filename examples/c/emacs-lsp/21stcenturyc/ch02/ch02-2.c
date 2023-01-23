#include <glib-2.0/glib.h>
#include <stdio.h>

/**

   $ /usr/bin/pkg-config --variable=includedir glib-2.0

   (add-hook 'c-initialization-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "/usr/include")))))


 */

int main(int __attribute__((unused)) argc,
         char **__attribute__((unused)) argv) {
  GList *list = NULL;

  list = g_list_append(list, "a");
  list = g_list_append(list, "b");
  list = g_list_append(list, "c");

  for (; list != NULL; list = list->next) {
    printf("%s\n", (char *)list->data);
  }

  g_list_free(list);
  list = NULL;

  return 0;
}
