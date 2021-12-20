#include <sys/queue.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// TODO: change the example to load /usr/share/dict/words (& do resevior sampling?)

struct Person {
  int64_t  userid;
  char     *email;
  SLIST_ENTRY(Person) next_person;
};

int main (int argc, char ** argv) {
  SLIST_HEAD(People, Person) people = SLIST_HEAD_INITIALIZER(people);
  // struct People *headp;
  int64_t userids = 0;

  SLIST_INIT(&people);

  struct Person *p1 = malloc(sizeof(struct Person)); // TODO: error check here!!
  p1->userid = ++userids;
  p1->email = "kyle.burton@gmail.com";
  SLIST_INSERT_HEAD(&people, p1, next_person);

  p1 = malloc(sizeof(struct Person)); // TODO: error check here!!
  p1->userid = ++userids;
  p1->email = "cfortuner@tehgmail.com";
  SLIST_INSERT_HEAD(&people, p1, next_person);

  struct Person *person;
  SLIST_FOREACH(person, &people, next_person) {
    // TODO: helper using xsprintf to format a Person struct as a (newly allocated) string
    printf("Person{id:%ld; email:%s}\n", person->userid, person->email);
  }

  // TODO: free() the entries in the list & the list!!!
}
