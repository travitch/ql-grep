#include "unknown.h"

extern const char * s;

int f1(const char* foo) {
  return strcmp(foo, s);
}

int f2(const char* foo, const char* bar) {
  return strcmp(bar, s) >= 0;
}
