// Match
void f1(const char* s) {
  printf("Literal string: %s\n", s);
}

// No match
void f2(const char* s1, const char* s2) {
  printf(s1, s2);
}
