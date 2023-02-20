// Match
void f1(const char* s) {
  fprintf(stderr, "Literal string: %s\n", s);
}

// No match
void f2(const char* s1, const char* s2) {
  fprintf(stderr, s1, s2);
}
