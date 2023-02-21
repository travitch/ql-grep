// No match
void f1(const char* s) {
  printf("Literal string: %s\n", s);
}

// Match
void f2(const char* s1, const char* s2) {
  printf(s1, s2);
}

int f3(int x) {
  printf("fmt %d\n", x);

  return x + 1;
}
