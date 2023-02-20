void func1() {
  pre_call();
  printf("foo: %s\n", global_string);
  post_call(itoa(""));
}

void func2() {
  func1();
}

void func3() {
  strlen();
}
