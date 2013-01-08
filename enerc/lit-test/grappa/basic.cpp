// RUN: %clang -cc1 -verify -std=c++11 -load %libdir/libGrappaTypeChecker.dylib -add-plugin grappa-type-checker %s

#define global __attribute__((qual("global")))
#define blah

int global * make_global(int * a) {
  int global * b;
  return b;
}

int main() {
  int x = 0;
  global int * a = make_global(&x);
  int * b = a; // expected-error {{Assignment of global pointer to non-global pointer.}}

  int y = *a;

  return 0;
}
