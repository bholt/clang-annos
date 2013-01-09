// RUN: %clang -cc1 -verify -std=c++11 -load %libdir/libGrappaTypeChecker.dylib -add-plugin grappa-type-checker %s

#define global __attribute__((qual("global")))

struct global Foo {}; // expected-warning {{'qual' attribute only applies to variables and functions}}

// make sure arguments can be global
void foo(int global * a) {}
void foo(int * a, int global * b) { // expected-note {{previous definition}} 
  int global * c;
  a = c; // expected-error {{Assignment of global pointer to non-global pointer.}}
  b = c;
}

// FIXME: Clang doesn't differentiate based on qualifiers...
void foo(int global * a, int global * b) {} // expected-error {{redefinition}} 

int global * make_global(int * a) {
  int * b;
  return b; // expected-error {{global pointer mismatch in return type}}
}

int main() {
  // c++11 alternate syntax supported (not full generalized attribute support though)
  int [[qual("global")]]* foo;

  int x = 0;
 
  global int * a = make_global(&x);
  int * b = a; // expected-error {{Assignment of global pointer to non-global pointer.}}
  a = b; // expected-error {{Assignment of non-global pointer to global pointer.}}

  // Note: C-style casts between global/non-global pointers disallowed
  // FIXME: bad error message, should say something about casts...
  int * v = (int*)a; // expected-error {{Assignment of global pointer to non-global pointer}}

  // FIXME: 'qual' attribute ignored, find out how to get them supported
  int global * vv = (int global*)&x; // expected-error {{attribute ignored}} expected-error {{Assignment of non-global pointer to global pointer}}

  int y = *a; // global deference!
  y = a[0];   // and another one!

  global int z; // expected-error {{invalid qualifier: 'global'}}

  char c = 'a';

  // alternate syntax...
  global int * aa = a;  

  global if (true) {} // expected-error {{expected unqualified-id}}
  global for (int i=0; i<1; i++) {} // expected-error {{expected unqualified-id}}

  return 0;
}
