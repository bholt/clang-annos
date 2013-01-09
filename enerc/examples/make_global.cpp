#define global __attribute__((qual("global")))

template<typename T>
T global* make_global(T * p) {
  T global* a;
  return a;
}

int main() {
  int x;
  int global* a = make_global(&x);
  *a = 7;

  int y = 3;
  int * b = &y;

  int z = *a + *b;

  return 0;
}