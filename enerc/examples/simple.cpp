#define global __attribute__((qual("global")))

int main() {
  int global* a;
  *a = 7;
  int y = *a;

  return 0;
}