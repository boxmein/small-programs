#include <stdio.h>

int main() {
  fprintf(stderr, "\e[?1003h\e[?1015h\e[?1006h"); //use stderr since not buffered turn on mouse event capture
  while (1) {
    char x = getchar();
    printf("%d,", x);
    if (x == 'M') {
      puts("\n");
      fflush(stdout);
    }
  }
}
