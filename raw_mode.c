/*
 * Small C program to turn on/off raw mode in terminal from CL FFI
*/
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

struct termios *enable_raw() {
  struct termios *orig_termios = malloc(sizeof(struct termios));
  struct termios raw;

  tcgetattr(STDIN_FILENO, orig_termios);

  raw = *orig_termios;

  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);

  return orig_termios;
}

void disable_raw(struct termios *orig_termios) {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, orig_termios);
  free(orig_termios);
}
