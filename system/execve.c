#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int
main(int argc, char *argv[])
{
  int pipefd[2];
  pipe(pipefd);

  printf("%d %d\n", pipefd[0], pipefd[1]);

  char *newargv[] = { NULL, "-f", "/home/olekz/soc/test_conn.R", NULL };
  char *newenviron[] = { NULL };

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <file-to-exec>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  newargv[0] = argv[1];

  execve(argv[1], newargv, newenviron);
  perror("execve"); /* execve() only returns on error */
  exit(EXIT_FAILURE);
}
