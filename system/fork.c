#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void pp(int *pipefd) {
	if (pipe(pipefd) == -1) {
		perror("pipe failed");
		exit(1);
	}
}

pid_t fk() {
	pid_t pid = fork();
	if (pid == -1) {
	  perror("fork failed");
		exit(1);
	}
	return pid;
}

void ex(char** cmd) {
	char **nargv = cmd;
	char *nargp[] = { NULL };
	if (execve(nargv[0], nargv, nargp) == -1) {
		fprintf(stderr, "[R] error in execve: %d\n", errno);
	}
}

char **kopiuj_argv(int argc, char **argv) {
	char **wynik = new char*[argc + 1];
	int i;
	for (i = 0; i < argc; i++) {
		wynik[i] = new char[strlen(argv[i]) + 1];
		strcpy(wynik[i], argv[i]);
	}
	wynik[argc] = NULL;
	return wynik;
}

void przepinaj(int **p, int we, int wy) {
	dup2(p[we][0], 0);
	dup2(p[wy][1], 1);
	int i, j;
	for (i = 0; p[i][0]; i++) {
		for (j = 0; j < 2; j++) {
			if (p[i][j] != p[we][0] && p[i][j] != p[wy][1]) {
				printf("Zamykam: %d %d\n", i, j);
				close(p[i][j]);
			}
		}
	}
}

int main(int argc, char **argv) {
	const int LICZBA_LACZ = 2;
	int **p = new int*[LICZBA_LACZ + 1];
	int i;
	for (i = 0; i < LICZBA_LACZ + 1; i++) {
		p[i] = new int[2];
		if (i < LICZBA_LACZ) {
			pp(p[i]);
		} else {
			p[i][0] = 0;
		}
	}

	if (fk()) {
		// Rodzic
		przepinaj(p, 1, 0);
		char* cmd[] = { "/usr/local/bin/Rscript", "./start_zlozony.R", NULL };
		ex(cmd);
	} else {
		przepinaj(p, 0, 1);
		ex(kopiuj_argv(argc - 1, argv + 1));
	}
	return 0;
}
