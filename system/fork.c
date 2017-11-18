#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

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

int main(void) {
	int **p = new int*[5];
	int i;
	for (i = 0; i < 5; i++) {
		p[i] = new int[2];
		if (i < 4) {
			pp(p[i]);
		} else {
			p[i][0] = 0;
		}
	}

	if (fk()) {
		// Rodzic
		przepinaj(p, 3, 0);
		char* cmd[] = { "/usr/bin/Rscript", "/home/olekz/soc/test_rodzic.R", NULL };
		ex(cmd);
	} else if (fk()) {
		// Filtr R -> D
		przepinaj(p, 0, 1);
		char* cmd[] = { "/usr/bin/Rscript", "/home/olekz/soc/klikadelko/system/filtr_ssh.R", NULL };
		ex(cmd);
	} else if (fk()) {
		// Dziecko
		przepinaj(p, 1, 2);
		char* cmd[] = { "/usr/bin/ssh", "ja@192.168.0.101", "/usr/bin/Rscript", "/home/ja/soc/test_dziecko.R", NULL };
		ex(cmd);
	} else {
		// Filtr D -> R
		przepinaj(p, 2, 3);
		char* cmd[] = { "/usr/bin/Rscript", "/home/olekz/soc/klikadelko/system/filtr_ssh.R", NULL };
		ex(cmd);
	}
	return 0;
}
