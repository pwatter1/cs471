#include <stdio.h>
#include <stdlib.h>

int ack(int m, int n) {
	if (m == 0) {
		return n+1;
  	} else if (n == 0) {
		return ack(m-1, 1);
  	} else {
		return ack(m-1, ack(m, n-1));
	}
}

int main(int argc, char **argv) {
	if (argc < 3) {
		printf("Too few arguments for function, needs 2 integers\n");
		exit(-1);
	}
	long a, b;
	a = strtol(argv[1], NULL, 0);
    b = strtol(argv[2], NULL, 0);
	int returnVal;
	returnVal = (int) ack(a,b);
	printf("%d \n", returnVal);
	return 0;
}
