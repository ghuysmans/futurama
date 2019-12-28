#include <stdio.h>

void main() {
	int tmp, v0=0, v1=1, v2=2, v3=3, v4=4, v5=5, v6=6, v7=7, v8=8, v9=9, v10=10, v11=11, v12=12, v13=13, v14=14, v15=15, i;
	for (i=0; i<1000000000; i++) {
#include "dance.c"
	}
	printf("%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n", v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15);
}
