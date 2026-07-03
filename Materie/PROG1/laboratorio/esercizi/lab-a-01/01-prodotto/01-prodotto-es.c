#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

int main(){
	int x = 5, y = 8, z = 11;

	assert(x == 5 && y == 8 && z == 11);
	
	printf("Il risultato della moltiplicazione e' %d", x*y*z);

	return 0;
}
