#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

int main(){

	int var1 = 1;
	int var2 = 4;
	int var3 = 9;
	int var4 = 8;
	int temp = 0;

	printf("var1 : %d\n",var1);
	printf("var2 : %d\n",var2);
	printf("var3 : %d\n",var3);
	printf("var4 : %d\n",var4);

	printf("\n\nscambio..\n\n");

	// scambio var1 <-> var2
	temp = var1;
	var1 = var2;
	var2 = temp;


	// scambio var2 <-> var3
	temp = var2;
	var2 = var3;
	var3 = temp;

	// scambio var3 <-> var4
	temp = var3;
	var3 = var4;
	var4 = temp;

	// scambio var4 <-> var1
	temp = var4;
	var4 = var1;
	var1 = temp;

	printf("var1 : %d\n", var1);
	printf("var2 : %d\n", var2);
	printf("var3 : %d\n", var3);
	printf("var4 : %d\n", var4);

	return 0;
}
