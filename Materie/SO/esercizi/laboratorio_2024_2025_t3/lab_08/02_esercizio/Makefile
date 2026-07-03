build_write:
	gcc -g semaphore.c shared_mem.c writer.c -o writer.out -D_GNU_SOURCE
build_read:
	gcc -g semaphore.c shared_mem.c reader.c -o reader.out -D_GNU_SOURCE
clean:
	rm *.out