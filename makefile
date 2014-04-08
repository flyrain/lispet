LIBS=-ledit -lm
all:
	cc -o lispet -std=c99 lispet.c mpc.c $(LIBS) -g
clean:
	rm -f lispet core 
