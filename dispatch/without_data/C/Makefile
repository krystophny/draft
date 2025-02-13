CC = gcc-14
CXX = g++-14
CFLAGS = -O3 -march=native
CXXFLAGS = -O3 -march=native -std=c++14

all: dispatch_switch.x dispatch_none.x

dispatch_switch.x: dispatch_switch.c
	$(CC) $(CFLAGS) -o dispatch_switch.x dispatch_switch.c

dispatch_none.x: dispatch_none.c
	$(CC) $(CFLAGS) -o dispatch_none.x dispatch_none.c

clean:
	rm -f *.x
