CC ?= gcc-14
CXX ?= g++-14
CFLAGS ?= -O3 -march=native
CXXFLAGS ?= -O3 -march=native -std=c++14

all: dispatch_switch.x dispatch_pointer.x dispatch_class.x

dispatch_pointer.x: dispatch_pointer.c
	$(CC) $(CFLAGS) -o dispatch_pointer.x dispatch_pointer.c

dispatch_switch.x: dispatch_switch.c
	$(CC) $(CFLAGS) -o dispatch_switch.x dispatch_switch.c

dispatch_class.x: dispatch_class.cpp
	$(CXX) $(CXXFLAGS) -o dispatch_class.x dispatch_class.cpp

asm: dispatch_switch.s dispatch_pointer.s dispatch_class.s

dispatch_pointer.s: dispatch_pointer.c
	$(CC) $(CFLAGS) -S -fverbose-asm -o dispatch_pointer.s dispatch_pointer.c

dispatch_switch.s: dispatch_switch.c
	$(CC) $(CFLAGS) -S -fverbose-asm -o dispatch_switch.s dispatch_switch.c

dispatch_class.s: dispatch_class.cpp
	$(CXX) $(CXXFLAGS) -S -fverbose-asm -o dispatch_class.s dispatch_class.cpp

clean:
	rm -f *.x *.s
