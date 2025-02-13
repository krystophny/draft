RUSTC = rustc
RFLAGS = -C opt-level=3 -C target-cpu=native

all: dispatch_class.x dispatch_switch.x

dispatch_class.x: dispatch_class.rs
	$(RUSTC) $(RFLAGS) -o dispatch_class.x dispatch_class.rs

dispatch_switch.x: dispatch_switch.rs
	$(RUSTC) $(RFLAGS) -o dispatch_switch.x dispatch_switch.rs

clean:
	rm -f *.x *.o
