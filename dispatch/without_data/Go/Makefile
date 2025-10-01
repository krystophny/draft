GO = go
GOFLAGS =

all: dispatch_switch

dispatch_switch: dispatch_switch.go
	$(GO) build $(GOFLAGS) dispatch_switch.go

asm: dispatch_switch.s

dispatch_switch.s: dispatch_switch.go
	$(GO) build -gcflags="-S" dispatch_switch.go 2> dispatch_switch.s

clean:
	rm -f dispatch_switch *.s
