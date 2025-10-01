GO = go
GOFLAGS =

all: dispatch_class

dispatch_class: dispatch_class.go
	$(GO) build $(GOFLAGS) dispatch_class.go

asm: dispatch_class.s

dispatch_class.s: dispatch_class.go
	$(GO) build -gcflags="-S" dispatch_class.go 2> dispatch_class.s

clean:
	rm -f dispatch_class *.s
