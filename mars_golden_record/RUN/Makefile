SUBDIRS = MARS BOOZER_MATLAB

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir clean; done

.PHONY: all $(SUBDIRS) clean
