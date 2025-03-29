MARS_TEMPLATE = $(CODE)/external/MARS/EXAMPLE/Kink
CHEASE_TEMPLATE = $(CODE)/external/MARS/EXAMPLE/Equi

ifeq ($(shell uname),Darwin)
	SED = gsed
else
	SED = sed
endif

all: run_mars

run_mars: RUN.IN PROFDEN.IN OUTRMAR OUTVMAR
	. /etc/profile.d/modules.sh && module load mars && mpirun marsq.x || true

RUN.IN PROFDEN.IN:
	cp $(MARS_TEMPLATE)/*.IN .
	$(SED) -i "s|NPROFT = 3|NPROFT = 2|g" RUN.IN

OUTRMAR OUTVMAR: run_chease

run_chease: datain EXPEQ
	. /etc/profile.d/modules.sh && module load mars && chease.x < datain

datain:
	$(SED) -e "s|NEGP = -1|NEGP = 0|g" \
		-e "s|NER = 1|NER = 2|g" \
		-e "s|NEGP=-1,NER= 1|NEGP= 0,NER= 2|g" \
		$(CHEASE_TEMPLATE)/datain > datain

EXPEQ:
	cp $(CHEASE_TEMPLATE)/EXPEQ EXPEQ

clean:
	@echo "Removing all files and directories except 'Makefile'..."
	@find . -mindepth 1 -maxdepth 1 ! -name 'Makefile' ! -name '.gitignore' -exec rm -rf {} +
	@echo "Done."

.PHONY: all run_chease run_mars inputs clean
