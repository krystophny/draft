MARS_TEMPLATE = $(CODE)/external/MARS/EXAMPLE/Kink
CHEASE_TEMPLATE = $(CODE)/external/MARS/EXAMPLE/Equi

all: run_mars

run_mars: RUN.IN PROFDEN.IN OUTRMAR OUTVMAR
	. /etc/profile.d/modules.sh && module load mars && mpirun marsq.x

RUN.IN PROFDEN.IN:
	cp $(MARS_TEMPLATE)/*.IN .

OUTRMAR OUTVMAR: run_chease

run_chease: datain EXPEQ
	. /etc/profile.d/modules.sh && module load mars && chease.x < datain

datain:
	sed -e "s|NEGP = -1|NEGP = 0|g" \
		-e "s|NER = 1|NER = 2|g" \
		-e "s|NEGP=-1,NER= 1|NEGP= 0,NER= 2|g" \
		$(CHEASE_TEMPLATE)/datain > datain

EXPEQ:
	cp $(CHEASE_TEMPLATE)/EXPEQ EXPEQ

clean:
	@echo "Removing all files and directories except 'Makefile'..."
	@find . -mindepth 1 -maxdepth 1 ! -name 'Makefile' -exec rm -rf {} +
	@echo "Done."

.PHONY: all run_chease run_mars inputs clean
