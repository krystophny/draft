
CHEASE_TEMPLATE = $(DATA)/DEMO/EQDSK/Equilibrium_DEMO2019_CHEASE/MOD_Qprof_Test
CHEASE_EQUILIBRIUM = $(DATA)/DEMO/EQDSK/Equilibrium_DEMO2019_CHEASE/MOD_Qprof_Test/EXPEQ.OUT

all: run

run: chease_namelist EXPEQ
	$(CODE)/external/CHEASE/src-f90/chease

chease_namelist:
	sed -e "s|NEGP = -1|NEGP = 0|g" \
		-e "s|NER = 1|NER = 2|g" \
		-e "s|NEGP=-1,NER= 1|NEGP= 0,NER= 2|g" \
		$(CHEASE_TEMPLATE)/chease_namelist > chease_namelist

EXPEQ:
	cp $(CHEASE_EQUILIBRIUM) EXPEQ

clean:
	rm -f EQDSK_COCOS_02.OUT EXPEQ.OUT.TOR NUPLO  data_input_equil_chease \
	      plasma_wall_boundary.data EQDSK_COCOS_02_POS.OUT \
	      EXPEQ_EQDSK.IN RZmetric_chease_python.data data_input_metrica_chease \
	      shafranov_shift.data ETAVAC EXPTNZ.OUT chease.bin fort.14 \
          EXPEQ chease.dat mappa_s_chi_chease EXPEQ.OUT NOUT chease_namelist \
	      mappa_schi_RZ_chease_python.data EXPEQ_EXPEQ.IN

.PHONY: all run clean
