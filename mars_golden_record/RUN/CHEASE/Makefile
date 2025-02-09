
CHEASE_TEMPLATE = $(DATA)/DEMO/EQDSK/Equilibrium_DEMO2019_CHEASE/MOD_Qprof_Test
CREATE_EQUILIBRIUM = $(DATA)/DEMO/EQDSK/Equil_2021_PMI_QH_mode_betap_1d04_li_1d02_Ip_18d27MA_SOF_std.eqdsk

all: run

run: chease_namelist EXPEQ
	$(CODE)/external/CHEASE/src-f90/chease

chease_namelist:
	sed -e "s|NEGP = -1|NEGP = 0|g" \
		-e "s|NER = 1|NER = 2|g" \
		-e "s|NEQDSK = 0|NEQDSK = 1|g" \
		$(CHEASE_TEMPLATE)/chease_namelist > chease_namelist

EXPEQ:
	cp $(CREATE_EQUILIBRIUM) EXPEQ

clean:
	rm -f EQDSK_COCOS_02.OUT EXPEQ.OUT.TOR NUPLO  data_input_equil_chease \
	      plasma_wall_boundary.data EQDSK_COCOS_02_POS.OUT \
	      EXPEQ_EQDSK.IN RZmetric_chease_python.data data_input_metrica_chease \
	      shafranov_shift.data ETAVAC EXPTNZ.OUT chease.bin fort.14 \
          EXPEQ chease.dat mappa_s_chi_chease EXPEQ.OUT NOUT chease_namelist \
	      mappa_schi_RZ_chease_python.data
