outputs = 170613_n1_ql_torque_finite_190 2016-01-27_n1_magdrift_template 2016-08-25_n1_nonlocal_runs 2016-08-24_n1_nomag_nonlin 170613_n1_ql_torque 160915_epsmscan 2016-02-22_n1_0.90 161130_s0.09_nomag_m1e 161130_epsmscan_s0.5_nomag 2016-02-22_n1_nomag 161123_epsmscan_s0.5 2016-03-29_rmp_90_shear 161123_epsmscan_s0.3 2016-03-11_n1_noshear 2016-03-11_n1_shear 170118_n1_nonlin 170118_n1_nomag_ql 161019_n1_nomag_nonlin 160914_n1_epsmscan 170613_n1_ql_torque_190 2016-03-xx_shear 171119_n1_ql_torque_noshear_intoutput 170608_n1_ql_full 2016-03-xx_template 2016-01-28_n1_0.75 171119_n1_ql_torque_nomag_intoutput 161017_epsmscan 170608_n1_ql_finite 170118_n1_noshear_nonlin 160407_rmp90_torfreq 2016-01-28_n1_0.85 2016-02-22_n1_0.70 2016-08-24_n1_shear_nonlin 2016-08-24_n1_shear_newmagfie 161012_rmp90_torfreq_noshear 2016-03-18_shear_0.6 170118_n1_nomag_nonlin 2016-03-xx_noshear 161221_epsmscan_s0.7155_b10 170118_n1_noshear_ql 2016-01-28_n1_nomag 170118_n1_ql 2016-03-18_shear_0.8 161220_epsmscan_s0.7155 2016-01-28_n1_0.45 2016-03-31_rmp_90_n3_noshear 161130_s0.09_nomag 170613_n1_ql_torque_finite 2016-08-24_n1_nomag 170613_n1_ql_torque_full_190 171119_n1_ql_torque_intoutput 2016-01-28_n1_0.6 161130_epsmscan_s0.5_nomag_m1e 2016-02-23_n1_magdrift_test 211018_35712_n1_ql 2016-03-31_rmp_90_n3_shear 2016-03-29_rmp_m150_shear 170613_n1_ql_transp_190 170608_n1_ql 161123_epsmscan_s0.5_m1e 2016-01-27_n1_magdrift 2016-02-22_n1_0.20 2016-02-22_n3_nomag 170608_n1_ql_transp
archive = $(DATA)/AUG/NEO-RT/30835/BEFORE_2021-10-18/BEFORE_2021-10-18.tar.gz

all: .is_extracted

.is_extracted: $(archive)
	tar -xzf $(archive)
	touch .is_extracted

compress:
	tar -czf $(archive) $(outputs)

clean:
	rm -rf $(outputs) .is_extracted
