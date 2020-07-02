#############################
# User variables
#############################
TB       = tb
SEED     = 2

#TESTNAME ?= mcdf_reg_reset_test	
#CASE = 1
#TESTNAME ?= mcdf_reg_read_write_test	
#CASE = 2
#TESTNAME ?= mcdf_reg_stability_test	
#CASE = 3
#TESTNAME ?= mcdf_reg_illegal_access_test	
#CASE = 4
#TESTNAME ?= mcdf_channel_disable_test		
#CASE = 5
#TESTNAME ?= mcdf_arbiter_priority_test		
#CASE = 6
#TESTNAME ?= mcdf_down_stream_low_bandwidth_test	
#CASE = 7
#TESTNAME ?= mcdf_data_consistence_basic_test	
#CASE = 8
TESTNAME ?= mcdf_full_random_test	
CASE = 9
#TESTNAME ?= mcdf_corner_direct_test	
#CASE = 10
#TESTNAME ?= mcdf_formatter_length_test
#CASE = 11

DFILES   = ../apb_mcdf/{arbiter.v,slave_fifo.v,reg.v,formater.v,mcdf.v}
VFILES  += apb_pkg/apb_pkg.sv arb_pkg.sv chnl_pkg.sv fmt_pkg.sv  apb_mcdf_rgm_pkg.sv apb_mcdf_pkg.sv $(TB).sv 


#############################
# Environment variables
#############################
VCOMP                = vlogan -full64 -ntb_opts uvm-1.1 -sverilog -timescale=1ps/1ps -nc -l comp.log +incdir+apb_pkg +incdir+../apb_mcdf
ELAB                 = vcs -full64 -ntb_opts uvm-1.1 -debug_all -l elab.log -sim_res=1ps -cm line+tgl+branch -cm_hier cm.cfg
RUN                  = ./sd$(SEED)_case$(CASE).simv -l run.log -sml -cm line+tgl+branch -cm_name $(TB)_$(SEED) +ntb_random_seed=$(SEED) +UVM_TESTNAME=$(TESTNAME)

comp:
	$(VCOMP) 
	$(VCOMP) $(DFILES) $(VFILES)

elab: 
	$(ELAB) -top $(TB) -o sd$(SEED)_case$(CASE).simv

run:	elab
	$(RUN) 

rung:
	$(RUN) -gui

editcov:
	urg -full64 -format both -dir $(TB).simv.vdb 
	dve -cov -dir $(TB).simv.vdb

# option for exlucde file given to generate coverage report 
# if there is such a coverage exclusion file
# urg -dir ... -elfile filename.el
viewcov:
	urg -full64 -format both -dir *.simv.vdb -dbname mergedir.vdb -report urgReport
	firefox urgReport/dashboard.html
	#firefox urg -dir mergedir.vdb -hier cover_conf/uncov_vcs -format html

clean:
	rm -rf AN.DB DVEfiles csrc *.simv *.simv.daidir *.simv.vdb *.vdb ucli.key
	rm -rf *.log* *.vpd *.h urgReport

