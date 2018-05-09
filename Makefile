SHELL=/bin/bash -o pipefail

.DEFAULT_GOAL:=help

ifdef SYSTEM
	-include sys/systems/$(SYSTEM).def
endif

include sys/make/make.def

LOG_NOTE?="none"

##################################################
# Verbose & Log
#################################################
QUIET:=@
ifdef VERBOSE
  QUIET:=
endif

RECORD:=
LOGDIR:= 
LOGDIRNAME:= logs
ifdef LOG
  LOGDIR:= $(LOGDIRNAME)
  RECORD:= | tee -a $(LOGDIR)/
endif
ifdef LOG_ALL
  LOG:=1 # LOG_ALL implies LOG
  LOGDIR:= $(LOGDIRNAME)
  RECORD:= 2>&1 | tee -a $(LOGDIR)/
endif

ifdef VERBOSE_TESTS
  VERBOSE_MODE = -DVERBOSE_MODE=1
endif

BSRUN:=
ifdef ADD_BATCH_SCHED
  BSRUN:= $(BATCH_SCHEDULER)  
endif

# Test running and results analyzer
RUN_TEST=$(CURDIR)/sys/scripts/run_test.sh
RESULTS_ANALYZER=$(CURDIR)/sys/scripts/createSummary.py
RESULTS_JSON_OUTPUT_FILE=results.json
RESULTS_HTML_OUTPUT_FOLDER=results_report
RESULTS_HTML_REPORT_TEMPLATE=$(CURDIR)/sys/results_template

##################################################
# Source files
#################################################


ifneq "$(SOURCES_C)" ""
OBJS_C := $(SOURCES_C:.c=.c.o)
RUN_DEP := $(addprefix $(BINDIR)/,$(notdir $(SOURCES_C:.c=.c.run)))
ALL_DEP := $(RUN_DEP)
COMP_DEP := $(OBJS_C)
endif
ifneq "$(SOURCES_CPP)" ""
OBJS_CPP := $(SOURCES_CPP:.cpp=.cpp.o)
RUN_DEP := $(addprefix $(BINDIR)/,$(notdir $(SOURCES_CPP:.cpp=.cpp.run)))
ALL_DEP := $(RUN_DEP)
COMP_DEP := $(OBJS_CPP)
endif

ifeq "$(or $(SOURCES_C),$(SOURCES_CPP))" ""
SOURCES_C := $(shell find $(CURDIR) -name *.c)
SOURCES_CPP := $(shell find $(CURDIR) -name *.cpp)
OBJS_C := $(SOURCES_C:.c=.c.o)
OBJS_CPP := $(SOURCES_CPP:.cpp=.cpp.o)
RUN_DEP := $(addprefix $(BINDIR)/,$(notdir $(SOURCES_C:.c=.c.run)))
RUN_DEP += $(addprefix $(BINDIR)/,$(notdir $(SOURCES_CPP:.cpp=.cpp.run)))
ALL_DEP := $(RUN_DEP)
COMP_DEP := $(OBJS_C) $(OBJS_CPP) 
endif

# parameters (1) Action (2) System (3) Filename (4) other Info (compiler) (5) Log File 
define log_section_header
  -$(if $(LOG), @echo -e "*-*-*BEGIN*-*-*"$(1)"*-*-*$$(date)*-*-*"$(2)"*-*-*"$(3)"*-*-*"$(4)"*-*-*" >> $(LOGDIR)/$(5);,)
endef

# parameters (1) Action (2) System (3) Output status  (4) other Info (compiler) (5) Log File 
define log_section_footer
  -$(if $(LOG), @echo -e "*-*-*END*-*-*"$(1)"*-*-*$$(date)*-*-*"$(2)"*-*-*"$(3)"*-*-*"$(4)"\n" >> $(LOGDIR)/$(5);,)
endef

.PHONY: all
all: MessageDisplay $(ALL_DEP)
	@echo "====COMPILE AND RUN DONE===="
	

.PHONY: compile
compile: MessageDisplay $(COMP_DEP)
	@echo "====COMPILE DONE===="

##################################################
# FOR RUNNING TESTS ONLY
#################################################

TESTS_TO_RUN ?= $(shell find $(BINDIR) -name *.o)
RUN_TESTS = $(TESTS_TO_RUN:.o=.o.run)

.PHONY: run
run: $(RUN_TESTS)
	@echo "====RUN DONE====="

%.o.run: 
	@echo -e $(TXTGRN)"\n\n running:" $(@:.run=) $(TXTNOC) ${RECORD}$(notdir $(@:.run=))
	-@$(call loadModules,$(C_COMPILER_MODULE) $(CXX_COMPILER_MODULE)) $(BSRUN)$(RUN_TEST) $(@:.run=) $(VERBOSE) $(RECORD)$(notdir $(@:.run=))

.PHONY: MessageDisplay
MessageDisplay:
	@echo "    ====    SOLLVE PROJECT MAKEFILE   ====   "
	@echo "Running make with the following compilers"
	@echo "CC = "$(CC) $(shell $(call loadModules,$(C_COMPILER_MODULE),"shut up") ${C_VERSION})
	@echo "CXX = "$(CXX) $(shell $(call loadModules,$(CXX_COMPILER_MODULE),"shut up") ${CXX_VERSION})
	$(if $(MODULE_LOAD), @echo "C_MODULE = "$(C_COMPILER_MODULE); echo "CXX_MODULE = "$(CXX_COMPILER_MODULE);,)

##################################################
# Loading modules
##################################################

define loadModules
	$(if $(MODULE_LOAD), module load $(1) $(CUDA_MODULE) $(if $(or $(QUIET), $(2)), > /dev/null 2> /dev/null,);,)
endef

##################################################
# Turn off offloading
##################################################

ifdef NO_OFFLOADING
	COFFLOADING = $(C_NO_OFFLOADING)
	CXXOFFLOADING = $(CXX_NO_OFFLOADING)
endif

##################################################
# Compilation rules
##################################################
# c files rule
%.c.o: %.c $(BINDIR) $(LOGDIR)
	@echo -e $(TXTYLW)"\n\n" compile: $< $(TXTNOC)
	$(call log_section_header,"COMPILE CC="${CCOMPILE},$(SYSTEM),$<,$(CC) $(shell $(call loadModules,$(C_COMPILER_MODULE),"shut up") $(C_VERSION)),$(notdir $(@:.o=.log)))
	-$(QUIET)$(call loadModules,$(C_COMPILER_MODULE)) $(CCOMPILE) $(VERBOSE_MODE) $< -o $(BINDIR)/$(notdir $@) $(if $(LOG),$(RECORD)$(notdir $(@:.o=.log))\
		&& echo "PASS" > $(LOGTEMPFILE) \
		|| echo "FAIL" > $(LOGTEMPFILE))
	-$(call log_section_footer,"COMPILE CC="${CCOMPILE},$(SYSTEM),$$(cat $(LOGTEMPFILE)),$(LOG_NOTE),$(notdir $(@:.o=.log)))
	-@$(if $(LOG), rm $(LOGTEMPFILE))
	
# c++ files rule
%.cpp.o: %.cpp $(BINDIR) $(LOGDIR)
	@echo -e $(TXTYLW)"\n\n" compile: $< $(TXTNOC) 
	$(call log_section_header,"COMPILE CPP="${CXXCOMPILE},$(SYSTEM),$<,$(CXX) $(shell $(call loadModules,$(CXX_COMPILER_MODULE),"shut up") $(CXX_VERSION)),$(notdir $(@:.o=.log)))
	-$(QUIET)$(call loadModules,$(CXX_COMPILER_MODULE)) $(CXXCOMPILE) $(VERBOSE_MODE) $< -o $(BINDIR)/$(notdir $@) $(if $(LOG),$(RECORD)$(notdir $(@:.o=.log))\
		&& echo "PASS" > $(LOGTEMPFILE) \
		|| echo "FAIL" > $(LOGTEMPFILE))
	-$(call log_section_footer,"COMPILE",$(SYSTEM),$$(cat $(LOGTEMPFILE)),$(LOG_NOTE),$(notdir $(@:.o=.log)))
	-@$(if $(LOG), rm $(LOGTEMPFILE))

##################################################
# Running tests rules
##################################################
# run c app rule
%.run: $(OBJS_C) $(OBJS_CPP)
	$(call log_section_header,"RUN",$(SYSTEM),$(@:.run=),$(LOG_NOTE),$(notdir $(@:.run=.log)))
	@echo -e $(TXTGRN)"\n\n" running: $@ $(TXTNOC) $(if $(LOG), ${RECORD}$(notdir $(@:.run=.log)))
	-$(call loadModules,$(C_COMPILER_MODULE)) $(BSRUN)$(RUN_TEST) $(@:.run=.o) $(VERBOSE) $(if $(LOG),$(RECORD)$(notdir $(@:.run=.log))\
		&& echo "PASS" > $(LOGTEMPFILE) \
		|| echo "FAIL" > $(LOGTEMPFILE))
	-$(call log_section_footer,"RUN",$(SYSTEM),$$(cat $(LOGTEMPFILE)),$(LOG_NOTE),$(notdir $(@:.run=.log)))
	-@$(if $(LOG), rm $(LOGTEMPFILE))

# Creates the BINDIR folder
$(BINDIR):
	mkdir $@

$(LOGDIR):
	mkdir $@

$(RESULTS_JSON_OUTPUT_FILE): 
	@echo "Creating $(RESULTS_JSON_OUTPUT_FILE) file"
	@echo "Currently we only support run logs that contain compilation and run outputs. Use the 'make all' rule to obtain these"
	@$(RESULTS_ANALYZER) -f json -o $(RESULTS_JSON_OUTPUT_FILE) $(LOGDIRNAME)/*

.PHONY: report_json
report_json: $(RESULTS_JSON_OUTPUT_FILE)
	@echo " === REPORT DONE === "

.PHONY: report_html
report_html: $(RESULTS_JSON_OUTPUT_FILE)
	@if [ -d "./$(RESULTS_HTML_OUTPUT_FOLDER)" ]; then \
    echo "A report exist already. Please move it before creating a new one"; \
	 else \
	  echo " === CREATING REPORT === "; \
		mkdir $(RESULTS_HTML_OUTPUT_FOLDER); \
		echo " folder $(RESULTS_HTML_OUTPUT_FOLDER) created"; \
	  cp -r $(RESULTS_HTML_REPORT_TEMPLATE)/* $(RESULTS_HTML_OUTPUT_FOLDER); \
		echo " template copied"; \
		mv $(RESULTS_JSON_OUTPUT_FILE) $(RESULTS_HTML_OUTPUT_FOLDER); \
		sed -i "1s/.*/var jsonResults = \[/g" $(RESULTS_HTML_OUTPUT_FOLDER)/$(RESULTS_JSON_OUTPUT_FILE); \
		sed -i "$$ s/.*/];/g" $(RESULTS_HTML_OUTPUT_FOLDER)/$(RESULTS_JSON_OUTPUT_FILE); \
		echo " json file processed"; \
	fi;
	
	@echo " === REPORT DONE === "

.PHONY: clean
clean:
	- rm -r $(BINDIR)

.PHONY: compilers
compilers:
	@echo "C compilers: "$(CCOMPILERS)
	@echo "C++ compilers: "$(CXXCOMPILERS)

.PHONY: help
help:
	@echo "OpenMP Offloading Validation Suite"
	@echo ""
	@echo " === USE ==="
	@echo "  make CC=ccompiler CXX=cppcompiler [OPTIONS] [RULE]"
	@echo ""
	@echo " === OPTIONS === "
	@echo "  VERBOSE=1                 Enables output of the commands that the make process executes"
	@echo "  VERBOSE_TESTS=1           Enables extra information display in the tests"
	@echo "  LOG=1                     Enables dump of the make process output into logs.txt"
	@echo "  LOG_ALL=1                 Enables dump of the make process output, errors, and binary execution outputs into logs.txt"
	@echo "  SYSTEM=sys_name           Includes the definitions for the requires modules and batch schedulers in the different systems"
	@echo "                            This definitions must be in sys/SYSTEM.def. (Do not include the .def extension)"
	@echo "  MODULE_LOAD=1             Before compiling or running, module load is called"
	@echo "  ADD_BATCH_SCHED=1         Add the jsrun command before the execution of the running script to send it to a compute node"
	@echo "  NO_OFFLOADING=1           Turn off offloading"
	@echo "  SOURCES_C=file.c          Specify the C file(s) that you want to apply the rule to. Cannot be combined with SOURCES_CPP"
	@echo "  SOURCES_CPP=file.cxx      Specify the CPP file(s) that you want to apply the rule to. Cannot be combined with SOURCES_C"
	@echo "  TESTS_TO_RUN=bin/file.o   Specify the binaries to run"
	@echo ""
	@echo " === RULES ==="
	@echo "  all"
	@echo "    Build and run SOURCES_CPP or SOURCES_C. If none is specified build and run all the OpenMP test files"
	@echo "  run"
	@echo "    run either TESTS_TO_RUN list, or all the OpenMP tests that are available within bin/ directory"
	@echo "  compile"
	@echo "    Compile the specific SOURCES_CPP or SOURCES_C files. If none is specified compile all the OpenMP test files"
	@echo "  clean"
	@echo "    Remove all executables from bin/ directory"
	@echo "  compilers"
	@echo "    Shows available compiler configuration"
	@echo "  report_json"
	@echo "    create a json file containing the results existing in the logs files inside the $(LOGDIRNAME) folder"
	@echo "    currently we only support runs that contain output for compile and run"
	@echo "  report_html"
	@echo "    create an html based results report. This rule takes the json file and a prebuild template and creates"
	@echo "    the $(RESULTS_HTML_OUTPUT_FOLDER) folder containing the report. This report allows filtering the results"
	@echo "    by system, compiler, and pass/fail result. It also allows to see the output of each tests"
	@echo ""
	@echo " === EXAMPLES ==="
	@echo "  make CC=gcc CXX=g++ all                 ==> compile and run all test cases with GCC"
	@echo "  make CC=gcc SOURCES_C=a.c all           ==> compile and run a.c with gcc"
	@echo "  make CXX=g++ SOURCES_CPP=a.cpp all      ==> compile and run a.cpp with g++"
	@echo "  make CC=xlc CXX=xlc++ compile           ==> compile all test cases with XL"
	@echo "  make run                                ==> run all the cases that exist inside bin/"
	@echo "  make report_html                        ==> Using the logs file created with the LOG option, create a report"
	@echo "  make TESTS_TO_RUN=bin/myTest run        ==> run myTest "
	@echo ""
	
.DEFAULT_GOAL := help
