SHELL=/bin/bash -o pipefail

.DEFAULT_GOAL:=help

include sys/make.def

##################################################
# Verbose & Log
#################################################
QUIET:=@
ifdef VERBOSE
  QUIET:=
endif

RECORD:=
LOGDIR:= 
ifdef LOG
  LOGDIR:= logs
  RECORD:= | tee -a $(LOGDIR)/
endif
ifdef LOG_ALL
  LOG:=1 # LOG_ALL implies LOG
  LOGDIR:= logs
  RECORD:= 2>&1 | tee -a $(LOGDIR)/
endif

ifdef VERBOSE_TESTS
  VERBOSE_MODE = -DVERBOSE_MODE=1
endif

BSRUN:=
ifdef ADD_BATCH_SCHED
  BSRUN:= $(BATCH_SCHEDULER)  
endif

RUN_TEST=$(CURDIR)/sys/run_test.sh


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

# parameters (1) Action (2) Filename (3) Log File 
define log_section_header
  -$(if $(LOG), @echo -e "*-*-*"$(1)"*-*-*"$(2)"*-*-*$$(date)*-*-*" >> $(LOGDIR)/$(3);,)
endef

# parameters (1) Action (2) Output status (3) Log File 
define log_section_footer
  -$(if $(LOG), @echo -e "*-*-*END*-*-*"$(1)"*-*-*$$(date)*-*-*"$(2)"*-*-*\n" >> $(LOGDIR)/$(3);,)
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
	@echo "CC = "$(CC)
	@echo "CXX = "$(CXX)
	$(if $(MODULE_LOAD), @echo "C_MODULE = "$(C_COMPILER_MODULE); echo "CXX_MODULE = "$(CXX_COMPILER_MODULE);,)

##################################################
# Loading modules
##################################################

define loadModules
	$(if $(MODULE_LOAD), module load $(1) $(CUDA_MODULE) $(if $(QUIET), > /dev/null 2> /dev/null,);,)
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
	$(call log_section_header,"COMPILE CC="${CCOMPILE},$<,$(notdir $(@:.o=.log)))
	-$(QUIET)$(call loadModules,$(C_COMPILER_MODULE)) $(CCOMPILE) $(VERBOSE_MODE) $< -o $(BINDIR)/$(notdir $@) $(if $(LOG),$(RECORD)$(notdir $(@:.o=.log))\
		&& echo "PASS" > $(LOGTEMPFILE) \
		|| echo "FAIL" > $(LOGTEMPFILE))
	-$(call log_section_footer,"COMPILE CC="${CCOMPILE},$$(cat $(LOGTEMPFILE)),$(notdir $(@:.o=.log)))
	-@$(if $(LOG), rm $(LOGTEMPFILE))
	
# c++ files rule
%.cpp.o: %.cpp $(BINDIR) $(LOGDIR)
	@echo -e $(TXTYLW)"\n\n" compile: $< $(TXTNOC) 
	$(call log_section_header,"COMPILE CPP="${CXXCOMPILE},$<,$(notdir $(@:.o=.log)))
	-$(QUIET)$(call loadModules,$(CXX_COMPILER_MODULE)) $(CXXCOMPILE) $(VERBOSE_MODE) $< -o $(BINDIR)/$(notdir $@) $(if $(LOG),$(RECORD)$(notdir $(@:.o=.log))\
		&& echo "PASS" > $(LOGTEMPFILE) \
		|| echo "FAIL" > $(LOGTEMPFILE))
	-$(call log_section_footer,"COMPILE",$$(cat $(LOGTEMPFILE)),$(notdir $(@:.o=.log)))
	-@$(if $(LOG), rm $(LOGTEMPFILE))

##################################################
# Running tests rules
##################################################
# run c app rule
%.run: $(OBJS_C) $(OBJS_CPP)
	$(call log_section_header,"RUN",$(@:.run=),$(notdir $(@:.run=.log)))
	@echo -e $(TXTGRN)"\n\n" running: $@ $(TXTNOC) $(if $(LOG), ${RECORD}$(notdir $(@:.run=.log)))
	-$(call loadModules,$(C_COMPILER_MODULE)) $(BSRUN)$(RUN_TEST) $(@:.run=.o) $(VERBOSE) $(if $(LOG),$(RECORD)$(notdir $(@:.run=.log))\
		&& echo "PASS" > $(LOGTEMPFILE) \
		|| echo "FAIL" > $(LOGTEMPFILE))
	-$(call log_section_footer,"RUN",$$(cat $(LOGTEMPFILE)),$(notdir $(@:.run=.log)))
	-@$(if $(LOG), rm $(LOGTEMPFILE))

# Creates the BINDIR folder
$(BINDIR):
	mkdir $@

$(LOGDIR):
	mkdir $@

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
	@echo ""
	@echo " === EXAMPLES ==="
	@echo "  make CC=gcc CXX=g++ all                 ==> compile and run all test cases with GCC"
	@echo "  make CC=gcc SOURCES_C=a.c all           ==> compile and run a.c with gcc"
	@echo "  make CXX=g++ SOURCES_CPP=a.cpp all      ==> compile and run a.cpp with g++"
	@echo "  make CC=xlc CXX=xlc++ compile           ==> compile all test cases with XL"
	@echo "  make run                                ==> run all the cases that exist inside bin/"
	@echo "  make TESTS_TO_RUN=bin/myTest run        ==> run myTest "
	@echo ""
	
.DEFAULT_GOAL := help
