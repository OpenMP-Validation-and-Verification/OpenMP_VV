SHELL=/usr/bin/env sh

include sys/make.def

##################################################
# Verbose & Log
#################################################
QUIET:=@
ifdef VERBOSE
	QUIET:=
endif
RECORD:= 
ifdef LOGS
	RECORD:= | tee -a logs.txt
endif
ifdef LOG_ALL
	RECORD:= 2>&1 | tee -a logs.txt
endif

RUN_TEST=sys/run_test.sh


##################################################
# Source files
#################################################


ifneq "$(SOURCES_C)" ""
OBJS_C := $(SOURCES_C:.c=.c.o)
RUN_C := $(addprefix $(BINDIR)/,$(notdir $(SOURCES_C:.c=.c.run)))
ALL_DEP := $(RUN_C)
COMP_DEP := $(OBJS_C)
endif
ifneq "$(SOURCES_CPP)" ""
OBJS_CPP := $(SOURCES_CPP:.cpp=.cpp.o)
RUN_CPP := $(addprefix $(BINDIR)/,$(notdir $(SOURCES_CPP:.cpp=.cpp.run)))
ALL_DEP := $(RUN_CPP)
COMP_DEP := $(OBJS_CPP)
endif

ifeq "$(or $(SOURCES_C),$(SOURCES_CPP))" ""
SOURCES_C := $(shell find $(CURDIR) -name *.c)
SOURCES_CPP := $(shell find $(CURDIR) -name *.cpp)
OBJS_C := $(SOURCES_C:.c=.c.o)
OBJS_CPP := $(SOURCES_CPP:.cpp=.cpp.o)
RUN_C := $(addprefix $(BINDIR)/,$(notdir $(SOURCES_C:.c=.c.run)))
RUN_CPP := $(addprefix $(BINDIR)/,$(notdir $(SOURCES_CPP:.cpp=.cpp.run)))
ALL_DEP := $(RUN_C) $(RUN_CPP)
COMP_DEP := $(OBJS_C) $(OBJS_CPP) 
endif

##################################################
# FOR RUNNING TESTS ONLY
#################################################

TESTS_TO_RUN ?= $(shell find $(BINDIR) -name *.o)
define run_test
	@echo -e $(TXTGRN)"\n\n" running: $(1) $(TXTNOC) ${RECORD}
	-@$(call loadModules,$(C_COMPILER_MODULE) $(CXX_COMPILER_MODULE)) $(RUN_TEST) $(1) $(VERBOSE) $(RECORD)
endef

.PHONY: all
all: MessageDisplay $(ALL_DEP)
	@echo "====COMPILE AND RUN DONE====" ${RECORD}
	

.PHONY: compile
compile: MessageDisplay $(COMP_DEP)
	@echo "====COMPILE DONE====" ${RECORD}

.PHONY: run
run:
	$(foreach TEST, $(TESTS_TO_RUN), $(call run_test,$(TEST)))
	@echo "====RUN DONE=====" ${RECORD}

.PHONY: MessageDisplay
MessageDisplay:
	@echo "    ====    SOLLVE PROJECT MAKEFILE   ====   "
	@echo "Running make with the following compilers"
	@echo "CC = "$(CC)
	@echo "CXX = "$(GCC)
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
%.c.o: %.c $(BINDIR) 
	@echo -e $(TXTYLW)"\n\n" compile: $< $(TXTNOC) ${RECORD}
	-$(QUIET)$(call loadModules,$(C_COMPILER_MODULE)) $(CCOMPILE) $< -o $(BINDIR)/$(notdir $@) $(RECORD)
	
# c++ files rule
%.cpp.o: %.cpp $(BINDIR)
	@echo -e $(TXTYLW)"\n\n" compile: $< $(TXTNOC) ${RECORD}
	-$(QUIET)$(call loadModules,$(CXX_COMPILER_MODULE)) $(CXXCOMPILE) $< -o $(BINDIR)/$(notdir $@) $(RECORD)

##################################################
# Running tests rules
##################################################
# run c app rule
%.c.run: $(OBJS_C)
	@echo -e $(TXTGRN)"\n\n" running: $@ $(TXTNOC) ${RECORD}
	-@$(call loadModules,$(C_COMPILER_MODULE)) $(RUN_TEST) $(@:.run=.o) $(VERBOSE) $(RECORD)

# run cpp app rule
%.cpp.run: $(OBJS_CPP)
	@echo -e $(TXTGRN)"\n\n" running: $@ $(TXTNOC) ${RECORD}
	-@$(call loadModules,$(CXX_COMPILER_MODULE)) $(RUN_TEST) $(@:.run=.o) $(VERBOSE) $(RECORD)

# Creates the BINDIR folder
$(BINDIR):
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
	@echo "  LOG=1                     Enables dump of the make process output into logs.txt"
	@echo "  LOG_ALL=1                 Enables dump of the make process output, errors, and binary execution outputs into logs.txt"
	@echo "  MODULE_LOAD=1             Before compiling or running, module load is called"
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
	

