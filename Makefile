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
ifdef LOGS_ALL
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
	-@$(RUN_TEST) $(1) $(VERBOSE) $(RECORD)
endef

.PHONY: all
all: $(ALL_DEP)
	@echo "====COMPILE AND RUN DONE====" ${RECORD}
	

.PHONY: compile
compile: $(COMP_DEP)
	@echo "====COMPILE DONE====" ${RECORD}

.PHONY: run
run: 
	$(foreach TEST, $(TESTS_TO_RUN), $(call run_test,$(TEST)))
	@echo "====RUN DONE=====" ${RECORD}


##################################################
# Compilation rules
##################################################
# c files rule
%.c.o: %.c $(BINDIR)
	@echo -e $(TXTYLW)"\n\n" compile: $< $(TXTNOC) ${RECORD}
	-$(QUIET)$(CCOMPILE) $< -o $(BINDIR)/$(notdir $@) $(RECORD)
	
# c++ files rule
%.cpp.o: %.cpp $(BINDIR)
	@echo -e $(TXTYLW)"\n\n" compile: $< $(TXTNOC) ${RECORD}
	-$(QUIET)$(CXXCOMPILE) $< -o $(BINDIR)/$(notdir $@) $(RECORD)

##################################################
# Running tests rules
##################################################
# run c app rule
%.c.run: $(OBJS_C)
	@echo -e $(TXTGRN)"\n\n" running: $@ $(TXTNOC) ${RECORD}
	-@$(RUN_TEST) $(@:.run=.o) $(VERBOSE) $(RECORD)

# run cpp app rule
%.cpp.run: $(OBJS_CPP)
	@echo -e $(TXTGRN)"\n\n" running: $@ $(TXTNOC) ${RECORD}
	-@$(RUN_TEST) $(@:.run=.o) $(VERBOSE) $(RECORD)

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
	@echo "Do make CC=ccompiler CXX=cppcompiler [VERBOSE=1] [LOGS=1] [LOGS_ALL=1] [SOURCES_C=file.c] [SOURCES_CPP=file.cpp] [TESTS_TO_RUN=bin/file.o] [rule] (e.g. make CC=clang CXX=clang++ all), where rule may be one of:"
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

