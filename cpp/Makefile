
all: test


#
# Directories
# 

BUILD_DIR = build
OBJ_DIR = $(BUILD_DIR)/objs

$(BUILD_DIR):
	@mkdir -p $@ 2> /dev/null || true

$(OBJ_DIR):
	@mkdir -p $@ 2> /dev/null || true


#
# Sources and Objects
#

COMMON_DIR = indra/llcommon
COMMON_SRCS = llsd.cpp llidl.cpp llsdserialize_impl.cpp \
	llsdserialize_binary.cpp llsdserialize_notation.cpp llsdserialize_xml.cpp \
	llsdserialize_json.cpp \
	stub.cpp

COMMON_TEST_DIR = indra/llcommon/tests
COMMON_TEST_SRCS = llsd_tests.cpp llidl_tests.cpp llsdserialize_tests.cpp

TEST_DIR = indra/test
TEST_SRCS = lltut.cpp test.cpp

SRC_DIRS = $(COMMON_DIR) $(COMMON_TEST_DIR) $(TEST_DIR)
INC_DIRS = $(SRC_DIRS) libraries/include
vpath %.cpp $(SRC_DIRS)


SRCS = $(COMMON_SRCS) $(COMMON_TEST_SRCS) $(TEST_SRCS)
OBJS = $(patsubst %.cpp,$(OBJ_DIR)/%.o,$(SRCS))
DEPS = $(patsubst %.o,%.d,$(OBJS))

deps: $(OBJ_DIR) $(DEPS)
include $(DEPS)


#
# Libraries
#

APR_INC = /usr/include/apr-1.0
APR_LIBS = apr-1 aprutil-1

EXPAT_INC = 
EXPAT_LIBS = expat

LIB_INCS = $(APR_INC) $(EXPAT_INC)
LIB_LIBS = $(APR_LIBS) $(EXPAT_LIBS)

#
# Applications
#

TEST_APP = $(BUILD_DIR)/testllsd
$(TEST_APP): $(OBJS)
	$(LINK.o) $^ $(patsubst %,-l%,$(LIB_LIBS)) -lstdc++ -o $@

test: $(BUILD_DIR) $(TEST_APP)

.PHONY: test


#
# Utility
#

clean:
	rm -f $(OBJ_DIR)/*
	rm -f $(TEST_APP)

distclean:
	rm -rf $(BUILD_DIR)


#
# Rules
#

CXXFLAGS = $(patsubst %,-I%,$(INC_DIRS))
CXXFLAGS += $(patsubst %,-I%,$(LIB_INCS)) -Wall -Werror
CXXFLAGS += -D_POSIX_C_SOURCE
#CXXFLAGS += -DLL_LEGACY

$(OBJ_DIR)/%.o: %.cpp
	$(COMPILE.cpp) $(OUTPUT_OPTION) $<

$(OBJ_DIR)/%.d: %.cpp
	@mkdir -p $(OBJ_DIR) 2> /dev/null || true
	@set -e; $(CXX) -MM $(CXXFLAGS) $(CPPFLAGS) $< \
	| sed 's,\(.*\.o\)\( *:\),$(OBJ_DIR)/\1 $@\2,g' > $@; \
	[ -s $@ ] || rm -f $@

