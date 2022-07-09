
LL_BASE = /Users/bos/Projects/linden-hg/trunk
LL_COMMON_H = $(LL_BASE)/indra/llcommon
LL_COMMON_LIB = $(LL_BASE)/indra/build-darwin-i386/llcommon/Release
EXT_LIB_DIR = $(LL_BASE)/libraries/universal-darwin/lib_release


CPPFLAGS += -I $(LL_COMMON_H) -Wall -arch i386 -m32 -O3
xLDFLAGS += -L $(INDRA_LIB)

EXT_LIBS = libapr-1.a libaprutil-1.a libboost_regex-mt.a libexpat.a

LDLIBS += $(LL_COMMON_LIB)/libllcommon.a $(addprefix $(EXT_LIB_DIR)/,$(EXT_LIBS))


testllsd: testdata.o testmain.o
	$(LINK.cpp) $^ $(LOADLIBES) $(LDLIBS) -o $@

testdata.o : testdata.h
testmain.o : testdata.h
