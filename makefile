PREFIX=.
SRC_PATH=src
TEST_PATH=tests
INCLUDE_PATH=$(PREFIX)/include
LIB_PATH=$(PREFIX)/lib
EXTERNAL_LIBS=$(PREFIX)/external-libs

INCLUDE_FILES=scm-lib_.scm class.scm class_.scm 
LIB_FILES=scm-lib.o1

all: prefix include lib

prefix:
ifneq "$(PREFIX)" "."
	mkdir -p $(PREFIX)
endif

include: $(foreach f,$(INCLUDE_FILES),$(INCLUDE_PATH)/$(f))
$(INCLUDE_PATH)/%.scm: $(SRC_PATH)/%.scm
	mkdir -p $(INCLUDE_PATH)
	cp $< $@

lib: $(foreach f,$(LIB_FILES),$(LIB_PATH)/$(f))
$(LIB_PATH)/%.o1: $(SRC_PATH)/%.scm
	mkdir -p $(LIB_PATH)
	gsc -o $@ $<

setup-libs: setup-scm-lib

$(SRC_PATH)/scm-lib.scm $(SRC_PATH)/scm-lib_.scm: setup-scm-lib
setup-scm-lib:
	mkdir -p $(LIB_PATH)
	mkdir -p $(EXTERNAL_LIBS)
ifeq "$(wildcard $(EXTERNAL_LIBS)/scm-lib)" ""
	cd $(EXTERNAL_LIBS) && git clone git://github.com/sthilaid/scm-lib.git
endif
	cd $(EXTERNAL_LIBS)/scm-lib && git pull
	$(MAKE) -C $(EXTERNAL_LIBS)/scm-lib
	cp $(EXTERNAL_LIBS)/scm-lib/include/* $(SRC_PATH)/
	cp $(EXTERNAL_LIBS)/scm-lib/src/* $(SRC_PATH)/
	cp $(EXTERNAL_LIBS)/scm-lib/lib/* $(LIB_PATH)/

TEST_INCLUDE_FILES=$(INCLUDE_PATH)/scm-lib_.scm \
	                 $(INCLUDE_PATH)/class.scm \
		               $(INCLUDE_PATH)/class_.scm \
                   $(TEST_PATH)/test-macro.scm
TEST_RUN_FILES=$(LIB_PATH)/scm-lib.o1 \
               $(TEST_PATH)/test.o1 \
	             $(TEST_PATH)/class-tests.o1
test: $(TEST_INCLUDE_FILES) $(TEST_RUN_FILES)
	gsi $(TEST_RUN_FILES) -e "(run-tests)"

$(TEST_PATH)/%.o1: $(TEST_PATH)/%.scm
	gsc -o $@ $<


clean:
	rm -rf $(EXTERNAL_LIBS) $(INCLUDE_PATH) $(LIB_PATH) $(TEST_PATH)/*.o1