PREFIX=.
BUILD_PATH=$(PREFIX)/build
INCLUDE_PATH=$(PREFIX)/include
LIB_PATH=$(PREFIX)/lib
EXTERNAL_LIBS=$(PREFIX)/external-libs

INCLUDE_FILES=class.scm class_.scm 
LIB_FILES=scm-lib.o1

vpath %.o1 build

all: prefix setup-libs include

prefix:
ifneq "$(PREFIX)" ""
	mkdir -p $(PREFIX)
endif

include: $(foreach f,$(INCLUDE_FILES),$(INCLUDE_PATH)/$(f))
$(INCLUDE_PATH)/%.scm: %.scm
	mkdir -p $(INCLUDE_PATH)
	cp $< $@

$(BUILD_PATH)/%.o1: %.scm
	mkdir -p $(BUILD_PATH)
	gsc -o $@ $<

setup-libs: setup-scm-lib
setup-scm-lib:
	mkdir -p $(INCLUDE_PATH)
	mkdir -p $(LIB_PATH)
	mkdir -p $(EXTERNAL_LIBS)
ifeq "$(wildcard $(EXTERNAL_LIBS)/scm-lib)" ""
	cd $(EXTERNAL_LIBS) && git clone git://github.com/sthilaid/scm-lib.git
endif
	cd $(EXTERNAL_LIBS)/scm-lib && git pull
	$(MAKE) -C $(EXTERNAL_LIBS)/scm-lib
	cp $(EXTERNAL_LIBS)/scm-lib/include/* $(INCLUDE_PATH)/
	cp $(EXTERNAL_LIBS)/scm-lib/lib/* $(LIB_PATH)/

TEST_INCLUDE_FILES=$(INCLUDE_PATH)/class.scm $(INCLUDE_PATH)/class_.scm test-macro.scm
TEST_RUN_FILES=$(LIB_PATH)/scm-lib.o1 $(BUILD_PATH)/test.o1 $(BUILD_PATH)/class-tests.o1
test: setup-libs $(TEST_INCLUDE_FILES) $(TEST_RUN_FILES)
	gsi $(TEST_RUN_FILES) -e "(run-tests)"

clean:
	rm -rf $(EXTERNAL_LIBS) $(INCLUDE_PATH) $(LIB_PATH)