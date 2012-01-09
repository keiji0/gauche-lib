TEST_LOG   = test.log
TEST_FILES = *.scm.test
CLEAN_FILES = *.log *.tmp

.PHONY: all test check clean

all: @echo "make check"

test check:
	@for i in $(TEST_FILES) ;{ ./test $$i >> $(TEST_LOG); }

clean:
	@rm -rf $(CLEAN_FILES)
	@echo "Cleanning: " $(CLEAN_FILES)
