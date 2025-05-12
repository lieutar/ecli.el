#>Makefile for the package ecli.el
#>
EMACS      ?= emacs
CASK       ?= scripts/lcask

PROJECT      := ecli.el
INDEX	     := ecli
SRC_DIR      := src
SRC_INDEX_EL := $(SRC_DIR)/$(INDEX_EL)
META_EL      := $(SRC_DIR)/$(INDEX)/META.el
SUBMOD_DIR   := $(SRC_DIR)/$(INDEX)
EMACS_OPTS   := --batch -Q -l scripts/exec.el

INDEX_EL      := $(INDEX).el
TARGET	      := $(INDEX).elc
INDEX_BUNDLED := $(SRC_DIR)/$(INDEX).bundled.el

################################################################################
.DEFAULT_GOAL := help
#
PHONY := help all build package clean clean-all clean-cask update Makefile-itself\
	test test-unit test-integration

emacs_common = $(EMACS) $(EMACS_OPTS) -L $(SRC_DIR)
emacs_integ  = $(EMACS) $(EMACS_OPTS) -L .
lelde_update = $(emacs_common) -l lelde -f lelde-update-project-files
lelde_fill   = $(emacs_common) -l lelde -f lelde-tinplate-fill
lelde_bundle = $(emacs_common) -l lelde -f lelde-elconc-bundle
lelde_stmax  = $(emacs_common) -l lelde -f lelde-stmax-file

# You can modify by custom.mk
-include custom.mk

.PHONY: $(PHONY)


################################################################################
#>This Makefile has following .PHONY tasks.
#>
#>help
#>    Show this message.
#>
help:
	@grep -e '^#>' Makefile |sed -e 's/^#>//'


################################################################################
#>all
#>    synonym to 'package'.
#>

all: package


################################################################################
#>package
#>    Build package to be able to distribute.
#>

.cask: Cask
	make clean-cask
	$(CASK) install

$(INDEX_EL): $(INDEX_BUNDLED)

$(INDEX_BUNDLED): $(SRC_DIR)/$(INDEX).el .cask
	$(lelde_bundle) $< $@

%.el: %.src.el .cask
	$(lelde_stmax) $<

%.elc: %.el .cask
	$(emacs_common) --eval "(setq byte-compile-error-on-warn t)"\
		-f batch-byte-compile $<

package:
	make update
	make $(INDEX_EL)

################################################################################
#>build
#>    Build the ecli.elc.
#>

build: package
	make $(TARGET)


################################################################################
#>update
#>    Update project files according to Lelde file
#>
#
update :=

update := $(update) Cask
Cask: Lelde
	$(lelde_update) $@

update := $(update) recipe/ecli
recipe/ecli: Lelde
	$(lelde_update) $@

update := $(update) src/ecli/META.el
src/ecli/META.el: Lelde
	$(lelde_update) $@

update := $(update) ecli.el
ecli.el: src/ecli.bundled.el Lelde
	$(lelde_fill) $< $@

update := $(update) README.md
README.md: src/README.md Lelde
	$(lelde_fill) $< $@

#>Makefile-itself
#>    Update Makefile itself.
#>
Makefile-itself:
	$(lelde_update) Makefile

update: $(update) Makefile-itself



################################################################################
#>clean
#>    Remove built files.
#>
clean:
	test -f $(TARGET) && rm $(TARGET) || true
	test -f $(INDEX_EL) && rm $(INDEX_EL) || true
	test -f $(SRC_DIR)/$(INDEX_EL) && rm $(SRC_DIR)/$(INDEX_EL) || true
	test -f $(INDEX_BUNDLED) && rm $(INDEX_BUNDLED) || true

#>clean-all
#>    Remove all generated files.
#>
clean-all: clean clean-cask

#>clean-cask
#>    Remove .cask directory.
#>
clean-cask:
	if test -d .cask; then rm -rf .cask; fi


################################################################################
#>test
#>    Run tests.
#>    If you want to run specific test script,
#>    you can use T environment variable. for example:
#>
#>        T=test/scripts/ecli.test.el make test
#>

T          ?=
I	   ?= $(shell find test/scripts -name '*.test.el')
U	   ?= $(shell find test/scripts -name '*.unit.el')
TCOLS	   ?= 320
LI	   := $(foreach f,$(I),-l $f)
LU	   := $(foreach f,$(U),-l $f)

test_common := $(emacs_common) -l ert 
test_integ  := $(emacs_integ)  -l ert 
test_runner :=   -f ert-run-tests-batch-and-exit
test_truncate := TCOLS=$(TCOLS) scripts/truncate

test:
	@scripts/test $(T)

test-unit:
	@$(test_common) $(LU) $(test_runner) | $(test_truncate)

test-integration: $(INDEX_EL)
	@$(test_integ) $(LI) $(test_runner) | $(test_truncate)
