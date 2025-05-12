# This file can be removed if isn't necessary.

PHONY := $(PHONY) rebuild

rebuild:
	[ -f  src/ecli/app/installer/rsc.el ] && rm src/ecli/app/installer/rsc.el
	make src/ecli/app/installer/rsc.el
	make clean package
	wc -l ecli.el
