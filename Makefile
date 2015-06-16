LISP_SRCS := \
	$(wildcard ../cl-evdev/*) \
	$(wildcard ../cl-gypsum-client/*) \
	$(wildcard ../silica/*) \
	build.lisp

silica: $(LISP_SRCS)
	ASDF_OUTPUT_TRANSLATIONS=/:$(shell pwd)/../out/silica-fasl sbcl --load build.lisp

clean:
	rm -f ../out/silica
	rm -rf ../out/silica-fasl

test:

.PHONY: clean test
