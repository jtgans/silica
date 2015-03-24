LISP_SRCS := \
	$(wildcard ../cl-evdev/*) \
	$(wildcard ../cl-gypsum-client/*) \
	$(wildcard ../silica/*) \
	build.lisp

silica: $(LISP_SRCS)
	sbcl --load build.lisp

clean:
	rm -f silica

test:

.PHONY: clean test
