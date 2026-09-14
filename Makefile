GOSH ?= gosh
TESTS = test/bencode.scm test/handlers.scm test/server.scm

.PHONY: test check
test check:
	@for t in $(TESTS); do $(GOSH) -I lib $$t || exit 1; done
