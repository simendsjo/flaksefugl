.PHONY: dev-server test
dev-server:
	LD_LIBRARY_PATH=$(LIBRARY_PATH) sbcl --load dev-server.lisp
test:
	LD_LIBRARY_PATH=$(LIBRARY_PATH) sbcl --non-interactive --eval "(asdf:test-system :flaksefugl)"
