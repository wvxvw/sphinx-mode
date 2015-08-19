all:
	/usr/local/bin/sbcl --non-interactive \
	--userinit ~/.sbcl-quicklisp-rc \
	--eval "(ql:quickload :sphinx-mode-cl)" \
	--eval "(asdf:operate 'asdf:program-op :sphinx-mode-cl)"

clean:
	rm -f ./sphinx-mode-cl
