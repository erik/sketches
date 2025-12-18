forth: forth.S boot.f
	clang -o forth forth.S

.PHONY: clean
clean:
	rm -f forth


.PHONY: test
test: forth
	./forth test/runner.f
