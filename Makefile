.PHONY: clean debug release test

debug:
	cargo build

release: test
	cargo build --release

test:
	cargo test

clean:
	rm -f *.o *.out *.bc output *.S
