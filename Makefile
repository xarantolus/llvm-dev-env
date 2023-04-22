.PHONY: clean debug release test

debug:
	cargo build

release:
	cargo build --release

test:
	cargo test

clean:
	rm -f *.o *.out *.bc output *.S
