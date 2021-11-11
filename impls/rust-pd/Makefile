STEPS = step0_repl

$(STEPS): build
build: 
	cargo build --release
	cp target/release/$(STEPS) $(STEPS)
all: build
clean:
	cargo clean
	rm -rf $(STEPS)
