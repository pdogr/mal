STEPS = step0_repl step1_read_print step2_eval
build: 
	cargo build --release
$(STEPS): build
	cp target/release/$@ $@
all: $(STEPS)
clean:
	cargo clean
	rm -rf $(STEPS)
