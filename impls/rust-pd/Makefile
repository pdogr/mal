STEPS = step0_repl step1_read_print step2_eval step3_env step4_if_fn_do step5_tco
build: 
	cargo build --release
$(STEPS): build
	cp target/release/$@ $@
all: $(STEPS)
clean:
	cargo clean
	rm -rf $(STEPS)
