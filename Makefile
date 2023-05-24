
UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ARCH := elf64
endif
ifeq ($(UNAME), Darwin)
ARCH := macho64
endif

test/%.s: test/%.snek src/main.rs
	cargo run -- $< test/$*.s

test/%.run: test/%.s runtime/start.rs
	nasm -f $(ARCH) test/$*.s -o runtime/our_code.o
	ar rcs runtime/libour_code.a runtime/our_code.o
	rustc -L runtime/ runtime/start.rs -o test/$*.run

tests/%.s: tests/%.snek src/main.rs
	cargo run -- $< tests/$*.s

tests/%.run: tests/%.s runtime/start.rs
	nasm -f $(ARCH) tests/$*.s -o tests/$*.o
	ar rcs tests/lib$*.a tests/$*.o
	rustc -L tests/ -lour_code:$* runtime/start.rs -o tests/$*.run

test/%.alls: test/%.s runtime/start.rs
	rustc --emit asm -L runtime/ runtime/start.rs -o test/$*.alls

.PRECIOUS: test/%.s

clean:
	rm -rf test/*.a tests/*.s tests/*.run tests/*.o
	rm -rf test/*.a test/*.s test/*.run test/*.o
	rm -rf test/input*.a test/input/*.s test/input/*.run test/input*.o
