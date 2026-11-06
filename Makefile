SCHEME ?= scheme
CAP ?=
OBJ := build/obj
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --script tests/run.ss
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 300
BENCH_PORT ?= 18501
TESTS := $(sort $(wildcard tests/*-test.sps))

H2O_SOURCE := vendor/h2o
H2O_BUILD := build/h2o
H2O_LIBRARY := $(H2O_BUILD)/cmake/libh2o-evloop.so
# Brotli and zstd are required, for compression; every other optional
# feature is off so that the build does not change with what happens to be
# installed. Some cmake builds (Nix's) do not search the system prefix, so
# pkg-config names it.
H2O_CMAKE_OPTIONS = -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON \
  -DOPENSSL_ROOT_DIR=$(shell pkg-config --variable=prefix openssl) \
  -DZLIB_ROOT=$(shell pkg-config --variable=prefix zlib) \
  -DWITH_MRUBY=OFF -DWITH_BROTLI=ON -DWITH_ZSTD=ON -DWITH_DTRACE=OFF \
  -DWITH_FUSION=OFF -DWITH_KTLS=OFF -DWITH_AEGIS=OFF -DWITH_IO_URING=OFF \
  -DWITH_CCACHE=OFF -DDISABLE_LIBUV=ON

.PHONY: build h2o test bench clean $(TESTS:%=run/%)

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build: h2o
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss

# cmake decides what is stale in H2O. Its make would otherwise join this
# make's job server and warn.
h2o:
	@mkdir -p $(H2O_BUILD)
	@test -f $(H2O_BUILD)/cmake/CMakeCache.txt || \
	  $(CAP) timeout $(BUILD_TIMEOUT) cmake -S $(H2O_SOURCE) -B $(H2O_BUILD)/cmake \
	    $(H2O_CMAKE_OPTIONS) > $(H2O_BUILD)/configure.log 2>&1 || \
	  { cat $(H2O_BUILD)/configure.log; exit 1; }
	@env -u MAKEFLAGS -u MAKELEVEL -u MFLAGS $(CAP) timeout $(BUILD_TIMEOUT) \
	  cmake --build $(H2O_BUILD)/cmake --target libh2o-evloop --parallel $(shell nproc)
	@$(MAKE) -s --no-print-directory $(H2O_BUILD)/layout.ss

# The generator is compiled with the flags H2O was compiled with, so it
# sees the same struct layouts.
$(H2O_BUILD)/layout.ss: src/h2o/layout.c $(H2O_LIBRARY)
	cc $$(sed -n 's/^C_\(DEFINES\|INCLUDES\|FLAGS\) = //p' \
	  $(H2O_BUILD)/cmake/CMakeFiles/libh2o-evloop.dir/flags.make) -std=gnu11 \
	  -DH2O_SHARED_OBJECT='"$(abspath $(H2O_LIBRARY))"' -o $(H2O_BUILD)/layout src/h2o/layout.c
	$(H2O_BUILD)/layout > $@.new && mv $@.new $@

# One process per test file, so a crash, a stuck thread or leaked global
# state stays inside its file. A test that starts its own Scheme reads
# SCHEME.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@SCHEME=$(SCHEME) $(CAP) timeout $(TEST_TIMEOUT) $(RUN) ./$(OBJ)/$(*:.sps=.so)

bench: build
	@mkdir -p build/bench
	cc -O2 -o build/bench/client bench/client.c
	$(CAP) timeout $(TEST_TIMEOUT) bench/run.sh "$(RUN)" $(BENCH_PORT)

clean:
	rm -rf build
