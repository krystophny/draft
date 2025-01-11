GENERATOR ?= Ninja
CONFIG ?= Release
INSTALL_PREFIX ?= install
CMAKE_FILES := CMakeLists.txt src/CMakeLists.txt

.PHONY: all configure build test install clean uninstall distclean
all: build

build/CMakeCache.txt: $(CMAKE_FILES)
	cmake -S . -B build -G $(GENERATOR) -DCMAKE_BUILD_TYPE=$(CONFIG)

configure: build/CMakeCache.txt

build: configure
	cmake --build build --config $(CONFIG)

install: build
	cmake --install build --prefix $(INSTALL_PREFIX)

test: build
	ctest --test-dir build/test -VV

clean:
	cmake --build build --target clean

uninstall:
	rm -rf $(INSTALL_PREFIX)

distclean: uninstall
	rm -rf build
