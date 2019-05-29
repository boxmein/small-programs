.PHONY: all
all: main

# Compiles the Precompiled Module Interface file from a cpp file defining the interface
# This is needed to import the file and resolve which symbols this module will define.
module1.pcm: module1/module1.cppm
	clang++ -fmodules-ts --precompile module1/module1.cppm

module1.o: module1.pcm
	clang++ -fmodules-ts -c module1.pcm -o module1.o


# To link the interface to the implementation, the impl is compiled separately from the interface
# in its own file. The file declares "module module1;" to signify it's part of a module, and
# defines the same function that's exported by module1.cppm.
module1_impl.o: module1/module1_impl.cpp
	clang++ -fmodules-ts -c module1/module1_impl.cpp -o module1_impl.o -fmodule-file=./module1.pcm

# This phase basically just compiles main.cpp and links it to the module1 implementation of hullo1()
main: module1.o module1_impl.o main.cpp
	clang++ -fmodules-ts -o main -fprebuilt-module-path=. module1.o module1_impl.o main.cpp
