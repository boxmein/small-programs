CXX=clang++

.PHONY: all clean

all: foreach

foreach: foreach.cpp
	$(CXX) -o $@ $< -std=c++17

clean:
	rm foreach
	rm foreach.o

