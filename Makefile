ALLFLAGS = -std=c++11 $(shell Magick++-config --cppflags --cxxflags --libs --ldflags) $(shell llvm-config --cxxflags --libs) -O2 -DNDEBUG
CXX ?= c++

.PHONY : all
all: pietc

pietc: $(wildcard *.?xx)
	$(CXX) -opietc main.cxx $(ALLFLAGS)

clean:
	rm pietc
