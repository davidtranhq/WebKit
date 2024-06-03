BIN := SIMDTest
SRC := main.cpp
OBJ := $(SRC:.cpp=.o)

CXX := clang++
CXXFLAGS := -std=c++20 -O3 -march=native -Wall -Wextra -pedantic -Werror
LDFLAGS := -lm

.PHONY: all clean

all: $(BIN)

$(BIN): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	$(RM) $(BIN) $(OBJ)

