SHELL=/bin/bash
.SUFFIXES: .oz .ozf
OZC=ozc
EXEC=lab1
SRC= $(wildcard *.oz)
OBJ= $(SRC:.oz=.ozf)

all: $(EXEC)

run: $(EXEC)
	./$(EXEC)


$(EXEC): $(OBJ)
	@$(OZC) -x $@.oz  # @ devant ligne désactive l'echo

%.ozf: %.oz
	@$(OZC) -o $@ -c $< $(CFLAGS)

# automatically rebuild dependances of .PHONY targets
.PHONY: clean mrproper 

clean:
	@rm -rf *.ozf $(EXEC)

