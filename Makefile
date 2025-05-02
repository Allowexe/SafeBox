
ERLC=erlc
SRC=src
EBIN=ebin

MODULES=$(wildcard $(SRC)/*.erl)
BEAMS=$(patsubst $(SRC)/%.erl,$(EBIN)/%.beam,$(MODULES))

all: $(BEAMS)

$(EBIN)/%.beam: $(SRC)/%.erl
	mkdir -p $(EBIN)
	$(ERLC) -o $(EBIN) $<

run:
	erl -pa $(EBIN)

console:
	make all && erl -pa $(EBIN) -eval "safebox_console:start()."

console-all:
	make all && erl -pa $(EBIN) -eval "start_safebox:start()."

clean:
	rm -rf $(EBIN)/*.beam

.PHONY: all clean run console console-all
