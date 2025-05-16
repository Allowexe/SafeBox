ERLC    = erlc
SRC_DIR = src
EBIN    = ebin
MODULES = safebox_server safebox_cli safebox_crypto

.PHONY: all clean

all: $(EBIN) $(MODULES:%=$(EBIN)/%.beam)

$(EBIN):
	mkdir -p $(EBIN)

$(EBIN)/%.beam: $(SRC_DIR)/%.erl | $(EBIN)
	$(ERLC) -o $(EBIN) $<

clean:
	rm -rf $(EBIN)
