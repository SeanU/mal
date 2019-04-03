SRCS = step0_repl.mlb step1_read_print.mlb
OTHER_SRCS = 
BINS = $(SRCS:%.mlb=%)

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): %: %.mlb $(OTHER_SRCS)
	mlton $<

clean:
	rm -f $(BINS)
