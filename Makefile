#
# cabal sandbox usage
# 
SANDBOX=sand.box

# install in sandbox
boot:: sand.box
	(cd wakarusa          ; cabal install )
	(cd wakarusa-scotty   ; cabal install )
	(cd wakarusa-wreq     ; cabal install )
	(cd wakarusa-examples ; cabal build )

sand.box:
	mkdir -p $(SANDBOX)
	cabal sandbox init --sandbox $(SANDBOX)
	(cd wakarusa          ; cabal sandbox init --sandbox ../$(SANDBOX) )
	(cd wakarusa-scotty   ; cabal sandbox init --sandbox ../$(SANDBOX) )
	(cd wakarusa-wreq     ; cabal sandbox init --sandbox ../$(SANDBOX) )
	(cd wakarusa-examples ; cabal sandbox init --sandbox ../$(SANDBOX) )

clean:
	rm -Rf $(SANDBOX)