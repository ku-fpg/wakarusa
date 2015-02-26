#
# cabal sandbox usage
# 

# install in sandbox
boot:: sandbox
	(cd wakarusa-examples ; cabal install --dependencies-only )
	(cd wakarusa-examples ; cabal build )

sandbox::
	(cd wakarusa-examples  ; cabal sandbox init )
	(cd wakarusa-examples  ; cabal sandbox add-source ../wakarusa )
	(cd wakarusa-examples  ; cabal sandbox add-source ../wakarusa-scotty )
	(cd wakarusa-examples  ; cabal sandbox add-source ../wakarusa-wreq )

clean:
	- (cd wakarusa-examples ; cabal sandbox delete )
	(cd wakarusa-examples ; cabal clean )
