#
# cabal sandbox usage
# 

# install in sandbox
boot:: sandbox
	- (cd wakarusa          ; cabal sandbox hc-pkg unregister wakarusa-scotty )
	- (cd wakarusa	      ; cabal sandbox hc-pkg unregister wakarusa-wreq )
	- (cd wakarusa	      ; cabal sandbox hc-pkg unregister wakarusa-examples )
	(cd wakarusa          ; cabal install )
	(cd wakarusa-scotty   ; cabal install )
	(cd wakarusa-wreq     ; cabal install )
	(cd wakarusa-examples ; cabal build )

sandbox::
	(cd wakarusa 	      ; cabal sandbox init )
	(cd wakarusa-scotty   ; cabal sandbox init --sandbox ../wakarusa/.cabal-sandbox )
	(cd wakarusa-wreq     ; cabal sandbox init --sandbox ../wakarusa/.cabal-sandbox )
	(cd wakarusa-examples ; cabal sandbox init --sandbox ../wakarusa/.cabal-sandbox )

clean:
	- (cd wakarusa          ; cabal sandbox delete )
	(cd wakarusa          ; cabal clean )
	(cd wakarusa-scotty   ; cabal clean )
	(cd wakarusa-wreq     ; cabal clean )
	(cd wakarusa-examples ; cabal clean )
