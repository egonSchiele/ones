all:
	cabal install && cabal run
repo:
	new_bitbucket_repo ones
spec:
	cabal install && .hsenv/cabal/bin/ones-spec
