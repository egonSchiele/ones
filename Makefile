all:
	cabal install && .hsenv/cabal/bin/ones
repo:
	new_bitbucket_repo ones
spec:
	cabal install && .hsenv/cabal/bin/ones-spec
