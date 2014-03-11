all:
	cabal install && .hsenv/cabal/bin/opentest
repo:
	new_bitbucket_repo opentest
spec:
	cabal install && .hsenv/cabal/bin/opentest-spec
