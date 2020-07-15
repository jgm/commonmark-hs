DATE=$(shell date +%Y-%m-%d-%H%M)
LOGS=log
ifdef PATTERN
  TESTARGS?='-p "$(PATTERN)" --hide-successes'
  BENCHARGS?='--output $(LOGS)/benchmark-$(DATE).html --time-limit=2 --match=pattern "$(PATTERN)"'
else
  TESTARGS?="--hide-successes"
  BENCHARGS?="--output $(LOGS)/benchmark-$(DATE).html --time-limit=2"
endif
SOURCEFILES?=$(shell find commonmark/src commonmark-cli/src commonmark-pandoc/src -name '*.hs')
GHC_OPTS=-Wall -fno-warn-unused-do-bind -Wnoncanonical-monad-instances -Wincomplete-uni-patterns -Werror=missing-home-modules -Widentities -Wcpp-undef -fhide-source-paths -fno-prof-auto
PROFTARGET?=b5.md

all:
	stack install --ghc-options="$(GHC_OPTS)" --test --test-arguments=--hide-successes --bench --no-run-benchmarks

quick:
	stack install --test --no-run-tests --fast

test:
	stack test --test-arguments=$(TESTARGS)

haddock:
	stack haddock

stan:
	for d in commonmark commonmark-extensions commonmark-pandoc commonmark-cli; do cd $$d; stan report --config-file=../.stan.toml; cd ..; done

prof:
	cabal build --enable-profiling --ghc-options="${GHC_OPTS}" commonmark-cli
	cabal run --enable-profiling --ghc-options="${GHC_OPTS}" commonmark-cli -- +RTS -hc -RTS ${PROFTARGET} >/dev/null
	cat commonmark.hp
	hp2ps -c commonmark.hp
	ps2pdf commonmark.ps commonmark.pdf
	cabal run --enable-profiling --ghc-options="${GHC_OPTS}" commonmark-cli -- +RTS -P -V0.00001 -RTS ${PROFTARGET} >/dev/null
	profiterole commonmark.prof
	awk '{print $$3,"\t",$$5}' commonmark.profiterole.txt | sort -n | uniq | grep '^[0-9]'


heapprof:
	cabal run --enable-profiling --ghc-options="${GHC_OPTS}" commonmark-cli -- +RTS -hc -RTS ${PROFTARGET} >/dev/null
	hp2ps -b -c commonmark.hp
	ps2pdf commonmark.ps
	open commonmark.pdf

flamegraph:
	cabal run --enable-profiling --ghc-options="${GHC_OPTS}" commonmark-cli -- +RTS -pj -RTS ${PROFTARGET} >/dev/null
	cat commonmark.prof | ghc-prof-aeson-flamegraph | flamegraph.pl > prof.svg
	open -a Safari prof.svg

$(LOGS):
	mkdir -p $(LOGS)

benchmark:
	commonmark ${PROFTARGET} +RTS -s >/dev/null
	sudo nice -n -20 bench "commonmark ${PROFTARGET}"

bench: $(LOGS)
	stack bench --benchmark-arguments=$(BENCHARGS) commonmark 2>&1 \
	    | tee $(LOGS)/benchmark-$(DATE).out

ghci:
	stack ghci --ghci-options "-XOverloadedStrings" commonmark
#	stack ghci --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint -XOverloadedStrings" --package pretty-simple

reformat:
	for f in $(SOURCEFILES); do echo $$f; stylish-haskell -i $$f ; done

lint:
	for f in $(SOURCEFILES); do echo $$f; hlint --verbose $$f; done

clean:
	stack clean

pathologicaltest:
	python3 test/pathological_tests.py --prog commonmark

check:
	cd commonmark && cabal check && packdeps commonmark.cabal
	cd commonmark-extensions && cabal check && packdeps commonmark-extensions.cabal
	cd commonmark-pandoc && cabal check && packdeps commonmark-pandoc.cabal
	cd commonmark-cli && cabal check && packdeps commonmark-cli.cabal


.PHONY: quick ghci spectest pathologicaltest test bench prof clean all reformat lint haddock profheap flamegraph benchmark check stan
