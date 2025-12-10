
BENCHDIR = bench

all: build

build:
	cabal build

benSt0PrStat:
	mkdir -p $(BENCHDIR)
	cabal test HyFM-FW-BenSt0PrStat \
	--test-options="--output=$(BENCHDIR)/BenSt0PrStat.html"

benSt0PrComp:
	mkdir -p $(BENCHDIR)
	cabal test HyFM-FW-BenSt0PrComp \
	--test-options="--output=$(BENCHDIR)/BenSt0PrComp.html"

testSt0Pr:
	cabal test HyFM-FW-TestSt0Pr --test-show-details=streaming

clean:
	cabal clean
	rm -rf $(BENCHDIR)
