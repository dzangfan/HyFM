
BENCHDIR = bench

all: build

build:
	cabal build

test: testSt0 testSt1

bench: benSt0PrStat benSt0MonStat benSt0PrComp benSt0MonComp

clean:
	cabal clean
	rm -rf $(BENCHDIR)

testSt0:
	cabal test HyFM-FW-TestSt0 --test-show-details=streaming

benSt0PrStat:
	mkdir -p $(BENCHDIR)
	cabal test HyFM-FW-BenSt0PrStat \
	--test-options="--output=$(BENCHDIR)/BenSt0PrStat.html"

benSt0MonStat:
	mkdir -p $(BENCHDIR)
	cabal test HyFM-FW-BenSt0MonStat \
	--test-options="--output=$(BENCHDIR)/BenSt0MonStat.html"

benSt0PrComp:
	mkdir -p $(BENCHDIR)
	cabal test HyFM-FW-BenSt0PrComp \
	--test-options="--output=$(BENCHDIR)/BenSt0PrComp.html"

benSt0MonComp:
	mkdir -p $(BENCHDIR)
	cabal test HyFM-FW-BenSt0MonComp \
	--test-options="--output=$(BENCHDIR)/BenSt0MonComp.html"

testSt1:
	cabal test HyFM-FW-TestSt1 --test-show-details=streaming

benSt1:
	mkdir -p $(BENCHDIR)
	cabal test HyFM-FW-BenSt1 \
	--test-options="--output=$(BENCHDIR)/BenSt1.html"
