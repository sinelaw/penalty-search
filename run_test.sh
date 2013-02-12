ghc --make -O2 -threaded test_search.hs
time bash -c './test_search +RTS -N4 | dot -Tpng -O'
