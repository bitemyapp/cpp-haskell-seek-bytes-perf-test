build:
	g++ -std=c++11 seek.cpp -o seek
	stack exec -- ghc seek.hs -O2 -o seek-hs

# $ time ./seek
# Avg: 67.4099 ms | Min: 64.7931 ms | Max: 125.903 ms
# ./seek  1.35s user 5.36s system 99% cpu 6.744 total

# ./seek-hs +RTS -p
# benchmarking seek/System.IO.hGetBuf
# time                 145.3 ms   (136.2 ms .. 156.1 ms)
#                      0.997 R²   (0.994 R² .. 1.000 R²)
# mean                 151.9 ms   (148.1 ms .. 161.7 ms)
# std dev              7.947 ms   (1.463 ms .. 11.09 ms)
# variance introduced by outliers: 13% (moderately inflated)

# benchmarking seek/Data.ByteString.hGetSome
# time                 146.8 ms   (137.8 ms .. 157.3 ms)
#                      0.996 R²   (0.986 R² .. 1.000 R²)
# mean                 149.7 ms   (146.8 ms .. 152.1 ms)
# std dev              3.863 ms   (2.494 ms .. 6.131 ms)
# variance introduced by outliers: 12% (moderately inflated)

profile-haskell:
	stack exec -- ghc seek.hs -prof -fprof-auto -O2 -o seek-hs
	./seek-hs +RTS -pa
