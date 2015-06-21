./build/Fouricle.jp: ./src/Fouricle.hs
	hastec -Wall -fno-warn-unused-do-bind -O2 ./src/Fouricle.hs -isrc -o ./build/Fouricle.js
