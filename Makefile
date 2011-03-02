IS_WINDOWS = $(shell uname | grep 'MINGW32_NT' -c)
EXT = 
ifeq ($(IS_WINDOWS), 1)
	EXT = .exe
endif

all: clean myfitnessdata-tests myfitnessdata

clean:
	rm -f bin/*

myfitnessdata:
	cd src; buildapp --output ../bin/myfitnessdata$(EXT) --load myfitnessdata.lisp --entry "myfitnessdata:main"; cd ..

myfitnessdata-tests:
	cd src; sbcl --script myfitnessdata-tests.lisp; cd ..


