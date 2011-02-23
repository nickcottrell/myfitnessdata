IS_WINDOWS = $(shell uname | grep 'MINGW32_NT' -c)
EXT = 
ifeq ($(IS_WINDOWS), 1)
	EXT = .exe
endif

all: clean myfitnessdata-tests myfitnessdata

clean:
	rm -f myfitnessdata$(EXT)

myfitnessdata:
	buildapp --output myfitnessdata$(EXT) --load myfitnessdata.lisp --entry "myfitnessdata:main"

myfitnessdata-tests:
	sbcl --script myfitnessdata-tests.lisp
