UNAME = $(shell uname)
EXT = bin
ifeq ($(UNAME), Windows)
	EXT = exe
endif

all: clean myfitnessdata

clean:
	rm -f myfitnessdata.bin

myfitnessdata:
	buildapp --output myfitnessdata.$(EXT) --load myfitnessdata.lisp --entry myfitnessdata\:main
