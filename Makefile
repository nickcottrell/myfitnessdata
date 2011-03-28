# MyFitnessData - a CSV web scraper for the MyFitnessPal website.
# Copyright (C) 2011 "Duncan Bayne" <dhgbayne@gmail.com>
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>

IS_WINDOWS = $(shell uname | grep 'MINGW32_NT' -c)
IS_UBUNTU = $(shell uname | grep 'Ubuntu' -c)
EXT = 
ifeq ($(IS_WINDOWS), 1)
	EXT = .exe
endif

all: clean myfitnessdata-tests myfitnessdata installer

clean:
	rm -f bin/*

myfitnessdata:
	cd src; buildapp --output ../bin/myfitnessdata$(EXT) --load myfitnessdata.lisp --entry "myfitnessdata:main"; cd ..

myfitnessdata-tests:
	cd src; sbcl --script myfitnessdata-test-runner.lisp; cd ..

installer: 
ifeq ($(IS_WINDOWS), 1)
	echo TODO: Windows installer build here
else
	echo TODO: Ubuntu installer build here
endif

