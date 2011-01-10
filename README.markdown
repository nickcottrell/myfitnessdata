myfitnessdata
=============
myfitnessdata is an application to extract diet progress data from the [MyFitnessPal](http://www.myfitnesspal.com/) website.

status
------
myfitnessdata is a work in progress; I'm using it as an exercise to learn Lisp so I imagine progress will be slow.  I will be posting to the MyFitnessPal discussion forum once it's complete and ready to use.

licence
-------
myfitnessdata is licensed under the GNU Lesser General Public License.

### why the LGPL?
The GPL is specifically designed to reduce the usefulness of GPL-licensed code to closed-source, proprietary software. The BSD license (and similar) don't mandate code-sharing if the BSD-licensed code is modified by licensees. The LGPL achieves the best of both worlds: an LGPL-licensed library can be incorporated within closed-source proprietary code, and yet those using an LGPL-licensed library are required to release source code to that library if they change it.

roadmap
-------
1. script that executes on any POSIX system running SBCL
2. parse just Weight information into weight.csv
3. Windows executable (bundled SBCL or other LISP? compiled?) with installer
4. parse additional measurements into separate CSV files

debian / ubuntu
---------------
As at 9 January 2011, there is a bug in the SBCL package for Debian & Ubuntu which will cause myfitnessdata to fail with an error like:
`Not an absolute pathname #P"~/.clc/systems/"`
The fix to this is documented [here](http://ikki.ws/showpost?postid=103).