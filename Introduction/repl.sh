#!/bin/sh

echo -e "\nTIP: System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)\n"
fsharpi -r ../FsLexYacc.Runtime.dll FrontEnd/Absyn.fs FrontEnd/LinePar.fs FrontEnd/LineLex.fs FrontEnd/LexParse.fs Interpret/StraightLine.fs
