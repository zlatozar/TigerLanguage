parse: lex
	@ echo -e "\nGenerate Tiger parser using 'TigerParse.fsy'...\n"
	@ mono ../packages/FsLexYacc/build/fsyacc.exe --module TigerParse TigerParse.fsy

lex:
	@ echo -e "\nGenerate Tiger lexer using 'TigerLex.fsl'...\n"
	@ mono ../packages/FsLexYacc/build/fslex.exe --unicode TigerLex.fsl

build:
	@ echo -e "\nStart building Tiger parser...\n"
	@ dotnet build

full: parse
	@ echo -e "\nStart building Tiger parser...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart parser interactive session. Modules: open Tiger.Parser;;\n"
	@ fsharpi -r ../FsLexYacc.Runtime.dll Absyn.fs TigerParse.fs TigerLex.fs Parser.fs
