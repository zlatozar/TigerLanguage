parse:
	@ echo -e "\nGenerate Tiger parser using 'TigerParse.fsy'...\n"
	@ mono ../packages/FsLexYacc/build/fsyacc.exe -v --module TigerParse TigerParse.fsy

build: parse
	@ echo -e "\nStart building Tiger parser...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart lexer interactive session. Modules: open Tiger.Parser;;\n"
	@ fsharpi -r ../FsLexYacc.Runtime.dll TigerParse.fs Parser.fs
