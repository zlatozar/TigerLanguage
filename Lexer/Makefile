lex:
	@ echo -e "\nGenerate Tiger lexer using 'TigerLex.fsl'...\n"
	@ mono ../packages/FsLexYacc/build/fslex.exe --unicode TigerLex.fsl

build: lex
	@ echo -e "\nStart building Tiger lexer...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart lexer interactive session. Modules: open Tiger.Lexer;;\n"
	@ fsharpi -r ../FsLexYacc.Runtime.dll Tokens.fs TigerLex.fs Lexer.fs
