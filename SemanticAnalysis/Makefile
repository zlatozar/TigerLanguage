build:
	@ echo -e "\nStart building Tiger syntactic analyzer...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart parser interactive session. Modules: open Tiger.Semantic;;\n"
	@ fsharpi -r ../FsLexYacc.Runtime.dll -r ../Parser/bin/Debug/netstandard2.0/Parser.dll \
                     ErrorMsg.fs Types.fs Env.fs StandardLibrary.fs Translate.fs TigerSemantic.fs Semantic.fs
