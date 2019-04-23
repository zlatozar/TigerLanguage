build:
	@ echo -e "\nStart building Tiger compiler backend...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart parser interactive session. Modules: open Tiger.BackEnd;;\n"
	@ fsharpi -r ../FsLexYacc.Runtime.dll -r ../Parser/bin/Debug/netstandard2.0/Parser.dll -r ../FrontEnd/bin/Debug/netstandard2.0/FrontEnd.dll \
                 Canon.fs BackEnd.fs

