build:
	@ echo -e "\nStart building Tiger compiler front end...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart parser interactive session. Modules: open Tiger.FrontEnd;;\n"
	@ fsharpi -r ../FsLexYacc.Runtime.dll -r ../Parser/bin/Debug/netstandard2.0/Parser.dll \
                     ErrorMsg.fs Types.fs Env.fs StandardLibrary.fs Translate.fs TigerFrontEnd.fs \
                     Temp.fs Tree.fs FrontEnd.fs
