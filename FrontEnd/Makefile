build:
	@ echo -e "\nStart building Tiger compiler front end...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart compiler frontend interactive session. Modules: open Tiger.FrontEnd;;\n"
	@ fsharpi -r ../FsLexYacc.Runtime.dll -r ../Parser/bin/Debug/netstandard2.0/Parser.dll \
                     Types.fs Temp.fs Tree.fs Frame.fsi MipsFrame.fs Translate.fs Env.fs StandardLibrary.fs \
                     ErrorMsg.fs FindEscape.fs TigerFrontEnd.fs FrontEnd.fs PrintTree.fs
