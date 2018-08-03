build:
	@ echo -e "\nStart building Tiger syntactic analyzer...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart parser interactive session. Modules: open Tiger.Semantic;;\n"
	@ fsharpi ../Parser/Store.fs ErrorMsg.fs Types.fs Env.fs Translate.fs
