build:
	@ echo -e "\nStart building Tiger compiler backend...\n"
	@ dotnet build

repl:
	@ echo -e "\nStart compiler interactive session. Modules: open Tiger.BackEnd;;\n"
	@ fsharpi -r ../FsLexYacc.Runtime.dll -r ../Parser/bin/Debug/netstandard2.0/Parser.dll -r ../FrontEnd/bin/Debug/netstandard2.0/FrontEnd.dll \
                 Canon.fs Assem.fs Codegen.fsi MipsCodegen.fs Graph.fs Flow.fs MakeGraph.fs Liveness.fs DoubleLinkedList.fs \
				 Color.fs RegAlloc.fs BackEnd.fs

