lexer.nn.go: lexer.l
	nex -o lexer.nn.go lexer.l
parser.yacc.go: parser.y
	goyacc -o parser.yacc.go -v parser.output parser.y

.PHONY: prebuild
prebuild: lexer.nn.go parser.yacc.go
	go fmt

.PHONY: build
build: main.go prebuild
	go build -o parser.out .

.PHONY: clean
clean:
	-rm *.output *.yacc.go *.nn.go

.PHONY: g
g: build clean
