TM_DIR=~/workplace/golang/src/github.com/inspirer/textmapper

all: gen

gen: skr.tm
	@${TM_DIR}/tm-tool/libs/textmapper.sh $<
	@go fmt ./... > /dev/null
	# @go install ./... > /dev/null

clean:
	$(RM) -v listener.go lexer.go lexer_tables.go parser.go parser_tables.go token.go
	$(RM) -rf -v ast/
	$(RM) -rf -v selector/

.PHONY: all gen clean
