ELM=docker run -it --rm -v "`pwd`:/code" -w "/code" -e "HOME=/tmp" codesimple/elm:0.17

docs/%.html: src/%.elm
	$(ELM) make $< --output $@
