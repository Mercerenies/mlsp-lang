
.PHONY:	build test-parser test-semantics

build:
	$(MAKE) -C hs

test-parser:
	./hs/main.exe -p -c ./test/test.txt >/dev/null
	./hs/main.exe -p -c ./test/test1.txt >/dev/null
	./hs/main.exe -p -c ./test/test2.txt >/dev/null
	./hs/main.exe -p -c ./test/test3.txt >/dev/null
	./hs/main.exe -p -c ./test/test4.txt >/dev/null

test-semantics:
	./hs/main.exe -c ./test/semantic
