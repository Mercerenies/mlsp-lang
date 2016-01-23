
.PHONY:	build test

build:
	$(MAKE) -C hs

test:
	./hs/main.exe -c ./test/test.txt >/dev/null
	./hs/main.exe -c ./test/test1.txt >/dev/null
	./hs/main.exe -c ./test/test2.txt >/dev/null
	./hs/main.exe -c ./test/test3.txt >/dev/null
	./hs/main.exe -c ./test/test4.txt >/dev/null
