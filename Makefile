
.PHONY:	build test

build:
	$(MAKE) -C hs

test:
	./hs/main.exe ./test/test.txt >/dev/null
	./hs/main.exe ./test/test1.txt >/dev/null
	./hs/main.exe ./test/test2.txt >/dev/null
	./hs/main.exe ./test/test3.txt >/dev/null
	./hs/main.exe ./test/test4.txt >/dev/null
