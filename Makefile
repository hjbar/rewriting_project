default: clear clean fmt build exec

clean:
	@dune clean

fullclean:
	@dune clean
	@rm -rf archives

build:
	@dune build

fmt:
	@dune fmt

clear:
	@clear

exec:
	@dune exec bin/main.exe
