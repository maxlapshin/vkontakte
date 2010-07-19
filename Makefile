
all: compile

compile:
	ERL_LIBS=..:../erlyvideo/deps/ erl -make
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

run: compile
	erl -pa ebin -boot start_sasl -s registrator -sname registrator
	
test:
	@erl -pa ebin -s registrator test -noshell -noinput -s init stop


archive: compile
	erl -pa ebin -hidden -noshell -s registrator archive -s init stop

