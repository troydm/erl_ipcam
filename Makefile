compile: 
	mkdir -p ebin
	gcc -I/usr/local/lib/erlang/usr/include -ljpeg -o jpeg_nif.so -fpic -shared src/*.c
	erl -noshell -pa ebin -s make all -s erlang halt

run: 
	erl -pa ebin -s erl_ipcam start -noshell 2>&1 1>erl_ipcam.log &

clean:
	rm -f jpeg_nif.so
	rm -Rf ebin
