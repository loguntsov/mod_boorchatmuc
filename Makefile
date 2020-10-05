all: compile

compile: lib
	rebar3 compile

lib: lib/ejabberd

lib/ejabberd:
	mkdir -p lib && cd lib && \
	git clone https://github.com/processone/ejabberd.git && \
	cd ejabberd && \
	git checkout 20.07 && \
	./rebar get-deps compile

upload: compile
	rsync -rltxSRzv \
	    --exclude .git \
	    --exclude *.log* \
	    --exclude *.pid \
	    --exclude .idea \
	    --exclude .rebar \
	    --exclude *.beam \
	    --exclude _build \
	    --exclude lib \
		. loguntsov@boorchat.ru:~/ejabberd/mod_boorchatmuc

