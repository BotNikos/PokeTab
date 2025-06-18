poke-tab: poke-tab.lisp
	sbcl --eval "(asdf:make :poke-tab/executable)"

install: poke-tab
	mkdir -p /usr/local/bin/poke-tab/
	cp ./poke-tab /usr/local/bin/poke-tab
	cp ./custom-tab.db /usr/local/bin/poke-tab
	cp -r ./resources/ /usr/local/bin/poke-tab/

uninstall:
	systemctl stop poke-tab
	systemctl stop poke-tab
	rm -r /usr/local/bin/poke-tab/

clean:
	rm poke-tab
