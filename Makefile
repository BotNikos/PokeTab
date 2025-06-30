poke-tab: poke-tab.lisp custom-tab.db
	sbcl --eval "(asdf:make :poke-tab/executable)"

install: poke-tab
	systemctl stop poke-tab
	mkdir -p /usr/local/bin/poke-tab/
	cp ./poke-tab /usr/local/bin/poke-tab
	cp -r ./resources/ /usr/local/bin/poke-tab/

custom-tab.db:
	sqlite3 custom-tab.db < database.sql

uninstall:
	systemctl stop poke-tab
	rm -r /usr/local/bin/poke-tab/

clean:
	rm poke-tab
