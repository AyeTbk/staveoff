TARGET_PACKAGE := staveoff.main

.PHONY: repl release serve dist

repl:
	clj -M --main cljs.main --compile $(TARGET_PACKAGE) --repl

release:
	clj -M -m cljs.main --optimizations advanced -c $(TARGET_PACKAGE)

serve:
	clj -M -m cljs.main --serve

dist: release
	mkdir -p dist/out || true
	cp index.html dist/index.html
	cp out/main.js dist/out/main.js
	cd dist && zip -r ../dist.zip *
