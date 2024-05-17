TARGET_PACKAGE := staveoff.main

repl:
	clj -M --main cljs.main --compile $(TARGET_PACKAGE) --repl

release:
	clj -M -m cljs.main --optimizations advanced -c $(TARGET_PACKAGE)

serve:
	clj -M -m cljs.main --serve
