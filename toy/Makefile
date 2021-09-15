all: danse.js danse.py pascal.inp

danse.js: ../day16.txt
	./to_js longue <$< |grep -v partner >$@

danse.py: ../day16.txt
	./to_python longue <$< |grep -v partner >$@

pascal.inp: ../day16.txt
	./to_pascal_input <$< >$@
