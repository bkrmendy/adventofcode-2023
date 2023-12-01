new:
	./tools/new.sh $(day)

run:
	stack build :day$(day) --exec day$(day)
