# vim: set ft=make :

help:
	@echo "just is a convenient command runner. Try just -l"

# Run the tests against mongo DB
test backend:
	./bin/test-backend {{backend}}
