all: .humming_touched

dist/build/humming/humming: bin/humming.hs
	./build.sh

tests/Tests: tests/Tests.hs
	./build-tests.sh

images/humming/humming: dist/build/humming/humming
	cp $< $@

images/humming/Tests: tests/Tests
	cp $< $@

.humming_touched: images/humming/Dockerfile images/humming/humming images/humming/Tests
	docker build -t noteed/humming images/humming
