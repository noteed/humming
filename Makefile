all: .humming_touched

dist/build/humming/humming: bin/humming.hs
	./build.sh

images/humming/humming: dist/build/humming/humming
	cp $< $@

.humming_touched: images/humming/Dockerfile images/humming/humming
	docker build -t noteed/humming images/humming
