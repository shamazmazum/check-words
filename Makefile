build-sources:
	$(MAKE) -C src

clean:
	$(MAKE) -C src $(.TARGET)

.PHONY: build-sources clean
