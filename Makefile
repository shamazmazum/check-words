build-sources:
	$(MAKE) -C src
	$(MAKE) -C gtk-gui

clean:
	$(MAKE) -C src $(.TARGET)
	$(MAKE) -C gtk-gui $(.TARGET)

.PHONY: build-sources clean
