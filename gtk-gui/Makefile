BUILD_CMD ?= sbcl --load
TARGET = check-words-gtk

$(TARGET): check-words-gtk.asd gui.lisp package.lisp build.lisp
	$(BUILD_CMD) build.lisp

clean:
	rm -f $(TARGET)
.PHONY: clean
