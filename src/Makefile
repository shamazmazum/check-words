BUILD_CMD ?= sbcl --load
TARGET = check-words

$(TARGET): check-words.asd check-words.lisp package.lisp build.lisp
	$(BUILD_CMD) build.lisp

clean:
	rm -f $(TARGET)
.PHONY: clean
