BUILD_CMD ?= sbcl --load
TARGET = check-words

$(TARGET): check-words.asd check-words.lisp package.lisp build.lisp
	env CHECK_WORDS_NAME=$(.TARGET) $(BUILD_CMD) build.lisp

clean:
	rm $(TARGET)
.PHONY: clean
