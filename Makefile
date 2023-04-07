SRC_FOLDER=src/
TEST_FOLDER=tests/

all: compile

compile:
	$(MAKE) -C $(SRC_FOLDER)


.PHONY: clean tests

tests: compile
	./$(SRC_FOLDER)comp ./$(TEST_FOLDER)rectangles.c ./$(TEST_FOLDER)out/rectangles.ps
	./$(SRC_FOLDER)comp ./$(TEST_FOLDER)progsimple.c ./$(TEST_FOLDER)out/progsimple.ps

clean:
	$(MAKE) $@ -C $(SRC_FOLDER)