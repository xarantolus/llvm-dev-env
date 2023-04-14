.PHONY: shell build

CONTAINER_NAME=llvm-test2

build:
	docker build -t $(CONTAINER_NAME) .

shell:
	docker run -it --rm -v "$(PWD):/workspace" $(CONTAINER_NAME) /bin/bash
