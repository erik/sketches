SYS_LIB_ROOT := $(shell xcrun -sdk macosx --show-sdk-path)
LFLAGS ?= -lsystem -syslibroot ${SYS_LIB_ROOT}
AS ?= as

forth: forth.o
	ld -o forth forth.o ${LFLAGS} -arch arm64

forth.o: forth.s
	${AS} -arch arm64 -g -o forth.o forth.S

clean:
	rm forth
	rm forth.o
