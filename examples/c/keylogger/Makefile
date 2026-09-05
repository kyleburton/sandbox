INPUT_EVENT_CODES_H := /usr/include/linux/input-event-codes.h

constants.h constants.c: scripts/gen_input_constants.py $(INPUT_EVENT_CODES_H)
	python3 scripts/gen_input_constants.py $(INPUT_EVENT_CODES_H) constants.h constants.c

main: *.h *.c constants.h constants.c
	gcc -std=c2x -Wall -Wextra -Wpedantic -Werror -o main *.c

main.dbg: *.h *.c constants.h constants.c
	gcc -std=c2x -g -Wall -Wextra -Wpedantic -Werror -o main.dbg *.c

debug: main.dbg
	sudo gdb -ex "start" ./main.dbg

clean:
	rm -f main main.dbg constants.h constants.c
