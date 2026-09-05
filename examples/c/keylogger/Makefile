main: *.h *.c
	gcc -std=c2x -Wall -Wextra -Wpedantic -Werror -o main *.c

main.dbg: *.h *.c
	gcc -std=c2x -g -Wall -Wextra -Wpedantic -Werror -o main.dbg *.c

debug: main.dbg
	sudo gdb -ex "start" ./main.dbg
