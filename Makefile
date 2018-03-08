CC = gcc
CFLAGS = -ggdb
RM = rm -f

.PHONY : all clean

all : file

file : file.s
	$(CC) $(CFLAGS) -o $@ $<

run : file
	@./file ; echo $$? || true

clean :
	$(RM) file

