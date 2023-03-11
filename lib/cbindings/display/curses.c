#include <stdio.h>
#define CAML_NAME_SPACE


#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"

#include <ncurses.h>


CAMLprim value caml_curses_test(value unit) {
    CAMLparam1(unit);

    int continu = 1;
    int screen_width, screen_height;

    initscr();


    while (continu) {

        getmaxyx(stdscr,screen_height, screen_width);
        start_color();

        int c = getchar();
        if ( c == 'q' || c == 'Q')
            continu = 0;
    }

    endwin();

    CAMLreturn(Val_unit);
}