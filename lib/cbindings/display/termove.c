////////////////////////////////////////////////////////////////////////////////////////////////
//                                                                                            //
// This file is part of Hisoka                                                                //
// Copyright (C) 2023 Yves Ndiaye                                                             //
//                                                                                            //
// Hisoka is free software: you can redistribute it and/or modify it under the terms          //
// of the GNU General Public License as published by the Free Software Foundation,            //
// either version 3 of the License, or (at your option) any later version.                    //
//                                                                                            //
// Hisoka is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        //
// without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           //
// PURPOSE.  See the GNU General Public License for more details.                             //
// You should have received a copy of the GNU General Public License along with Hisoka.       //
// If not, see <http://www.gnu.org/licenses/>.                                                //
//                                                                                            //
////////////////////////////////////////////////////////////////////////////////////////////////

#include "termove.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <termios.h>

const char* NEW_SCREEN_BUFF_SEQ = "\033[?1049h\033[H";
const char* END_SRCEEN_BUFF_SEQ = "\033[?1049l";
const char* CLEAR_CONSOLE = "\033[2J";
const char* UPPER_LEFT_CORNER = "┌";
const char* UPPER_RIGHT_CORNER = "┐";
const char* LOWER_LEFT_CORNER = "└";
const char* LOWER_RIGTH_CORNER = "┘";
const char* HORIZONTAL_LINE = "─"; // "─" != '-'
const char* VERTICAL_LINE = "│";

void set_cursor_at(unsigned int line, unsigned int colmn) {
    fprintf(stdout, "\033[%u;%uf", line, colmn);
    fflush(stdout);
}

void move_down(unsigned int l) {
    fprintf(stdout, "\033[%uB", l);
    fflush(stdout);
}

void clear(){
    fprintf(stdout, "%s", CLEAR_CONSOLE);
    fflush(stdout);
}

void move_forward_column(unsigned int c) {
    fprintf(stdout, "\033[%uC", c);
    fflush(stdout);
}


void draw_vertical_line() {
    fprintf(stdout, "%s", VERTICAL_LINE);
    fflush(stdout);
}

void draw_gstring(GString* gstring){
    fwrite (gstring->str, sizeof (char), gstring->len, stdout);
    fflush(stdout);
}

void draw_horizontal_line() {
    fprintf(stdout, "%s", HORIZONTAL_LINE);
    fflush(stdout);
}

void draw_string(const char* s) {
    fprintf(stdout, "%s", s);
    fflush(stdout);
}

void draw_char(char c) {
    fprintf(stdout, "%c", c);
    fflush(stdout);
}

void next_line(unsigned int current_line) {
    set_cursor_at(current_line + 1, 0);
    fflush(stdout);
}

void redraw_empty(const struct winsize* w){
    set_cursor_at(0, 0);
    for (unsigned int i = 0; i < w->ws_col * w->ws_row; i += 1) {
        draw_char(' ');
    } 
}