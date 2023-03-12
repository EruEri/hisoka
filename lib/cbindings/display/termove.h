#ifndef TERMOVE_H
#define TERMOVE_H

#include <chafa.h>
#include <termios.h>

extern const char* NEW_SCREEN_BUFF_SEQ;
extern const char* END_SRCEEN_BUFF_SEQ;
extern const char* CLEAR_CONSOLE;
extern const char* UPPER_LEFT_CORNER;
extern const char* UPPER_RIGHT_CORNER;
extern const char* LOWER_LEFT_CORNER;
extern const char* LOWER_RIGTH_CORNER;
extern const char* HORIZONTAL_LINE; // "─" != '-'
extern const char* VERTICAL_LINE;

void set_cursor_at(unsigned int line, unsigned int colmn);

void move_down(unsigned int l);

void clear();

void move_forward_column(unsigned int c);

void draw_vertical_line();

void draw_gstring(GString* gstring);

void draw_horizontal_line();

void draw_string(const char* s);

void draw_char(char c);

void next_line(unsigned int current_line);

void redraw_empty(const struct winsize* w);



#endif