


#define CAML_NAME_SPACE

#include <chafa.h>
#include <caml/mlvalues.h>
#include "caml/memory.h"
#include "caml/misc.h"
#include <stdint.h>
#include <MagickWand/MagickWand.h>
#include "MagickCore/magick-type.h"
#include "MagickCore/pixel.h"
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <string.h>

#define RGBA "RGBA"
const char* NEW_SCREEN_BUFF_SEQ = "\033[?1049h\033[H";
const char* END_SRCEEN_BUFF_SEQ = "\033[?1049l";
const char* HORIZONTAL_LINE = "─"; // "─" != '-'
const char* VERTICAL_LINE = "⎸";

#define SCALE 0.93

void set_cursor_at(unsigned int line, unsigned int colmn) {
    fprintf(stdout, "\033[%u;%uf", line, colmn);
    fflush(stdout);
}

void move_down(unsigned int l) {
    fprintf(stdout, "\033[%uB", l);
    fflush(stdout);
}

void move_forward_column(unsigned int c) {
    fprintf(stdout, "\033[%uC", c);
    fflush(stdout);
}

void start_window() {
    write(STDOUT_FILENO, NEW_SCREEN_BUFF_SEQ, strlen(NEW_SCREEN_BUFF_SEQ));
}

void end_window() {
    write(STDOUT_FILENO, END_SRCEEN_BUFF_SEQ, strlen(END_SRCEEN_BUFF_SEQ));
}

void draw_vertical_line() {
    fprintf(stdout, "%s", VERTICAL_LINE);
    fflush(stdout);
}

void draw_horizontal_line() {
    fprintf(stdout, "%s", HORIZONTAL_LINE);
    fflush(stdout);
}

void draw_char(char c) {
    fprintf(stdout, "%c", c);
    fflush(stdout);
}

void next_line() {
    fprintf(stdout, "\n");
    fflush(stdout);
}

void draw_first_line(const char* title, const struct winsize *w, int endline) {
    size_t title_len = strlen(title);
    draw_vertical_line();
    draw_horizontal_line();
    for (unsigned int n = 2; n < w->ws_col - 1; n += 1) {
        unsigned int current_char_index = n - 2;
        if (current_char_index < title_len) {
            draw_char(title[current_char_index]);
        } else {
            draw_horizontal_line();
        }
    }
    draw_vertical_line();
    if (endline) next_line();
}

void draw_middle_line(const struct winsize *w, unsigned int row, int endline) {
    set_cursor_at(row, 0);
    draw_vertical_line();
    // move_forward_column(w->ws_col - 1);
    set_cursor_at(row, w->ws_col);
    draw_vertical_line();

    if (endline) next_line();
}

void draw_main_window(const char* title, int n, int outof) {
    struct winsize w;
    ioctl(0, TIOCGWINSZ, &w);
    size_t title_len = strlen(title);
    for (int nrow = 0; nrow < w.ws_row; nrow += 1) {
        if (nrow == 0 || nrow == w.ws_row) draw_first_line(title, &w, 1);
        // else if (n == w.ws_row) { printf("Hello world"); } 
        else draw_middle_line(&w, nrow, 1);
    }
    
}

CAMLprim value caml_chafa_test(value path, value unit) {
    CAMLparam2(path, unit);
    const char* cpath = String_val(path);



    MagickWandGenesis();
    MagickWand* magick_wand = NewMagickWand();
    MagickBooleanType status = MagickReadImage(magick_wand, cpath);

    if (status == MagickFalse) {
        fprintf(stderr, "fail to load image \"%s\"", cpath);
        CAMLreturn(Val_unit);
    }
    // MagickResizeImage(magick_wand, image_box_cols, image_box_line, LanczosFilter);

    size_t image_width = MagickGetImageWidth(magick_wand);
    size_t image_height = MagickGetImageHeight(magick_wand);

    size_t row_stride =  image_width * 4;
    const unsigned char * pixels = malloc( sizeof(unsigned char) * image_height * row_stride);

    if (! pixels) {
        fputs("Fail to allocated enough memory", stderr);
        MagickWandTerminus();
        CAMLreturn(Val_unit);
    }
    status = MagickExportImagePixels(magick_wand, 0, 0, image_width, image_height, RGBA, CharPixel, (void *) pixels);

    if (status == MagickFalse) {
        fputs("Fail to export image", stderr);
        MagickWandTerminus();
        free((void *) pixels);
        CAMLreturn(Val_unit);
    }
    struct winsize w;
    ioctl(0, TIOCGWINSZ, &w);

    printf ("lines %d\n", w.ws_row);
    printf ("columns %d\n", w.ws_col);
    const char* c = "─";


    ChafaCanvasConfig* config = chafa_canvas_config_new();
    chafa_canvas_config_set_pixel_mode(config, CHAFA_PIXEL_MODE_ITERM2);
    chafa_canvas_config_set_geometry(config, w.ws_col - 4, w.ws_row - 4);
    ChafaCanvas* canvas = chafa_canvas_new(config);
    chafa_canvas_draw_all_pixels (canvas,
                                  CHAFA_PIXEL_RGBA8_UNASSOCIATED,
                                  pixels,
                                  image_width,
                                  image_height,
                                  row_stride);

    GString* s = chafa_canvas_print(canvas, NULL);
    char selc[CHAFA_TERM_SEQ_LENGTH_MAX];
    ChafaTermInfo* info = chafa_term_info_new();
    
    
    
    // printf("\033[?1049h\033[H");
    // gchar* c = chafa_term_info_emit_begin_iterm2_image(info, selc, 4, 4);
    // puts("Hello world");
    // write (STDOUT_FILENO, "\033[?1049h\033[H", strlen("\033[?1049h\033[H"));
    // for (unsigned short i = 0; i < w.ws_col; i += 1) {
    //     write(STDOUT_FILENO, "─", strlen("─"));
        
    // }
    // write(STDOUT_FILENO, "\n  ", 3);
    // write (STDOUT_FILENO, s->str, sizeof(char) * s->len);
    // sleep(4);
    // write(STDOUT_FILENO, "\033[?1049l", strlen("\033[?1049l"));
    // chafa_term_info_emit_end_iterm2_image (info, selc);
    // sleep(5);
    // printf("\033[?1049l");

    start_window();
    draw_main_window("hello world", 0, 10);
    sleep(5);
    end_window();

    chafa_term_info_unref(info);
    // endwin();

    // fwrite (s->str, sizeof (char), s->len, stdout);
    // fputc ('\n', stdout);
    // fprintf(stdout, "\n\npixels = %lu, len = %lu width = %lu, height = %lu", sizeof(unsigned char) * image_height * row_stride,  s->len / 8, image_width, image_height);




    g_string_free (s, TRUE);
    chafa_canvas_unref(canvas);
    chafa_canvas_config_unref(config);
    DestroyMagickWand(magick_wand);
    MagickWandTerminus();
    free((void *) pixels);
    CAMLreturn(Val_unit);
}