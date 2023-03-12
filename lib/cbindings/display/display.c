#include <signal.h>
#define CAML_NAME_SPACE

#include <chafa.h>
#include <caml/mlvalues.h>
#include "caml/memory.h"
#include "caml/misc.h"
#include <MagickWand/MagickWand.h>
#include "MagickCore/magick-type.h"
#include "MagickCore/pixel.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <string.h>

#define RGBA "RGBA"
#define NCHANNEL 4
#define SCALE 0.93

#define NO_FILE "The list is empty"
#define NO_IMAGE_FILE "Not an image file"

const char* NEW_SCREEN_BUFF_SEQ = "\033[?1049h\033[H";
const char* END_SRCEEN_BUFF_SEQ = "\033[?1049l";
const char* CLEAR_CONSOLE = "\033[2J";
const char* UPPER_LEFT_CORNER = "┌";
const char* UPPER_RIGHT_CORNER = "┐";
const char* LOWER_LEFT_CORNER = "└";
const char* LOWER_RIGTH_CORNER = "┘";
const char* HORIZONTAL_LINE = "─"; // "─" != '-'
const char* VERTICAL_LINE = "│";

struct termios raw;
struct termios orig_termios;

void enableRawMode();
void disableRawMode();
void end_window();

void enableRawMode() {
    tcgetattr(STDIN_FILENO, &orig_termios);
    struct termios raw = orig_termios;
  
    raw.c_lflag &= ~(ECHO | ICANON);
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

void disableRawMode() {
  raw.c_lflag |= (ECHO | ICANON);  
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

void handle_sigint(int signo) {
    disableRawMode();
    end_window();
}


typedef enum pixel_mode {
    ITERM = 0,
    KITTY,
    SIXEL,
    NONE
} pixel_mode_t;

typedef enum {
    NO_ERROR,
    MALLOC_FAIL,
    MAGICKNULL,
    MAGICK_EXPORT_FAIl,
} exit_status_t;

typedef struct {
    const char* image_name;
    const unsigned char* image_bytes;
    size_t bytes_len;
} image_t;

typedef struct {
    image_t* images;
    size_t count;
} image_array_t;

void set_pixel_mode(ChafaCanvasConfig* config, pixel_mode_t mode) {
    switch (mode) {
    case ITERM:
        chafa_canvas_config_set_pixel_mode(config, CHAFA_PIXEL_MODE_ITERM2);
        break;
    case KITTY:
        chafa_canvas_config_set_pixel_mode(config, CHAFA_PIXEL_MODE_KITTY);
        break;
    case SIXEL:
        chafa_canvas_config_set_pixel_mode(config, CHAFA_PIXEL_MODE_SIXELS);
        break;
    case NONE:
      break;
    }
}

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



void start_window() {
    enableRawMode();
    write(STDOUT_FILENO, NEW_SCREEN_BUFF_SEQ, strlen(NEW_SCREEN_BUFF_SEQ));
    
}

void end_window() {
    write(STDOUT_FILENO, END_SRCEEN_BUFF_SEQ, strlen(END_SRCEEN_BUFF_SEQ));
    disableRawMode();
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

void draw_first_line(const char* title, const struct winsize *w, int endline) {
    size_t title_len = strlen(title);
    draw_string(UPPER_LEFT_CORNER);
    draw_horizontal_line();
    for (unsigned int n = 2; n < w->ws_col - 1; n += 1) {
        unsigned int current_char_index = n - 2;
        if (current_char_index < title_len) {
            draw_char(title[current_char_index]);
        } else {
            draw_horizontal_line();
        }
    }
    draw_string(UPPER_RIGHT_CORNER);
    if (endline) next_line(w->ws_row);
}

void draw_last_line(const struct winsize *w, int n, int outof) {
    // size_t title_len = strlen(title);
    int len = snprintf(NULL, 0, "%u/%u", n, outof);
    draw_string(LOWER_LEFT_CORNER);
    draw_horizontal_line();

    if (len > w->ws_col - 1) {
        for (unsigned int n = 2; n < w->ws_col - 1; n += 1) {
            draw_horizontal_line();
        }
    } else {
        for (unsigned int n = 2; n < w->ws_col - 1 - len; n += 1) {
            draw_horizontal_line();
        }
        fprintf(stdout, "%u/%u", n + 1, outof);
        fflush(stdout);
    }

    draw_string(LOWER_RIGTH_CORNER);
}

void draw_middle_line(const struct winsize *w, unsigned int row, int endline) {
    set_cursor_at(row, 0);
    draw_vertical_line();
    // move_forward_column(w->ws_col - 1);
    set_cursor_at(row, w->ws_col);
    draw_vertical_line();

    if (endline) next_line(row);
}

void draw_error_message(const struct winsize *w, const char* message) {
    set_cursor_at(w->ws_row / 2 , w->ws_col / 2);
    draw_string(message);
}

void draw_main_window(const char* title, int n, int outof) {
    struct winsize w;
    ioctl(0, TIOCGWINSZ, &w);
    size_t title_len = strlen(title);
    for (int nrow = 0; nrow < w.ws_row; nrow += 1) {
        if (nrow == 0 ) draw_first_line(title, &w, 1);
        else if (nrow == w.ws_row - 1) draw_last_line(&w, n, outof);
        else draw_middle_line(&w, nrow + 1, 1);
    }
}

void draw_image(const struct winsize *w, pixel_mode_t mode, size_t image_width,  size_t image_height, size_t row_stride, const unsigned char* pixels) {
    size_t scaled_width = w->ws_col * SCALE;
    size_t scaled_height = w->ws_row * SCALE;
    unsigned int start_point_draw_x = ((w->ws_col - scaled_width) / 2) + 1;
    unsigned int start_point_draw_y = ((w->ws_row - scaled_height) / 2) + 1;
    ChafaCanvasConfig* config = chafa_canvas_config_new();
    set_pixel_mode(config, mode);


    chafa_canvas_config_set_geometry(config, scaled_width, scaled_height);
    ChafaCanvas* canvas = chafa_canvas_new(config);
    chafa_canvas_draw_all_pixels (canvas,
                                  CHAFA_PIXEL_RGBA8_UNASSOCIATED,
                                  pixels,
                                  image_width,
                                  image_height,
                                  row_stride);

    GString* s = chafa_canvas_print(canvas, NULL);
    set_cursor_at(start_point_draw_y, start_point_draw_x);
    draw_gstring(s);

    g_string_free(s, TRUE);
    chafa_canvas_unref(canvas);
    chafa_canvas_config_unref(config);
    // chafa_term_info_unref(terminfo);
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

    start_window();
    draw_main_window("Hello world", 0, 10);
    draw_image(&w, ITERM, image_width, image_height, row_stride, pixels);
    sleep(4);
    end_window();

    DestroyMagickWand(magick_wand);
    MagickWandTerminus();
    free((void *) pixels);
    CAMLreturn(Val_unit);
}

image_array_t* create_image_array(size_t nbimage) {
    image_array_t* array = malloc(sizeof(image_array_t));
    if (!array) return NULL;

    image_t* images_ptr = malloc(sizeof(image_t) * nbimage);
    if (!images_ptr) return NULL;

    array->images = images_ptr;
    array->count = nbimage;
    return array;
}


void free_image_array(image_array_t* array) {
    free((void *) array->images);
    free(array);
}


// (string * string ) 
// meaning (string * bytes)
image_t convert_list_cell(value elt_list) {
    CAMLparam1(elt_list);
    CAMLlocal2(image_name, image_data);
    image_name = Field(elt_list, 0);
    image_data = Field(elt_list, 1);
    const char* cimage_name = String_val(image_name);
    const unsigned char* cimage_data = Bytes_val(image_data);
    size_t data_len = caml_string_length(image_data);
    image_t im = { .image_name = cimage_name, .image_bytes = cimage_data, .bytes_len = data_len };
    return im;
}

// ml_list : (string * string) list
void array_image_of_list_image(value ml_list, image_array_t* image_array) {
    CAMLparam1(ml_list);
    CAMLlocal1( head );
    size_t index = 0;
    while (ml_list != Val_emptylist) {
        head = Field(ml_list, 0);
        image_t image = convert_list_cell( head );
        image_array->images[index] = image;
        ml_list = Field(ml_list, 1); 
        index += 1;
    }
}

MagickWand* create_wand_of_image(image_t image) {
    MagickWand* magick_wand = NewMagickWand();
    MagickBooleanType status = MagickReadImageBlob(magick_wand, image.image_bytes, image.bytes_len);
    if (status == MagickFalse) return NULL;
    return magick_wand;
}

exit_status_t draw_image_wand(const struct winsize* w, MagickWand* magick_wand, const image_t* image, const size_t current_image_index, const size_t nbimage) {
    if (!magick_wand) return MAGICKNULL;

    size_t image_width = MagickGetImageWidth(magick_wand);
    size_t image_height = MagickGetImageHeight(magick_wand);


    size_t row_stride =  image_width * NCHANNEL;
    const unsigned char *pixels = malloc( sizeof(unsigned char) * image_height * row_stride);
    if (!pixels) return MALLOC_FAIL;

    MagickStatusType status = MagickExportImagePixels(magick_wand, 0, 0, image_width, image_height, RGBA, CharPixel, (void *) pixels);
    if (status == MagickFalse) {
        free( (void *) pixels);
        return MAGICK_EXPORT_FAIl;
    }
    // clear();
    set_cursor_at(0, 0);
    draw_main_window(image->image_name, current_image_index, nbimage);
    draw_image(w, ITERM, image_width, image_height, row_stride, pixels);


    free( (void *) pixels);
    return NO_ERROR;
}

// ml_list : (string * string) list
CAMLprim value caml_hisoka_show(value name_byte_list, value list_len, value mode, value unit) {

    // signal(SIGINT, handle_sigint);
    CAMLparam4(name_byte_list, list_len, mode, unit);
    size_t nbimage = Long_val(list_len);
    size_t current_image_index = 0;
    size_t previous_image_index = nbimage;
    printf("c len = %lu\n", nbimage);
    pixel_mode_t pmode = Int_val(mode);
    int RUNNING = 1;
    image_array_t* image_array = create_image_array(nbimage);
    if (!image_array) {
        printf("Empty list");
        CAMLreturn(unit);
    } 
    array_image_of_list_image(name_byte_list, image_array);
    start_window();
    draw_main_window("hisoka", current_image_index, nbimage);
    MagickWandGenesis();
    MagickWand* current = NULL;
    char c;
    image_t im;
    while (RUNNING) {
        struct winsize w;
        ioctl(0, TIOCGWINSZ, &w);
        if (current_image_index != previous_image_index) {
            previous_image_index = current_image_index;
            im = image_array->images[current_image_index];
            MagickWand* tmp = create_wand_of_image(im);
            if (tmp) {
                if (current) {
                    DestroyMagickWand(current);
                }
                current = tmp;
                exit_status_t status = draw_image_wand(&w, current, &im, current_image_index, nbimage);
            }
        }
        int _ = read(STDIN_FILENO, &c, 1);
        switch (c) {
            case 'q': {
                RUNNING = 0;
                break;
            }
            case 'j': {
                current_image_index = (current_image_index - 1) % nbimage;
                break;
            }
            case 'l': {
                current_image_index = (current_image_index + 1) % nbimage;
                break;
            }
        }
    }

    if (current) DestroyMagickWand(current);
    end_window();
    MagickWandTerminus();
    free_image_array(image_array);
    CAMLreturn(unit);
}