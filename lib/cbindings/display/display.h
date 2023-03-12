#ifndef DISPLAY_H
#define DISPLAY_H

#include <stddef.h>
#include <chafa.h>

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


#endif