

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

#define RGBA "RGBA"

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

    ChafaCanvasConfig* config = chafa_canvas_config_new();
    chafa_canvas_config_set_pixel_mode(config, CHAFA_PIXEL_MODE_ITERM2);
    // chafa_canvas_config_set_geometry(config, 230 , 60);
    ChafaCanvas* canvas = chafa_canvas_new(config);
    chafa_canvas_draw_all_pixels (canvas,
                                  CHAFA_PIXEL_RGBA8_UNASSOCIATED,
                                  pixels,
                                  image_width,
                                  image_height,
                                  row_stride);

    GString* s = chafa_canvas_print(canvas, NULL);

    fwrite (s->str, sizeof (char), s->len, stdout);
    fputc ('\n', stdout);


    g_string_free (s, TRUE);
    chafa_canvas_unref(canvas);
    chafa_canvas_config_unref(config);
    MagickWandTerminus();
    free((void *) pixels);
    CAMLreturn(Val_unit);
}