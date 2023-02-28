
#define CAML_NAME_SPACE

#include <chafa.h>
#include <caml/mlvalues.h>
#include "caml/memory.h"
#include "caml/misc.h"
#include <stdint.h>


#define PIX_WIDTH 3
#define PIX_HEIGHT 3
#define N_CHANNELS 4

CAMLprim value caml_chafa_test(value bytes, value len, value unit) {
    CAMLparam3(bytes, len, unit);

    const unsigned char* pixels = (unsigned char*) String_val(bytes);
    const uint64_t clen = Long_val(len);

    ChafaCanvas* canvas =  chafa_canvas_new(NULL);

    chafa_canvas_draw_all_pixels (canvas,
                                  CHAFA_PIXEL_RGBA8_UNASSOCIATED,
                                  pixels,
                                  PIX_WIDTH,
                                  PIX_HEIGHT,
                                  PIX_WIDTH * N_CHANNELS);

    GString* s = chafa_canvas_print(canvas, NULL);

    fwrite (s->str, sizeof (char), s->len, stdout);
    fputc ('\n', stdout);


    g_string_free (s, TRUE);
    chafa_canvas_unref(canvas);
    CAMLreturn(Val_unit);
}