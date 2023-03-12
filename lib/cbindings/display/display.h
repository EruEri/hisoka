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