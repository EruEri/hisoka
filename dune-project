(lang dune 3.6)

;**********************************************************************************************;
;*                                                                                            *;
;* This file is part of Hisoka                                                                *;
;* Copyright (C) 2023 Yves Ndiaye                                                             *;
;*                                                                                            *;
;* Hisoka is free software: you can redistribute it and/or modify it under the terms          *;
;* of the GNU General Public License as published by the Free Software Foundation,            *;
;* either version 3 of the License, or (at your option) any later version.                    *;
;*                                                                                            *;
;* Hisoka is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *;
;* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *;
;* PURPOSE.  See the GNU General Public License for more details.                             *;
;* You should have received a copy of the GNU General Public License along with Hisoka.       *;
;* If not, see <http://www.gnu.org/licenses/>.                                                *;
;*                                                                                            *;
;**********************************************************************************************;

(name hisoka)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "EruEri")

(maintainers "EruEri")

(license GPL-3.0)

; (documentation https://url/to/documentation)

(package
 (name hisoka)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune cryptokit cmdliner yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
