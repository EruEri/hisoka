(lang dune 3.2)

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

(version "0.1.1")

(generate_opam_files true)

(source
 (github EruEri/hisoka))

(authors "EruEri")

(maintainers "EruEri")

(license GPL-3.0)

; (documentation https://url/to/documentation)

(package
 (name hisoka)
 (synopsis "An encrypted-file manager")
; (description "A longer description")
 (depends (ocaml (>= "4.13")) dune (cryptokit (>= "1.17")) (cmdliner (>= "1.1.0")) (ppx_deriving_yojson (>= "3.5.3")) (yojson (>= "2.0.0")))
 (tags
  (file-manager encryption)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
