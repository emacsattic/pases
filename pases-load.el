;;; pases-load.el --- packaging & system definition for Emacs
;;
;; Copyright (C) 2008-2010 Erik Hetzner
;;
;; Author: Erik Hetzner <egh@e6h.org>
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Load luna.
(load (expand-file-name "pases-luna"
                        (file-name-directory load-file-name)))

;; Load pases core.
(load (expand-file-name "pases"
                        (file-name-directory load-file-name)))

;; Load packaging.
(load (expand-file-name "pases-package"
                        (file-name-directory load-file-name)))

;; Load all systems.
(pases:load-all)
