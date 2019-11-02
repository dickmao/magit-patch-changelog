;;; magit-patch-changelog.el --- git format-patch for emacs-devel -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of magit-patch-changelog.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0.1.0
;; Keywords: git tools vc
;; URL: https://github.com/dickmao/magit-patch-changelog
;; Package-Requires: ((emacs "25.1") (magit "2.90.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nnhackernews.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate a patch suitable according to emacs-mirror/CONTRIBUTE.

;;; Code:

(require 'magit)
