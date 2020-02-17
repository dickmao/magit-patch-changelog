|build-status|

Generate a patch according to emacs-mirror/CONTRIBUTE.

.. |build-status|
   image:: https://travis-ci.com/dickmao/magit-patch-changelog.svg?branch=master
   :target: https://travis-ci.com/dickmao/magit-patch-changelog
   :alt: Build Status
.. |melpa-dev|
   Image:: https://melpa.org/packages/magit-patch-changelog-badge.svg
   :target: http://melpa.org/#/magit-patch-changelog
   :alt: MELPA current version
.. |melpa-stable|
   image:: http://melpa-stable.milkbox.net/packages/ein-badge.svg
   :target: http://melpa-stable.milkbox.net/#/ein
   :alt: MELPA stable version

.. image:: screencast.gif

Install
=======
As described in `Getting started`_, ensure melpa's whereabouts in ``init.el`` or ``.emacs``::

   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

Then

::

   M-x package-refresh-contents RET
   M-x package-initialize RET
   M-x package-install RET magit-patch-changelog RET

And, finally add (before any mentions of magit) in your ``.emacs``,

::

   (with-eval-after-load 'magit
     (require 'magit-patch-changelog))

Alternatively, copy ``magit-patch-changelog.el`` to a directory among ``C-h v RET load-path`` and add ``(require 'magit-patch-changelog)`` to ``.emacs``.

Usage
=====
From the magit status buffer (``C-x g``)::

   W c e

.. _Getting started: http://melpa.org/#/getting-started
