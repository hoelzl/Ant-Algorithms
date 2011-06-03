;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:ants-asd
  (:use :common-lisp :asdf))

(in-package :ants-asd)

(defsystem "ants"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :maintainer "Matthias Hoelzl <tc@xantira.com>"
  :license "BSD sans advertising (see file COPYING for details)"
  :description "Implementation of some ant colony optimization algorithms."
  :depends-on ("alexandria" "iterate")
  :serial t
  :components ((:file "package")
	       (:file "graph")))
