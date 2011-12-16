#lang racket

(require "hl.rkt")
(require "prim.rkt")
(require "tree-example.rkt")
(require "util.rkt")

;;; Test code

(define lorem-ipsum-sentence "Lorem ipsum dolor sit amet, consectetur adipiscing elit.")

(define lorem-ipsum-paragraph "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur. Donec ut libero sed arcu vehicula ultricies a non tortor. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean ut gravida lorem. Ut turpis felis, pulvinar a semper sed, adipiscing id dolor. Pellentesque auctor nisi id magna consequat sagittis. Curabitur dapibus enim sit amet elit pharetra tincidunt feugiat nisl imperdiet. Ut convallis libero in urna ultrices accumsan. Donec sed odio eros. Donec viverra mi quis quam pulvinar at malesuada arcu rhoncus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. In rutrum accumsan ultricies. Mauris vitae nisi at sem facilisis semper ac in est.")

(define w-lst '(5 10 15 25 35 45 55))

(for* ((w w-lst)
       (test (list testtree testtree^)))
      (printfln "~a (w=~a)" test w)
      (newline)
      (test w)
      (newline))

(for* ((w w-lst)
       (d (list
           (text "foo")
           (text "foobar")
           (<+> (text "foo") (text "bar"))
           (</> (text "foo") (text "bar"))
           (spread (list (text "foo") (text "bar") (text "baz")))
           (stack (list (text "foo") (text "bar") (text "baz")))
           (fillwords lorem-ipsum-sentence)
           (fillwords lorem-ipsum-paragraph)
           )))
      (printfln "~a (w=~a)" d w)
      (newline)
      (displayln (pretty w d))
      (newline))
