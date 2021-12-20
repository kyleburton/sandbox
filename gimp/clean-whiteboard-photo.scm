;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clean-whiteboard-photo.scm --- Cleans up whiteboard Photographs to bring out the writing.
;;
;;
;; Copyright (C) 2009 Kyle R. Burton <kyle.burton@gmail.com>
;;
;; Author: Kyle R. Burton <kyle.burton@gmail.com>
;;
;; Commentary:
;; 
;; Copy or symlink this into your gimp rc directory, which on most
;; Linuxes should be:
;;
;;   $HOME/.gimp-V.v/scripts.  
;;
;; On OS X you can copy or symlink this into:
;;
;;   /Applications/Gimp.app/Contents/Resources/share/gimp/2.0/scripts
;;
;; It should register itself under the menu:
;;
;;    <Toolbox>/Xtns/WhiteBoard
;;
;; On the Mac this seems to be Filters/WhiteBoard
;;

(define (cwb-get-top-layer theImage)
  (let* ((layer-info (gimp-image-get-layers theImage))
         (num-layers (car layer-info))
         (layer-ids (cadr layer-info)))
    (aref layer-ids 0)))

(define (clean-whiteboard-photo theImage)
  (let* ((mainLayer (cwb-get-top-layer theImage))
         (blurLayer (car (gimp-layer-copy mainLayer 0))))
    (gimp-image-add-layer theImage blurLayer -1)
    (plug-in-gauss 1 theImage blurLayer 25 25 0)
    (gimp-invert blurLayer)
    (gimp-layer-set-mode blurLayer DODGE-MODE)
    (gimp-image-merge-down theImage blurLayer CLIP-TO-IMAGE)
    (gimp-brightness-contrast (cwb-get-top-layer theImage) -100 80)
    (gimp-drawable-set-visible (cwb-get-top-layer theImage) TRUE)))

(script-fu-register
  ;; name
  "clean-whiteboard-photo"
  ;; menu label
  "Clean Whiteboard Photo"
  ;; description
  "Given a photo of a whiteboard, reduce background noise and bring out the writing and drawing."
  ;; author
  "Kyle R. Burton <kyle.burton@gmail.com>"
  ;; copyright
  "Copyright 2009, Kyle R. Burton"
  ;; date created
  "2009-09-17"
  ;; image type that it works on
  "RGB"
  ;; rest are parameters [if any]
  SF-IMAGE      "The image"     0)

(script-fu-menu-register "clean-whiteboard-photo" "<Toolbox>/Xtns/WhiteBoard")
