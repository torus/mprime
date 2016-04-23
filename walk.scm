#!/usr/bin/gosh
;;
;; 3-D gear wheels.  This program is in the public domain.
;;
;; Brian Paul
;;
;; Conversion to GLUT by Mark J. Kilgard
;; Conversion to GtkGLExt by Naofumi Yasufuku
;; Port to Scheme/Gauche(GtkGLExt) by Shiro Kawai
;; Port to Scheme/Gauche(GLUT) by YOKOTA Hiroshi

(use math.const)
(use gl)
(use gl.glut)

;(use slib)
;(require 'printf)

;; Draw a gear wheel.  You'll probably want to call this function when
;; building a display list since we do a lot of trig here.
;;
;; Input:  inner_radius - radius of hole at center
;; outer_radius - radius at center of teeth
;; width - width of gear
;; teeth - number of teeth
;; tooth_depth - depth of tooth

(define (gear inner-radius outer-radius width teeth tooth-depth)
  (let ((r0 inner-radius)
	(r1 (- outer-radius (/ tooth-depth 2.0)))
	(r2 (+ outer-radius (/ tooth-depth 2.0)))
	(da (* 2.0 (/ pi teeth 4.0))))
    (gl-shade-model GL_FLAT)
    (gl-normal 0.0 0.0 1.0)

    ;; draw front face
    (gl-begin GL_QUAD_STRIP)
    (dotimes (i (+ teeth 1))
      (let1 _angle (* i 2.0 (/ pi teeth))
	(gl-vertex (* r0 (cos _angle)) (* r0 (sin _angle)) (* width 0.5))
	(gl-vertex (* r1 (cos _angle)) (* r1 (sin _angle)) (* width 0.5))
	(when (< i teeth)
	  (gl-vertex (* r0 (cos _angle)) (* r0 (sin _angle)) (* width 0.5))
	  (gl-vertex (* r1 (cos (+ _angle (* 3 da))))
		     (* r1 (sin (+ _angle (* 3 da))))
		     (* width 0.5)))))
    (gl-end)

    ;; draw front sides of teeth
    (gl-begin GL_QUADS)
    (dotimes (i teeth)
      (let1 _angle (* i 2.0 (/ pi teeth))
	(gl-vertex (* r1 (cos _angle)) (* r1 (sin _angle)) (* width 0.5))
	(gl-vertex (* r2 (cos (+ _angle da)))
		   (* r2 (sin (+ _angle da)))
		   (* width 0.5))
	(gl-vertex (* r2 (cos (+ _angle (* 2 da))))
		   (* r2 (sin (+ _angle (* 2 da))))
		   (* width 0.5))
	(gl-vertex (* r1 (cos (+ _angle (* 3 da))))
		   (* r1 (sin (+ _angle (* 3 da))))
		   (* width 0.5))))
    (gl-end)

    (gl-normal 0.0 0.0 -1.0)

    ;; draw back face
    (gl-begin GL_QUAD_STRIP)
    (dotimes (i (+ teeth 1))
      (let1 _angle (* i 2.0 (/ pi teeth))
	(gl-vertex (* r1 (cos _angle)) (* r1 (sin _angle)) (* width -0.5))
	(gl-vertex (* r0 (cos _angle)) (* r0 (sin _angle)) (* width -0.5))
	(when (< i teeth)
	  (gl-vertex (* r1 (cos (+ _angle (* 3 da))))
		     (* r1 (sin (+ _angle (* 3 da))))
		     (* width -0.5))
	  (gl-vertex (* r0 (cos _angle)) (* r0 (sin _angle)) (* width -0.5)))))
    (gl-end)

    ;; draw back sides of teeth
    (gl-begin GL_QUADS)
    (dotimes (i teeth)
      (let1 _angle (* i 2.0 (/ pi teeth))
	(gl-vertex (* r1 (cos (+ _angle (* 3 da))))
		   (* r1 (sin (+ _angle (* 3 da))))
		   (* width -0.5))
	(gl-vertex (* r2 (cos (+ _angle (* 2 da))))
		   (* r2 (sin (+ _angle (* 2 da))))
		   (* width -0.5))
	(gl-vertex (* r2 (cos (+ _angle da)))
		   (* r2 (sin (+ _angle da)))
		   (* width -0.5))
	(gl-vertex (* r1 (cos _angle)) (* r1 (sin _angle)) (* width -0.5))))
    (gl-end)

    ;; draw outward faces of teeth
    (gl-begin GL_QUAD_STRIP)
    (dotimes (i teeth)
      (let ((_angle (* i 2.0 (/ pi teeth)))
	    (u 0)
	    (v 0)
	    (len 0))
	(gl-vertex (* r1 (cos _angle)) (* r1 (sin _angle)) (* width 0.5))
	(gl-vertex (* r1 (cos _angle)) (* r1 (sin _angle)) (* width -0.5))

	(set! u (- (* r2 (cos (+ _angle da))) (* r1 (cos _angle))))
	(set! v (- (* r2 (sin (+ _angle da))) (* r1 (sin _angle))))
	(set! len (sqrt (+ (* u u) (* v v))))
	;; canonicalize normal vector
	(set! u (/ u len))
	(set! v (/ v len))

	(gl-normal v (- u) 0.0)

	(gl-vertex (* r2 (cos (+ _angle da)))
		   (* r2 (sin (+ _angle da)))
		   (* width 0.5))
	(gl-vertex (* r2 (cos (+ _angle da)))
		   (* r2 (sin (+ _angle da)))
		   (* width -0.5))
	(gl-normal (cos _angle) (sin _angle) 0.0)
	(gl-vertex (* r2 (cos (+ _angle (* 2 da))))
		   (* r2 (sin (+ _angle (* 2 da))))
		   (* width 0.5))
	(gl-vertex (* r2 (cos (+ _angle (* 2 da))))
		   (* r2 (sin (+ _angle (* 2 da))))
		   (* width -0.5))

	(set! u (- (* r1 (cos (+ _angle (* 3 da))))
		   (* r2 (cos (+ _angle (* 2 da))))))
	(set! v (- (* r1 (sin (+ _angle (* 3 da))))
		   (* r2 (sin (+ _angle (* 2 da))))))

	(gl-normal v (- u) 0.0)

	(gl-vertex (* r1 (cos (+ _angle (* 3 da))))
		   (* r1 (sin (+ _angle (* 3 da))))
		   (* width 0.5))
	(gl-vertex (* r1 (cos (+ _angle (* 3 da))))
		   (* r1 (sin (+ _angle (* 3 da))))
		   (* width -0.5))
	(gl-normal (cos _angle) (sin _angle) 0.0)))
    (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5))
    (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) (* width -0.5))
    (gl-end)

    (gl-shade-model GL_SMOOTH)

    ;; draw inside radius cylinder
    (gl-begin GL_QUAD_STRIP)
    (dotimes (i (+ teeth 1))
      (let1 _angle (* i 2.0 (/ pi teeth))
	(gl-normal (- (cos _angle)) (- (sin _angle)) 0.0)
	(gl-vertex (* r0 (cos _angle)) (* r0 (sin _angle)) (* width -0.5))
	(gl-vertex (* r0 (cos _angle)) (* r0 (sin _angle)) (* width 0.5))))
    (gl-end)
    ))

(define *view-rotx* 90.0)
(define *view-roty* 0.0)
(define *view-rotz* 0.0)
(define *gear1* 0)
(define *gear2* 0)
(define *gear3* 0)
(define *angle* 0.0)
(define *timer* #f)
(define *frames* 0)
(define *t0*	 0)

(define (draw)
  ;;*** OpenGL BEGIN ***
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (begin
    (gl-push-matrix)
    (gl-rotate *view-rotx* 1.0 0.0 0.0)
    (gl-rotate *view-roty* 0.0 1.0 0.0)
    (gl-rotate *view-rotz* 0.0 0.0 1.0)
    #;(begin
      (gl-push-matrix)
      (gl-translate -3.0 -2.0 0.0)
      (gl-rotate *angle* 0.0 0.0 1.0)
      (gl-call-list *gear1*)
      (gl-pop-matrix))
    #;(begin
      (gl-push-matrix)
      (gl-translate 3.1 -2.0 0.0)
      (gl-rotate (- (* -2.0 *angle*) 9.0) 0.0 0.0 1.0)
      (gl-call-list *gear2*)
      (gl-pop-matrix))
    #;(begin
      (gl-push-matrix)
      (gl-translate -3.1 4.2 0.0)
      (gl-rotate (- (* -2.0 *angle*) 25.0) 0.0 0.0 1.0)
      (gl-call-list *gear3*)
      (gl-pop-matrix))

    (draw-world (glut-get GLUT_ELAPSED_TIME))
    (gl-pop-matrix))

  (glut-swap-buffers)

  (inc! *frames*)

  (let1 t (glut-get GLUT_ELAPSED_TIME)
    (when (>= (- t *t0*) 5000)
      (let1 seconds (/ (- t *t0*) 1000.0)
        (print #`",*frames* in ,seconds seconds = ,(/ *frames* seconds) FPS")
        (set! *t0*	   t)
        (set! *frames* 0)))))

;; new window size or exposure
(define (reshape width height)
  (set! *window-width* width)
  (set! *window-height* height)

  (print `(reshape ,width ,height))

  (let1 h (/ height width)
    ;;*** OpenGL BEGIN ***
    (gl-viewport 0 0 width height)
    (gl-matrix-mode GL_PROJECTION)
    (gl-load-identity)
    (gl-frustum -1.0 1.0 (- h) h 5.0 60.0)
    (gl-matrix-mode GL_MODELVIEW)
    (gl-load-identity)
    (gl-translate 0.0 0.0 -40.0)
    ;;*** OpenGL END ***
    ))

(define (init)
  ;;*** OpenGL BEGIN ***
  (gl-light GL_LIGHT0 GL_POSITION '#f32(5.0 5.0 10.0 0.0))
  (gl-enable GL_CULL_FACE)
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_DEPTH_TEST)

  ;; make the gears
  (set! *gear1* (gl-gen-lists 1))
  (gl-new-list *gear1* GL_COMPILE)
  (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.8 0.1 0.0 1.0))
  (gear 1.0 4.0 1.0 20 0.7)
  (gl-end-list)

  (set! *gear2* (gl-gen-lists 1))
  (gl-new-list *gear2* GL_COMPILE)
  (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.0 0.8 0.2 1.0))
  (gear 0.5 2.0 2.0 10 0.7)
  (gl-end-list)

  (set! *gear3* (gl-gen-lists 1))
  (gl-new-list *gear3* GL_COMPILE)
  (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.2 0.2 1.0 1.0))
  (gear 1.3 2.0 0.5 10 0.7)
  (gl-end-list)
      
  (gl-enable GL_NORMALIZE)

  (newline)
  (print #`"GL_RENDERER	  = ,(gl-get-string GL_RENDERER)")
  (print #`"GL_VERSION	  = ,(gl-get-string GL_VERSION)")
  (print #`"GL_VENDOR	  = ,(gl-get-string GL_VENDOR)")
  (print #`"GL_EXTENSIONS = ,(gl-get-string GL_EXTENSIONS)")
  (newline)
  ;;*** OpenGL END ***
  )

(define (idle)
  (inc! *angle* 0.5)
  (if (> *angle* 360)
      (set! *angle* (fmod *angle* 360)))
  (glut-post-redisplay))

;; change view angle, exit upon ESC 
(define (key k x y)
  (let1 q (lambda () (glut-post-redisplay))
    (cond
     ((= k (char->integer #\z))
      (set! *view-rotz* (fmod (+ *view-rotz* 5.0) 360)) (q))
     ((= k (char->integer #\Z))
      (set! *view-rotz* (fmod (- *view-rotz* 5.0) 360)) (q))
     ((= k (char->integer #\escape)) (exit)))))

;; change view angle
(define (special k x y)
  (let1 q (lambda () (glut-post-redisplay))
    (cond
     ((= k GLUT_KEY_UP)
      (set! *view-rotx* (fmod (+ *view-rotx* 5.0) 360)) (q))
     ((= k GLUT_KEY_DOWN)
      (set! *view-rotx* (fmod (- *view-rotx* 5.0) 360)) (q))
     ((= k GLUT_KEY_LEFT)
      (set! *view-roty* (fmod (+ *view-roty* 5.0) 360)) (q))
     ((= k GLUT_KEY_RIGHT)
      (set! *view-roty* (fmod (- *view-roty* 5.0) 360)) (q)))))

;; Mouse
(define (mouse-fn button state x y)
  (print `(click ,button ,state ,x ,y))
  (cond [(and (= button GLUT_LEFT_BUTTON) (= state GLUT_DOWN))
         (on-click x y)
         ]
        [else
         ]))

(define (draw-cursor))

(define (dig2rad a)
  (* (/ a 180) 3.14159265358979))

(define (on-click x y)
  (print `(rot ,*view-rotx* ,*view-roty* ,*view-rotz*))
  (let ((s (vector4f x y 0 0))
        (c (vector4f 0 0 40 0))
        (o (vector4f 0 0 0 0))
        (rot (euler-angle->matrix4f (dig2rad *view-rotx*)
                                    (dig2rad *view-roty*)
                                    (dig2rad *view-rotz*)
                                    'zyx)))
    (print `(rot ,rot))
    (vector4f-sub! s (vector4f (/ *window-width* 2) (/ *window-height* 2) 0 0))
    (vector4f-div! s *window-width*)
    (vector4f-set! s 1 (- (vector4f-ref s 1)))
    (vector4f-add! s (vector4f 0 0 (- 40 5)))
    (vector4f-sub! s c)
    (let ((p (* rot #,(vector4f 1 0 0 0)))
          (q (* rot #,(vector4f 0 0 1 0))))
      (print `(p ,p q ,q s ,s))
      (let1 mat (matrix4f (vector4f-ref p 0) (vector4f-ref p 1) (vector4f-ref p 2) 0
                          (vector4f-ref q 0) (vector4f-ref q 1) (vector4f-ref q 2) 0
                          (vector4f-ref s 0) (vector4f-ref s 1) (vector4f-ref s 2) 0
                          0 0 0 1)
        (print mat)
        (matrix4f-inverse! mat)
        (print mat)
        (vector4f-sub! c o)
        (let1 solution (* mat c)
          (print `(solution ,solution))
          (set! draw-cursor
                (lambda ()
                  (gl-push-matrix)
                  (gl-translate (ref solution 0) -0.5 (ref solution 1))
                  (glut-solid-cube 1.5)
                  (gl-pop-matrix)))
        )
        ))))

(define (visible vis)
  (if (= vis GLUT_VISIBLE)
      (glut-idle-func idle)
      (glut-idle-func #f)))

(define *window-width* 300)
(define *window-height* 300)

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-position 0	 0)
  (glut-init-window-size     *window-width* *window-height*)

  (glut-create-window "Gears")
  (init)

  (glut-display-func	draw)
  (glut-reshape-func	reshape)
  (glut-keyboard-func	key)
  (glut-special-func	special)
  (glut-visibility-func visible)

  (glut-mouse-func	mouse-fn)

  (glut-main-loop)
  0)

;;;;;;;;;;;;;;;;;;

(define (draw-world elapsed)
  (gl-push-matrix)
    (gl-translate 0 -0.5 0)
    (gl-scale 10 1 10)
    (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.3 1 0.4 1.0))
    (glut-solid-cube 1)
  (gl-pop-matrix)

  (gl-push-matrix)
  (let ((seconds (/ elapsed 1000)))
    (gl-translate (* 3 (cos seconds)) 0.15 (* 3 (sin seconds)))
    (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.8 0.1 0.0 1.0))
    (glut-solid-cube 0.3)
    )
  (gl-pop-matrix)

  (draw-cursor)
  )

