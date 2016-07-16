#!/usr/bin/gosh

(use math.const)
(use gl)
(use gl.glut)

(use util.queue)

(use gauche.record)
(load "./constraint")

(define *view-rotx* 20.0)
(define *view-roty* -30.0)
(define *view-rotz* 0.0)
(define *angle* 0.0)
(define *timer* #f)
(define *frames* 0)
(define *t0*	 0)

(define (draw state)
  (initialize-state state)
  (lambda ()
    ;;*** OpenGL BEGIN ***
    (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (begin
      (gl-push-matrix)
      (gl-rotate *view-rotx* 1.0 0.0 0.0)
      (gl-rotate *view-roty* 0.0 1.0 0.0)
      (gl-rotate *view-rotz* 0.0 0.0 1.0)

      (draw-world state (glut-get GLUT_ELAPSED_TIME))
      (gl-pop-matrix))

    (glut-swap-buffers)

    (inc! *frames*)

    (let1 t (glut-get GLUT_ELAPSED_TIME)
      (when (>= (- t *t0*) 5000)
        (let1 seconds (/ (- t *t0*) 1000.0)
          (print #`",*frames* in ,seconds seconds = ,(/ *frames* seconds) FPS")
          (set! *t0*	   t)
          (set! *frames* 0))))))

;; new window size or exposure
(define (reshape width height)
  (set! *window-width* width)
  (set! *window-height* height)

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
  (cond [(and (= button GLUT_LEFT_BUTTON) (= state GLUT_DOWN))
         (on-click x y)
         ]
        [else
         ]))

(define (draw-cursor))

(define (dig2rad a) (/ a 180/pi))

(define *click-queue* (make-queue))

(define (for-each-in-queue proc queue)
  (every-in-queue (lambda (e) (proc e) #t) queue))

(define (on-click x y)
  (let ((s (vector4f x y 0 0))
        (c (vector4f 0 0 40 0))
        (o (vector4f 0 0 0 0))
        (rot (euler-angle->matrix4f (dig2rad *view-rotx*)
                                    (dig2rad *view-roty*)
                                    (dig2rad *view-rotz*)
                                    'zyx)))
    (vector4f-sub! s (vector4f (/ *window-width* 2) (/ *window-height* 2) 0 0))
    (vector4f-div! s (/ *window-width* 2))
    (vector4f-set! s 1 (- (vector4f-ref s 1)))
    (vector4f-add! s (vector4f 0 0 (- 40 5)))
    (vector4f-sub! s c)
    (let ((p (* rot #,(vector4f 1 0 0 0)))
          (q (* rot #,(vector4f 0 0 1 0))))
      (let1 mat (matrix4f (vector4f-ref p 0) (vector4f-ref p 1) (vector4f-ref p 2) 0
                          (vector4f-ref q 0) (vector4f-ref q 1) (vector4f-ref q 2) 0
                          (vector4f-ref s 0) (vector4f-ref s 1) (vector4f-ref s 2) 0
                          0 0 0 1)
        (matrix4f-inverse! mat)
        (vector4f-sub! c o)
        (let1 solution (* mat c)
          (let ((x (ref solution 0))
                (z (ref solution 1)))
            (enqueue! *click-queue* (point4f x 0 z 0))
            (set! draw-cursor
                  (lambda ()
                    (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.8 0.1 0.0 1.0))
                    (for-each-in-queue draw-marker *click-queue*)
                    ))
            ))))))

(define (draw-marker point)
  (let ((x (ref point 0))
        (y (ref point 1))
        (z (ref point 2)))
    (gl-push-matrix)
    (gl-translate x (+ y 0.1) z)
    (glut-solid-cube 0.2)
    (gl-pop-matrix)
    ))

(define (visible vis)
  (if (= vis GLUT_VISIBLE)
      (glut-idle-func idle)
      (glut-idle-func #f)))

(define *window-width* 800)
(define *window-height* 600)

(define-record-type state %make-state #t
  queue
  (elapsed)
  (cube-pos)

  (start-pos)
  (end-pos)
  (start-time)
  (current-pos)

  (active?))

(define (make-state click-queue)
  (let ((elapsed (mecs-new-var))
        (cube-pos (mecs-new-var))

        (start-pos (mecs-new-var))
        (end-pos (mecs-new-var))
        (start-time (mecs-new-var))
        (current-pos (mecs-new-var))

        (active? (mecs-new-var))

        (circle (mecs-new-func
                 (lambda (e)
		   #;(print #`"circle ,e")
                   (let1 seconds (/ e 1000)
                     (point4f (* 3 (cos seconds)) 0.15 (* 3 (sin seconds)))))))

        (linear (mecs-new-func
                (lambda (active? start-pos end-pos start-time elapsed)
		  (print #`"linear ,active? ,start-pos ,end-pos ,start-time ,elapsed")
                  (unless (and active? start-pos end-pos) (raise (condition (<mecs-skip-calculation>))))
                  (let ((new-pos (+ start-pos
                                    (* (- end-pos start-pos)
                                       (min 1 (/ (- elapsed start-time) 1000))))))
		    (print #`"linear -> ,new-pos")
                    new-pos)
                  )))

        (activate (mecs-new-func
                   (lambda (elapsed start-time)
		     (print #`"activate ,elapsed ,start-time")
                     (< (- elapsed start-time) 1000)
                     )))

        (update-start-pos
         (mecs-new-func (lambda (active? end-pos)
			  (print #`"update-start-pos ,active? ,end-pos")
                          (when (or active? (not end-pos)) (raise (condition (<mecs-skip-calculation>))))
			  (print #`"update-start-pos -> ,end-pos")
                          end-pos)))

        (update-end-pos
         (mecs-new-func (lambda (start-pos elapsed)
			  (print #`"update-end-pos ,start-pos ,elapsed")
                          (when (queue-empty? click-queue)
                            (raise (condition (<mecs-skip-calculation>))))
			  (print #`"update-end-pos -> ,elapsed")
                          (values (dequeue! click-queue) elapsed)
                          )))

        )

    (mecs-connect! circle `(,elapsed) `(,cube-pos))
    (mecs-connect! linear `(,active? ,start-pos ,end-pos ,start-time ,elapsed)
                   `(,current-pos))
    (mecs-connect! activate `(,elapsed ,start-time) `(,active?))
    (mecs-connect! update-start-pos `(,active? ,end-pos) `(,start-pos))
    (mecs-connect! update-end-pos `(,start-pos ,elapsed) `(,end-pos ,start-time))

    (%make-state click-queue
                 elapsed cube-pos
                 start-pos end-pos start-time current-pos
                 active?)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-position 0	 0)
  (glut-init-window-size     *window-width* *window-height*)

  (glut-create-window "Gears")
  (init)

  (glut-display-func	(draw (make-state *click-queue*)))
  (glut-reshape-func	reshape)
  (glut-keyboard-func	key)
  (glut-special-func	special)
  (glut-visibility-func visible)

  (glut-mouse-func	mouse-fn)

  (glut-main-loop)
  0)

;;;;;;;;;;;;;;;;;;

(define *prev-front* #f)

(define (initialize-state state)
  (mecs-update! `((,(state-active? state) . #t))))

(define (draw-world state elapsed)
  (mecs-update! `((,(state-elapsed state) . ,elapsed)))

  (gl-push-matrix)
    (gl-translate 0 -0.5 0)
    (gl-scale 10 1 10)
    (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.3 1 0.4 1.0))
    (glut-solid-cube 1)
  (gl-pop-matrix)

  (gl-push-matrix)
  (let ((pos (mecs-var-value (mecs-var-node-var (state-cube-pos state)))))
    (gl-translate (ref pos 0) (ref pos 1) (ref pos 2))
    (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.8 0.1 0.0 1.0))
    (glut-solid-cube 0.3)
    )
  (gl-pop-matrix)

  (when (mecs-var-defined? (mecs-var-node-var (state-current-pos state)))
    (gl-push-matrix)
    (let ((pos (mecs-var-value (mecs-var-node-var (state-current-pos state)))))
      (gl-translate (ref pos 0) (ref pos 1) (ref pos 2))
      (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.2 0.4 0.9 1.0))
      (glut-solid-cube 0.3)
      )
    (gl-pop-matrix)
    )

  (draw-cursor)
  )

