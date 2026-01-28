(define-module (raylib)
  #:use-module (system foreign)
  #:export (key-w
            key-a
            key-s
            key-d
            key-right
            key-left
            init-window
            set-window-state
            flag-window-resizable
            get-screen-width
            get-screen-height
            set-target-fps
            window-should-close
            is-window-resized
            begin-drawing
            clear-background
            draw-rectangle
            draw-rectangle-lines
            draw-rectangle-v
            draw-rectangle-lines-ex
            is-key-pressed
            is-key-pressed-repeat
            get-mouse-x
            get-mouse-y
            end-drawing
            close-window))

(define raylib (dynamic-link "libraylib"))

(define key-w 87)
(define key-a 65)
(define key-s 83)
(define key-d 68)
(define key-right 262)
(define key-left 263)

(define init-window (pointer->procedure void (dynamic-func "InitWindow" raylib) (list int int '*)))
(define set-window-state (pointer->procedure void (dynamic-func "SetWindowState" raylib) (list int)))
(define flag-window-resizable 4)
(define get-screen-width (pointer->procedure int (dynamic-func "GetScreenWidth" raylib) (list)))
(define get-screen-height (pointer->procedure int (dynamic-func "GetScreenHeight" raylib) (list)))
(define set-target-fps (pointer->procedure void (dynamic-func "SetTargetFPS" raylib) (list int)))
(define window-should-close (pointer->procedure int (dynamic-func "WindowShouldClose" raylib) (list)))
(define is-window-resized (pointer->procedure int (dynamic-func "IsWindowResized" raylib) (list)))
(define begin-drawing (pointer->procedure void (dynamic-func "BeginDrawing" raylib) (list)))
(define clear-background (pointer->procedure void (dynamic-func "ClearBackground" raylib) (list unsigned-int)))
(define draw-rectangle (pointer->procedure void (dynamic-func "DrawRectangle" raylib) (list int int int int uint32)))
(define draw-rectangle-lines (pointer->procedure void (dynamic-func "DrawRectangleLines" raylib) (list int int int int uint32)))
(define draw-rectangle-v (pointer->procedure void (dynamic-func "DrawRectangleV" raylib) (list double double uint32)))
(define draw-rectangle-lines-ex (pointer->procedure void (dynamic-func "DrawRectangleLinesEx" raylib) (list double double float uint32)))
(define is-key-pressed (let ([f (pointer->procedure uint8 (dynamic-func "IsKeyPressed" raylib) (list int))]) (λ (key) (not (= 0 (f key))))))
(define is-key-pressed-repeat (let ([f (pointer->procedure uint8 (dynamic-func "IsKeyPressedRepeat" raylib) (list int))]) (λ (key) (not (= 0 (f key))))))
(define get-mouse-x (pointer->procedure int (dynamic-func "GetMouseX" raylib) (list)))
(define get-mouse-y (pointer->procedure int (dynamic-func "GetMouseY" raylib) (list)))
(define end-drawing (pointer->procedure void (dynamic-func "EndDrawing" raylib) (list)))
(define close-window (pointer->procedure void (dynamic-func "CloseWindow" raylib) (list)))

