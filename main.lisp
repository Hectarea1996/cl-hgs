
(in-package :hgs)

(defun process-input (window)
    (when (eql (glfw:get-key window glfw:+key-escape+) glfw:+press+)
        (glfw:set-window-should-close window t)))

(glfw:def-framebuffer-size-callback fbs-callback (window width height)
    (gl:viewport 0 0 width height))

(defun main ()
    (glfw:init)
    
    (glfw:window-hint glfw:+context-version-major+ 3)
    (glfw:window-hint glfw:+context-version-minor+ 3)
    (glfw:window-hint glfw:+opengl-profile+ glfw:+opengl-core-profile+)
    (let ((window (glfw:create-window 640 480 "Prueba" nil nil)))
        (when (null window)
            (error "Failed to create GLFW window."))
        (glfw:make-context-current window)
        (glfw:set-framebuffer-size-callback window 'fbs-callback)
        (gl:viewport 0 0 800 600)

        (do
            ()
            ((glfw:window-should-close window))

            ; Input
            (process-input window)

            ; Rendering commands
            (gl:clear-color 0.2 0.3 0.3 1.0)
            (gl:clear :color-buffer-bit)
            
            ; Check and call events and swap the buffers
            (glfw:poll-events)
            (glfw:swap-buffers window))

        (glfw:destroy-window window))
    
    (glfw:terminate))