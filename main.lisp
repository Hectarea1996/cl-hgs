
(in-package :hgs)

(defun process-input (window)
  (when (eql (glfw:get-key window glfw:+key-escape+) glfw:+press+)
    (glfw:set-window-should-close window t)))

(glfw:def-framebuffer-size-callback fbs-callback (window width height)
   (gl:viewport 0 0 width height))

(defun vertex-shader-source ()
  '("#version 330 core\n"
    "layout (location = 0) in vec3 aPos;\n"
    "void main()\n"
    "{\n"
    "gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n"
    "}\0"))

(defun fragment-shader-source ()
  '("#version 330 core\n"
    "out vec4 FragColor;\n"
    "void main()\n"
    "{\n"
    "FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n"
    "}\0"))


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

    (loop

      while (glfw:window-should-close window)
      do
      (progn

       ; Input
       (process-input window)

       ; Rendering commands
       (gl:clear-color 0.2 0.3 0.3 1.0)
       (gl:clear :color-buffer-bit)

       (let ((VBO (gl:gen-buffer)))
         (gl:bind-buffer :array-buffer VBO)
         (gl:with-gl-array (vertices gl:float :initial-contents '(-0.5 -0.5  0.0
                                                                 0.5 -0.5  0.0
                                                                 0.0  0.5  0.0))

           (let ((vbo (gl:gen-buffer))
                 (vertex-shader (gl:create-shader :vertex-shader))
                 (fragment-shader (gl:create-shader :fragment-shader))
                 (shader-program (gl:create-program)))

             (gl:bind-buffer :array-buffer vbo)
             (gl:buffer-data :array-buffer :static-draw vertices)

             (gl:shader-source vertex-shader (vertex-shader-source))
             (gl:compile-shader vertex-shader)

             (unless (gl:get-shader vertex-shader :compile-status)
               (error "ERROR::SHADER::VERTEX::COMPILATION_FAILED:\n ~S"
                      (gl:get-shader-info-log vertex-shader)))

             (gl:shader-source fragment-shader (fragment-shader-source))
             (gl:compile-shader fragment-shader)

             (gl:attach-shader shader-program vertex-shader)
             (gl:attach-shader shader-program fragment-shader)
             (gl:link-program shader-program)

             (unless (gl:get-program shader-program :link-status)
               (error "ERROR::SHADER::PROGRAM::LINK_FAILED:\n ~S"
                      (gl:get-program-info-log shader-program)))

             (gl:use-program shader-program)

             (gl:delete-shader vertex-shader)
             (gl:delete-shader fragment-shader)

             ; Hacer una funcion sizeof que funcione con todos los tipos (primitivos y gl-array).
             (gl:vertex-attrib-pointer 0 3 :float nil (* 3 (cffi:foreign-type-size :float)) 0)
             (gl:enable-vertex-attrib-array 0)))))

      ; Check and call events and swap the buffers
      (glfw:poll-events)
      (glfw:swap-buffers window))

    (glfw:destroy-window window))

  (glfw:terminate))
