
(asdf:defsystem "cl-hgs"
  :description "A graphics engine experiment"
  :author "Hector Galbis Sanchis <hectometrocuadrado@gmail.com>"
  :depends-on ("cl-glfw" "cl-opengl")
  :components ((:file "packages")
               (:file "main")))