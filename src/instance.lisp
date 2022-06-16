
(in-package :cvk)


;: Creates an application info structure
(defun create-application-info (&key ((:sType sType-c) VK_STRUCTURE_TYPE_APPLICATION_INFO)
				  ((:pNext _pNext) nil) ((:pApplicationName _pApplicationName) nil)
				  ((:applicationVersion applicationVersion-c) 0)
				  ((:pEngineName _pEngineName) nil) ((:engineVersion engineVersion-c) 0)
				  ((:apiVersion apiVersion-c) 0))
  (let ((pNext-c            (or _pNext (cffi:null-pointer)))
	(pApplicationName-c (if _pApplicationName
				(cffi:foreign-string-alloc _pApplicationName)
				(cffi:null-pointer)))
        (pEngineName-c      (if _pEngineName
				(cffi:foreign-string-alloc _pEngineName)
				(cffi:null-pointer)))
	(application-info-c (alloc-vulkan-object '(:struct VkApplicationInfo))))
    (cffi:with-foreign-slots ((sType pNext pApplicationName applicationVersion pEngineName engineVersion
				     apiVersion)
                              application-info-c (:struct VkApplicationInfo))
      (setf sType              sType-c
	    pNext              pNext-c
            pApplicationName   pApplicationName-c
            applicationVersion applicationVersion-c
            pEngineName        pEngineName-c
            engineVersion      engineVersion-c
            apiVersion         apiVersion-c))
    (values application-info-c)))

;; Destroys an application info structure
(defun destroy-application-info (application-info-c)
  (cffi:with-foreign-slots ((pApplicationName pEngineName) application-info-c (:struct VkApplicationInfo))
    (cffi:foreign-string-free pApplicationName)
    (cffi:foreign-string-free pEngineName))
  (free-vulkan-object app-info))

;; With application info macro
(defwith with-application-info
         create-application-info
         destroy-application-info)


;; List of wanted layers
(defun get-required-layers (&optional (validation t))
  (if validation
      '("VK_LAYER_KHRONOS_validation")
      nil))


;; Check whether the wanted layers are available
(defun check-required-layers (required-layers)

  ;; We get the available layers
  (cffi:with-foreign-object (count :uint32)
    (vkEnumerateInstanceLayerProperties count (cffi:null-pointer))
    (cffi:with-foreign-object (properties '(:struct VkLayerProperties) (cffi:mem-ref count :uint32))
      (vkEnumerateInstanceLayerProperties count properties)
      (loop for required-layer in required-layers
            always (loop for i from 0 below (cffi:mem-ref count :uint32)
                         for property-ptr = (cffi:mem-aptr properties '(:struct VkLayerProperties) i)
                         thereis (equal required-layer
                                        (cffi:foreign-string-to-lisp (cffi:foreign-slot-value property-ptr
                                                                                              '(:struct VkLayerProperties)
                                                                                              'layerName))))))))


;; Returns the wanted extensions
(defun get-required-extensions ()
  (let ((glfw-extensions (glfw:get-required-instance-extensions)))
    (unless glfw-extensions
      (error "glfw error: ~S" (glfw:get-error)))
    (cons VK_EXT_DEBUG_UTILS_EXTENSION_NAME glfw-extensions)))


;; Check whether the wanted extensions are available
(defun check-required-extensions (required-extensions)

  ;; We get the available extensions
  (cffi:with-foreign-object (count :uint32)
    (vkEnumerateInstanceExtensionProperties (cffi:null-pointer) count (cffi:null-pointer))
    (cffi:with-foreign-object (properties '(:struct VkExtensionProperties) (cffi:mem-ref count :uint32))
      (vkEnumerateInstanceExtensionProperties (cffi:null-pointer) count properties)
      ;; We check the availability of the wanted extensions
      (loop for required-extension in required-extensions
        always (loop for i from 0 below (cffi:mem-ref count :uint32)
                 thereis (equal required-extension
                                (cffi:foreign-string-to-lisp (cffi:foreign-slot-value (cffi:mem-aptr properties '(:struct VkExtensionProperties) i)
                                                                                     '(:struct VkExtensionProperties) 'extensionName))))))))


;; Creates an instance create info structure
(defun create-instance-info (instance-flags app-info enabled-layers enabled-extensions)

  ;; Create strings and array of strings
  (let ((enabled-layers-str     (cffi:foreign-alloc :string :initial-contents enabled-layers))
        (enabled-extensions-str (cffi:foreign-alloc :string :initial-contents enabled-extensions)))

    ;; Create instance info
    (let ((instance-info (alloc-vulkan-object '(:struct VkInstanceCreateInfo))))
      (cffi:with-foreign-slots ((sType flags pApplicationInfo enabledLayerCount
                                 ppEnabledLayerNames enabledExtensionCount ppEnabledExtensionNames)
                                instance-info (:struct VkInstanceCreateInfo))
        (setf sType                   VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
              flags                   instance-flags
              pApplicationInfo        app-info
              enabledLayerCount       (length enabled-layers)
              ppEnabledLayerNames     enabled-layers-str
              enabledExtensionCount   (length enabled-extensions)
              ppEnabledExtensionNames enabled-extensions-str))
      (values instance-info))))


;; Destroys an instance info
(defun destroy-instance-info (instance-info)
  (cffi:with-foreign-slots ((enabledLayerCount ppEnabledLayerNames enabledExtensionCount
			     ppEnabledExtensionNames)  instance-info (:struct VkInstanceCreateInfo))
    (loop for i from 0 below enabledLayerCount
	  do (cffi:foreign-free (cffi:mem-aref ppEnabledLayerNames :pointer i)))
    (loop for i from 0 below enabledExtensionCount
	  do (cffi:foreign-free (cffi:mem-aref ppEnabledExtensionNames :pointer i)))
    (cffi:foreign-free ppEnabledLayerNames)
    (cffi:foreign-free ppEnabledExtensionNames))
  (cffi:foreign-free instance-info))

;; With application info macro
(defwith with-instance-info
         create-instance-info
         destroy-instance-info)


;;; -------------------------
;;; ---- Public functions ---
;;; -------------------------

;; Creates the vulkan instance
(defun create-instance (&optional (validation t))

  ;; Application info
  (with-application-info app-info ("Common Vulkan example" (make-version 0 1 1) "Common Vulkan"
                                   (make-version 0 1 1) (make-version 1 0 0))
    ;; Layers and extensions
    (let ((required-layers     (get-required-layers validation))
          (required-extensions (get-required-extensions)))
      (when (not (check-required-layers required-layers))
        (error "Required layers not present"))                                                                 
      (when (not (check-required-extensions required-extensions))
        (error "Required extensions not present"))

      ;; Instance info
      (with-instance-info instance-info (0 app-info required-layers required-extensions)

        ;; Instance
        (cffi:with-foreign-object (instance-ptr 'VkInstance)
          (check-vk-result (vkCreateInstance instance-info (cffi:null-pointer) instance-ptr))
          (cffi:mem-ref instance-ptr 'VkInstance))))))


;; Destroyes a vulkan instance
(defun destroy-instance (instance)
  (vkDestroyInstance instance (cffi:null-pointer)))


;; With macro for vulkan instance
(defwith with-instance
         create-instance
         destroy-instance)
