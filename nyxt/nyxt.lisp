(define-configuration buffer
  ((default-modes
     (append
      %slot-default%
      '(emacs-mode auto-mode blocker-mode noscript-mode)))
   (search-engines
    (append
     %slot-default%
     (list
      (make-instance 'search-engine
		    :shortcut "hoo"
		    :search-url "https://hoogle.haskell.org/?hoogle=~a"
		    :fallback-url (quri.uri:uri "https://hoogle.haskell.org/")
		    :completion-function (make-search-completion-function
					  :base-url "https://hoogle.haskell.org?mode=json&format=text&hoogle=~a&start=1&count=1"
					  :processing-function #'(lambda (nyxt::results)
								   (mapcar #'cdar
									   (json:decode-json-from-string
									    nyxt::results)))))
      (make-instance 'search-engine
		     :shortcut "srx"
		     :search-url "https://searx.be?q=~a"
		     :fallback-url (quri.uri:uri "https://searx.be")
		     :completion-function (make-search-completion-function
					   :base-url "https://searx.be/autocompleter?q=~a"
					   :processing-function (alexandria:compose #'second
										    #'json:decode-json-from-string))))))))

(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "#282828"
         :color "#ebdbb2")))))))

(define-configuration prompt-buffer
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
	 :background-color "#282828"
	 :color "#ebdbb2")
	(\#prompt-area
	 :background-color "#1d2021"
	 :color "#ebdbb2")
	(.source-content
	 :background-color "#282828"
	 :color "#ebdbb2")
	(".source-content th"
	 :background-color "#1d2021"
	 :color "#ebdbb2")
	(\#selection
	 :background-color "#d79921"
	 :color "#282828")
	(.source-name
	 :background-color "#504945"
	 :color "#ebdbb2")
	(\#input
	 :background-color "#32302f"
	 :color "#ebdbb2")))))))

(define-configuration internal-buffer
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((title
         :color "#d79921")
        (body
         :background-color "#282828"
         :color "#ebdbb2")
        (hr
         :color "#a89984")
        (a
         :color "#689d6a")
        (.button
         :color "#ebdbb2"
         :background-color "#222222")))))))

