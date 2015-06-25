(require 'bufshow)
(require 'color-theme-github)

(set-face-attribute 'default nil :family "Source_Code_Pro" :height 150)

(bufshow-start
 [
  ("ssa/dead1.ml" "clean-0")
  ("ssa/dead1.ml" "clean-1")
  ("ssa/dead1.ml" "collect-vars-init")
  ("ssa/dead1.ml" "vars-of-exp") 
  ("ssa/dead1.ml" "collect-vars-phi")
  ("ssa/dead1.ml" "collect-vars-phi+def")
  ("ssa/dead1.ml" "vars-of-label")
  ("ssa/dead1.ml" "collect-vars-phi+def+jmp")
  ("ssa/dead1.ml" "collect-vars-XXX")
  ])


(set-face-attribute 'default nil :family "Source_Code_Pro" :height 160)
