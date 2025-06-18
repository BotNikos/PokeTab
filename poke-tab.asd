(defsystem "poke-tab"
  :depends-on ("clwebserv" "sqlite")
  :components ((:file "poke-tab")))

(defsystem "poke-tab/executable"
  :build-operation "program-op"
  :build-pathname "poke-tab"
  :entry-point "poke-tab:main"
  :depends-on ("poke-tab")
  :components ((:file "poke-tab")))
