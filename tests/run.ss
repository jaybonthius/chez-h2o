;; Loading a library object checks the files the library included, and
;; include finds them only through source-directories.
(source-directories '("src" "."))
;; The program sees its own path and arguments, as when run with --program.
(command-line (cdr (command-line)))
(command-line-arguments (cdr (command-line-arguments)))
(load-program (car (command-line)))
