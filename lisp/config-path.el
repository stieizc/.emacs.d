;;; -*- lexical-binding: t; -*-

;;; Section: Path configs.

(defconst my:scratchdir (expand-file-name "~/zscratchpad/"))
(defconst my:todo (expand-file-name "todo.org" my:scratchdir))

(provide 'config-path)
