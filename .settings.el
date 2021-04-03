
;;; package --- Summary
;;; Commentary: islam
;;; Project initial file:

(when (or (equal major-mode 'c++-mode) (equal major-mode 'c-mode))
  (setf cmake-ide-project-dir (expand-file-name lds-dir))
  (setf cmake-ide-build-dir (expand-file-name (concat lds-dir "build")))
  t)

