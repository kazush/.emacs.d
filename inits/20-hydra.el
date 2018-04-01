;;; 20-hydra.el --- Configuration for Hydra
;;; Commentary:
;;; Code:

(use-package hydra
  :ensure t
  :config
  (require 'hydra-examples))

;; window management
(defhydra hydra-window (:color red
                        :hint nil)
   "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _a_ce  _u_ndo  _r_edo"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("H" (hydra-move-splitter-left 4))
   ("J" (hydra-move-splitter-down 4))
   ("K" (hydra-move-splitter-up 4))
   ("L" (hydra-move-splitter-right 4))
   ("|" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)))
   ("_" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)))
   ("v" split-window-right)
   ("x" split-window-below)
   ;; winner-mode must be enabled
   ("u" winner-undo)
   ("r" winner-redo) ;;Fi
   ("o" delete-other-windows :exit t)
   ("a" ace-window :exit t)
   ("f" new-frame :exit t)
   ("s" ace-swap-window)
   ("da" ace-delete-window)
   ("dw" delete-window)
   ("db" kill-this-buffer)
   ("df" delete-frame :exit t)
   ("q" nil))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _q_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("q" nil))

(defhydra hydra-next-error
    (global-map "C-x")
    "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
    ("`" next-error     nil)
    ("j" next-error     nil :bind nil)
    ("k" previous-error nil :bind nil)
    ("h" first-error    nil :bind nil)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil))
     nil :bind nil)
    ("q" nil            nil :color blue))

(provide '20-hydra)
;;; 20-hydra.el ends here
