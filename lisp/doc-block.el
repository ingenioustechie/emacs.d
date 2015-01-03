(defun doc-block ()
  "Insert doc block"
  (interactive)
  (let
      (
       (beg (point))
       (is-function (looking-at-p " *\\(\\(protected\\)\\|\\(public\\)\\|\\(protected\\)\\)? *function +\\([0-Z_-]+\\)(\\([^,)]+,?\\)*)"))
       (is-class (looking-at-p "\s*class")))
    ( insert-string " /**
 * ")
    (if is-function (insert-string
                     " new function
 *
 * @param string $param
 * @retrun null"))
    (if is-class (insert-string " new class
   * "))
    (insert-string "
 * @author ")
    (insert-string (user-real-login-name))
    (insert-string "
 */
")
    ;; re intent everything
    (forward-line 1)
    (indent-region beg (point))
    ;; select the description for easy edit
    (search-backward "/**")
    (forward-line 1)
    (search-forward "* ")
    (set-mark-command nil)
    (move-end-of-line nil)
    (setq deactivate-mark nil)))
(global-set-key "\C-cId" 'doc-block)
