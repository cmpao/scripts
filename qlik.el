(defvar *qlik-engine-root* (file-truename default-directory))

(defun qlik-cd-engine-root ()
  (interactive)
  (cd *qlik-engine-root*))

(defun qlik-set-engine-root ()
  (interactive)
  (setq *qlik-engine-root* (read-directory-name "Engine root: " *qlik-engine-root*))
  (qlik-cd-engine-root))

(defun qlik-engine-compile ()
  (interactive)
  (and (qlik-cd-engine-root)
       (compile (concat "make -k "  (read-string "Target: " "QlikMain") " CONFIG=RELEASE"))))

(global-set-key (kbd "C-c c") 'qlik-engine-compile)

(defun qlik-engine-path (path)
  (concat *qlik-engine-root* path))

(defvar *qlik-engine-full-access* t)

(defun qlik-engine-allow-path ()
  (qlik-engine-path (concat "test/qlikviewTests/ProtocolTester4Net/Resources/rules/"
                            (if *qlik-engine-full-access*
                                "fullaccess.yml"
                              "rules.yml"))))

(defun qlik-engine-settings ()
    `((AdjustMemUseFromOs 0)
      (EnableInternalTest 1)
      (EnableRestartOnSessionStall 1)
      (DocumentDirectory ,(qlik-engine-path "Packages/G3"))
      (PerformanceLogVerbosity 0)
      (SystemLogVerbosity 1)
      (GlobalLogMinuteInterval 10)
      (AllowExternalSocket 1)
      (LockD 65522)
      (AcceptEULA yes)
      (QvLogUseStdoutLogger 1)
      (MaxSessionsLimit 1000000)
      (Gen3 1)
      (EnableABAC 1)
      (PersistenceMode 2)
      (BuildAppCacheAtStartup 0)
      (SystemRules ,(qlik-engine-allow-path))
      (UseSTAN 0)
      (STANUrl nats://localhost:4222)
      (STANCluster test-cluster)
      (ValidateJsonWebTokens 0)
                                        ;(Autosave 1)
                                        ;(AutosaveInterval 3)
      ))

(defun qlik-engine-settings-to-string ()
  (with-output-to-string
    (mapc (lambda (s)
            (princ (format " -S %s=%s" (car s) (cadr s))))
          (qlik-engine-settings))))

(defun qlik-engine-options-string ()
  (concat " --WsPath " (qlik-engine-path "Packages/Client") " --EnableInternalTest --MigrationPort -1 -p 9076 "
          (qlik-engine-settings-to-string)))

(defun qlik-engine-debug ()
  (interactive)
  (qlik-cd-engine-root)
  (gdb (concat "gdb -i=mi --args Packages/Engine/engine-sym "
               (qlik-engine-options-string))))
;;  (gdb-many-windows))

(global-set-key (kbd "C-c d") 'qlik-engine-debug)

(defun qlik-engine-debug-remote ()
  (interactive)
  (insert (concat "target remote "
                  (read-string "ip: " "172.18.0.2")
                  ":2345")))

(global-set-key (kbd "C-c r") 'qlik-engine-debug-remote)

;; Ctags
(defun qlik-ctags-c++ ()
  (interactive)
  (let* ((ctags-root (read-directory-name "Tags root: " (concat *qlik-engine-root* "src/")))
         (ctags-file (concat ctags-root "TAGS")))
    (shell-command (concat "ctags -e -R --c++-kinds=+p --fields=+iaS --extra=+q -f "
                           ctags-file
                           " "
                           ctags-root))
    (visit-tags-table ctags-file)))

(defun qlik-engine-rgrep ()
  (interactive)
  (let ((pattern (read-string "Pattern: " (if (region-active-p)
                                              (buffer-substring (mark) (point))
                                            (thing-at-point 'word)))))
    (rgrep pattern (read-string "Files: " "*.h *.c *.cpp") *qlik-engine-root* nil)))

(global-set-key (kbd "M-/") 'qlik-engine-rgrep)

(defun qlik-engine-find-file ()
  (interactive)
  (let ((dir (read-directory-name "Find root: " *qlik-engine-root*)))
    (find-name-dired dir (read-string "Pattern: " ".*"))))

(global-set-key (kbd "M-\\") 'qlik-engine-find-file)
