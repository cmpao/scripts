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

(defun qlik-engine-full-access () t)

(defun qlik-engine-allow-path ()
  (qlik-engine-path (concat "test/qlikviewTests/ProtocolTester4Net/Resources/rules/"
                            (if (qlik-engine-full-access)
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
      (BuildAppCacheAtStartup 1)
      (SystemRules ,(qlik-engine-allow-path))
      (UseEventBus 1)
      (UseEventTransactions 1)
      (EventTransactionsDirectory ,(qlik-engine-path "Packages/event_transactions"))
      (EventBusLogVerbosity 5)
      (UseSTAN 1)
      (STANUrl nats://localhost:4222)
      (STANCluster test-cluster)
      (STANMaxReconnect 60)
      (ValidateJsonWebTokens 0)
      (ShutdownWait 1)
      (Autosave 0)
      (AutosaveInterval 5)
      ))

(defun qlik-engine-settings-to-string (settings)
  (with-output-to-string
    (mapc (lambda (s)
            (princ (format " -S %s=%s" (car s) (cadr s))))
          settings)))

(defun qlik-engine-options-string (port)
  (concat " --WsPath " (qlik-engine-path "Packages/Client") " --EnableInternalTest --MigrationPort -1 -p " port " "
          (qlik-engine-settings-to-string (qlik-engine-settings))))

(defun qlik-engine-debug ()
  (interactive)
  (qlik-cd-engine-root)
  (gdb (concat "gdb -i=mi --args Packages/Engine/engine-sym "
               (qlik-engine-options-string "9079")))
  (gdb-many-windows))

(global-set-key (kbd "C-c d") 'qlik-engine-debug)

(defun run-command-in-buffer (name command)
  (generate-new-buffer name)
  (switch-to-buffer name)
  (rename-uniquely)
  (async-shell-command command (current-buffer)))

(defun stan-server-run ()
  (interactive)
  (run-command-in-buffer "run-stan" "docker run --net=host -v /home/cmp/files/nats_store:/store nats-streaming -SD -store file -dir /store"))

(defun qlik-engine-run (&optional port settings)
  (interactive)
  (unless port
    (setq port (string-to-number (read-string "port: " "9076"))))
  (qlik-cd-engine-root)
  (run-command-in-buffer "run-engine"
                         (concat *qlik-engine-root*
                                 "Packages/Engine/engine-sym "
                                 (qlik-engine-options-string (number-to-string port))
                                 (qlik-engine-settings-to-string settings)
                                 " | jq")))

(global-set-key (kbd "C-c r") 'qlik-engine-run)

(defun qlik-engine-run-all()
  (interactive)
  (delete-other-windows)
  (stan-server-run)
  (qlik-engine-run 9076)
  (split-window-right)
  (qlik-engine-run 9077 '((EventBusSubscribe 1))))

(defun stop-running-shell (name)
  (switch-to-buffer name)
  (comint-interrupt-subjob)
  (while (process-live-p (get-buffer-process (current-buffer)))
    (sleep-for 0 200))
  (kill-buffer name))

(defun qlik-engine-stop-all ()
  (interactive)
  (mapc (lambda (buf)
          (stop-running-shell (buffer-name buf)))
        (cl-remove-if-not (lambda (buf)
                            (string-match-p "run-engine.*" (buffer-name buf)))
                          (buffer-list)))
  (stop-running-shell "run-stan<2>")
  (delete-other-windows))

(defun qlik-engine-debug-remote ()
  (interactive)
  (insert (concat "target remote "
                  (read-string "ip: " "172.18.0.2")
                  ":2345")))

(defun qlik-engine-src-dir ()
  (concat *qlik-engine-root* "src/"))

(defun qlik-engine-build-dir ()
  (concat *qlik-engine-root*
          "build/"
          (replace-regexp-in-string "/" "-" (shell-command-to-string "printf %s \"$(git rev-parse --abbrev-ref HEAD)\""))
          "-release/"))

(defun qlik-engine-qix-dir ()
  (concat (qlik-engine-build-dir) "src/Qidl/gen/Qix/"))

;; Ctags
(defun qlik-ctags-c++ ()
  (interactive)
  (let* ((ctags-dirs (concat (qlik-engine-src-dir) " " (qlik-engine-qix-dir)))
         (ctags-file (concat *qlik-engine-root* "TAGS")))
    (shell-command (concat "ctags -e -R --c++-kinds=+p --fields=+iaS --extra=+q -f "
                           ctags-file
                           " "
                           ctags-dirs))
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
