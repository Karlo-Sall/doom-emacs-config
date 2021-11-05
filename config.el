;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Karlo Sall"
      user-mail-address "k@rlo.dk")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "monospace" :size 11 :weight 'semi-light)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(cl-defun my/org-roam-node--filter-by-tags (node &optional included-tags excluded-tags)
  "Filter org-roam-node by tags."
  (let* ((tags (org-roam-node-tags node))
         (file-path (org-roam-node-file node))
         (rel-file-path (f-relative file-path org-roam-directory))
         (parent-directories (butlast (f-split rel-file-path)))
         (tags (cl-union tags parent-directories)))
    (if (or
         ;; (and included-tags (cl-notevery (lambda (x) (cl-member x tags :test #'string=)) included-tags))
         (and included-tags (not (cl-intersection included-tags tags :test #'string=)))
         (and excluded-tags (cl-intersection excluded-tags tags :test #'string=))
         ) nil t)))

(cl-defun my/org-roam-node-find (included-tags excluded-tags)
  "Modded org-roam-node-find which filters nodes using tags."
  (interactive)
  (org-roam-node-find nil nil
                      (lambda (node) (my/org-roam-node--filter-by-tags node included-tags excluded-tags))))

(cl-defun my/org-roam-node-insert (included-tags excluded-tags)
  "Modded org-roam-node-insert which filters nodes using tags."
  (interactive)
  (org-roam-node-insert
   (lambda (node) (my/org-roam-node--filter-by-tags node included-tags excluded-tags))))

(map!
 :leader
 (:prefix ("r" . "org-roam")
  "f" #'(lambda () (interactive) (my/org-roam-node-find nil '("daily" "captures" "dnd")))
  "i" #'(lambda () (interactive) (my/org-roam-node-insert nil '("daily" "captures" "dnd")))
  "F" #'(lambda () (interactive) (my/org-roam-node-find '("daily" "captures") nil))
  "I" #'(lambda () (interactive) (my/org-roam-node-insert '("daily" "captures") nil))
  "d" #'(lambda () (interactive) (my/org-roam-node-find'("dnd") nil))
  "D" #'(lambda () (interactive) (my/org-roam-node-insert '("dnd") nil))
 ))

;; ORG ROAM
;; Consider this for filtering tags: https://org-roam.discourse.group/t/filter-org-roam-node-find-insert-using-tags-and-folders/1907
;;(setq org-roam-db-node-include-function
;;      (defun dotfiles/org-roam-include ()
;;        (not (member "dnd" (org-get-tags)))))

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))
(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (append (my/org-roam-list-notes-by-tag "wip")
                                 (my/org-roam-list-notes-by-tag "todo")
                                 '("~/org/roam/daily" "~/org"))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?"
                                  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

(global-set-key (kbd "C-c n b") #'my/org-roam-capture-inbox)

(defun my/org-roam-inser-daily-time-reg ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* TODO Time %t\n** CAPEX\n** KTLO\n** BAU\n** OPEX\n** Lunch"
                                  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))



(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "wip"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: :wip:"
                                                          ("Tasks"))))))
(global-set-key (kbd "C-c n t") #'my/org-roam-capture-task)

(setq org-roam-capture-templates '(("d" "default" plain "%?"
     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+category: ${title}\n#+filetags: :${title}:")
     :unnarrowed t)))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies-capture-today t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(after! org
(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))
  (my/org-roam-refresh-agenda-list))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Typescript
;;(setq typescript-fmt-tool 'prettier)

;; LSP
(defadvice! fix-lookup-handlers (ret)
  :filter-return '(+lsp-lookup-references-handler +lsp-lookup-definition-handler)
  (when ret 'deferred))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
