;;; 034b_org_compile_latex_framework_redo --- 15 Mar 2021 18:14
;;; Commentary:
;; This is a redo from scratch to use a better template scheme
;; based on symbolic link from template folder to compile folder.
;; see accompanying .org file for documentation

;;; Code:

;;; ================================================================
;;; Required packages and global option settings
;; First load this package to initialize variables:
(require 'ox-latex)

;; Use xelatex as latex compiler, thus enabling use of native fonts for greek etc.
(setq org-latex-compiler "xelatex")

;;; ================================================================
;;; PATHS:
;;; org-latex-base org-latex-default-template
;;; org-latex-exports-dir org-latex-templates-dir org-latex-compile-dir

(defcustom org-latex-base (file-truename "~/latex-exports/")
  "Directory where latex template and export files are stored.
Both the source files and the resulting pdf files are stored in this directory."
  :group 'org-latex-compile)

(defcustom org-latex-template-property 'LATEX-TEMPLATE
  "Name of the org property that stores the latex template name."
  :group 'org-latex-compile)

(defcustom org-latex-default-template "."
  "Default template directory name."
  :group 'org-latex-compile)

(defun org-latex-exports-dir ()
  "Directory where exported pdf files are stored."
  (concat org-latex-base "exports/"))

(defun org-latex-templates-dir ()
  "Directory storing templates for exporting to latex."
  (concat org-latex-base "templates/"))

(defun org-latex-compile-dir ()
  "Directory where latex is exported and compiled to pdf.
This is created by copying the selected template directory."
  (concat org-latex-base "compile_framework"))

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun org-latex-selected-template-path (&optional subtreep)
  "Get path of selected template.
If selected template is '.', then return path of folder containing current file.
Else concatenate global latex templates dir path with selected template."
  (let ((template-name (org-latex-selected-template subtreep)))
    (if (equal template-name ".")
        (directory-file-name (file-name-directory (buffer-file-name)))
      (concat (org-latex-templates-dir) template-name))))

(defun org-latex-selected-template (&optional subtreep)
  "Get name of selected template.
If subtreep return subtree property. Else return file property"
  ;; dirty fix for the -or-variable part:
  (set org-latex-template-property org-latex-default-template)
  (stringify (if subtreep
      (or (org-property-or-variable-value org-latex-template-property t)
            (org-latex-get-file-template))
    (org-latex-get-file-template))))

(defun stringify (theargument)
  "Turn symbol into string if needed."
  (if (symbolp theargument)
      (symbol-name theargument)
    theargument))

(defun org-latex-get-file-template ()
  "If defined, return file property for latex template, else return default template."
  (org-get-global-buffer-property
   (symbol-name org-latex-template-property)
   org-latex-default-template))

;;; Helper functions for getting global file property
(defun org-global-props (property &optional buffer)
  "Get the plists of global org properties of current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (el)
        (when (string-match property (org-element-property :key el)) el)))))

(defun org-get-global-buffer-property (prop &optional default)
  "Get the value of property prop defined globally in current buffer.
If not defined, return the default provided as optional argument."
  (or
   (car (mapcar (lambda (prop) (org-element-property :value prop))
            (org-global-props prop)))
   default))

;;; ================================================================
;;; THE MAIN FUNCTION

(defun org-compile-latex-with-custom-framework (&optional filep)
  "Export org to latex and compile that to pdf.
If filep is true export the entire file, else only the current section."
  (interactive "P")
  (let* ((subtreep (not filep))
         (compiler-type (cadr (read-multiple-choice
          "Use which pdf compiler?"
          '((?p "pdflatex" "for english conference templates")
            (?x "xelatex" "for multilingual texts")))))
         (template-path (org-latex-selected-template-path subtreep))
         ;; path of source framework file to compile to pdf
         (compile-path (concat (org-latex-compile-dir) "/framework.tex"))
         (pdf-file-path (concat (org-latex-compile-dir) "/framework.pdf"))
         (latex-output (org-export-as
                        ;; backend subtreep visible-only body-only ext-plist
                        'latex     subtreep      nil          t         nil
                        )))
    ;; prepare directories and files for compiling
    (message "you selected %s compiler" compiler-type)
    (delete-directory (org-latex-compile-dir) t)
    (copy-directory template-path (org-latex-compile-dir))
    (delete-file (concat (org-latex-compile-dir) "/framework.pdf"))
    ;; ;; save latex output as body file:
    (with-temp-buffer
      (insert "\\noindent\n") ;; start first paragraph non-indented
      ;; This works when printing texts without an initial heading.
      (insert latex-output)
      (write-file (concat (org-latex-compile-dir) "/body.tex")))
    ;; compile framework using content from exported body
    (setq pdf-file                                       ;; t: do not open!
          (latex-compile-file-with-latexmk compiler-type compile-path))
    (let ((final-copy-path
           (concat (org-latex-exports-dir)
                   ;; input filename from user, providing default
                   (read-string
                    "pdf export file copy name base:"
                    (concat
                     (if (and subtreep (org-get-heading t t))
                         (substring-no-properties
                          (replace-regexp-in-string "\\W" "" (org-get-heading t t)))
                       (file-name-nondirectory (file-name-sans-extension
                                                (buffer-file-name))))
                     (format-time-string "_%y%m%d_%I%M%S")))
                   ".pdf")))
      (message "copying file to: %s" final-copy-path)
      (copy-file pdf-file-path final-copy-path t)
      (open-pdf-file final-copy-path)
      )))

(defun latex-compile-file-with-latexmk (&optional compiler filename)
  "Compile tex file using latexmk.
  If PDFLATEXP then use pdflatex instead of xelatex.
  Open resulting pdf file with default macos open method."
  ;; (message "*Async Shell Command*")
  ;; (message "*Async Shell Command* exists? %s"
  ;;          (gnus-buffer-exists-p "*Async Shell Command*"))
 (when (gnus-buffer-exists-p "*Async Shell Command*")
    (kill-buffer "*Async Shell Command*"))
  ;; provide file name from buffer if needed
  (let* ((file (or filename (buffer-file-name)))
          (pdf-file (concat ;; compute pdf filename from tex filename
                     (file-name-sans-extension file)
                     ".pdf"))
          (org-latex-pdf-process
           ;; see https://orgmode.org/worg/exporters/anno-bib-template-worg.html
           (list
            ;; trying bibtex here
            (format
              "latexmk -shell-escape -g -pdf -pdflatex=\"%s\" -outdir=%%o %%f"
              compiler)
            "biber %b" ;; "biblatex %b" ;; "biber %b"
            (format
             "latexmk -shell-escape -g -pdf -pdflatex=\"%s\" -outdir=%%o %%f"
             compiler)
            (format
             "latexmk -shell-escape -g -pdf -pdflatex=\"%s\" -outdir=%%o %%f"
             compiler)
            )
           )
         )
    (message "file is: %s" file)
    (message "latex compile command is:\n %s" org-latex-pdf-process)
    (message "deleting file: %s" (concat (org-latex-compile-dir) "/framework.pdf"))
    (delete-file (concat (org-latex-compile-dir) "framework.pdf"))
    (message "DELETED FILE: %s" (concat (org-latex-compile-dir) "/framework.pdf"))
    (org-latex-compile file)
    (message "tex->pdf produced: %s" pdf-file)
    (when (gnus-buffer-exists-p "*Async Shell Command*") ;; NEEDED??????
      (kill-buffer "*Async Shell Command*"))
    pdf-file
    ))

(defun open-pdf-file (pdf-file)
  "Open pdf file with epdfview.
Only works in linux with epdfview installed."
  (interactive)
  ;; open the exported file:
  ;; requires epdfview installed in arch linux
  ;; for macos, use "open" instead.
  (message "Opening:\n%s" (shell-quote-argument pdf-file))
  (shell-command (concat "epdfview " (shell-quote-argument pdf-file) " & "))
  ;;    ;; (shell-command (concat "open " (shell-quote-argument pdf-file)))
  )

;;; ================================================================
;; helper functions

(defun org-latex-set-subtree-template ()
    "Set value of LATEX_HEADER_PATH property in current subtree."
    (interactive)
    (let ((path (org-latex-read-template-path)))
      (org-set-property (symbol-name org-latex-template-property) path)
     (message "You selected: \n%s" path)))

(file-name-as-directory "/alphs")

(file-name-nondirectory (directory-file-name "/exeter/alphsb/"))

(defun org-latex-read-template-path ()
  "Read template path interactively from default folder."
  (interactive)
  (let
      ((result
        (read-file-name "Select template file: " "~/latex-exports/templates/")))
    (file-name-nondirectory
     (directory-file-name
      (if (file-directory-p result)
          result
        (file-name-directory result))))))

(defun org-latex-set-buffer-template ()
  "Set value of LATEX_HEADER_PATH property globally in current buffer."
  (interactive)
  (let ((path (org-latex-read-template-path)))
    (save-excursion
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((new-line "")
             (here (re-search-forward
                    (concat "^"
                            (regexp-quote
                             (concat "#+"
                                     (symbol-name org-latex-template-property)
                                     ":"))
                            " ?") nil t)))
         (cond
          (here
           (goto-char here)
           (beginning-of-line)
           (kill-line))
          (t
           (setq new-line "\n")
           (goto-char (point-min))))
         (insert
          "#+"
          (symbol-name org-latex-template-property)
          ": "
          path
          new-line))))
    (message "You selected: \n%s" path)))

(defun org-latex-post-subtree-template-path ()
  "Post the path of the latex template file for current subtree."
  (interactive)
  (message "The subtree template path is:\n%s" (org-latex-selected-template-path t)))

(defun org-latex-post-file-template-path ()
  "Post the path of the latex template file for current subtree."
  (interactive)
  (message "The subtree template path is:\n%s" (org-latex-selected-template-path)))

(defun org-latex-find-file-template-file ()
  "Open file's template file."
  (interactive)
  (find-file (concat (org-latex-selected-template-path) "/framework.tex")))

(defun org-latex-find-subtree-template-file ()
  "Open file's template file."
  (interactive)
  (find-file (concat (org-latex-selected-template-path t) "/framework.tex")))

(defun compile-subtree-to-pdf ()
  "Compile subtree to pdf."
  (interactive)
  (org-compile-latex-with-custom-framework))

(defun compile-buffer-to-pdf ()
  "Compile subtree to pdf."
  (interactive)
  (org-compile-latex-with-custom-framework t))

;;; ================================================================
;;; Keyboard shortcuts
(global-set-key (kbd "C-M-S-c") 'org-compile-latex-with-custom-framework)

(defhydra hydra-latex (:color red :columns 2)
  "latex hydra"
  ("l" compile-buffer-to-pdf "buffer->pdf")
  ("L" compile-subtree-to-pdf "subtree->pdf")
  ("t" org-latex-set-buffer-template "set buffer template")
  ("T" org-latex-set-subtree-template "set subtree template")
  ("/" org-latex-post-file-template-path "post file template path")
  ("?" org-latex-post-subtree-template-path "post subtree template path")
  ("f" org-latex-find-file-template-file "find file template file")
  ("F" org-latex-find-subtree-template-file "find subtree template file")
  ("q" quit "exit hydra" :exit t))


(global-set-key (kbd "C-M-S-l") 'hydra-latex/body)

;;; 034b_org_compile_latex_framework_redo ends here
