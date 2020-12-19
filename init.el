(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
		      sound-wav
		      magit
		      anki-editor
		      ;; 代码块
		      yasnippet
		      ;; search
		      helm-ag
		      ;; hugo
		      ox-hugo
		      ;; --- Auto-completion ---
		      company
		      ;; --- Better Editor ---
		      hungry-delete
		      swiper
		      counsel
		      smartparens
		      ;; --- Major Mode ---
		      js2-mode
		      ;; --- Minor Mode ---
		      nodejs-repl
		      exec-path-from-shell
		      ;; --- Themes ---
		      monokai-theme
		      ;; solarized-theme
		      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

(unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; 显示时间
(display-time-mode 1) ;; 常显
(setq display-time-24hr-format t) ;;格式
(setq display-time-day-and-date t) ;;显示时间、星期、日期

;; 隐藏菜单栏
(menu-bar-mode 0)
;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
(global-linum-mode 1)

;; 更改光标的样式（不能生效，解决方案见第二集）
(setq-default cursor-type 'bar)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 关闭缩进 (第二天中被去除)，使用;;注释解决缩进的问题
;; (electric-indent-mode -1)

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 160)

;; 快速打开todo文件
(defun open-todo-file()
  (interactive)
  (find-file "d:/notebook/org-roam/follow_along_120_days_and_30000rmb_to_start_affiliate_media_buy.org"))

;; 这一行代码，将函数 open-todo-file 绑定到 <f3> 键上
(global-set-key (kbd "<f8>") 'open-todo-file)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f10>") 'open-init-file)

;; 快速加载配置文件
(defun load-init-file()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 load-init-file 绑定到 <f1> 键上
(global-set-key (kbd "<f12>") 'load-init-file)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nyan-mode t)
 '(org-agenda-files
   (quote
    ("d:/notebook/inbox.org" "d:/notebook/org-roam/ab_exe.org" "d:/notebook/org-roam/affiliate_networks.org" "d:/notebook/org-roam/billing.org" "d:/notebook/org-roam/binom.org" "d:/notebook/org-roam/emacs.org" "d:/notebook/org-roam/emacs常用快捷键.org" "d:/notebook/org-roam/follow_along_120_days_and_30000rmb_to_start_affiliate_media_buy.org" "d:/notebook/org-roam/gtd.org" "d:/notebook/org-roam/gtd_archive.org" "d:/notebook/org-roam/gtd_calendar.org" "d:/notebook/org-roam/gtd_goal.org" "d:/notebook/org-roam/gtd_maybe.org" "d:/notebook/org-roam/gtd_next_actions.org" "d:/notebook/org-roam/gtd_projects.org" "d:/notebook/org-roam/gtd_reference.org" "d:/notebook/org-roam/gtd_trash.org" "d:/notebook/org-roam/gtd_waiting.org" "d:/notebook/org-roam/html_css.org" "d:/notebook/org-roam/html常见元素.org" "d:/notebook/org-roam/html版本.org" "d:/notebook/org-roam/http.org" "d:/notebook/org-roam/ioncube加密软件的安装步骤.org" "d:/notebook/org-roam/java.org" "d:/notebook/org-roam/javascript.org" "d:/notebook/org-roam/joomla使用redis缓存加速网站.org" "d:/notebook/org-roam/linux.org" "d:/notebook/org-roam/mariadb安装配置.org" "d:/notebook/org-roam/media_buy.org" "d:/notebook/org-roam/mysql.org" "d:/notebook/org-roam/mysql添加字段命令.org" "d:/notebook/org-roam/nginx配置https.org" "d:/notebook/org-roam/offer.org" "d:/notebook/org-roam/org_roam.org" "d:/notebook/org-roam/org_roam_bibtex文献管理.org" "d:/notebook/org-roam/org_roam_rebuild_db.org" "d:/notebook/org-roam/org_roam_server预览404_not_found.org" "d:/notebook/org-roam/org_roam如何删除节点.org" "d:/notebook/org-roam/seo.org" "d:/notebook/org-roam/shell.org" "d:/notebook/org-roam/slip_box_method.org" "d:/notebook/org-roam/spring_boot部署遇到的问题.org" "d:/notebook/org-roam/spring_mvc_unsupported_media_type_json.org" "d:/notebook/org-roam/spy广告.org" "d:/notebook/org-roam/traffic_networks.org" "d:/notebook/org-roam/vi.org" "d:/notebook/org-roam/vortex_40_day_tutorial_2019.org" "d:/notebook/org-roam/win10开机如何不进入系统选择界面.org" "d:/notebook/org-roam/windows.org" "d:/notebook/org-roam/win键被锁处理方法.org" "d:/notebook/org-roam/上海公司注册.org" "d:/notebook/org-roam/使用linux中crontab定时任务按时备份lampp中mysql数据库.org" "d:/notebook/org-roam/公司事务.org" "d:/notebook/org-roam/公司社保开户流程.org" "d:/notebook/org-roam/发展中国家.org" "d:/notebook/org-roam/周工作量统计.org" "d:/notebook/org-roam/响应式web设计_viewport.org" "d:/notebook/org-roam/多线程.org" "d:/notebook/org-roam/学习工作.org" "d:/notebook/org-roam/操作系统.org" "d:/notebook/org-roam/数据库.org" "d:/notebook/org-roam/日工作量统计.org" "d:/notebook/org-roam/月工作量统计.org" "d:/notebook/org-roam/泛型.org" "d:/notebook/org-roam/爱宝book.org" "d:/notebook/org-roam/爱宝产品部署操作记录.org" "d:/notebook/org-roam/社保缴纳_税务缴纳.org" "d:/notebook/org-roam/编程语言.org" "d:/notebook/org-roam/编辑器.org" "d:/notebook/org-roam/阿里云ecs配置搭建服务.org" "d:/notebook/org-roam/集合.org")))
 '(package-selected-packages
   (quote
    (treemacs-all-the-icons treemacs-projectile projectile treemacs google-translate pomidor org-download web-mode emmet-mode company-org-roam org-ref helm-bibtex org-roam-bibtex org-noter use-package org-roam-server org-roam anki-editor org-pomodoro evil undo-tree nyan-mode company smart-input-source))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; 开启全局 Company 补全
(global-company-mode 1)

;; 使用下面的配置来加入最近打开过文件的选项让我们更快捷的在图形界面的菜单中打开最近 编辑过的文件。
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;; 这个快捷键绑定可以用之后的插件 counsel 代替
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; 使用下面的配置文件将删除功能配置成与其他图形界面的编辑器相同，即当你选中一段文字 之后输入一个字符会替换掉你选中部分的文字。
(delete-selection-mode 1)

;; 启动时全屏
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 启用自动括号匹配（Highlight Matching Parenthesis）
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; 高亮当前行，当文本内容很多时可以很容易找到光标的位置。
(global-hl-line-mode 1)

;; 安装主题
(add-to-list 'my/packages 'monokai-theme)

;; 然后使用下面的配置使其每次打开编辑器时加载主题
(load-theme 'monokai 1)

;;行号背景
(set-face-background 'linum "#000000")
;;行号前景
(set-face-foreground 'linum "#CD661D")
;;当前行背景
(set-face-background 'hl-line "blue")
;;当前行前景
(set-face-foreground 'hl-line "white")

;; 添加 Org-mode 文本内语法高亮
(require 'org)
(setq org-src-fontify-natively t)

;; 设置默认 Org Agenda 文件目录
;; (setq org-agenda-files '("d:/notebook/todo.org"))
(setq org-agenda-files (quote ("d:/notebook/org-roam")))

;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)

;; 配置代码来设置一个模板（其中设置了待办事项的 优先级还有触发键）
;;(setq org-capture-templates
;;      '(("t" "TODO" entry (file+headline "~/notebook/todo.org" "工作安排")
;;	 "* TODO [#B] %?\n  %i\n"
;;	 :empty-lines 1)))
(setq org-capture-templates
      '(("t" "TODO" entry (file "d:/notebook/inbox.org")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)))
;; 设置添加待办事项的快捷键
(global-set-key (kbd "C-c t") 'org-capture)

;; 因为每次保存中文的时候都需要选择解码，我们可以使用下面的配置将文本解码设置默认为 UTF-8
(set-language-environment "UTF-8")

;; 安装好 ag 后我们就可以安装 helm-ag 插件了。（它的 GitHub 仓库地址在这里）在安装 完成后可以为其设置快捷键
(global-set-key (kbd "C-c s") 'helm-do-ag-project-root)

;; 代码块配置
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; 中英文混排表格对齐
;; Setting English Font
(set-face-attribute
'default nil :font "Consolas 11")
;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
(set-fontset-font (frame-parameter nil 'font)
charset
(font-spec :family "Microsoft Yahei" :size 16)))

;; Enable Evil 不使用evil-mode了，总是误操作
(require 'evil)
(evil-mode 1)

;; 配置番茄钟，在mac中可以alert提醒
(defun notify-osx (title message)
  (sound-wav-play "C:/Users/76783/AppData/Roaming/.emacs.d/sound/oh.wav")
  )

(add-hook 'org-pomodoro-finished-hook
	(lambda ()
		(notify-osx "Pomodoro completed!" "Time for a break.")))
(add-hook 'org-pomodoro-break-finished-hook
	(lambda ()
        (notify-osx "Pomodoro Short Break Finished" "Ready for Another?")))
(add-hook 'org-pomodoro-long-break-finished-hook
	(lambda ()
		(notify-osx "Pomodoro Long Break Finished" "Ready for Another?")))
(add-hook 'org-pomodoro-killed-hook    
	(lambda ()
		(notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))
;; 配置mobile-org
(setq org-directory "d:/notebook/")

(setq org-mobile-directory "d:/notebook/org-mobile")

(setq org-mobile-inbox-for-pull "d:/notebook/inbox.org")

(setq org-mobile-files (list "d:/notebook/todo.org" "d:/notebook/blog.org"  "d:/notebook/inbox.org"))
;; 使用smart-input-source前需先安装macism，这个是切换mac中输入法的工具
;; 配置smart-input-source,20200725目前最新版的包名已经改成sis，包括方法前缀均已改变
(setq smart-input-source-external-ism "im-select.exe") ; for windows 命令行切换输入法工具
(setq smart-input-source-english "1033")
(setq-default smart-input-source-other "2052")
;; enable the /cursor color/ mode
(smart-input-source-global-cursor-color-mode t)
;; enable the /respect/ mode
(smart-input-source-global-respect-mode t)
;; enable the /follow context/ mode for all buffers
(smart-input-source-global-follow-context-mode t)
;; enable the /inline english/ mode for all buffers
(smart-input-source-global-inline-mode t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
;; 解决anki-editor中文报错的问题
(setq anki-editor-create-decks t)
;; Emacs支持外部程序的粘贴
(setq x-select-enable-clipboard t)
;; Counsel来增强你的M-x
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h o") 'counsel-describe-symbol)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
;; 关闭自动生成的备份文件
(setq make-backup-files nil)
;; 配置f5刷新文件
(defun refresh-file ()
(interactive)
(revert-buffer t (not (buffer-modified-p)) t))
(global-set-key (kbd "<f5>") 'refresh-file)
(setq org-roam-directory "d:/notebook/org-roam")
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-server-host "127.0.0.1"
      org-roam-server-port 9090
      org-roam-server-export-inline-images t
      org-roam-server-authenticate nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60
      org-roam-server-network-label-wrap-length 20)
(org-roam-server-mode)
(require 'org-roam-protocol)
;; server 配置
(setq server-name "emacs-server-file")
(server-start)
(setq org-roam-graph-executable "D:/package/graphviz-2.44.1-win32/Graphviz/bin/dot")
(setq org-roam-graph-viewer "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

(global-set-key (kbd "C-c n l") 'org-roam)
(global-set-key (kbd "C-c n f") 'org-roam-find-file)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n i") 'org-roam-insert)
(global-set-key (kbd "C-c n I") 'org-roam-insert-immediate)
(global-set-key (kbd "C-c n c") 'org-roam-capture)

;; If you cloned the repository
(add-to-list 'load-path "~/.emacs.d/projects/org-roam-bibtex/") ;Modify with your own path
(require 'org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)

(use-package company-org-roam
  :ensure t
  ;; You may want to pin in case the version from stable.melpa.org is not working 
  ; :pin melpa
  :config
  (push 'company-org-roam company-backends))


(setq
   org_notes (concat "D:/notebook/org-roam/")
   zot_bib (concat "D:/notebook/zotero/masterLib.bib")
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes
   )

(use-package deft
  :commands deft
  :init
  (setq deft-default-extension "org"
        ;; de-couples filename and note title:
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        ;; disable auto-save
        deft-auto-save-interval -1.0
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  :config
  (add-to-list 'deft-extensions "tex")
  )

(setq
 bibtex-completion-notes-path "D:/notebook/org-roam/"
 bibtex-completion-bibliography "D:/notebook/zotero/masterLib.bib"
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )


(use-package org-ref
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography (list "D:/notebook/zotero/masterLib.bib")
         org-ref-bibliography-notes "D:/notebook/notes/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "D:/notebook/org-roam/"
         org-ref-notes-function 'orb-edit-notes
    ))

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
(use-package org-roam-protocol
  :after org-protocol)


(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

(use-package org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_notes)
   )
  )



(global-set-key (kbd "C-c n h") 'helm-bibtex)
(global-set-key (kbd "C-c n r") 'helm-bibtex-with-local-bibliography)

(unless (server-running-p)
  (org-roam-server-mode))

(add-to-list 'auto-mode-alist '("\\.html$" . 'emmet-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html" . web-mode))
(add-hook 'web-mode-hook 'emmet-mode)
(add-to-list 'load-path "~/.emacs.d/projects/highlight-matching-tag/") ;Modify with your own path
(require 'highlight-matching-tag)
(highlight-matching-tag 1)

;;(require 'linum-relative)
;;(linum-relative-on)

(setq org-roam-capture-templates
  '(("d" "default" plain (function org-roam-capture--get-point)
     "%?"
     :file-name "${slug}"
     :head "#+TITLE: ${title}\n"
     :unnarrowed t)))


(global-set-key (kbd "C-c C-.") 'org-insert-structure-template)
(add-to-list 'org-structure-template-alist '("r" . "src R"))
(add-to-list 'org-structure-template-alist '("j" . "src JAVA"))
(add-to-list 'org-structure-template-alist '("js" . "src JAVASCRIPT"))

(use-package org-download
  :ensure t 
  ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
  :bind ("C-S-y" . org-download-screenshot)
  :config
  (require 'org-download)
  ;; Drag and drop to Dired
  (add-hook 'dired-mode-hook 'org-download-enable)
  )
(setq-default org-download-image-dir "D:/notebook/images")

;;; init-pomidor.el --- pomidor
;;; https://github.com/TatriX/pomidor
;;; Commentary:
;;; Code:

(use-package pomidor
  :ensure t
  :defer t
  :init
  (global-set-key "\M-p" #'pomidor)
  :config
  (setq
   ;;pomidor-sound-tick nil ;; nil取消声音
   ;;pomidor-sound-tack nil ;; nil取消声音
   pomidor-sound-tick (expand-file-name (concat (getenv "HOME") "C:/Users/76783/AppData/Roaming/.emacs.d/sound/oh.wav"))
   pomidor-sound-tack (expand-file-name (concat (getenv "HOME") "C:/Users/76783/AppData/Roaming/.emacs.d/sound/oh.wav"))
   pomidor-sound-overwork (expand-file-name (concat (getenv "HOME") "C:/Users/76783/AppData/Roaming/.emacs.d/sound/oh.wav"))
   pomidor-sound-break-over (expand-file-name (concat (getenv "HOME") "C:/Users/76783/AppData/Roaming/.emacs.d/sound/oh.wav"))
   )

  ;; log
  ;; https://github.com/TatriX/pomidor/issues/20
  (defadvice pomidor-stop (before pomidor-save-log activate)
    "Log pomidor data to the D:/notebook/pomidor-log.csv file.
     Columns: date,work,overwork,break"
    (write-region (format "%s,%d,%d,%d\n"
                          (format-time-string "%Y/%m/%d")
                          (/ (time-to-seconds (pomidor-work-duration)) 60)
                          (/ (time-to-seconds (or (pomidor-overwork-duration) 0)) 60)
                          (/ (time-to-seconds (or (pomidor-break-duration) 0)) 60))
                  nil
                  "D:/notebook/pomidor-log.csv"
                  'append))

  (cond
   ((eq system-type 'windows-nt)
    (setq alert-default-style 'toaster)
    )
   ((eq system-type 'gnu/linux)
    (setq alert-default-style 'libnotify)
    ))
  )
(provide 'init-pomidor)
;;; init-pomidor.el ends here

(require 'google-translate)
(require 'google-translate-default-ui)
(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
;;需要将谷歌翻译地址的.com修改成.cn,否则在国内无法进行访问
(setq google-translate-base-url
     "https://translate.google.cn/translate_a/single")
(setq google-translate-listen-url
     "https://translate.google.cn/translate_tts")
(setq google-translate--tkk-url
     "https://translate.google.cn")
;;配置默认语言
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "zh-CN")
(setq-default google-translate-enable-ido-completion t)
;; 配置快捷键
;;(global-set-key "\C-ct" 'google-translate-at-point)
;;(global-set-key "\C-cT" 'google-translate-query-translate)
;;(global-set-key (kbd "\C-cq") 'google-translate-smooth-translate)

(setq-default target-lang "it")
;;(setq-default target-lang "fr")
(setq-default source-lang "en")

(defun insert-next-line (translation)
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent))
  (insert translation)
  )

(defun translation-tookit()
  (interactive)
(insert-next-line 
 (google-translate-json-translation (google-translate-request source-lang target-lang (thing-at-point 'line)))
 )
)

(global-set-key (kbd "\C-ct") 'translation-tookit)


(let ((style "google"))
 (setq format-command (format "astyle --style=%s" style)))

(global-auto-revert-mode t)

(defun format-code ()
 "Format current buffer"
 (interactive)
 (let ((file (buffer-file-name)))
  (save-excursion
    (shell-command-to-string (format "%s %s" format-command file))
    (message "Code formatted"))))

(global-set-key "\C-f" 'format-code)



(defun format-code-coolformat ()
 "Format current buffer"
 (interactive)
 (let ((file (buffer-file-name)))
  (save-excursion
    (shell-command-to-string (format "%s %s" "coolformat -f" file))
    (message "Code formatted"))))

(global-set-key (kbd "C-c C-f") 'format-code-coolformat)
(global-set-key (kbd "C-c ps") 'treemacs-switch-workspace)
(global-set-key (kbd "<f9>") 'treemacs)

;; 相对行号和绝对行号同时显示
(defun linum-relative-right-set-margin ()
  "Make width of right margin the same as left margin"
  (let* ((win (get-buffer-window))
     (width (car (window-margins win))))
    (set-window-margins win width width)))

(defadvice linum-update-current (after linum-left-right-update activate)
  "Advice to run right margin update"
  (linum-relative-right-set-margin)
  (linum-relative-right-update (line-number-at-pos)))

(defadvice linum-delete-overlays (after linum-relative-right-delete activate)
  "Set margins width to 0"
  (set-window-margins (get-buffer-window) 0 0))

(defun linum-relative-right-update (line)
  "Put relative numbers to the right margin"
  (dolist (ov (overlays-in (window-start) (window-end)))
    (let ((str (overlay-get ov 'linum-str)))
      (if str
      (let ((nstr (number-to-string
               (abs (- (string-to-number str) line)))))
        ;; copy string properties
        (set-text-properties 0 (length nstr) (text-properties-at 0 str) nstr)
        (overlay-put ov 'after-string
             (propertize " " 'display `((margin right-margin) ,nstr))))))))

(global-linum-mode 1)
