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
		      org-bullets
		      sound-wav
		      ;;magit
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
		      ;;smartparens
		      ;; --- Major Mode ---
		      ;;js2-mode
		      ;; --- Minor Mode ---
		      ;;nodejs-repl
		      ;;exec-path-from-shell
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
  (find-file "d:/notebook/todo.org"))

;; 这一行代码，将函数 open-todo-file 绑定到 <f3> 键上
(global-set-key (kbd "<f8>") 'open-todo-file)

;; 快速打开todo文件
(defun open-my-dir()
  (interactive)
  (find-file "d:/notebook/org-roam/"))

;; 这一行代码，将函数 open-todo-file 绑定到 <f3> 键上
(global-set-key (kbd "<f9>") 'open-my-dir)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f6>") 'open-init-file)

;; 快速加载配置文件
(defun load-init-file()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 load-init-file 绑定到 <f1> 键上
(global-set-key (kbd "<f7>") 'load-init-file)

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
    (windresize easy-hugo counsel-projectile projectile ranger google-translate pomidor org-download web-mode emmet-mode company-org-roam org-ref helm-bibtex org-roam-bibtex org-noter use-package org-roam-server org-roam anki-editor org-pomodoro evil undo-tree nyan-mode company smart-input-source)))
 '(word-wrap nil))
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
;; (set-face-background 'linum "#000000")
;;行号前景
;; (set-face-foreground 'linum "#CD661D")
;;当前行背景
;; (set-face-background 'hl-line "blue")
;;当前行前景
;; (set-face-foreground 'hl-line "white")

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
      '(("t" "TODO" entry (file "d:/notebook/todo.org")
	 "* TODO [#C] %?\n  %i\n"
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

(setq org-roam-capture-templates
  '(("d" "default" plain (function org-roam-capture--get-point)
     "%?"
     :file-name "${slug}"
     :head "#+TITLE: ${title}\n#+HUGO_BASE_DIR: ../../eoebook/\n#+HUGO_SECTION: ../content/posts/\n#+HUGO_WEIGHT: 2001\n#+EXPORT_FILE_NAME: ${slug}\n#+DATE: ${date}\n#+EXPORT_DATE: ${date}\n#+HUGO_AUTO_SET_LASTMOD: t\n#+HUGO_TAGS: \n#+HUGO_DRAFT: false\n#+author:\n#+hugo_custom_front_matter: :author \"赔了命3000\"\n# Local Variables:\n# eval: (org-hugo-auto-export-mode)\n# End:\n\n\n#+hugo: more\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
     :unnarrowed t)))
  
(global-set-key (kbd "C-c n l") 'org-roam)
(global-set-key (kbd "C-c n f") 'org-roam-find-file)
(global-set-key (kbd "C-c n i") 'org-roam-insert)
(global-set-key (kbd "C-c n I") 'org-roam-insert-immediate)
(global-set-key (kbd "C-c n c") 'org-roam-capture)

(use-package company-org-roam
  :ensure t
  ;; You may want to pin in case the version from stable.melpa.org is not working 
  ; :pin melpa
  :config
  (push 'company-org-roam company-backends))

(global-visual-line-mode t)

(global-set-key (kbd "C-c C-.") 'org-insert-structure-template)
(add-to-list 'org-structure-template-alist '("r" . "src R"))
(add-to-list 'org-structure-template-alist '("j" . "src JAVA"))
(add-to-list 'org-structure-template-alist '("js" . "src JAVASCRIPT"))

;; 快速插入当前时间
(defun insert-current-time ()
"Insert the date in current position."
(interactive)
(insert (format-time-string "[%Y-%m-%d %H:%M]")))
(global-set-key (kbd "C-c c") 'insert-current-time)


;; 如何隐藏 headlines 前面的一堆*符号？
;; (setq org-startup-indented t)
;; 如何将 headlines 的符号从*变成其它符号？
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("☰" "☷" "☯" "☭"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

;; (setq org-ellipsis " ▼ ")
;; 如何直接显示 加粗 、 删除线 、 强调 等样式？
(setq org-hide-emphasis-markers t)
;; 如何设置 headline 的 TODO keywords？
;; "|"之前的为进行中的状态， 之后的为结束的状态
;;(setq org-todo-keywords
;;      '((sequence "TODO" "HAND" "|" "DONE")))
;; 如何给 TODO keywords 增加前景色、背景色？
(setf org-todo-keyword-faces
      '(("TODO" . (:foreground "white" :background "#95A5A6"   :weight bold))
        ("HAND" . (:foreground "white" :background "#2E8B57"  :weight bold))
        ("DONE" . (:foreground "white" :background "#3498DB" :weight bold))))


(require 'cl)   ; for delete*
;;; custom org emhasis color
(setq org-emphasis-alist
  '(("*" (bold :foreground "Orange" ))
    ("/" italic)
    ("_" underline)
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t))))



(global-set-key (kbd "C-c p s") 'profiler-start)
(global-set-key (kbd "C-c p e") 'profiler-stop)
(global-set-key (kbd "C-c p r") 'profiler-report)

;; 该代码段(进入您的.emacs定制文件)将使Emacs暂时认为，杀死它时没有 Activity 的进程，因此不会出现烦人的提示。
(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))
;; 解决org折叠展开卡顿的问题
(setq inhibit-compacting-font-caches t)

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;; easy-hugo

(setq easy-hugo-basedir "D:/eoebook")
(setq easy-hugo-postdir "content/posts")
(setq easy-hugo-url "https://qikun1990.github.io")
(setq easy-hugo-previewtime "300")
(define-key global-map (kbd "<f12>") 'easy-hugo)

 (dolist (m '(easy-hugo-mode))
   (add-to-list 'evil-emacs-state-modes m))



(global-set-key (kbd "<f10>") 'org-hugo-export-to-md)


;; dired-dwim-target是在`dired.el'中定义的变量。它的值为零
;; 文档：如果非零，Dired会尝试猜测默认的目标目录。这意味着：如果下一个窗口中显示Dired缓冲区，请使用其当前目录，而不是此Dired缓冲区的当前目录。
;; 该目标用于提示文件复制，重命名等。
;; 将其放在您的init文件中：(setq dired-dwim-target t)。然后，转到split-window-vertically目录，拆分窗口，然后转到另一个目录。当您按C键进行复制时，拆分窗格中的另一个目录将成为默认目标。
(setq dired-dwim-target t)



;;;windresize
(require 'windresize)
 (global-set-key (kbd "C-c m") 'windresize)

;;;Winner-mode
;;可以使用 Ctrl-c ← （就是向左的箭头键）组合键，退回你的上一个窗口设置。）
;;可以使用 Ctrl-c → （前进一个窗口设置。）
(when (fboundp 'winner-mode) 
  (winner-mode) 
  (windmove-default-keybindings)) 

;;;windmove-mode
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings)
    (global-set-key (kbd "C-c l")  'windmove-left)
    (global-set-key (kbd "C-c h") 'windmove-right)
    (global-set-key (kbd "C-c k")    'windmove-up)
    (global-set-key (kbd "C-c j")  'windmove-down))
