# i3bar.el

An Emacs package for displaying the output of an `i3status` compatible command in the `mode-line`
(or `tab-bar`).

```elisp
(use-package i3bar
  :quelpa (i3bar :fetcher github :repo "Stebalien/i3bar.el")
  :config
  (i3bar-mode 1))

(use-package tab-bar
  :custom
  (tab-bar-format '(tab-bar-format-tabs
                    tab-bar-format-align-right
                    tab-bar-format-global))
  :config
  (tab-bar-mode 1)
```
