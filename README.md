# i3bar.el

An Emacs package for displaying the output of an `i3status` compatible command in the `mode-line`
or `tab-bar`.

This package is primarily useful for EXWM users who wish to render a status-bar within Emacs itself:

- It can be combined with the tab-bar to save vertical space.
- It can be themed by Emacs (see [Theming](#theming)).

## Installation

`i3bar` is available on [MELPA](https://melpa.org/#/i3bar).

1. Install and configure an [i3status compatible status-bar generator](https://wiki.archlinux.org/title/I3#i3status).
2. Install any required fonts (likely `ttf-font-awesome`).
3. Install and configure this package.

```elisp
(use-package i3bar
  :ensure t ; assumes you have enabled MELPA per https://melpa.org/#/getting-started
  ;; Or with straight:
  ;:straight (i3bar :type git :host github :repo "Stebalien/i3bar.el")
  :config
  (i3bar-mode 1))
```

## Usage with EXWM (or more generally without i3)

To use this package with EXWM, you need to create a =~/.config/i3/config= file
with the following entry:

```text
general {
        output_format = "i3bar"
}
```

This will cause i3 to output the status-bar to stdout in the i3status format.
Then, you can use the following command to start i3bar:

```elisp
(use-package! i3bar
  :config
  (setopt tab-bar-format '(tab-bar-format-tabs        ; Optional: Remove to _only_ display the bar.
                           tab-bar-format-align-right ; Optional: Remove to align left.
                           tab-bar-format-global))
  (i3bar-mode 1))
```

## Using as a fallback status bar

If you are using this package in many different environments, you may want to use it as a fallback
when no other status bar is running. You can do this by defining a macro which
only loads the package if no other status bar is running:

```elisp
(defun check-processes-async (process-list callback)
  "Check asynchronously if none of the processes in PROCESS-LIST are running and then call CALLBACK."
  (let ((buffer (generate-new-buffer " *check-proc-async*")))
    (set-process-sentinel
     (apply #'start-process " *check-proc-async*" buffer "pidof" process-list)
     (lambda (_process _event)
       (unwind-protect
           (funcall callback (= (buffer-size buffer) 0)) ; if buffer is empty, no processes are running
         (kill-buffer buffer)))))
  nil)

(defmacro when-none-of-these-processes-running (process-list arg-form)
  "Execute ARG-FORM if none of the processes in PROCESS-LIST are running."
  `(check-processes-async ',process-list
    (lambda (none-running)
      (when none-running
        ,arg-form))))
        
(when-none-of-these-processes-running
 ("polybar" "xmobar")
 (use-package! i3bar
   :config
   (setopt tab-bar-format '(tab-bar-format-tabs        ; Optional: Remove to _only_ display the bar.
                            tab-bar-format-align-right ; Optional: Remove to align left.
                            tab-bar-format-global)))
   (i3bar-mode 1))
```

## Screenshot

![Screenshot](screenshot.png)

## Tab Bar

You can place the i3bar in the tab-bar as follows:

```elisp
(use-package tab-bar
  :custom
  (tab-bar-format '(tab-bar-format-tabs        ; Optional: Remove to _only_ display the bar.
                    tab-bar-format-align-right ; Optional: Remove to align left.
                    tab-bar-format-global))
  :config
  (tab-bar-mode 1))
```

## Theming

By default, this package uses the colors specified by your `i3status` command. However, you can
define a custom `i3bar-face-function` to override this.

For example, I use the following theme with `i3status-rust`:

```toml
idle_bg = "#000000"
idle_fg = "#aaaaaa"
info_bg = "#000000"
info_fg = "#bbbbbb"
good_bg = "#000000"
good_fg = "#cccccc"
warning_bg = "#000000"
warning_fg = "#eeeeee"
critical_bg = "#000000"
critical_fg = "#ffffff"
separator = "\ue0b2"
alternating_tint_bg = "#111111"
separator_bg = "auto"
separator_fg = "auto"
```

Then I use the following "theme" function to make the status-bar's theme match my Emacs theme:

```elisp
(defun i3bar-face-function-theme (foreground background)
  (list
    (pcase (and foreground (upcase foreground))
      ("#000000" `(:foreground ,(face-background 'default nil t)))
      ("#111111" `(:foreground ,(face-background 'hl-line nil t)))
      ("#AAAAAA" 'shadow)
      ("#BBBBBB" nil)
      ("#CCCCCC" 'success)
      ("#EEEEEE" 'warning)
      ("#FFFFFF" 'error))
    (pcase (and background (upcase background))
      ("#000000" nil)
      ("#111111" 'hl-line))))

(custom-set-variables '(i3bar-face-function i3bar-face-function-theme))
```

## Known Issues

This package is missing featuers and features and has some rough edges. I'm happy to accept patches
for any of these issues, assuming the patches don't introduce _other_ issues.

### Unimplemented Features

This package implements just enough of the i3status protocol to be useful, but not everything.

- Click Events
- Pixel Spacing
- Pango Markup

### Fonts & Icons

This package makes no attempt to correctly configure fonts. Ideally it would create a special
font-set that would "do the right thing", but my understanding of such black arts are still quite
limited.
