keymap-suggest.el
=================

Let Emacs suggest you which key bindings to press. 

This package will probably be most useful to new Emacs users wondering how to
unleash the power under their fingerprints.


Why
===

Have you ever learned all emacs key bindings ? Most likely you know some of them just to
keep efficent while coding. Do you know all those shiny new bindings installed by some
cool minor mode? Can you remember them all?

What if Emacs could give you suggestions on which keys to press when it notices you have
entered just part of a command. For example, if you press `C-x` and wait for a sec you'll
be shown in the echo area which keys are available under that mapping, so you'll see for
example `f`, and just pressing `f` will execute the `find-file` command.


Installation
============

Install with `package.el` from [MELPA](http://melpa.milkybox.net) or [Marmalade](http://marmalade-repo.org)

```elisp
M-x package-install RET keymap-suggest RET
```

Or with [el-get](https://github.com/dimitri/el-get)

```elisp
(el-get 'sync 'keymap-suggest)
```

Usage
=====

```elisp
(keymap-suggest-mode 1)
```


Customization
=============

You can set the `keymap-suggest:interval` variable to the number (or fraction) of seconds
after which emacs should present suggestions. The default value is 1 second.


