# ansible-vault-mode

Minor mode for in place manipulation of [ansible-vault][ansible-vault].

## Installation

### Recommended way

Put this to `~/.emacs`:

```lisp
(use-package ansible-vault
  :init
  (add-to-list 'magic-mode-alist (cons #'ansible-vault--is-encrypted-vault-file #'ansible-vault-mode)))
```

### Manual way

```
M-x package-install RET ansible-vault RET
```

### Development way

Put this into `~/.emacs`:

```lisp
(use-package git :pin melpa-stable)

(setq my/ansible-vault-mode-dir    "~/repos/github.com/freehck/ansible-vault-mode")
(setq my/ansible-vault-mode-repo   "https://github.com/freehck/ansible-vault-mode.git")
(setq my/ansible-vault-mode-branch "master")

(use-package ansible-vault
  :pin manual
  :load-path my/ansible-vault-mode-dir
  :init
  (unless (file-accessible-directory-p my/ansible-vault-mode-dir)
    (make-directory my/ansible-vault-mode-dir))
  (let ((git-repo (f-full my/ansible-vault-mode-dir)))
    (unless (git-repo? my/ansible-vault-mode-dir)
      (git-clone my/ansible-vault-mode-repo git-repo))
    (unless (git-on-branch? my/ansible-vault-mode-branch)
      (git-checkout my/ansible-vault-mode-branch)))
  (add-to-list 'magic-mode-alist (cons #'ansible-vault--is-encrypted-vault-file #'ansible-vault-mode))
  (load (expand-file-name "ansible-vault.el" my/ansible-vault-mode-dir) nil t)
  )
```

### Good old very manual way

Or manually downloading `ansible-vault-mode` and adding the following
lines to your conf files:

```lisp
(add-to-list 'load-path "/path/to/ansible-vault")
(require 'ansible-vault)
```


## Usage

The only thing I recommend is to update `magic-mode-alist` in order to enable the mode automatically
when you open an encrypted file. Anyway, it's already written in the examples above.

```
(add-to-list 'magic-mode-alist (cons #'ansible-vault--is-encrypted-vault-file #'ansible-vault-mode))
```

When enabled, the mode tries to find `ansible.cfg` file. First it checks `ANSIBLE_CONFIG`
environment variable. If not set, it performs an upward search starting from the file location. Then
it tries `~/.ansible.cfg` and eventually `/etc/ansible/ansible.cfg`.

So I recommend storing `ansible.cfg` in the root of the repo with the infrastructure code.

The mode decrypts and encrypts files automatically: decrypts when you enable the mode, encrypts back
when you save the modifed buffer.

After initialization it tries to activate an appropriate major-mode for by calling `normal-mode` on
already decrypted buffer.

If case of errors look into ```*ansible-vault-error*``` buffer.



### Vault Id configuration

Ansible Vault now supports vault-id for multiple passwords. You can persistently track vault ids
between sessions by configuring the `ansible-vault-vault-id-alist` value with `(vault-id
. password-file)` pairs.

```lisp
(setq
 ansible-vault-vault-id-alist
 '(("nonprod" . "/home/notprod/ansible/vault/nonprod-secret")
   ("prod" . "/home/notprod/ansible/vault/prod-secret")
   ("foo" . "/etc/foo.secret")))
```

This allows properly tagged v1.2 vault files to automatically find and use their associated password
files.

Nota Bene:
The current maintainer didn't test this functionality, so you're on your own with it.

### Notes on version 0.6.0

 - Now `ansible-vault-mode` allows to change major mode, and even do it by default right after
   initialization, so you can work with encrypted files as if they were the ordinary ones. They will
   be re-encrypted when you save your changes.
   
 

### Notes on version 0.5.0 and beyond

 - `ansible-vault-mode` is now more aggressive in detecting valid password files. If it fails to
   locate a valid password file it will prompt the user for input.

 - The minor mode now defines some key bindings under `C-c a`
    - `C-c a d` Decrypts the current file and saves it
    - `C-c a D` Decrypts the current region
    - `C-c a e` Encrypts the current file and saves it
    - `C-c a E` Encrypts the current region
    - `C-c a p` Updates the password of the current buffer
    - `C-c a i` Updates the vault-id of the current buffer

## Contributing

Bug reports and pull requests are welcome on [GitHub issues][issues].

Feature requests are welcome too, but I strongly recommend to consider filing a PR additionally.

## Copyright

Copyright (C) 2016 Zachary Elliott &lt;contact@zell.io&gt;
Copyright (C) 2025 Dmitrii Kashin  &lt;freehck@yandex.ru&gt;

This program is licensed under (GPLv3)[LICENSE].
