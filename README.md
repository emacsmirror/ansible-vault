# ansible-vault-mode

Minor mode for in-place manipulations with files encrypted by [ansible-vault][ansible-vault].

## Installation

### Recommended way

Put this into `~/.emacs`:

```lisp
(use-package ansible-vault :ensure t)
```

### Manual way

```
M-x package-install RET ansible-vault RET
```

### Development way

Put this into `~/.emacs`:

```lisp
(use-package git :ensure t)

(defun my/setup-git-package (&rest args)
  (require 'git)
  (require 'f)
  (let ((repo-url (plist-get args :repo-url))
        (repo-dir (plist-get args :repo-dir))
        (repo-branch (plist-get args :repo-branch))
        (feature (plist-get args :feature)))
    (unless (and repo-dir repo-url)
      (error "Keys :repo-dir and :repo-url are required"))
    (let* ((repo-branch (or repo-branch "master"))
           (repo-dir (f-full repo-dir)))
      (unless (file-directory-p repo-dir)
        (message "Create directory: %s" repo-dir)
        (make-directory repo-dir t))
      (let ((git-repo repo-dir))
        (unless (git-repo? repo-dir)
          (message "Clone repo %s to %s" repo-url repo-dir)
          (git-clone repo-url repo-dir))
        (unless (git-on-branch? repo-branch)
          (message "Checkout to branch: %s" repo-branch)
          (git-checkout repo-branch)))
      (let* ((feature-filename (concat feature ".el"))
             (feature-file-fullpath (expand-file-name feature-filename repo-dir)))
        (message "Load feature from file: %s" feature-file-fullpath)
        (load feature-file-fullpath nil t)))))

(my/setup-git-package :feature "ansible-vault"
                      :repo-dir "~/repos/github.com/freehck/ansible-vault-mode"
                      :repo-url "git@github.com:freehck/ansible-vault-mode.git"
                      :repo-branch "develop")

```

### Good old very manual way

Download this repo, store somewhere on disk, and put this into `~/.emacs`:

```lisp
(add-to-list 'load-path "/path/to/ansible-vault")
(require 'ansible-vault)
```



## Usage

When enabled, the mode tries to find `ansible.cfg` file. First it checks `ANSIBLE_CONFIG`
environment variable. If not set, it performs an upward search starting from your encrypted file
location. Then it tries `~/.ansible.cfg` and eventually `/etc/ansible/ansible.cfg`.

So I recommend storing `ansible.cfg` in the root of the repo with your ansible code.

When the mode found `ansible.cfg` file, it takes `vault_password_file` directive from it to
detirmine where to take the vault password from. Then it uses it to decrypt/encrypt the file.

The mode decrypts and encrypts files automatically: decrypts when you enable the mode, encrypts back
when you save the modifed buffer.

After initialization it tries to activate an appropriate major-mode for by calling `normal-mode` on
already decrypted buffer.

In case of errors look into ```*ansible-vault-error*``` buffer.

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



## Release Notes

### version 0.6.0

 - Now `ansible-vault-mode` allows to change major mode, and even do it by default right after
   initialization, so you can work with encrypted files as if they were the ordinary ones. They will
   be re-encrypted when you save your changes.
   
 

### version 0.5.0 and beyond

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



## License

This program is licensed under [GPLv3][license].



## Authors and Contributors

Zachary Elliott &lt;contact@zell.io&gt;<br/>
Dmitrii Kashin  &lt;freehck@yandex.ru&gt;<br/>
Peter Bray      [@illumino](https://github.com/illumino)<br/>



[ansible-vault]: http://docs.ansible.com/ansible/playbooks_vault.html
[yaml]: http://yaml.org/
[issues]: https://github.com/freehck/ansible-vault-mode
[license]: https://raw.githubusercontent.com/freehck/ansible-vault-mode/refs/heads/master/LICENSE
