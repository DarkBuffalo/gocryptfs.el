# GoCryptFs.el
<img src="assets/logo.png" align="right">

gocryptfs.el is a tool to easily mount and unmount an encrypted folder to store its most sensitive data.



## Inspiration

This package is inspired by the [code embedded](https://github.com/abougouffa/minemacs/blob/main/elisp/ecryptfs.el) in @abougouffa [minemacs distribution](https://github.com/abougouffa/minemacs).



## Requirements

-   install [gocryptfs](https://github.com/rfjakob/gocryptfs)

    `sudo pacman -S gocryptfs`

-   Initialize new encrypted filesystem

        gocryptfs -init [OPTIONS] CIPHERDIR

    see [manpage](https://github.com/rfjakob/gocryptfs/blob/master/Documentation/MANPAGE.md#initialize-new-encrypted-filesystem)



## Installation

    (use-package gocryptfs
       :straight (:local-repo "~/your folder/gocryptfs"))



## Use

https://github.com/DarkBuffalo/gocryptfs.el/assets/65488203/8592d088-986f-48e7-957c-76ba67de6c40


## Project

-   [ ] add init auto
-   [ ] add reverse mode option
