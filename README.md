Pallet
======

Pallet is a simple package-management system for Emacs.

It uses rejeep's excellent [Carton](https://github.com/rejeep/carton.git) as a platform to keep track of installed Elpa packages.

`M-x pallet-init` will look at your installed packages and source archives and create a valid Carton file in your Emacs directory. You now no longer need to keep your /elpa directory under version control; simply keep your Carton file under version control, and use Carton to keep your packages synchronised across Emacs installs.

Pallet will update your Carton file when you close Emacs, or when you run `M-x pallet-repack`, so you can use package.el (or any other method) to install and delete packages as normal.

You can install your Carton-managed packages using `pallet-install`, and update them using `pallet-update`. These commands are just interactive aliases of the relevant Carton functions.

This is an early release of Pallet. Forthcoming releases will do more, and the package will shortly be released on Melpa.
