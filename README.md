#Pallet

##tl;dr

Pallet is a simple package-management system for Emacs.

It uses rejeep's excellent [Carton](https://github.com/rejeep/carton.git) as a platform to keep track of your installed packages.

You can install it, via [Melpa](http://melpa.milkbox.net), using `package-list-packages`.

##What problem does Pallet solve?

You are an Emacs user, and you use package.el to maintain a set of installed packages, via the Elpa archive (and/or Melpa, Marmalade, whatever). You use Emacs at more than one site or on more than one machine, and you want to use the same packages with each Emacs installation.

The standard way of synchronising settings among Emacs installs is to keep your emacs.d directory under version control, perhaps using Git. That way, you can simply push your settings to a repo when they change, and pull them at another site. This way of working also works with the `/elpa` subdirectory of emacs.d, keeping your installed packages in a versioned repository.

###Package management

Working this way with the `/elpa` directory quickly becomes a chore, though. The directory can become large and complex, and you can encounter merge conflicts when trying to synchronise it across Emacs installs. The solution is a dependency management system (like Ruby's [Bundler](http://gembundler.com), for example) which allows you to keep one 'manifest' file under version control, and ignore the *actual* installed packages; the manifest file lists packages and versions, and the dependency manager can install and update the listed packages whenever required to.

###Carton

[Carton](https://github.com/rejeep/carton.git) is a dependency manager for Emacs, which is gaining currency especially in new Elisp projects. It provides a simple format for creating manifest files, and a set of functionality to install and update packages (as well as some very useful utilities for package *development*).

The piece missing from Carton is the functionality to create and maintain a manifest file *in tandem* with package.el, Emacs' built-in package system.

You can, of course, manually maintain your Carton (manifest) file, but most of us like to use `M-x package-list-packages` to discover and install packages. Pallet lets you do exactly this.

##How does it work?

`M-x pallet-init` will look at your installed packages and source archives and create a valid Carton file in your Emacs directory. You now no longer need to keep your `/elpa` directory under version control; simply keep your Carton file under version control, and use Carton to keep your packages synchronised across Emacs installs.

Pallet will update your Carton file when you close Emacs, or when you run `M-x pallet-repack`, so you can use `M-x package-list-packages` (or any other method) to install and delete packages as normal.

You can install your Carton-managed packages using `pallet-install`, and update them using `pallet-update`. These commands are just interactive aliases of the relevant Carton functions.

##Alternatives

[el-get](https://github.com/dimitri/el-get) is a popular and feature-packed project which does much more than Pallet. Pallet just tries to do one simple thing well enough.

##What's coming?

More configurability, maybe package versioning and rollbacks, dependency awareness ... tell me what you need, or, better, contribute.

