#Pallet

##tl;dr

Pallet is a simple package-management system for Emacs.

It uses rejeep's excellent
[Cask](https://github.com/rejeep/cask.el) as a platform to keep
track of your installed packages.

##Installation

To install Pallet, you should first install Cask, following the
instructions [here](https://github.com/rejeep/cask.el). At present,
just install Cask -- don't add anything to your .emacs or init.el file.

After installing Cask, there are two ways you can go, depending on
your situation:

1. **I have a working Emacs install, with packages already installed,
   and can access [MELPA](http://melpa.milbox.org).**

   In this case run `M-x list-packages`, and install Pallet. Then, below
   the lines which initialize your package system, add `(require
   'pallet)`.

   Restart Emacs, and run `pallet-init`. Now you have a Cask file in your
   emacs.d directory which contains listings for all files you've
   previously installed via `package-install`, and your .emacs.d/elpa
   directory has been replicated under .emacs.d/.cask/.

   You can if you wish now delete your .emacs.d/elpa directory, and go to
   step 3.

2. **I have a newly installed Emacs and/or am not set up to use
   package-install.**

   In this case, create a file called `Cask` in your emacs.d
   directory. Add the following lines to it:

   ```lisp
   (source melpa)

   (depends-on "pallet")
   ```

   Then, in terminal and in your emacs.d directory, run

   ```
   cask install
   ```

   This will create a `.cask` directory inside your .emacs.d directory,
   initialize a package directory under .emacs.d/.cask/, and install
   Pallet to it.

3. If you have any package initialization lines in your init.el file,
   you can delete them. To replace those lines, add:

   ```lisp
   (require 'cask "~/.cask/cask.el")
   (cask-initialize)
   ```

   Retain any `require` statements below.


##What problem does Pallet solve?

You are an Emacs user, and you use package.el to maintain a set of
installed packages, via the Elpa archive (and/or Melpa, Marmalade,
whatever). You use Emacs at more than one site or on more than one
machine, and you want to use the same packages with each Emacs
installation.

The standard way of synchronising settings among Emacs installs is to
keep your emacs.d directory under version control, perhaps using
Git. That way, you can simply push your settings to a repo when they
change, and pull them at another site. This way of working also works
with the `/elpa` subdirectory of emacs.d, keeping your installed
packages in a versioned repository.

###Package management

Working this way with the `/elpa` directory quickly becomes a chore,
though. The directory can become large and complex, and you can
encounter merge conflicts when trying to synchronise it across Emacs
installs. The solution is a dependency management system (like Ruby's
[Bundler](http://gembundler.com), for example) which allows you to
keep one 'manifest' file under version control, and ignore the
*actual* installed packages; the manifest file lists packages and
versions, and the dependency manager can install and update the listed
packages whenever required to.

###Cask

[Cask](https://github.com/rejeep/cask.el) is a dependency manager
for Emacs, which is gaining currency especially in new Elisp
projects. It provides a simple format for creating manifest files, and
a set of functionality to install and update packages (as well as some
very useful utilities for package *development*).

The piece missing from Cask is the functionality to create and
maintain a manifest file *in tandem* with package.el, Emacs' built-in
package system.

You can, of course, manually maintain your Cask (manifest) file, but
most of us like to use `M-x package-list-packages` to discover and
install packages. Pallet lets you do exactly this.

##How does it work?

First, you need to install Pallet (see above).

`M-x pallet-init` will look at your installed packages and source
archives and create a valid Cask file in your Emacs directory. You
now no longer need to keep your `/elpa` directory under version
control (in fact, you can delete it as Cask will now manage your
packages for you); simply keep your Cask file under version control, and use
Cask and Pallet to keep your packages synchronised across Emacs
installs. Pallet will update your Cask file when you add or delete packages via
`list-packages`.

##Alternatives

[el-get](https://github.com/dimitri/el-get) is a popular and
feature-packed project which does much more than Pallet. Pallet just
tries to do one simple thing well enough.
