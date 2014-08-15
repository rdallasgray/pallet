#Pallet

Pallet is a package management helper for Emacs.

It uses @rejeep's excellent
[Cask](https://github.com/cask/cask) as a platform to keep
track of your installed packages.

##News
Pallet version 0.7 is now available. This version introduces a
significant breaking change: it is now necessary to start
`pallet-mode` for pallet to track your package installs and
deletes. See the instructions below.

Version 0.7 introduces a new integration test harness using
[Servant](https://github.com/cask/servant). This is intended to allow
safer and quicker addition of new features going forward. The tests
have at present only been run in Emacs 24.4.

##Target platform

Pallet should work with Emacs 24 (including recent snapshots).

##Use
Pallet has a very simple interface:
- `M-x pallet-init` creates a Cask file using information about
  installed packages from the package.el system
- `M-x pallet-install` installs packages listed in your Cask file
- `M-x pallet-update` updates installed packages

Pallet's main job, though, is to add and delete package references
from your Cask file as you install and delete them using the built-in
Emacs package management system. Turn this on by adding `(pallet-mode
t)` to your Emacs init file, or by calling `pallet-mode` interactively (`M-x
pallet-mode`).

##Installation

To install pallet, you should first install Cask, following the
instructions [here](http://cask.readthedocs.org/en/latest/). **At present,
just install Cask -- don't add anything to your .emacs or init.el file**.

After installing Cask, there are two ways you can go, depending on
your situation:

1. **I have a working Emacs install, with packages already installed,
   and can access [Melpa](http://melpa.milbox.org).**

   In this case run `M-x list-packages`, and install pallet.  Then,
   run `M-x pallet-init`. Now you have a Cask file in your emacs.d
   directory which contains listings for all files you've previously
   installed via `package-install`. Run `M-x pallet-install`, and your
   .emacs.d/elpa directory will be replicated under .emacs.d/.cask/.

   You can if you wish now delete your .emacs.d/elpa directory, and
   remove any lines from your init.el adding archives to
   `package-archive`, or running `package-initialize`.

2. **I have a newly installed Emacs and/or am not set up to access
   Melpa.**

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

   This will create a .cask directory inside your .emacs.d directory,
   initialize a package directory under .emacs.d/.cask/, and install
   pallet to it.

**Finally, make sure the following lines are in your init.el, before any
  packages are required:**

```lisp
(require 'cask "<path-to-cask>/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
```

`<path-to-cask>` will vary depending on how you installed Cask: if you
installed via the `curl` method, it is likely to be `~/.cask`; if you
installed via Homebrew, it is likely to be
`/usr/local/Cellar/cask/<version>`.

If you want pallet to maintain your Cask file automatically as you
install and delete packages using Emacs' built-in package-management,
enable `pallet-mode` by calling `(pallet-mode t)`. You can enable or
disable `pallet-mode` at any time by interactively calling
`pallet-mode` (`M-x pallet-mode`).

##Contributing
Contributions to pallet are very welcome.

Fork and clone the repo, then run `git
submodule update --init`, which will install
[el.mk](http://github.com/rdallasgray/el.mk).

Now, [install Cask](https://github.com/rejeep/cask.el).

Then run `cask install`. You should now be able to run the tests using
`make test`.

Any new feature or bugfix should be covered by tests -- see the files
in /test for guidance on how to write your own. When you've
created your feature, make a pull request against master in this repo.
