[![Melpa Status](http://melpa.milkbox.net/packages/pallet-badge.svg)](http://melpa.milkbox.net/#/pallet)

# Pallet
Pallet is a package management helper for Emacs.

It uses @rejeep's excellent
[Cask](https://github.com/cask/cask) as a platform to keep
track of your installed packages.

## News
Version 0.9 introduces support for Cask version and VC references,
which will now be retained if specified in your Cask file:
```
(depends-on "graphene" "20141030.219")
(depends-on "pallet" :git "https://github.com/rdallasgray/pallet" :ref "master")
```
Many thanks to [Sam Brightman](https://github.com/sambrightman) for
implementing this feature.

Version 0.8 introduces the `;;;pallet-ignore` comment, which allows
you to tell Pallet to ignore (and retain) text following the comment.

Version 0.7 introduces a significant breaking change: it is now
necessary to start `pallet-mode` for pallet to track your package
installs and deletes. See the instructions below.

Version 0.7 introduces a new integration test harness using
[Servant](https://github.com/cask/servant). This is intended to allow
safer and quicker addition of new features going forward.

## Target platform
Pallet is currently tested with Emacs versions 24.3 through 24.4.

## Use
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

## Installation
To install pallet, you should first install Cask, following the
instructions [here](http://cask.readthedocs.org/en/latest/). **At present,
just install Cask -- don't add anything to your .emacs or init.el file**.

After installing Cask, there are two ways you can go, depending on
your situation:

1. **I have a working Emacs install, with packages already installed,
   and can access [Melpa](http://melpa.org).**

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

## Ignoring a section of your Cask file
If you prefer to have Pallet ignore part of your Cask file (e.g. so
you can use Cask's
[VC dependencies](http://cask.readthedocs.org/en/latest/guide/dsl.html#dependencies)),
use the `;;;pallet-ignore` comment. Pallet will ignore any text after
this comment.
```lisp
(source melpa)
(depends-on "s")
;;;pallet-ignore
(depends-on "newlisp" :git
"https://github.com/coldnew/newlisp-mode.git")
```

## Contributing
Contributions to pallet are very welcome.

Fork and clone the repo, then run `git
submodule update --init`, which will install
[el.mk](http://github.com/rdallasgray/el.mk).

### Simple testing
Install [Cask](http://cask.readthedocs.org/en/latest).

Then run `cask install` to install development dependencies. You
should now be able to run the tests: `make test`.

### Complete testing
The pallet dev setup includes a Vagrantfile, which allows pallet to be
tested against a selection of recent Emacs releases.

Having installed [Vagrant](https://vagrantup.com), add the necessary
box by running:
```bash
vagrant box add trusty-server \
https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-i386-vagrant-disk1.box
```

Then run `vagrant up`. This may take a while, as several versions of
Emacs may be downloaded and installed from source.

Shell into the vm by running `vagrant ssh`, and run the tests using
`./test_all.sh`. This will run the complete test suite against all
installed Emacs versions.

### Pull requests
Any new feature or fix should be covered by tests -- see the files
in /test for guidance on how to write your own. When you've
created your feature or fix, make a pull request against master in this repo.
