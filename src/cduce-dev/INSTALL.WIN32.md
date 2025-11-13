# Building and installing ℂDuce on Windows

## Prerequisite

The recommended way of building ℂDuce on Windows is to use [fdopen's OCaml for Windows](https://fdopen.github.io/opam-repository-mingw/). 
This procedure has only been tested with Windows 10 on x86_64 architecture. 
Installation notes (you are advised to follow this order):

1. Install the [64 bit version of the graphical installer](https://fdopen.github.io/opam-repository-mingw/installation#graphical-installer/). Leave all the defaults. This will install a proper Cygwin environment with `bash`, `git`, a `mingw64` compiler and
setup an `opam switch` with a recent version of OCaml.

2. (optional) By default, `OCaml for Windows` uses `/home/foo` as the user's home directory while Cygwin sometimes chooses `C:\Users\foo`. If that is your case, a solution can be to edit `/home/foo/.bash_profile` and just add:
   ```
   export HOME=/home/foo
   ```
   where `foo` is your username. At this point, launching the Cygwin terminal and running:
   ```
   $ opam switch
   ```
   should show you a configured switch.
It may also happen that the basic Cygwin `bin` directory is not added to your `$PATH`. If so add `/cygdrive/c/OCaml64/bin` to your `$PATH` in your `.bash_profile`


3. **Important** add the `mingw64` compiler path to your `$PATH`. This way, third-party libraries will be installed using `mingw64` and **not** using their Cygwin version.
   ```
   export PATH="/usr/x86_64-w64-mingw32/sys-root/mingw/bin/:$PATH"
   ```
   you can make this change permanent by putting it in your `.bashrc` or `.bash_profile` configuration file.

4. Install `depext` and `depext-cygwinports`, which can be used to conveniently install third party C libraries.
   ```
   $ opam install depext depext-cygwinports
   ```

5. Install third-party dependencies
   ```
   $ opam depext -i conf-openssl conf-ssl conf-expat conf-curl
   ```
You can now follow the instructions in the INSTALL.md
file to build ℂDuce as you would on a Unix system.

## Installing the binary

If you wish to deploy the `cduce.exe` executable, you
can use the convenience script :
```
  $ tools/mk_win32_zip.sh
```
This script must be run from the Cygwin shell as it requires a POSIX shell and `sed`. Furthermore, either the `zip`
utility or the `powershell.exe` shell should be available on your `$PATH`. This script will bundle `cduce.exe`, `dtd2cduce.exe` and `cduce_mktop.exe` in a zip file named
`cduce-VERSION-x86_64.zip`, together with the required
DLL files and their licenses (each bundled DLL is under an MIT-like license that authorizes its distribution in binary form together with its copyright notice and license).






