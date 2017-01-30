## Test environments

### Local
* Arch Linux 4.8.13-1-ARCH, R-release

### Win-builder
* Windows Server 2008 (64-bit), R-devel

### Travis
* Ubuntu Linux 12.04 LTS, R-devel
* Ubuntu Linux 12.04 LTS, R-release 3.2.5
* Ubuntu Linux 12.04 LTS, R-oldrel 3.2.0

### AppVeyor
* Windows Server 2012 R2 (x64), R-devel, i386, mingw_32
* Windows Server 2012 R2 (x64), R-devel, x64, mingw_64
* Windows Server 2012 R2 (x64), R-release, x64, GCC
* Windows Server 2012 R2 (x64), R-stable, i386, GCC
* Windows Server 2012 R2 (x64), R-patched, i386, GCC
* Windows Server 2012 R2 (x64), R-oldrel, i386, GCC

### Rhub
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Duncan Garmonsway <nacnudus@gmail.com>’

  New submission

  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  File 'LICENSE':
    YEAR: 2016
    COPYRIGHT HOLDER: Duncan Garmonsway

  Possibly mis-spelled words in DESCRIPTION:
    Unpivot (2:8)

Unpivot is not a misspelling.

## Downstream dependencies
There are currently no downstream dependencies for this package.
