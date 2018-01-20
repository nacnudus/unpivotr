## Resubmission
This is a resubmission. In this version I explain an apparently spurious invalid
URL seen by CRAN

> Found the following (possibly) invalid URLs:
>   URL: http://archive.stats.govt.nz/browse_for_stats/people_and_communities/Well-being/nzgss-info-releases.aspx
>     From: man/purpose.Rd
>     Status: 503
>     Message: Service Unavailable

The page is available both in browsers and via tools like wget.  Presumably the
503 status was a temporary glitch.

## Test environments

### Local
* Arch Linux 4.14.13-1-ARCH  R-release 3.4.3

### Docker
* x86_64-pc-linux-gnu                R-devel   2018-01-17 r74130
* x86_64-pc-linux-gnu                R-release 3.4.3

### Win-builder
* Windows Server 2008 (64-bit)       R-devel   2018-01-17 r74130

### Travis
* Ubuntu Linux 12.04 LTS x64         R-devel   2018-01-18 r74132
* Ubuntu Linux 12.04 LTS x64         R-release 3.4.2
* Ubuntu Linux 12.04 LTS x64         R-oldrel  3.3.3

### AppVeyor
* Windows Server 2012 R2 (x64) x64   R-release 3.4.3
* Windows Server 2012 R2 (x64) i386  R-stable  3.4.3
* Windows Server 2012 R2 (x64) i386  R-patched 2018-01-16 r74130
* Windows Server 2012 R2 (x64) i386  R-oldrel  3.3.3

### Rhub
* Ubuntu Linux 16.04 LTS GCC         R-release 3.4.2
* Fedora Linux clang gfortran        R-devel   2018-01-13 r74117

## Downstream dependencies
There are currently no downstream dependencies for this package.
