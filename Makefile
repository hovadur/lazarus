#
# Don't edit, this file is generated by FPCMake Version 1.1 [2003/03/16]
#
default: all
MAKEFILETARGETS=linux go32v2 win32 os2 freebsd beos netbsd amiga atari sunos qnx netware openbsd wdosx
override PATH:=$(subst \,/,$(PATH))
ifeq ($(findstring ;,$(PATH)),)
inUnix=1
SEARCHPATH:=$(filter-out .,$(subst :, ,$(PATH)))
else
SEARCHPATH:=$(subst ;, ,$(PATH))
endif
PWD:=$(strip $(wildcard $(addsuffix /pwd.exe,$(SEARCHPATH))))
ifeq ($(PWD),)
PWD:=$(strip $(wildcard $(addsuffix /pwd,$(SEARCHPATH))))
ifeq ($(PWD),)
$(error You need the GNU utils package to use this Makefile)
else
PWD:=$(firstword $(PWD))
SRCEXEEXT=
endif
else
PWD:=$(firstword $(PWD))
SRCEXEEXT=.exe
endif
ifndef inUnix
ifeq ($(OS),Windows_NT)
inWinNT=1
else
ifdef OS2_SHELL
inOS2=1
endif
endif
else
ifneq ($(findstring cygwin,$(MACHTYPE)),)
inCygWin=1
endif
endif
ifeq ($(OS_TARGET),freebsd)
BSDhier=1
endif
ifeq ($(OS_TARGET),netbsd)
BSDhier=1
endif
ifeq ($(OS_TARGET),openbsd)
BSDhier=1
endif
ifdef inUnix
BATCHEXT=.sh
else
ifdef inOS2
BATCHEXT=.cmd
else
BATCHEXT=.bat
endif
endif
ifdef inUnix
PATHSEP=/
else
PATHSEP:=$(subst /,\,/)
ifdef inCygWin
PATHSEP=/
endif
endif
ifdef PWD
BASEDIR:=$(subst \,/,$(shell $(PWD)))
ifdef inCygWin
ifneq ($(findstring /cygdrive/,$(BASEDIR)),)
BASENODIR:=$(patsubst /cygdrive%,%,$(BASEDIR))
BASEDRIVE:=$(firstword $(subst /, ,$(BASENODIR)))
BASEDIR:=$(subst /cygdrive/$(BASEDRIVE)/,$(BASEDRIVE):/,$(BASEDIR))
endif
endif
else
BASEDIR=.
endif
ifdef inOS2
ifndef ECHO
ECHO:=$(strip $(wildcard $(addsuffix /gecho$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(ECHO),)
ECHO:=$(strip $(wildcard $(addsuffix /echo$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(ECHO),)
ECHO=echo
else
ECHO:=$(firstword $(ECHO))
endif
else
ECHO:=$(firstword $(ECHO))
endif
endif
export ECHO
endif
ifndef FPC
ifdef PP
FPC=$(PP)
endif
endif
ifndef FPC
FPCPROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH))))
ifneq ($(FPCPROG),)
FPCPROG:=$(firstword $(FPCPROG))
FPC:=$(shell $(FPCPROG) -PB)
ifneq ($(findstring Error,$(FPC)),)
override FPC=ppc386
endif
else
override FPC=ppc386
endif
endif
override FPC:=$(subst $(SRCEXEEXT),,$(FPC))
override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)
ifndef FPC_VERSION
FPC_VERSION:=$(shell $(FPC) -iV)
endif
export FPC FPC_VERSION
unexport CHECKDEPEND ALLDEPENDENCIES
ifeq ($(findstring 1.0.,$(FPC_VERSION)),)
COMPILERINFO:=$(shell $(FPC) -iSP -iTP -iSO -iTO)
ifndef CPU_SOURCE
CPU_SOURCE:=$(word 1,$(COMPILERINFO))
endif
ifndef CPU_TARGET
CPU_TARGET:=$(word 2,$(COMPILERINFO))
endif
ifndef OS_SOURCE
OS_SOURCE:=$(word 3,$(COMPILERINFO))
endif
ifndef OS_TARGET
OS_TARGET:=$(word 4,$(COMPILERINFO))
endif
else
ifndef CPU_SOURCE
CPU_SOURCE:=$(shell $(FPC) -iSP)
endif
ifndef CPU_TARGET
CPU_TARGET:=$(shell $(FPC) -iTP)
endif
ifndef OS_SOURCE
OS_SOURCE:=$(shell $(FPC) -iSO)
endif
ifndef OS_TARGET
OS_TARGET:=$(shell $(FPC) -iTO)
endif
endif
ifndef CPU_TARGET
ifdef CPU_TARGET_DEFAULT
CPU_TARGET=$(CPU_TARGET_DEFAULT)
endif
endif
ifndef OS_TARGET
ifdef OS_TARGET_DEFAULT
OS_TARGET=$(OS_TARGET_DEFAULT)
endif
endif
FULL_TARGET=$(CPU_TARGET)-$(OS_TARGET)
FULL_SOURCE=$(CPU_SOURCE)-$(OS_SOURCE)
ifneq ($(FULL_TARGET),$(FULL_SOURCE))
CROSSCOMPILE=1
endif
ifeq ($(findstring makefile,$(MAKECMDGOALS)),)
ifeq ($(findstring $(OS_TARGET),$(MAKEFILETARGETS)),)
$(error The Makefile doesn't support target $(OS_TARGET), please run fpcmake first)
endif
endif
export OS_TARGET OS_SOURCE CPU_TARGET CPU_SOURCE FULL_TARGET FULL_SOURCE CROSSCOMPILE
ifdef FPCDIR
override FPCDIR:=$(subst \,/,$(FPCDIR))
ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)
override FPCDIR=wrong
endif
else
override FPCDIR=wrong
endif
ifdef DEFAULT_FPCDIR
ifeq ($(FPCDIR),wrong)
override FPCDIR:=$(subst \,/,$(DEFAULT_FPCDIR))
ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)
override FPCDIR=wrong
endif
endif
endif
ifeq ($(FPCDIR),wrong)
ifdef inUnix
override FPCDIR=/usr/local/lib/fpc/$(FPC_VERSION)
ifeq ($(wildcard $(FPCDIR)/units),)
override FPCDIR=/usr/lib/fpc/$(FPC_VERSION)
endif
else
override FPCDIR:=$(subst /$(FPC),,$(firstword $(strip $(wildcard $(addsuffix /$(FPC),$(SEARCHPATH))))))
override FPCDIR:=$(FPCDIR)/..
ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)
override FPCDIR:=$(FPCDIR)/..
ifeq ($(wildcard $(addprefix $(FPCDIR)/,rtl units)),)
override FPCDIR=c:/pp
endif
endif
endif
endif
ifndef CROSSDIR
CROSSDIR:=$(FPCDIR)/cross/$(FULL_TARGET)
endif
ifndef CROSSTARGETDIR
CROSSTARGETDIR=$(CROSSDIR)/$(FULL_TARGET)
endif
ifdef CROSSCOMPILE
UNITSDIR:=$(wildcard $(CROSSTARGETDIR)/units)
ifeq ($(UNITSDIR),)
UNITSDIR:=$(wildcard $(FPCDIR)/units/$(OS_TARGET))
endif
else
UNITSDIR:=$(wildcard $(FPCDIR)/units/$(OS_TARGET))
endif
PACKAGESDIR:=$(wildcard $(FPCDIR) $(FPCDIR)/packages/base $(FPCDIR)/packages/extra)
override PACKAGE_NAME=lazarus/lcl
override PACKAGE_VERSION=0.8a
override TARGET_DIRS+=interfaces
override TARGET_UNITS+=allunits
override TARGET_IMPLICITUNITS+=arrow actnlist buttons calendar clipbrd clistbox comctrls commctrl controls dialogs dynamicarray  dynhasharray extctrls extendedstrings filectrl forms graphics graphmath graphtype grids imglist interfacebase lazqueue lclmemmanager lcllinux lclstrconsts lcltype lmessages lresources menus messages registry spin stdctrls stringhashlist toolwin utrace vclglobals printers postscriptprinter
override TARGET_RSTS+=dialogs
override CLEAN_FILES+=$(wildcard units/*$(OEXT)) $(wildcard units/*$(PPUEXT)) $(wildcard units/*$(RSTEXT))$(wildcard *$(OEXT)) $(wildcard *$(PPUEXT)) $(wildcard *$(RSTEXT))
override INSTALL_BUILDUNIT=allunits
override COMPILER_OPTIONS+=-gl
override COMPILER_INCLUDEDIR+=include
override COMPILER_UNITDIR+=interfaces/abstract .
override COMPILER_UNITTARGETDIR+=units
ifdef REQUIRE_UNITSDIR
override UNITSDIR+=$(REQUIRE_UNITSDIR)
endif
ifdef REQUIRE_PACKAGESDIR
override PACKAGESDIR+=$(REQUIRE_PACKAGESDIR)
endif
ifdef ZIPINSTALL
ifeq ($(OS_TARGET),linux)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_TARGET),freebsd)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_TARGET),netbsd)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_TARGET),openbsd)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_TARGET),sunos)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_TARGET),qnx)
UNIXINSTALLDIR=1
endif
else
ifeq ($(OS_SOURCE),linux)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_SOURCE),freebsd)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_SOURCE),netbsd)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_SOURCE),openbsd)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_TARGET),sunos)
UNIXINSTALLDIR=1
endif
ifeq ($(OS_TARGET),qnx)
UNIXINSTALLDIR=1
endif
endif
ifndef INSTALL_PREFIX
ifdef PREFIX
INSTALL_PREFIX=$(PREFIX)
endif
endif
ifndef INSTALL_PREFIX
ifdef UNIXINSTALLDIR
INSTALL_PREFIX=/usr/local
else
ifdef INSTALL_FPCPACKAGE
INSTALL_BASEDIR:=/pp
else
INSTALL_BASEDIR:=/$(PACKAGE_NAME)
endif
endif
endif
export INSTALL_PREFIX
ifdef INSTALL_FPCSUBDIR
export INSTALL_FPCSUBDIR
endif
ifndef DIST_DESTDIR
DIST_DESTDIR:=$(BASEDIR)
endif
export DIST_DESTDIR
ifndef INSTALL_BASEDIR
ifdef UNIXINSTALLDIR
ifdef INSTALL_FPCPACKAGE
INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/fpc/$(FPC_VERSION)
else
INSTALL_BASEDIR:=$(INSTALL_PREFIX)/lib/$(PACKAGE_NAME)
endif
else
INSTALL_BASEDIR:=$(INSTALL_PREFIX)
endif
endif
ifndef INSTALL_BINDIR
ifdef UNIXINSTALLDIR
INSTALL_BINDIR:=$(INSTALL_PREFIX)/bin
else
INSTALL_BINDIR:=$(INSTALL_BASEDIR)/bin
ifdef INSTALL_FPCPACKAGE
INSTALL_BINDIR:=$(INSTALL_BINDIR)/$(OS_TARGET)
endif
endif
endif
ifndef INSTALL_UNITDIR
ifdef CROSSCOMPILE
INSTALL_UNITDIR:=$(INSTALL_BASEDIR)/cross/$(FULL_TARGET)/units
else
INSTALL_UNITDIR:=$(INSTALL_BASEDIR)/units/$(OS_TARGET)
endif
ifdef INSTALL_FPCPACKAGE
ifdef PACKAGE_NAME
INSTALL_UNITDIR:=$(INSTALL_UNITDIR)/$(PACKAGE_NAME)
endif
endif
endif
ifndef INSTALL_LIBDIR
ifdef UNIXINSTALLDIR
INSTALL_LIBDIR:=$(INSTALL_PREFIX)/lib
else
INSTALL_LIBDIR:=$(INSTALL_UNITDIR)
endif
endif
ifndef INSTALL_SOURCEDIR
ifdef UNIXINSTALLDIR
ifdef BSDhier
SRCPREFIXDIR=share/src
else
SRCPREFIXDIR=src
endif
ifdef INSTALL_FPCPACKAGE
ifdef INSTALL_FPCSUBDIR
INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSION)/$(INSTALL_FPCSUBDIR)/$(PACKAGE_NAME)
else
INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/fpc-$(FPC_VERSION)/$(PACKAGE_NAME)
endif
else
INSTALL_SOURCEDIR:=$(INSTALL_PREFIX)/$(SRCPREFIXDIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
endif
else
ifdef INSTALL_FPCPACKAGE
ifdef INSTALL_FPCSUBDIR
INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(INSTALL_FPCSUBDIR)/$(PACKAGE_NAME)
else
INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source/$(PACKAGE_NAME)
endif
else
INSTALL_SOURCEDIR:=$(INSTALL_BASEDIR)/source
endif
endif
endif
ifndef INSTALL_DOCDIR
ifdef UNIXINSTALLDIR
ifdef BSDhier
DOCPREFIXDIR=share/doc
else
DOCPREFIXDIR=doc
endif
ifdef INSTALL_FPCPACKAGE
INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/fpc-$(FPC_VERSION)/$(PACKAGE_NAME)
else
INSTALL_DOCDIR:=$(INSTALL_PREFIX)/$(DOCPREFIXDIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
endif
else
ifdef INSTALL_FPCPACKAGE
INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc/$(PACKAGE_NAME)
else
INSTALL_DOCDIR:=$(INSTALL_BASEDIR)/doc
endif
endif
endif
ifndef INSTALL_EXAMPLEDIR
ifdef UNIXINSTALLDIR
ifdef INSTALL_FPCPACKAGE
ifdef BSDhier
INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/examples/fpc-$(FPC_VERSION)/$(PACKAGE_NAME)
else
INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/fpc-$(FPC_VERSION)/examples/$(PACKAGE_NAME)
endif
else
ifdef BSDhier
INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/share/examples/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
else
INSTALL_EXAMPLEDIR:=$(INSTALL_PREFIX)/doc/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
endif
endif
else
ifdef INSTALL_FPCPACKAGE
INSTALL_EXAMPLEDIR:=$(INSTALL_BASEDIR)/examples/$(PACKAGE_NAME)
else
INSTALL_EXAMPLEDIR:=$(INSTALL_BASEDIR)/examples
endif
endif
endif
ifndef INSTALL_DATADIR
INSTALL_DATADIR=$(INSTALL_BASEDIR)
endif
ifdef CROSSCOMPILE
ifndef CROSSBINDIR
CROSSBINDIR:=$(wildcard $(CROSSTARGETDIR)/bin/$(FULL_SOURCE))
ifeq ($(CROSSBINDIR),)
CROSSBINDIR:=$(wildcard $(INSTALL_BASEDIR)/cross/$(FULL_TARGET)/bin/$(FULL_SOURCE))
endif
endif
else
CROSSBINDIR=
endif
ifdef inUnix
ifndef GCCLIBDIR
GCCLIBDIR:=$(shell dirname `(gcc -v 2>&1)| head -n 1| awk '{ print $$4 } '`)
endif
ifeq ($(OS_TARGET),linux)
ifndef OTHERLIBDIR
OTHERLIBDIR:=$(shell grep -v "^\#" /etc/ld.so.conf | awk '{ ORS=" "; print $1 }')
endif
endif
ifeq ($(OS_TARGET),netbsd)
OTHERLIBDIR+=/usr/pkg/lib
endif
export GCCLIBDIR OTHERLIB
endif
LOADEREXT=.as
EXEEXT=.exe
PPLEXT=.ppl
PPUEXT=.ppu
OEXT=.o
ASMEXT=.s
SMARTEXT=.sl
STATICLIBEXT=.a
SHAREDLIBEXT=.so
STATICLIBPREFIX=libp
RSTEXT=.rst
FPCMADE=fpcmade
ifeq ($(OS_TARGET),go32v1)
PPUEXT=.pp1
OEXT=.o1
ASMEXT=.s1
SMARTEXT=.sl1
STATICLIBEXT=.a1
SHAREDLIBEXT=.so1
STATICLIBPREFIX=
FPCMADE=fpcmade.v1
PACKAGESUFFIX=v1
endif
ifeq ($(OS_TARGET),go32v2)
STATICLIBPREFIX=
FPCMADE=fpcmade.dos
ZIPSUFFIX=go32
endif
ifeq ($(OS_TARGET),linux)
EXEEXT=
HASSHAREDLIB=1
FPCMADE=fpcmade.lnx
ZIPSUFFIX=linux
endif
ifeq ($(OS_TARGET),freebsd)
EXEEXT=
HASSHAREDLIB=1
FPCMADE=fpcmade.freebsd
ZIPSUFFIX=freebsd
endif
ifeq ($(OS_TARGET),netbsd)
EXEEXT=
HASSHAREDLIB=1
FPCMADE=fpcmade.netbsd
ZIPSUFFIX=netbsd
endif
ifeq ($(OS_TARGET),openbsd)
EXEEXT=
HASSHAREDLIB=1
FPCMADE=fpcmade.openbsd
ZIPSUFFIX=openbsd
endif
ifeq ($(OS_TARGET),win32)
PPUEXT=.ppw
OEXT=.ow
ASMEXT=.sw
SMARTEXT=.slw
STATICLIBEXT=.aw
SHAREDLIBEXT=.dll
FPCMADE=fpcmade.w32
ZIPSUFFIX=w32
endif
ifeq ($(OS_TARGET),os2)
PPUEXT=.ppo
ASMEXT=.so2
OEXT=.oo2
AOUTEXT=.out
SMARTEXT=.sl2
STATICLIBPREFIX=
STATICLIBEXT=.ao2
SHAREDLIBEXT=.dll
FPCMADE=fpcmade.os2
ZIPSUFFIX=emx
ECHO=echo
endif
ifeq ($(OS_TARGET),amiga)
EXEEXT=
PPUEXT=.ppu
ASMEXT=.asm
OEXT=.o
SMARTEXT=.sl
STATICLIBEXT=.a
SHAREDLIBEXT=.library
FPCMADE=fpcmade.amg
endif
ifeq ($(OS_TARGET),atari)
PPUEXT=.ppu
ASMEXT=.s
OEXT=.o
SMARTEXT=.sl
STATICLIBEXT=.a
EXEEXT=.ttp
FPCMADE=fpcmade.ata
endif
ifeq ($(OS_TARGET),beos)
PPUEXT=.ppu
ASMEXT=.s
OEXT=.o
SMARTEXT=.sl
STATICLIBEXT=.a
EXEEXT=
FPCMADE=fpcmade.be
ZIPSUFFIX=be
endif
ifeq ($(OS_TARGET),sunos)
PPUEXT=.ppu
ASMEXT=.s
OEXT=.o
SMARTEXT=.sl
STATICLIBEXT=.a
EXEEXT=
FPCMADE=fpcmade.sun
ZIPSUFFIX=sun
endif
ifeq ($(OS_TARGET),qnx)
PPUEXT=.ppu
ASMEXT=.s
OEXT=.o
SMARTEXT=.sl
STATICLIBEXT=.a
EXEEXT=
FPCMADE=fpcmade.qnx
ZIPSUFFIX=qnx
endif
ifeq ($(OS_TARGET),netware)
STATICLIBPREFIX=
PPUEXT=.ppn
OEXT=.on
ASMEXT=.s
SMARTEXT=.sl
STATICLIBEXT=.a
SHAREDLIBEXT=.nlm
FPCMADE=fpcmade.nw
ZIPSUFFIX=nw
EXEEXT=.nlm
endif
ifndef ECHO
ECHO:=$(strip $(wildcard $(addsuffix /gecho$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(ECHO),)
ECHO:=$(strip $(wildcard $(addsuffix /echo$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(ECHO),)
ECHO=
else
ECHO:=$(firstword $(ECHO))
endif
else
ECHO:=$(firstword $(ECHO))
endif
endif
export ECHO
ifndef DATE
DATE:=$(strip $(wildcard $(addsuffix /gdate$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(DATE),)
DATE:=$(strip $(wildcard $(addsuffix /date$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(DATE),)
DATE=
else
DATE:=$(firstword $(DATE))
endif
else
DATE:=$(firstword $(DATE))
endif
endif
export DATE
ifndef GINSTALL
GINSTALL:=$(strip $(wildcard $(addsuffix /ginstall$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(GINSTALL),)
GINSTALL:=$(strip $(wildcard $(addsuffix /install$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(GINSTALL),)
GINSTALL=
else
GINSTALL:=$(firstword $(GINSTALL))
endif
else
GINSTALL:=$(firstword $(GINSTALL))
endif
endif
export GINSTALL
ifndef CPPROG
CPPROG:=$(strip $(wildcard $(addsuffix /cp$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(CPPROG),)
CPPROG=
else
CPPROG:=$(firstword $(CPPROG))
endif
endif
export CPPROG
ifndef RMPROG
RMPROG:=$(strip $(wildcard $(addsuffix /rm$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(RMPROG),)
RMPROG=
else
RMPROG:=$(firstword $(RMPROG))
endif
endif
export RMPROG
ifndef MVPROG
MVPROG:=$(strip $(wildcard $(addsuffix /mv$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(MVPROG),)
MVPROG=
else
MVPROG:=$(firstword $(MVPROG))
endif
endif
export MVPROG
ifndef ECHOREDIR
ECHOREDIR:=$(subst /,$(PATHSEP),$(ECHO))
endif
ifndef COPY
COPY:=$(CPPROG) -fp
endif
ifndef COPYTREE
COPYTREE:=$(CPPROG) -rfp
endif
ifndef MOVE
MOVE:=$(MVPROG) -f
endif
ifndef DEL
DEL:=$(RMPROG) -f
endif
ifndef DELTREE
DELTREE:=$(RMPROG) -rf
endif
ifndef INSTALL
ifdef inUnix
INSTALL:=$(GINSTALL) -c -m 644
else
INSTALL:=$(COPY)
endif
endif
ifndef INSTALLEXE
ifdef inUnix
INSTALLEXE:=$(GINSTALL) -c -m 755
else
INSTALLEXE:=$(COPY)
endif
endif
ifndef MKDIR
MKDIR:=$(GINSTALL) -m 755 -d
endif
export ECHOREDIR COPY COPYTREE MOVE DEL DELTREE INSTALL INSTALLEXE MKDIR
ifndef PPUMOVE
PPUMOVE:=$(strip $(wildcard $(addsuffix /ppumove$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(PPUMOVE),)
PPUMOVE=
else
PPUMOVE:=$(firstword $(PPUMOVE))
endif
endif
export PPUMOVE
ifndef FPCMAKE
FPCMAKE:=$(strip $(wildcard $(addsuffix /fpcmake$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(FPCMAKE),)
FPCMAKE=
else
FPCMAKE:=$(firstword $(FPCMAKE))
endif
endif
export FPCMAKE
ifndef ZIPPROG
ZIPPROG:=$(strip $(wildcard $(addsuffix /zip$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(ZIPPROG),)
ZIPPROG=
else
ZIPPROG:=$(firstword $(ZIPPROG))
endif
endif
export ZIPPROG
ifndef TARPROG
TARPROG:=$(strip $(wildcard $(addsuffix /tar$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(TARPROG),)
TARPROG=
else
TARPROG:=$(firstword $(TARPROG))
endif
endif
export TARPROG
ASNAME=as
LDNAME=ld
ARNAME=ar
RCNAME=rc
ifeq ($(OS_TARGET),win32)
ASNAME=asw
LDNAME=ldw
ARNAME=arw
endif
ifndef ASPROG
ifdef CROSSBINDIR
ASPROG=$(CROSSBINDIR)/$(ASNAME)$(SRCEXEEXT)
else
ASPROG=$(ASNAME)
endif
endif
ifndef LDPROG
ifdef CROSSBINDIR
LDPROG=$(CROSSBINDIR)/$(LDNAME)$(SRCEXEEXT)
else
LDPROG=$(LDNAME)
endif
endif
ifndef RCPROG
ifdef CROSSBINDIR
RCPROG=$(CROSSBINDIR)/$(RCNAME)$(SRCEXEEXT)
else
RCPROG=$(RCNAME)
endif
endif
ifndef ARPROG
ifdef CROSSBINDIR
ARPROG=$(CROSSBINDIR)/$(ARNAME)$(SRCEXEEXT)
else
ARPROG=$(ARNAME)
endif
endif
AS=$(ASPROG)
LD=$(LDPROG)
RC=$(RCPROG)
AR=$(ARPROG)
PPAS=ppas$(BATCHEXT)
ifdef inUnix
LDCONFIG=ldconfig
else
LDCONFIG=
endif
ifdef DATE
DATESTR:=$(shell $(DATE) +%Y%m%d)
else
DATESTR=
endif
ifndef UPXPROG
ifeq ($(OS_TARGET),go32v2)
UPXPROG:=1
endif
ifeq ($(OS_TARGET),win32)
UPXPROG:=1
endif
ifdef UPXPROG
UPXPROG:=$(strip $(wildcard $(addsuffix /upx$(SRCEXEEXT),$(SEARCHPATH))))
ifeq ($(UPXPROG),)
UPXPROG=
else
UPXPROG:=$(firstword $(UPXPROG))
endif
else
UPXPROG=
endif
endif
export UPXPROG
ZIPOPT=-9
ZIPEXT=.zip
ifeq ($(USETAR),bz2)
TAROPT=vI
TAREXT=.tar.bz2
else
TAROPT=vz
TAREXT=.tar.gz
endif
override REQUIRE_PACKAGES=rtl fcl
ifeq ($(OS_TARGET),linux)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_NETDB=1
REQUIRE_PACKAGES_FCL=1
REQUIRE_PACKAGES_MYSQL=1
REQUIRE_PACKAGES_IBASE=1
endif
ifeq ($(OS_TARGET),go32v2)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifeq ($(OS_TARGET),win32)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_NETDB=1
REQUIRE_PACKAGES_FCL=1
REQUIRE_PACKAGES_MYSQL=1
REQUIRE_PACKAGES_IBASE=1
endif
ifeq ($(OS_TARGET),os2)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifeq ($(OS_TARGET),freebsd)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_NETDB=1
REQUIRE_PACKAGES_FCL=1
REQUIRE_PACKAGES_MYSQL=1
REQUIRE_PACKAGES_IBASE=1
endif
ifeq ($(OS_TARGET),beos)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifeq ($(OS_TARGET),netbsd)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_NETDB=1
REQUIRE_PACKAGES_FCL=1
REQUIRE_PACKAGES_MYSQL=1
REQUIRE_PACKAGES_IBASE=1
endif
ifeq ($(OS_TARGET),amiga)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifeq ($(OS_TARGET),atari)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifeq ($(OS_TARGET),sunos)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifeq ($(OS_TARGET),qnx)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifeq ($(OS_TARGET),netware)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifeq ($(OS_TARGET),openbsd)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_NETDB=1
REQUIRE_PACKAGES_FCL=1
REQUIRE_PACKAGES_MYSQL=1
REQUIRE_PACKAGES_IBASE=1
endif
ifeq ($(OS_TARGET),wdosx)
REQUIRE_PACKAGES_RTL=1
REQUIRE_PACKAGES_PASZLIB=1
REQUIRE_PACKAGES_FCL=1
endif
ifdef REQUIRE_PACKAGES_RTL
PACKAGEDIR_RTL:=$(firstword $(subst /Makefile.fpc,,$(strip $(wildcard $(addsuffix /rtl/$(OS_TARGET)/Makefile.fpc,$(PACKAGESDIR))))))
ifneq ($(PACKAGEDIR_RTL),)
ifneq ($(wildcard $(PACKAGEDIR_RTL)/$(OS_TARGET)),)
UNITDIR_RTL=$(PACKAGEDIR_RTL)/$(OS_TARGET)
else
UNITDIR_RTL=$(PACKAGEDIR_RTL)
endif
ifdef CHECKDEPEND
$(PACKAGEDIR_RTL)/$(FPCMADE):
	$(MAKE) -C $(PACKAGEDIR_RTL) $(FPCMADE)
override ALLDEPENDENCIES+=$(PACKAGEDIR_RTL)/$(FPCMADE)
endif
else
PACKAGEDIR_RTL=
UNITDIR_RTL:=$(subst /Package.fpc,,$(strip $(wildcard $(addsuffix /rtl/Package.fpc,$(UNITSDIR)))))
ifneq ($(UNITDIR_RTL),)
UNITDIR_RTL:=$(firstword $(UNITDIR_RTL))
else
UNITDIR_RTL=
endif
endif
ifdef UNITDIR_RTL
override COMPILER_UNITDIR+=$(UNITDIR_RTL)
endif
endif
ifdef REQUIRE_PACKAGES_PASZLIB
PACKAGEDIR_PASZLIB:=$(firstword $(subst /Makefile.fpc,,$(strip $(wildcard $(addsuffix /paszlib/Makefile.fpc,$(PACKAGESDIR))))))
ifneq ($(PACKAGEDIR_PASZLIB),)
ifneq ($(wildcard $(PACKAGEDIR_PASZLIB)/$(OS_TARGET)),)
UNITDIR_PASZLIB=$(PACKAGEDIR_PASZLIB)/$(OS_TARGET)
else
UNITDIR_PASZLIB=$(PACKAGEDIR_PASZLIB)
endif
ifdef CHECKDEPEND
$(PACKAGEDIR_PASZLIB)/$(FPCMADE):
	$(MAKE) -C $(PACKAGEDIR_PASZLIB) $(FPCMADE)
override ALLDEPENDENCIES+=$(PACKAGEDIR_PASZLIB)/$(FPCMADE)
endif
else
PACKAGEDIR_PASZLIB=
UNITDIR_PASZLIB:=$(subst /Package.fpc,,$(strip $(wildcard $(addsuffix /paszlib/Package.fpc,$(UNITSDIR)))))
ifneq ($(UNITDIR_PASZLIB),)
UNITDIR_PASZLIB:=$(firstword $(UNITDIR_PASZLIB))
else
UNITDIR_PASZLIB=
endif
endif
ifdef UNITDIR_PASZLIB
override COMPILER_UNITDIR+=$(UNITDIR_PASZLIB)
endif
endif
ifdef REQUIRE_PACKAGES_NETDB
PACKAGEDIR_NETDB:=$(firstword $(subst /Makefile.fpc,,$(strip $(wildcard $(addsuffix /netdb/Makefile.fpc,$(PACKAGESDIR))))))
ifneq ($(PACKAGEDIR_NETDB),)
ifneq ($(wildcard $(PACKAGEDIR_NETDB)/$(OS_TARGET)),)
UNITDIR_NETDB=$(PACKAGEDIR_NETDB)/$(OS_TARGET)
else
UNITDIR_NETDB=$(PACKAGEDIR_NETDB)
endif
ifdef CHECKDEPEND
$(PACKAGEDIR_NETDB)/$(FPCMADE):
	$(MAKE) -C $(PACKAGEDIR_NETDB) $(FPCMADE)
override ALLDEPENDENCIES+=$(PACKAGEDIR_NETDB)/$(FPCMADE)
endif
else
PACKAGEDIR_NETDB=
UNITDIR_NETDB:=$(subst /Package.fpc,,$(strip $(wildcard $(addsuffix /netdb/Package.fpc,$(UNITSDIR)))))
ifneq ($(UNITDIR_NETDB),)
UNITDIR_NETDB:=$(firstword $(UNITDIR_NETDB))
else
UNITDIR_NETDB=
endif
endif
ifdef UNITDIR_NETDB
override COMPILER_UNITDIR+=$(UNITDIR_NETDB)
endif
endif
ifdef REQUIRE_PACKAGES_FCL
PACKAGEDIR_FCL:=$(firstword $(subst /Makefile.fpc,,$(strip $(wildcard $(addsuffix /fcl/Makefile.fpc,$(PACKAGESDIR))))))
ifneq ($(PACKAGEDIR_FCL),)
ifneq ($(wildcard $(PACKAGEDIR_FCL)/$(OS_TARGET)),)
UNITDIR_FCL=$(PACKAGEDIR_FCL)/$(OS_TARGET)
else
UNITDIR_FCL=$(PACKAGEDIR_FCL)
endif
ifdef CHECKDEPEND
$(PACKAGEDIR_FCL)/$(FPCMADE):
	$(MAKE) -C $(PACKAGEDIR_FCL) $(FPCMADE)
override ALLDEPENDENCIES+=$(PACKAGEDIR_FCL)/$(FPCMADE)
endif
else
PACKAGEDIR_FCL=
UNITDIR_FCL:=$(subst /Package.fpc,,$(strip $(wildcard $(addsuffix /fcl/Package.fpc,$(UNITSDIR)))))
ifneq ($(UNITDIR_FCL),)
UNITDIR_FCL:=$(firstword $(UNITDIR_FCL))
else
UNITDIR_FCL=
endif
endif
ifdef UNITDIR_FCL
override COMPILER_UNITDIR+=$(UNITDIR_FCL)
endif
endif
ifdef REQUIRE_PACKAGES_MYSQL
PACKAGEDIR_MYSQL:=$(firstword $(subst /Makefile.fpc,,$(strip $(wildcard $(addsuffix /mysql/Makefile.fpc,$(PACKAGESDIR))))))
ifneq ($(PACKAGEDIR_MYSQL),)
ifneq ($(wildcard $(PACKAGEDIR_MYSQL)/$(OS_TARGET)),)
UNITDIR_MYSQL=$(PACKAGEDIR_MYSQL)/$(OS_TARGET)
else
UNITDIR_MYSQL=$(PACKAGEDIR_MYSQL)
endif
ifdef CHECKDEPEND
$(PACKAGEDIR_MYSQL)/$(FPCMADE):
	$(MAKE) -C $(PACKAGEDIR_MYSQL) $(FPCMADE)
override ALLDEPENDENCIES+=$(PACKAGEDIR_MYSQL)/$(FPCMADE)
endif
else
PACKAGEDIR_MYSQL=
UNITDIR_MYSQL:=$(subst /Package.fpc,,$(strip $(wildcard $(addsuffix /mysql/Package.fpc,$(UNITSDIR)))))
ifneq ($(UNITDIR_MYSQL),)
UNITDIR_MYSQL:=$(firstword $(UNITDIR_MYSQL))
else
UNITDIR_MYSQL=
endif
endif
ifdef UNITDIR_MYSQL
override COMPILER_UNITDIR+=$(UNITDIR_MYSQL)
endif
endif
ifdef REQUIRE_PACKAGES_IBASE
PACKAGEDIR_IBASE:=$(firstword $(subst /Makefile.fpc,,$(strip $(wildcard $(addsuffix /ibase/Makefile.fpc,$(PACKAGESDIR))))))
ifneq ($(PACKAGEDIR_IBASE),)
ifneq ($(wildcard $(PACKAGEDIR_IBASE)/$(OS_TARGET)),)
UNITDIR_IBASE=$(PACKAGEDIR_IBASE)/$(OS_TARGET)
else
UNITDIR_IBASE=$(PACKAGEDIR_IBASE)
endif
ifdef CHECKDEPEND
$(PACKAGEDIR_IBASE)/$(FPCMADE):
	$(MAKE) -C $(PACKAGEDIR_IBASE) $(FPCMADE)
override ALLDEPENDENCIES+=$(PACKAGEDIR_IBASE)/$(FPCMADE)
endif
else
PACKAGEDIR_IBASE=
UNITDIR_IBASE:=$(subst /Package.fpc,,$(strip $(wildcard $(addsuffix /ibase/Package.fpc,$(UNITSDIR)))))
ifneq ($(UNITDIR_IBASE),)
UNITDIR_IBASE:=$(firstword $(UNITDIR_IBASE))
else
UNITDIR_IBASE=
endif
endif
ifdef UNITDIR_IBASE
override COMPILER_UNITDIR+=$(UNITDIR_IBASE)
endif
endif
ifndef NOCPUDEF
override FPCOPTDEF=$(CPU_TARGET)
endif
ifneq ($(OS_TARGET),$(OS_SOURCE))
override FPCOPT+=-T$(OS_TARGET)
endif
ifeq ($(OS_SOURCE),openbsd)
override FPCOPT+=-FD$(NEW_BINUTILS_PATH)
endif
ifdef UNITDIR
override FPCOPT+=$(addprefix -Fu,$(UNITDIR))
endif
ifdef LIBDIR
override FPCOPT+=$(addprefix -Fl,$(LIBDIR))
endif
ifdef OBJDIR
override FPCOPT+=$(addprefix -Fo,$(OBJDIR))
endif
ifdef INCDIR
override FPCOPT+=$(addprefix -Fi,$(INCDIR))
endif
ifdef LINKSMART
override FPCOPT+=-XX
endif
ifdef CREATESMART
override FPCOPT+=-CX
endif
ifdef DEBUG
override FPCOPT+=-gl
override FPCOPTDEF+=DEBUG
endif
ifdef RELEASE
ifeq ($(CPU_TARGET),i386)
FPCCPUOPT:=-OG2p3
else
FPCCPUOPT:=
endif
override FPCOPT+=-Xs $(FPCCPUOPT) -n
override FPCOPTDEF+=RELEASE
endif
ifdef STRIP
override FPCOPT+=-Xs
endif
ifdef OPTIMIZE
ifeq ($(CPU_TARGET),i386)
override FPCOPT+=-OG2p3
endif
endif
ifdef VERBOSE
override FPCOPT+=-vwni
endif
ifdef COMPILER_OPTIONS
override FPCOPT+=$(COMPILER_OPTIONS)
endif
ifdef COMPILER_UNITDIR
override FPCOPT+=$(addprefix -Fu,$(COMPILER_UNITDIR))
endif
ifdef COMPILER_LIBRARYDIR
override FPCOPT+=$(addprefix -Fl,$(COMPILER_LIBRARYDIR))
endif
ifdef COMPILER_OBJECTDIR
override FPCOPT+=$(addprefix -Fo,$(COMPILER_OBJECTDIR))
endif
ifdef COMPILER_INCLUDEDIR
override FPCOPT+=$(addprefix -Fi,$(COMPILER_INCLUDEDIR))
endif
ifdef CROSSBINDIR
override FPCOPT+=-FD$(CROSSBINDIR)
endif
ifdef COMPILER_TARGETDIR
override FPCOPT+=-FE$(COMPILER_TARGETDIR)
ifeq ($(COMPILER_TARGETDIR),.)
override TARGETDIRPREFIX=
else
override TARGETDIRPREFIX=$(COMPILER_TARGETDIR)/
endif
endif
ifdef COMPILER_UNITTARGETDIR
override FPCOPT+=-FU$(COMPILER_UNITTARGETDIR)
ifeq ($(COMPILER_UNITTARGETDIR),.)
override UNITTARGETDIRPREFIX=
else
override UNITTARGETDIRPREFIX=$(COMPILER_UNITTARGETDIR)/
endif
else
ifdef COMPILER_TARGETDIR
override COMPILER_UNITTARGETDIR=$(COMPILER_TARGETDIR)
override UNITTARGETDIRPREFIX=$(TARGETDIRPREFIX)
endif
endif
ifdef GCCLIBDIR
override FPCOPT+=-Fl$(GCCLIBDIR)
endif
ifdef OTHERLIBDIR
override FPCOPT+=$(addprefix -Fl,$(OTHERLIBDIR))
endif
ifdef OPT
override FPCOPT+=$(OPT)
endif
ifdef FPCOPTDEF
override FPCOPT+=$(addprefix -d,$(FPCOPTDEF))
endif
ifdef CFGFILE
override FPCOPT+=@$(CFGFILE)
endif
ifdef USEENV
override FPCEXTCMD:=$(FPCOPT)
override FPCOPT:=!FPCEXTCMD
export FPCEXTCMD
endif
override COMPILER:=$(FPC) $(FPCOPT)
ifeq (,$(findstring -s ,$(COMPILER)))
EXECPPAS=
else
ifeq ($(OS_SOURCE),$(OS_TARGET))
EXECPPAS:=@$(PPAS)
endif
endif
.PHONY: fpc_units
ifdef TARGET_UNITS
override ALLTARGET+=fpc_units
override UNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_UNITS))
override IMPLICITUNITPPUFILES=$(addsuffix $(PPUEXT),$(TARGET_IMPLICITUNITS))
override INSTALLPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)
override CLEANPPUFILES+=$(UNITPPUFILES) $(IMPLICITUNITPPUFILES)
endif
fpc_units: $(UNITPPUFILES)
ifdef TARGET_RSTS
override RSTFILES=$(addsuffix $(RSTEXT),$(TARGET_RSTS))
override CLEANRSTFILES+=$(RSTFILES)
endif
.PHONY: fpc_all fpc_smart fpc_debug fpc_release
$(FPCMADE): $(ALLDEPENDENCIES) $(ALLTARGET)
	@$(ECHOREDIR) Compiled > $(FPCMADE)
fpc_all: $(FPCMADE)
fpc_smart:
	$(MAKE) all LINKSMART=1 CREATESMART=1
fpc_debug:
	$(MAKE) all DEBUG=1
fpc_release:
	$(MAKE) all RELEASE=1
.SUFFIXES: $(EXEEXT) $(PPUEXT) $(OEXT) .pas .pp .rc .res
%$(PPUEXT): %.pp
	$(COMPILER) $<
	$(EXECPPAS)
%$(PPUEXT): %.pas
	$(COMPILER) $<
	$(EXECPPAS)
%$(EXEEXT): %.pp
	$(COMPILER) $<
	$(EXECPPAS)
%$(EXEEXT): %.pas
	$(COMPILER) $<
	$(EXECPPAS)
%.res: %.rc
	windres -i $< -o $@
vpath %.pp $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)
vpath %.pas $(COMPILER_SOURCEDIR) $(COMPILER_INCLUDEDIR)
vpath %$(PPUEXT) $(COMPILER_UNITTARGETDIR)
.PHONY: fpc_install fpc_sourceinstall fpc_exampleinstall
ifdef INSTALL_UNITS
override INSTALLPPUFILES+=$(addsuffix $(PPUEXT),$(INSTALL_UNITS))
endif
ifdef INSTALL_BUILDUNIT
override INSTALLPPUFILES:=$(filter-out $(INSTALL_BUILDUNIT)$(PPUEXT),$(INSTALLPPUFILES))
endif
ifdef INSTALLPPUFILES
override INSTALLPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(INSTALLPPUFILES)) $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(INSTALLPPUFILES)))
override INSTALLPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(INSTALLPPUFILES))
override INSTALLPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPREFIX),$(INSTALLPPULINKFILES)))
override INSTALL_CREATEPACKAGEFPC=1
endif
ifdef INSTALLEXEFILES
override INSTALLEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(INSTALLEXEFILES))
endif
fpc_install: all $(INSTALLTARGET)
ifdef INSTALLEXEFILES
	$(MKDIR) $(INSTALL_BINDIR)
ifdef UPXPROG
	-$(UPXPROG) $(INSTALLEXEFILES)
endif
	$(INSTALLEXE) $(INSTALLEXEFILES) $(INSTALL_BINDIR)
endif
ifdef INSTALL_CREATEPACKAGEFPC
ifdef FPCMAKE
ifdef PACKAGE_VERSION
ifneq ($(wildcard Makefile.fpc),)
	$(FPCMAKE) -p -T$(OS_TARGET) Makefile.fpc
	$(MKDIR) $(INSTALL_UNITDIR)
	$(INSTALL) Package.fpc $(INSTALL_UNITDIR)
endif
endif
endif
endif
ifdef INSTALLPPUFILES
	$(MKDIR) $(INSTALL_UNITDIR)
	$(INSTALL) $(INSTALLPPUFILES) $(INSTALL_UNITDIR)
ifneq ($(INSTALLPPULINKFILES),)
	$(INSTALL) $(INSTALLPPULINKFILES) $(INSTALL_UNITDIR)
endif
ifneq ($(wildcard $(LIB_FULLNAME)),)
	$(MKDIR) $(INSTALL_LIBDIR)
	$(INSTALL) $(LIB_FULLNAME) $(INSTALL_LIBDIR)
ifdef inUnix
	ln -sf $(LIB_FULLNAME) $(INSTALL_LIBDIR)/$(LIB_NAME)
endif
endif
endif
ifdef INSTALL_FILES
	$(MKDIR) $(INSTALL_DATADIR)
	$(INSTALL) $(INSTALL_FILES) $(INSTALL_DATADIR)
endif
fpc_sourceinstall: distclean
	$(MKDIR) $(INSTALL_SOURCEDIR)
	$(COPYTREE) $(BASEDIR)/* $(INSTALL_SOURCEDIR)
fpc_exampleinstall: $(addsuffix _distclean,$(TARGET_EXAMPLEDIRS))
ifdef HASEXAMPLES
	$(MKDIR) $(INSTALL_EXAMPLEDIR)
endif
ifdef EXAMPLESOURCEFILES
	$(COPY) $(EXAMPLESOURCEFILES) $(INSTALL_EXAMPLEDIR)
endif
ifdef TARGET_EXAMPLEDIRS
	$(COPYTREE) $(addsuffix /*,$(TARGET_EXAMPLEDIRS)) $(INSTALL_EXAMPLEDIR)
endif
.PHONY: fpc_distinstall
fpc_distinstall: install exampleinstall
.PHONY: fpc_zipinstall fpc_zipsourceinstall fpc_zipexampleinstall
ifndef PACKDIR
ifndef inUnix
PACKDIR=$(BASEDIR)/../fpc-pack
else
PACKDIR=/tmp/fpc-pack
endif
endif
ifndef ZIPNAME
ifdef DIST_ZIPNAME
ZIPNAME=$(DIST_ZIPNAME)
else
ZIPNAME=$(ZIPPREFIX)$(PACKAGE_NAME)$(ZIPSUFFIX)
endif
endif
ifndef ZIPTARGET
ifdef DIST_ZIPTARGET
ZIPTARGET=DIST_ZIPTARGET
else
ZIPTARGET=install
endif
endif
ifndef USEZIP
ifdef inUnix
USETAR=1
endif
endif
ifndef inUnix
USEZIPWRAPPER=1
endif
ifdef USEZIPWRAPPER
ZIPPATHSEP=$(PATHSEP)
ZIPWRAPPER=$(subst /,$(PATHSEP),$(DIST_DESTDIR)/fpczip$(BATCHEXT))
else
ZIPPATHSEP=/
endif
ZIPCMD_CDPACK:=cd $(subst /,$(ZIPPATHSEP),$(PACKDIR))
ZIPCMD_CDBASE:=cd $(subst /,$(ZIPPATHSEP),$(BASEDIR))
ifdef USETAR
ZIPDESTFILE:=$(DIST_DESTDIR)/$(ZIPNAME)$(TAREXT)
ZIPCMD_ZIP:=$(TARPROG) cf$(TAROPT) $(ZIPDESTFILE) *
else
ZIPDESTFILE:=$(DIST_DESTDIR)/$(ZIPNAME)$(ZIPEXT)
ZIPCMD_ZIP:=$(subst /,$(ZIPPATHSEP),$(ZIPPROG)) -Dr $(ZIPOPT) $(ZIPDESTFILE) *
endif
fpc_zipinstall:
	$(MAKE) $(ZIPTARGET) INSTALL_PREFIX=$(PACKDIR) ZIPINSTALL=1
	$(MKDIR) $(DIST_DESTDIR)
	$(DEL) $(ZIPDESTFILE)
ifdef USEZIPWRAPPER
ifneq ($(ECHOREDIR),echo)
	$(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CDPACK))" > $(ZIPWRAPPER)
	$(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_ZIP))" >> $(ZIPWRAPPER)
	$(ECHOREDIR) -e "$(subst \,\\,$(ZIPCMD_CDBASE))" >> $(ZIPWRAPPER)
else
	echo $(ZIPCMD_CDPACK) > $(ZIPWRAPPER)
	echo $(ZIPCMD_ZIP) >> $(ZIPWRAPPER)
	echo $(ZIPCMD_CDBASE) >> $(ZIPWRAPPER)
endif
ifdef inUnix
	/bin/sh $(ZIPWRAPPER)
else
	$(ZIPWRAPPER)
endif
	$(DEL) $(ZIPWRAPPER)
else
	$(ZIPCMD_CDPACK) ; $(ZIPCMD_ZIP) ; $(ZIPCMD_CDBASE)
endif
	$(DELTREE) $(PACKDIR)
fpc_zipsourceinstall:
	$(MAKE) fpc_zipinstall ZIPTARGET=sourceinstall ZIPSUFFIX=src
fpc_zipexampleinstall:
ifdef HASEXAMPLES
	$(MAKE) fpc_zipinstall ZIPTARGET=exampleinstall ZIPSUFFIX=exm
endif
fpc_zipdistinstall:
	$(MAKE) fpc_zipinstall ZIPTARGET=distinstall
.PHONY: fpc_clean fpc_cleanall fpc_distclean
ifdef EXEFILES
override CLEANEXEFILES:=$(addprefix $(TARGETDIRPREFIX),$(CLEANEXEFILES))
endif
ifdef CLEAN_UNITS
override CLEANPPUFILES+=$(addsuffix $(PPUEXT),$(CLEAN_UNITS))
endif
ifdef CLEANPPUFILES
override CLEANPPULINKFILES:=$(subst $(PPUEXT),$(OEXT),$(CLEANPPUFILES)) $(addprefix $(STATICLIBPREFIX),$(subst $(PPUEXT),$(STATICLIBEXT),$(CLEANPPUFILES)))
override CLEANPPUFILES:=$(addprefix $(UNITTARGETDIRPREFIX),$(CLEANPPUFILES))
override CLEANPPULINKFILES:=$(wildcard $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANPPULINKFILES)))
endif
fpc_clean: $(CLEANTARGET)
ifdef CLEANEXEFILES
	-$(DEL) $(CLEANEXEFILES)
endif
ifdef CLEANPPUFILES
	-$(DEL) $(CLEANPPUFILES)
endif
ifneq ($(CLEANPPULINKFILES),)
	-$(DEL) $(CLEANPPULINKFILES)
endif
ifdef CLEANRSTFILES
	-$(DEL) $(addprefix $(UNITTARGETDIRPREFIX),$(CLEANRSTFILES))
endif
ifdef CLEAN_FILES
	-$(DEL) $(CLEAN_FILES)
endif
ifdef LIB_NAME
	-$(DEL) $(LIB_NAME) $(LIB_FULLNAME)
endif
	-$(DEL) $(FPCMADE) Package.fpc $(PPAS) script.res link.res $(FPCEXTFILE) $(REDIRFILE)
fpc_distclean: clean
ifdef COMPILER_UNITTARGETDIR
TARGETDIRCLEAN=fpc_clean
endif
fpc_cleanall: $(CLEANTARGET) $(TARGETDIRCLEAN)
ifdef CLEANEXEFILES
	-$(DEL) $(CLEANEXEFILES)
endif
	-$(DEL) *$(OEXT) *$(PPUEXT) *$(RSTEXT) *$(ASMEXT) *$(STATICLIBEXT) *$(SHAREDLIBEXT) *$(PPLEXT)
	-$(DELTREE) *$(SMARTEXT)
	-$(DEL) $(FPCMADE) Package.fpc $(PPAS) script.res link.res $(FPCEXTFILE) $(REDIRFILE)
ifdef AOUTEXT
	-$(DEL) *$(AOUTEXT)
endif
.PHONY: fpc_baseinfo
override INFORULES+=fpc_baseinfo
fpc_baseinfo:
	@$(ECHO)
	@$(ECHO)  == Package info ==
	@$(ECHO)  Package Name..... $(PACKAGE_NAME)
	@$(ECHO)  Package Version.. $(PACKAGE_VERSION)
	@$(ECHO)
	@$(ECHO)  == Configuration info ==
	@$(ECHO)
	@$(ECHO)  FPC.......... $(FPC)
	@$(ECHO)  FPC Version.. $(FPC_VERSION)
	@$(ECHO)  Source CPU... $(CPU_SOURCE)
	@$(ECHO)  Target CPU... $(CPU_TARGET)
	@$(ECHO)  Source OS.... $(OS_SOURCE)
	@$(ECHO)  Target OS.... $(OS_TARGET)
	@$(ECHO)  Full Source.. $(FULL_SOURCE)
	@$(ECHO)  Full Target.. $(FULL_TARGET)
	@$(ECHO)
	@$(ECHO)  == Directory info ==
	@$(ECHO)
	@$(ECHO)  Required pkgs... $(REQUIRE_PACKAGES)
	@$(ECHO)
	@$(ECHO)  Basedir......... $(BASEDIR)
	@$(ECHO)  FPCDir.......... $(FPCDIR)
	@$(ECHO)  CrossBinDir..... $(CROSSBINDIR)
	@$(ECHO)  UnitsDir........ $(UNITSDIR)
	@$(ECHO)  PackagesDir..... $(PACKAGESDIR)
	@$(ECHO)
	@$(ECHO)  GCC library..... $(GCCLIBDIR)
	@$(ECHO)  Other library... $(OTHERLIBDIR)
	@$(ECHO)
	@$(ECHO)  == Tools info ==
	@$(ECHO)
	@$(ECHO)  As........ $(AS)
	@$(ECHO)  Ld........ $(LD)
	@$(ECHO)  Ar........ $(AR)
	@$(ECHO)  Rc........ $(RC)
	@$(ECHO)
	@$(ECHO)  Mv........ $(MVPROG)
	@$(ECHO)  Cp........ $(CPPROG)
	@$(ECHO)  Rm........ $(RMPROG)
	@$(ECHO)  GInstall.. $(GINSTALL)
	@$(ECHO)  Echo...... $(ECHO)
	@$(ECHO)  Shell..... $(SHELL)
	@$(ECHO)  Date...... $(DATE)
	@$(ECHO)  FPCMake... $(FPCMAKE)
	@$(ECHO)  PPUMove... $(PPUMOVE)
	@$(ECHO)  Upx....... $(UPXPROG)
	@$(ECHO)  Zip....... $(ZIPPROG)
	@$(ECHO)
	@$(ECHO)  == Object info ==
	@$(ECHO)
	@$(ECHO)  Target Loaders........ $(TARGET_LOADERS)
	@$(ECHO)  Target Units.......... $(TARGET_UNITS)
	@$(ECHO)  Target Implicit Units. $(TARGET_IMPLICITUNITS)
	@$(ECHO)  Target Programs....... $(TARGET_PROGRAMS)
	@$(ECHO)  Target Dirs........... $(TARGET_DIRS)
	@$(ECHO)  Target Examples....... $(TARGET_EXAMPLES)
	@$(ECHO)  Target ExampleDirs.... $(TARGET_EXAMPLEDIRS)
	@$(ECHO)
	@$(ECHO)  Clean Units......... $(CLEAN_UNITS)
	@$(ECHO)  Clean Files......... $(CLEAN_FILES)
	@$(ECHO)
	@$(ECHO)  Install Units....... $(INSTALL_UNITS)
	@$(ECHO)  Install Files....... $(INSTALL_FILES)
	@$(ECHO)
	@$(ECHO)  == Install info ==
	@$(ECHO)
	@$(ECHO)  DateStr.............. $(DATESTR)
	@$(ECHO)  ZipPrefix............ $(ZIPPREFIX)
	@$(ECHO)  ZipSuffix............ $(ZIPSUFFIX)
	@$(ECHO)  Install FPC Package.. $(INSTALL_FPCPACKAGE)
	@$(ECHO)
	@$(ECHO)  Install base dir..... $(INSTALL_BASEDIR)
	@$(ECHO)  Install binary dir... $(INSTALL_BINDIR)
	@$(ECHO)  Install library dir.. $(INSTALL_LIBDIR)
	@$(ECHO)  Install units dir.... $(INSTALL_UNITDIR)
	@$(ECHO)  Install source dir... $(INSTALL_SOURCEDIR)
	@$(ECHO)  Install doc dir...... $(INSTALL_DOCDIR)
	@$(ECHO)  Install example dir.. $(INSTALL_EXAMPLEDIR)
	@$(ECHO)  Install data dir..... $(INSTALL_DATADIR)
	@$(ECHO)
	@$(ECHO)  Dist destination dir. $(DIST_DESTDIR)
	@$(ECHO)  Dist zip name........ $(DIST_ZIPNAME)
	@$(ECHO)
.PHONY: fpc_info
fpc_info: $(INFORULES)
.PHONY: fpc_makefile fpc_makefiles fpc_makefile_sub1 fpc_makefile_sub2 \
	fpc_makefile_dirs
fpc_makefile:
	$(FPCMAKE) -w -T$(OS_TARGET) Makefile.fpc
fpc_makefile_sub1:
ifdef TARGET_DIRS
	$(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGET_DIRS))
endif
ifdef TARGET_EXAMPLEDIRS
	$(FPCMAKE) -w -T$(OS_TARGET) $(addsuffix /Makefile.fpc,$(TARGET_EXAMPLEDIRS))
endif
fpc_makefile_sub2: $(addsuffix _makefile_dirs,$(TARGET_DIRS) $(TARGET_EXAMPLEDIRS))
fpc_makefile_dirs: fpc_makefile_sub1 fpc_makefile_sub2
fpc_makefiles: fpc_makefile fpc_makefile_dirs
TARGET_DIRS_INTERFACES=1
ifdef TARGET_DIRS_INTERFACES
interfaces_all:
	$(MAKE) -C interfaces all
interfaces_debug:
	$(MAKE) -C interfaces debug
interfaces_smart:
	$(MAKE) -C interfaces smart
interfaces_release:
	$(MAKE) -C interfaces release
interfaces_examples:
	$(MAKE) -C interfaces examples
interfaces_shared:
	$(MAKE) -C interfaces shared
interfaces_install:
	$(MAKE) -C interfaces install
interfaces_sourceinstall:
	$(MAKE) -C interfaces sourceinstall
interfaces_exampleinstall:
	$(MAKE) -C interfaces exampleinstall
interfaces_distinstall:
	$(MAKE) -C interfaces distinstall
interfaces_zipinstall:
	$(MAKE) -C interfaces zipinstall
interfaces_zipsourceinstall:
	$(MAKE) -C interfaces zipsourceinstall
interfaces_zipexampleinstall:
	$(MAKE) -C interfaces zipexampleinstall
interfaces_zipdistinstall:
	$(MAKE) -C interfaces zipdistinstall
interfaces_clean:
	$(MAKE) -C interfaces clean
interfaces_distclean:
	$(MAKE) -C interfaces distclean
interfaces_cleanall:
	$(MAKE) -C interfaces cleanall
interfaces_info:
	$(MAKE) -C interfaces info
interfaces_makefiles:
	$(MAKE) -C interfaces makefiles
interfaces:
	$(MAKE) -C interfaces all
.PHONY: interfaces_all interfaces_debug interfaces_smart interfaces_release interfaces_examples interfaces_shared interfaces_install interfaces_sourceinstall interfaces_exampleinstall interfaces_distinstall interfaces_zipinstall interfaces_zipsourceinstall interfaces_zipexampleinstall interfaces_zipdistinstall interfaces_clean interfaces_distclean interfaces_cleanall interfaces_info interfaces_makefiles interfaces
endif
debug: fpc_debug
smart: fpc_smart
release: fpc_release
examples: $(addsuffix _examples,$(TARGET_DIRS))
shared: $(addsuffix _shared,$(TARGET_DIRS))
install: fpc_install $(addsuffix _install,$(TARGET_DIRS))
sourceinstall: fpc_sourceinstall
exampleinstall: fpc_exampleinstall $(addsuffix _exampleinstall,$(TARGET_DIRS))
distinstall: fpc_distinstall
zipinstall: fpc_zipinstall
zipsourceinstall: fpc_zipsourceinstall
zipexampleinstall: fpc_zipexampleinstall $(addsuffix _zipexampleinstall,$(TARGET_DIRS))
zipdistinstall: fpc_zipdistinstall $(addsuffix _zipdistinstall,$(TARGET_DIRS))
clean: fpc_clean $(addsuffix _clean,$(TARGET_DIRS))
distclean: fpc_distclean $(addsuffix _distclean,$(TARGET_DIRS))
cleanall: fpc_cleanall $(addsuffix _cleanall,$(TARGET_DIRS))
info: fpc_info
makefiles: fpc_makefiles
.PHONY: debug smart release examples shared install sourceinstall exampleinstall distinstall zipinstall zipsourceinstall zipexampleinstall zipdistinstall clean distclean cleanall info makefiles
ifneq ($(wildcard fpcmake.loc),)
include fpcmake.loc
endif
.PHONY: cleartarget all
cleartarget:
	-$(DEL) $(COMPILER_UNITTARGETDIR)/allunits$(PPUEXT)
all: cleartarget allunits$(PPUEXT) $(TARGET_DIRS)
