#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1999-2011. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%
#

#
# Makefile only for Unix and Win32/Cygwin.
# 

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk
# ----------------------------------------------------
# SSL locations and include options from configure
# ----------------------------------------------------
SSL_LIBDIR = /usr/lib
SSL_INCLUDE = -I/usr/include
SSL_CRYPTO_LIBNAME = crypto
SSL_SSL_LIBNAME = ssl

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(SSL_VSN)

# ----------------------------------------------------
# Commands 
# ----------------------------------------------------
CC = gcc
LD = $(CC)
SHELL = /bin/sh
LIBS = -lutil -ldl -lm  
PLAIN_CFLAGS = -g -O2 -I/home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/i686-pc-linux-gnu  -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 -fno-tree-copyrename  -D_GNU_SOURCE

# ----------------------------------------------------
# Includes and libs
# ----------------------------------------------------

ALL_CFLAGS = -Wall -Wstrict-prototypes -Wmissing-prototypes -Wdeclaration-after-statement -g -O2 -I/home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/i686-pc-linux-gnu  -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 -fno-tree-copyrename  -D_GNU_SOURCE -DHAVE_CONFIG_H $(TYPE_FLAGS)
TARGET = i686-pc-linux-gnu

ifeq ($(TYPE),debug)
TYPEMARKER = .debug
TYPE_FLAGS = -g -DDEBUG -g
else
TYPEMARKER =
TYPE_FLAGS = -O2
endif

PRIVDIR = ../priv
BINDIR = $(PRIVDIR)/bin/$(TARGET)
OBJDIR = $(PRIVDIR)/obj/$(TARGET)

# ----------------------------------------------------
# File suffixes
# ----------------------------------------------------
exe = 
obj = .o

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/ssl-$(VSN)

# ----------------------------------------------------
# Common Macros
# ----------------------------------------------------
OBJS = 	$(OBJDIR)/esock$(obj) \
	$(OBJDIR)/debuglog$(obj) \
	$(OBJDIR)/esock_poll$(obj) \
	$(OBJDIR)/esock_osio$(obj) \
	$(OBJDIR)/esock_utils$(obj) \
	$(OBJDIR)/esock_posix_str$(obj) \
	$(OBJDIR)/esock_openssl$(obj)

PORT_PROGRAM = $(BINDIR)/ssl_esock$(exe)

SKIP_BUILDING_BINARIES := false

# Try to be BC for R10
ifeq ($(findstring @SSL_,yes),@SSL_)
DYNAMIC_CRYPTO_LIB=yes
else
DYNAMIC_CRYPTO_LIB=yes
endif


ifeq ($(DYNAMIC_CRYPTO_LIB),yes)

ifneq ($(findstring win32,$(TARGET)),win32)
SSL_MAKEFILE = $(OBJDIR)/Makefile
else
SSL_MAKEFILE =
endif

CC_R_FLAG=-Wl,-R

ifeq ($(findstring @,$(CC_R_FLAG)),@)
# Old erts configure used which hasn't replaced -Wl,-R;
# we try our best here instead...

ifeq ($(findstring darwin,$(TARGET)),darwin)	# darwin: no flag
CC_R_FLAG =
else
ifeq ($(findstring osf,$(TARGET)),osf)		# osf1: -Wl,-rpath,
CC_R_FLAG = -Wl,-rpath,
else						# Default: -Wl,-R
CC_R_FLAG = -Wl,-R
endif
endif
endif

ifeq ($(strip $(CC_R_FLAG)),)
CC_R_OPT =
else
CC_R_OPT = $(CC_R_FLAG)$(SSL_LIBDIR)
endif

SSL_CC_RUNTIME_LIBRARY_PATH=-Wl,-R/usr/lib -Wl,-R/usr/local/lib -Wl,-R/usr/sfw/lib -Wl,-R/opt/local/lib -Wl,-R/usr/pkg/lib -Wl,-R/usr/local/openssl/lib -Wl,-R/usr/lib/openssl/lib -Wl,-R/usr/openssl/lib -Wl,-R/usr/local/ssl/lib -Wl,-R/usr/lib/ssl/lib -Wl,-R/usr/ssl/lib
# Sigh...
ifeq ($(findstring @,$(SSL_CC_RUNTIME_LIBRARY_PATH)),@)
SSL_CC_RUNTIME_LIBRARY_PATH = $(CC_R_OPT)
endif

SSL_LINK_LIB=-L$(SSL_LIBDIR) -l$(SSL_SSL_LIBNAME) -l$(SSL_CRYPTO_LIBNAME)
else 
# not dynamic crypto lib (default from R11B-5)
NEED_KERBEROS=
NEED_ZLIB=no
SSL_MAKEFILE =
CC_R_OPT =
SSL_CC_RUNTIME_LIBRARY_PATH=
SSL_LINK_LIB = $(SSL_LIBDIR)/lib$(SSL_SSL_LIBNAME).a $(SSL_LIBDIR)/lib$(SSL_CRYPTO_LIBNAME).a 
ifeq ($(NEED_KERBEROS),yes)
SSL_LINK_LIB += 
endif
ifeq ($(NEED_ZLIB),yes)
SSL_LINK_LIB += 
endif
endif

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(OBJDIR) $(BINDIR) $(OBJS) $(PORT_PROGRAM) $(SSL_MAKEFILE)

$(OBJDIR):
	-@mkdir -p $(OBJDIR)

$(BINDIR):
	-@mkdir -p $(BINDIR)

$(OBJDIR)/esock_openssl$(obj):	esock_openssl.c
	$(CC) -c -o $@ $(ALL_CFLAGS) $(SSL_INCLUDE) $<

$(OBJDIR)/%$(obj): %.c
	$(CC) -c -o $@ $(ALL_CFLAGS) $<

# Unix
$(BINDIR)/ssl_esock: $(OBJS)
	$(CC) $(PLAIN_CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS) $(SSL_CC_RUNTIME_LIBRARY_PATH) $(SSL_LINK_LIB)

# Win32/Cygwin
$(BINDIR)/ssl_esock.exe: $(OBJS)
	$(LD) $(SSL_CC_RUNTIME_LIBRARY_PATH) -L$(SSL_LIBDIR) -o $@ $^ -lwsock32 -l$(SSL_CRYPTO_LIBNAME) -l$(SSL_SSL_LIBNAME)

# Unix only, and only when linking statically
$(SSL_MAKEFILE):
	sed 	-e "s;%BINDIR%;../../bin/$(TARGET);" \
		-e "s;%SSL_LIBDIR%;$(SSL_LIBDIR);" \
		-e "s;%OBJS;$(OBJS);" \
		-e "s;%LIBS%;$(LIBS);" ./Makefile.dist \
		> $(OBJDIR)/Makefile


clean:
	rm -f $(PORT_PROGRAM) $(OBJS) core *~ $(SSL_MAKEFILE)

docs:

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv/bin
	$(INSTALL_PROGRAM) $(PORT_PROGRAM) $(RELSYSDIR)/priv/bin
ifneq ($(SSL_MAKEFILE),)
	$(INSTALL_DIR) $(RELSYSDIR)/priv/obj
	$(INSTALL_DATA) $(OBJS) $(RELSYSDIR)/priv/obj
	sed 	-e "s;%BINDIR%;../bin;" \
		-e "s;%SSL_LIBDIR%;$(SSL_LIBDIR);" \
		-e "s;%OBJS;$(OBJS);" \
		-e "s;%LIBS%;$(LIBS);" ./Makefile.dist \
		> $(RELSYSDIR)/priv/obj/Makefile
endif

release_docs_spec:

