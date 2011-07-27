#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

CXX = c++
CC = gcc
LIBS = -lutil -ldl -lm  

OBJDIR = ../priv/obj/$(TARGET)

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(ORBER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/orber-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
CC_FILES = \
	InitialReference.cc

HH_FILES = \
	InitialReference.hh

ALL_CFLAGS = -g -O2 -I/home/jon/Lisp-On-Yaws/3rdparty/yaws/erts/i686-pc-linux-gnu  -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 -fno-tree-copyrename  -D_GNU_SOURCE -DHAVE_CONFIG_H $(CFLAGS) 

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(OBJDIR) orber

ifeq ($(findstring win32,$(TARGET)),win32)
orber: 
	echo "Nothing to build on NT"
else
ifeq ($(findstring vxworks,$(TARGET)),vxworks)
orber: 
	echo "Nothing to build for VxWorks"

else
orber: 
	echo "Nothing to build"	
endif
endif

clean:

docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(OBJDIR):
	-mkdir -p $(OBJDIR)

$(OBJDIR)/%.o: %.c
	$(CC) -c -o $@ $(ALL_CFLAGS) $<

# ----------------------------------------------------
# Release Target
# ---------------------------------------------------- 
include $(ERL_TOP)/make/otp_release_targets.mk

ifeq ($(findstring win32,$(TARGET)),win32)
release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv/src
	$(INSTALL_DIR) $(RELSYSDIR)/priv/include
	$(INSTALL_PROGRAM) $(CC_FILES) $(RELSYSDIR)/priv/src
	$(INSTALL_PROGRAM) $(HH_FILES) $(RELSYSDIR)/priv/include
else
ifeq ($(findstring vxworks,$(TARGET)),vxworks)
release_spec:
	$(INSTALL_DIR) $(RELSYSDIR)/priv/src
	$(INSTALL_DIR) $(RELSYSDIR)/priv/include
	$(INSTALL_PROGRAM) $(CC_FILES) $(RELSYSDIR)/priv/src
	$(INSTALL_PROGRAM) $(HH_FILES) $(RELSYSDIR)/priv/include
else
release_spec: opt
	$(INSTALL_DIR) $(RELSYSDIR)/priv/src
	$(INSTALL_DIR) $(RELSYSDIR)/priv/include
	$(INSTALL_DATA) $(CC_FILES) $(RELSYSDIR)/priv/src
	$(INSTALL_DATA) $(HH_FILES) $(RELSYSDIR)/priv/include
endif
endif


release_docs_spec:
