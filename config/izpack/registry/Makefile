##############################################################################

BINDIR			 = bin
SOURCEDIR		 = src
OUTPUTDIR		 = classes

JAVA_FILES		 = Main
JAVA_SOURCES		+= $(JAVA_FILES:%=$(SOURCEDIR)/%.java)

APP_MAIN		 = Main
APP_ARGS		 =

CYGWIN			 = $(filter CYGWIN%,$(shell uname))
CYGPATH			 = $(if $(CYGWIN),`cygpath -wp $(1)`,$(1))

REGISTRY_HOME		?= /home/linuxsoft/apps/registry
REGISTRY_JARFILE	 = $(REGISTRY_HOME)/bin/registry.jar

JAVA_HOME		 = /home/linuxsoft/apps/java-1.4

JAVAC			?= $(JAVA_HOME)/bin/javac
JAVAC_OPTIONS		+= -classpath $(call CYGPATH,$(REGISTRY_JARFILE))
JAVAC_OPTIONS		+= -d $(OUTPUTDIR)

JAR			?= $(JAVA_HOME)/bin/jar
JAR_OPTIONS		+= 
SETENV_JARFILE		 = $(BINDIR)/setenv.jar

JAVA			+= $(JAVA_HOME)/bin/java
JAVA_OPTIONS		+= -cp $(call CYGPATH,$(REGISTRY_JARFILE):$(OUTPUTDIR))
JAVA_OPTIONS		+= -Djava.library.path=$(call CYGPATH,$(REGISTRY_HOME)/bin)

##############################################################################

MKDIR			 = mkdir -p
RM			 = rm -f
TOUCH			 = touch

##############################################################################
# Commands

all: .latest-jc

clean:
	@$(RM) -r $(OUTPUTDIR) .latest-*

run: all
	@$(JAVA) $(JAVA_OPTIONS) $(APP_MAIN) $(APP_ARGS)

.PHONY: all
.PHONY: clean
.PHONY: run

##############################################################################
# Rules

.latest-jc: $(JAVA_SOURCES)
	@[ -d "$(OUTPUTDIR)" ] || $(MKDIR) $(OUTPUTDIR)
	$(JAVAC) $(JAVAC_OPTIONS) $<
	$(JAR) cfm $(SETENV_JARFILE) \
	    src/MANIFEST.MF -C $(OUTPUTDIR) \
	    $(APP_MAIN).class
	@$(TOUCH) $@

##############################################################################
