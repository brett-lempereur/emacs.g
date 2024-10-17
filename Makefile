###
### Makefile
###
### Collective building and management.
###

# Configuration
DRONES_DIR := user-package
BORG_CLEAN_ELN := true
INIT_FILES := early-init.el init.el $(wildcard user-lisp/*.el) \
			  $(wildcard user-machine/*.el)

# Include the main makefile.
include $(DRONES_DIR)/borg/borg.mk

##
## Bootstrap
##

bootstrap-borg:
	@git submodule--helper clone --name borg --path "$(DRONES_DIR)/borg" \
		--url git@github.com:emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD

##
## Maintenance
##

clean-all: clean
	@rm -rf eln-cache local auto-save-list
