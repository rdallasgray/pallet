PROJECT_LCNAME=pallet
include el.mk/el.mk

test:
	@cask exec ert-runner
