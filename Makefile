.PHONY: clean build format

ps-sources := $(shell fd --no-ignore-parent -epurs)
nix-sources := $(shell fd --no-ignore-parent -enix --exclude='spago*')
purs-args := "--stash --censor-lib --censor-codes=ImplicitImport,ImplicitQualifiedImport,ImplicitQualifiedImportReExport,UserDefinedWarning"

requires-nix-shell:
	@[ "$(IN_NIX_SHELL)" ] || \
		( echo "The '$(MAKECMDGOALS)' target must be run from inside a nix shell, run 'nix develop' first." \
				&& false \
		)

clean:
	rm -r output

build: requires-nix-shell
	spago build --purs-args ${purs-args}

format: requires-nix-shell
	@echo '1. Formatting PureScript sources:'
	purs-tidy format-in-place ${ps-sources}
	@echo -e '\n2. Formatting Nix sources:'
	nixpkgs-fmt ${nix-sources}
	@echo -e '\n3. Generating table of contents for Markdown files:'
	doctoc README.md --github --notitle
