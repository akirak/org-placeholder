rice-flake := "github:emacs-twist/rice-config"

melpa := "github:akirak/melpa/akirak"

common-options := "--override-input rice-src \"path:$PWD\" --override-input melpa " + quote(melpa)

emacs-version := "emacs-release-snapshot"

package := "org-placeholder"

arch := shell('nix eval --expr builtins.currentSystem --impure --raw')

# Show the flake
show *OPTIONS:
    nix flake show {{ rice-flake }} {{ OPTIONS }} {{ common-options }}

# Evaluate an attribute on the flake, e.g. just eval melpaRecipes.
eval ATTR *OPTIONS:
    nix eval {{rice-flake}}\#{{ATTR}} {{OPTIONS}} {{ common-options }}

# Enter a shell for byte-compiling individual source files
shell-compile:
    nix develop {{ rice-flake }}\#{{ emacs-version }}-for-{{ package }} {{ common-options }}

# Re-run byte-compile every time a file is modified
watch-compile:
    nix develop {{ rice-flake }}\#{{ emacs-version }}-for-{{ package }} {{ common-options }} -c bash -c 'echo >&2 Watching *.el; ls *.el | entr -p elisp-byte-compile /_'

# Byte-compile the package
check-compile:
    nix build {{ rice-flake }}\#checks.{{ arch }}.{{ package }}-compile-{{ emacs-version }} {{ common-options }} --print-build-logs

# Enter a shell for running tests
shell-emacs *OPTIONS:
    nix shell {{ rice-flake }}\#{{ emacs-version }}-with-packages {{ common-options }} {{ OPTIONS }}
