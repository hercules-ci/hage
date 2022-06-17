
# `hage`

_A Haskell implementation of [age encryption](https://age-encryption.org/)_

## Status

Work in progress.

## Hacking

This repository provides its own tooling with a Nix flake, and it includes
VSCode configuration.

To run the tests with live reloading:

```console
# nix develop currently has an issue with shell completion for
# system commands such as nix, so we use nix-shell.
$ nix-shell
[nix-shell]$ live-tests
```

To test and build

```console
nix build .
```

To try the command in a shell

```console
nix develop .#try
```
