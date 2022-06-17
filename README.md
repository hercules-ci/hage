
# `hage`

_A Haskell implementation of [age encryption](https://age-encryption.org/)_

Pronunciation: [/haːxə/](http://ipa-reader.xyz/?text=ha%CB%90x%C9%99&voice=Ruben), as in Dutch [_'s Gravenhage_](https://translate.google.com/?sl=nl&tl=en&text=%27s%20Gravenhage&op=translate).

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
