# upcast

> tl;dr: you really *need* upcast if you're using a Mac, like visiting Starbucks or want to avoid wasting your laptop's battery

Upcast builds Nix packages remotely by wrapping `nix-build` and `nix-copy-closure` to avoid copying store paths back and
forth under the usual [Nix Remote Builds](https://nixos.org/nix/manual/#chap-distributed-builds) semantics.

## Showcase

[![asciicast](https://asciinema.org/a/171654.png)](https://asciinema.org/a/171654)

`upcast build` and `upcast install` wrap around `nix-instantiate`/`nix-build`/`nix-copy-closure`.

Build NixOS configuration on `hydra` host and install it on `node1`.
Note that `upcast build` instantiates derivations locally and ships them to a remote host.
YMMV if you're using import-from-derivation.

```console
% upcast build -t hydra --nixos -A node1 network.nix \
    | xargs -tn1 upcast install -c ssh_config -f hydra -t node1 --switch
```

Same example without `upcast build` if you want to do it locally:

```console
% nix-build -A some-image infra.nix
% upcast install -c ssh_config -t node1 $(readlink ./result)
```

### Nix profile installs

Read more about Nix profiles [here](http://nixos.org/nix/manual/#sec-profiles).

Profiles are used by NixOS to pin a root system path to a known location.

You can use nix profiles to pin your own packages (or collection of packages using functions like 
[buildEnv](https://github.com/NixOS/nixpkgs/blob/d232390d5dc3dcf912e76ea160aea62f049918e1/pkgs/build-support/buildenv/default.nix)).

For example, install a random package and pin it to a `hello` profile (`target-instance` needs to be able to access `hydra`):

```bash
upcast build -t hydra -A my-package default.nix | xargs -n1t upcast install -f hydra -p /nix/var/nix/profiles/hello -t target-instance
```

## Performace Tips

### Enable OpenSSH `ControlMaster`

`ControlMaster` helps speed up subsequent ssh sessions by reusing a single TCP connection. See [ssh_config(5)](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man5/ssh_config.5?query=ssh_config).

```console
% cat ~/.ssh/config
Host *
    ControlPath ~/.ssh/master-%r@%h:%p
    ControlMaster auto
    ControlPersist yes
```

## History

A previous version of this app featured a homegrown AWS-only nixops clone and is located at [zalora/upcast](https://github.com/zalora/upcast).
