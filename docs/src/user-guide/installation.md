# Installation

Currently, the only way to install Komposition is from source. It can be built
on macOS, Windows, and Linux. If you're not an experienced Haskell developer,
it's recommended to use
[Stack](https://docs.haskellstack.org/en/stable/README/) to build the
application. The following instructions will be based on Stack, so go ahead
and [install that first](https://docs.haskellstack.org/en/stable/README/#how-to-install).

!!! note "Using another build tool"
    If you know your way around building Haskell programs, you might want to
    build it using Nix or regular Cabal, instead.

## Getting the Source Code

Next, clone the source code repository using Git.

```shell
git clone https://github.com/owickstrom/komposition.git
cd komposition
```

Alternatively, if you're not using Git, download a [ZIP
archive](https://github.com/owickstrom/komposition/archive/master.zip):

```shell
wget https://github.com/owickstrom/komposition/archive/master.zip -O komposition-master.zip
unzip komposition-master.zip
cd komposition-master
```

You now have the source code. Jump on to the instructions below specific to
your operating system.

## Linux

First, install the required dependencies:

```shell
sudo apt-get install \
    ffmpeg \
    sox \
    libgmp-dev \
    libavutil-dev \
    libavformat-dev \
    libavcodec-dev \
    libswscale-dev \
    libavdevice-dev \
    libgirepository1.0-dev \
    libgtk-3-dev \
    libpango1.0-dev \
    libgdk-pixbuf2.0-dev \
    libgstreamer1.0-dev \
    gstreamer1.0-libav \
    gstreamer1.0-gtk3 \
    gstreamer1.0-plugins-base \
    gstreamer1.0-plugins-good \
    gstreamer1.0-plugins-bad
```

!!! warning
    If you find additional packages that needs to be installed, please [submit
    an issue on GitHub](https://github.com/owickstrom/komposition).

Next, build and install the application using Stack:

```shell
stack install
```

You should now have Komposition available:

```shell
~/.local/bin/komposition
```

If you have added `~/.local/bin` to your `PATH`, run:

```shell
komposition
```

## macOS

```shell
brew install pkg-config gobject-introspection gtk+3 ffmpeg sox gstreamer  gst-plugins-base gst-libav
brew install gst-plugins-good --with-gtk+3

export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"

# if you get an error in the next step about 'happy' not being on your
# PATH, run this command first:
stack build happy
stack install
```

## Windows

*These instructions will be available soon.*

```
# something like this...
pacman -S mingw-w64-x86_64-gstreamer mingw-w64-x86_64-gst-libav mingw-w64-x86_64-gst-plugins-{base,good,bad}
```

## Nix/NixOS

Komposition is not yet in [nixpkgs](https://github.com/NixOS/nixpkgs), but it
can be installed with Nix from an archive on GitHub.

First, consider installing [Cachix](https://cachix.org/) and using the
Komposition binary cache. It's not strictly required, but will save you time
waiting on compilation.

```shell
cachix use komposition
```

Next, use `nix-env` to install Komposition from the `master` branch:

```shell
nix-env -iA komposition -f https://github.com/owickstrom/komposition/archive/master.tar.gz
```

Run it from the command line:

```shell
komposition
```
