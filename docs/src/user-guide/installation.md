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
    libgirepository1.0-dev \
    gstreamer1.0-gtk3 \
    gstreamer1.0-libav \
    libgstreamer1.0-0 \
    libgstreamer-plugins-base1.0-dev \
    libgstreamer-plugins-good1.0-dev \
    libgstreamer-plugins-bad1.0-dev
```

!!! warning
    These might not be all the required dependencies. If you find
    more that needs to be installed, please [submit an issue on
    GitHub](https://github.com/owickstrom/komposition).

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

*These instructions will be available soon.*

## Windows

*These instructions will be available soon.*

```
# something like this...
pacman -S mingw-w64-x86_64-gstreamer mingw-w64-x86_64-gst-libav mingw-w64-x86_64-gst-plugins-{base,good,bad}
```
