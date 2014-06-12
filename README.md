This fork is for stack traces in Haskell. // Arash Rouhani June 2014

You can read about my master thesis here: http://arashrouhani.com/papers.html

# Stack traces

Below are instructions for building this branch. I've only used Ubuntu for this. I
know for sure that the current stack trace implementation only works on Linux.

## Preinstalls

Preinstallations (in addition to ghc)

```
sudo apt-get install ghc libelf-dev
sudo apt-get remove libdwarf-dev    # Yes, remove
```

Since the `libdwarf-dev` package is stupid and does not include
`/usr/lib/libdwarf.so` but only the `.a`, we have to compile it manually. Yes,
it sucks.

```
mkdir empty_folder
cd empty_folder
LATEST_LIBDWARF=http://www.prevanders.net/libdwarf-20140519.tar.gz
wget $LATEST_LIBDWARF
tar -zxvf *.tar.gz
cd dwarf*
./configure --enable-shared
make
sudo cp libdwarf/libdwarf.so /usr/local/lib
sudo cp libdwarf/libdwarf.h /usr/local/include
sudo cp libdwarf/dwarf.h /usr/local/include
```

If you don't remove the `libdwarf-dev` package, ghc will try to use the `.a`
file which is gonna give build failures.

## Checkout projects

Get subprojects.

```
git checkout master     # Just to make sure we are on it
git remote add tarrasch https://github.com/Tarrasch/ghc.git
git fetch tarrasch
git reset --hard tarrasch/master
git submodule update --recursive
utils/fingerprint/fingerprint.py restore --from-fp=./libraries.fp
git checkout master     # For some reason fingerprint switches branch ... 
cp build.mk.temp mk/build.mk

rm -rf libraries/dph  # It doesn't build with data parallel Haskell
```

Get Arash version of base (will hopefully be deprecated soon)

```
cd libraries/base
git remote add tarrasch https://github.com/Tarrasch/packages-base.git
git checkout master     # For some reason fingerprint switches branch ... 
git fetch tarrasch
git reset --hard tarrasch/master
```

## Build 

Now lets compile ourselves a ghc with stack traces

```
perl boot
./configure           # I have super-verbose output on failures, check for that
make -j10
```

## Run 

Now try it out

```
./inplace/bin/ghc-stage2 -g -rtsopts --make ./my/Main.hs
./my/Main +RTS --stack-trace -RTS
```

Hopefully you now see the stack trace when the program crashes! :)
