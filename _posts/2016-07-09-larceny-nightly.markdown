---
layout: post
title:  "Larceny Nightly Available"
date:   2016-07-09 18:33:00 +0200
categories: news example hacking
---

Rapid Scheme now works with Larceny nightly builds. No more
compilation of Larceny by hand is necessary. (The current release
builds of Larceny aren't R7RS-compliant enough to run Rapid Scheme.)

Here is a step-by-step explanation how to get Rapid Scheme running on
a fresh (Ubuntu) system:

1. Enter home directory:

   ```shell
   $ cd
   ```

2. Install necessary tools:

   ```shell
   $ sudo apt update
   $ sudo apt -y upgrade
   $ sudo apt -y install wget git make
   ```

3. Install 32-bit libraries needed by Larceny on 64-bit systems:

   ```shell
   $ sudo apt -y install lib32z1 libc6-i386
   ```

4. Download and install nightly build of Larceny:

   ```shell
   $ wget http://www.cesura17.net/~larcenists/Nightly/larceny-nightly-bin-native-ia32-linux86.tgz
   $ tar xzf larceny-nightly-bin-native-ia32-linux86.tgz
   $ ln -s larceny-nightly-bin-native-ia32-linux86 larceny
   ```

5. Fetch development version of Rapid Scheme:

   ```shell
   $ git clone https://gitlab.com/nieper/rapid-scheme.git
   ```

6. Build Rapid Scheme:

   ```shell
   $ cd rapid-scheme
   $ make LARCENY=$PWD/../larceny/larceny
   ```

7. Compile example program to stdout

   ```shell
   $ ./rapid-compiler -I share examples/hello-world.scm
   ```
  
8. Run example program:

   ```shell
   $ ./rapid-scheme -I share examples/hello-world.scm 
   ```

9. Run checks:

   ```shell
   $ make check
   ```
