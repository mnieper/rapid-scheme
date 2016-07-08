---
layout: page
title: About
permalink: /about/
---

Rapid Scheme is an implementation of the Scheme programming language
as described by the
[R7RS](http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf).

As Rapid Scheme is written in portable Scheme code itself, it depends
on the existence of a working implementation of the R7RS.

One may ask what the purpose of an implementation of R7RS is if one
already needs an implementation of R7RS in order to use it.

There are actually several reasons for the existence of Rapid Scheme:

  * Rapid Scheme outputs any error detected during expansion in a
    standard format (which is also used by gcc and consumable by
    Emacs) with exact line and column numbers of the erroneous
    construct including the context due to library imports, file
    inclusion and macro expansion.

  * Rapid Scheme tries to detect as many errors as possible. In
    particular, it helps writing portable code.

  * Rapid Scheme can and will be used to prototype syntactic
    extensions to the R7RS. This may come handy in sample
    implementations of SRFIs, in particular those envisaged for
    inclusion in R7RS-large.

  * Compiler writers can use Rapid Scheme as a front-end of a
    standalone implementation of R7RS. In particular, it should be
    relatively easy to combine an existing implementation of an
    earlier report such as R5RS with Rapid Scheme to produce a
    stand-alone implementation of R7RS.

  * Rapid Scheme itself may one day receive a back-end and become
    a stand-alone implementation.

  * Rapid Scheme is a proof that one can write non-trivial programs
    with the language as described by the R7RS. The development of
    Rapid Scheme helped to find a number of bugs in existing
    implementations of the R7RS, which are now fixed.

  * Each new implementation of R7RS is a proof that the R7RS has
    fulfilled its goals.

You can find the source code for Rapid Scheme at:
{% include icon-gitlab.html username="nieper" %} /
[rapid-scheme](https://gitlab.com/nieper/rapid-scheme)
