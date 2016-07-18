---
layout: post
title:  "Rapid Scheme 0.1.3"
date:   2016-07-18 19:33:00 +0200
categories: rapid-scheme release update
---

A compiled program now uses Rapid Scheme's own reader instead of using
the read procedure of a backend.  This makes it easier to use Rapid
Scheme as an R7RS frontend with a backend that does not fully
implement the syntax of R7RS.
