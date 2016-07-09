---
layout: post
title:  "Rapid Scheme 0.1.1"
date:   2016-07-09 00:07:41 +0200
categories: rapid-scheme release update
---

Rapid Scheme has just got its first middle-end pass, which does simple
lambda lifting.  Procedures are maximally hoisted without changing
their set of free variables.  This pass also ensures that every
procedure is bound to an identifier and only referenced via this
identifier.
