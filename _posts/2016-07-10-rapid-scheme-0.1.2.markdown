---
layout: post
title:  "Rapid Scheme 0.1.2"
date:   2016-07-10 11:13:05 +0200
categories: rapid-scheme release update
---

Rapid Scheme has received a second middle-end pass, which performs a
modified version of the fixing-letrec algorithm by R. Kent Dybvig and
Abdulaziz Ghuloum.  This pass ensures that recursive definitions are
restricted to procedures (via letrec). All other values are bound with
let-values.
