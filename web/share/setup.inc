# Setup file for this site.
#
# Include the version macros.
# FIXME: Figure out a way to use SETUPFILE based on the directory of
#        the currently SETUPFILE loaded file.
#+SETUPFILE: "/home/wk/s/gnupg-doc/web/swdb.mac"


# Org version 8 does not anymore expand macros in all places.  We use
# some ugly macros instead of the #+begin_example et al.

#+macro: begin_example @@html:<pre class="example">@@
#+macro: end_example   @@html:</pre>@@
#+macro: begin_chksum  @@html:<pre class="example">@@
#+macro: end_chksum    @@html:</pre>@@

#+macro: twodashes     @@html:--@@
