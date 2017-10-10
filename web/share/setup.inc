# Setup file for this site.
#
# Note: To include swdb.org in an org file add the line
#   #+GPGWEB-NEED-SWDB
# in the first 2k of the file.  It is not possible to include that file
# from here because we don't have a way to specify gpgweb-root-dir as
# part of the filename.  A dedicated pre-process feature like above is
# probably more efficient anyway.


# Org version 8 does not anymore expand macros in all places.  We use
# some ugly macros instead of the #+begin_example et al.

#+macro: begin_example @@html:<pre class="example">@@
#+macro: end_example   @@html:</pre>@@
#+macro: begin_chksum  @@html:<pre class="example">@@
#+macro: end_chksum    @@html:</pre>@@

#+macro: twodashes     @@html:--@@
