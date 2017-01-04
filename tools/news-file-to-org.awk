# news-file-to-org.awk - Build a history org file from the NEWS file
# Copyright (C) 2006, 2017 g10 Code GmbH
#
# This file is free software; as a special exception the author gives
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Empty lines as well as lines starting with a hash mark are entirly
# ignored.  Paragraphs are indicated by a "*" marker.  A new revision
# history starts with the "Noteworthy .." line; the release date is
# expected in parentheses on that line.  If it is not given the
# section won't be rendered.
#
# This script is based on build-history.sh from gpg4win but stripped
# down and changed from M4 output to org-mode output.  As of now it
# works only with GnuPG.


BEGIN {
    if ( lang == "" )
        lang = "en";
    in_section = 0;
    in_para = 0;
    in_vers = 0;
    any_para = 0;
    version = "";
    reldate = "";
    lines = "";
    header = ""
    seealso = ""

    ml_base_url = "https://lists.gnupg.org/pipermail/"

    header_text["en"] = ""                      \
        "** GnuPG\n" ;

    header_text["de"] = ""                      \
        "** GnuPG\n" ;

    release_text["en"] = "released ";
    release_text["de"] = "veröffentlicht ";
    noreldate_text["en"] = "[ in progress; not yet released ]";
    noreldate_text["de"] = "[ in Arbeit; bisher noch nicht veröffentlicht ]";
    explicit_dl_text["en"] = "Explicit download of this version:";
    explicit_dl_text["de"] = "Expliziter Download dieser Version:";
    readme_text["en"] = "Details in the README of this version:";
    readme_text["de"] = "Details im README dieser Version:";

    print header_text[lang];
}

function flush() {
    if (in_section) {
        printf "*** GnuPG "
        if (seealso)
            printf "[[%s%s][%s]]", ml_base_url, seealso, hdr_version
        else
            printf "%s", hdr_version
        printf " %s (%s)\n", release_text[lang], hdr_reldate;
        printf ""                                             \
            "    :PROPERTIES:\n"                              \
        "    :CUSTOM_ID: gnupg-%s\n"                      \
        "    :END:\n\n",
            hdr_version;
        print lines;
    }
    lines = ""
    seealso = ""
    in_section = 0;
    in_para = 0;
    in_vers = 0;
    any_para = 0;
}


/^#/   { next }
/^---/ { next }

in_section && $0 ~ /^(Noteworthy|Version| Copyright)/ {
    if (in_vers)
        lines = lines "#end_example";
    flush()
}


# Handle single Version lines used to announce a release date,
!in_section && $0 ~ /^Version/ {
    version = $2;
    reldate = "";
    if (index ($0, "(")) {
        sub (/^.*\(/, "");
        sub (/\).*$/, "");
        reldate = $0;
        print "*** GnuPG " version " " release_text[lang] " (" reldate ")";
        printf "    :PROPERTIES:\n    :CUSTOM_ID: gnupg-%s\n", version;
        print "    :END:\n"
}
    next;
}

!in_section {
    if ($0 !~ /^Noteworthy/)
        next;
    if ( ! index ($0, "("))
        next;
    version = $5;
    reldate = "";
    sub (/^.*\(/, "");
    sub (/\).*$/, "");
    reldate = $0;
    if (reldate ~ "unreleased")
        next;
    hdr_version = version
    hdr_reldate = reldate
    in_section = 1;
    lines = "";
    in_para = 0;
    in_vers = 0;
    any_para = 0;

    next;
}

in_section && $0 ~ /^[ ]+[*] / {
    in_para = 1;
    any_para = 1;
    indent_para = index($0, "*") + 2;
    lines = lines sprintf("    - %s\n", substr ($0, indent_para));
    next;
}


# Handle beta version announcement inside a section
in_section && $0 ~ /^[ ]+\[Noteworthy changes in version / {
    any_para = 1;
    lines = lines sprintf("    %s\n", substr ($0, index($0, "[")));
    next;
}


# Handle See-also lines inside a section
in_section && $0 ~ /^[ ]+\See-also: [a-z]+/ {
    seealso = $2;
    next;
}


# We don't use the next in GnuPG's NEWS, but lets support it anyway.
in_section && !in_vers && /^~~~/ {
    if ( in_para ) {
        in_para = 0;
    }
    in_para = 0;
    in_vers = 1;
    lines = lines "#begin_example"
    next;
}

in_para {
    lines = lines sprintf("      %s\n", substr ($0, indent_para));
}

in_vers && /^~~~/ {
    in_vers = 0;
    lines = lines "#end_example"
}

in_vers {
    lines = lines $0;
}


END {
    flush()
}
